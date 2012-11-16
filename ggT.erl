-module(ggT).
-export([start/8, init/8]).
-record(config,
    {	teamID=14,
    	praktikumsgruppe=02,
	koordPID,
	arbeitszeit,
	termZeit,
	prozessNR,
	starterNR,
	namensDPID,
	myID,
	leftN,
	rightN,
	logfile,
	lastSendy}).

%-import(util, [log/3]).
%-define(Log(FromatS, ArgsS), werkzeug:logging("GGTP_" ++ Config#config.myID ++ "@" ++ net_adm:localhost() ++ ".log", nice_format(FormatS, ArgsS))).

nice_format(Format, Args) ->
	lists:flatten(io_lib:format("~s ~n",[lists:flatten(io_lib:format(Format, Args))])).	

gen_myid(PG, TID, PNR, SNR) ->
	lists:flatten(io_lib:format("~B~B~B~B", [ PG, TID, PNR, SNR ])).

% parameterreihenfolge nicht mehr �ndern
start(KoordName,Arbeitzeit, TermZeit,ProzessNR, StarterNR,NamensDPID, Praktikumsgruppe, TeamID) ->
	spawn(ggT, init, [KoordName,Arbeitzeit, TermZeit,ProzessNR, StarterNR,NamensDPID, Praktikumsgruppe, TeamID]).

resolve_neighbors(Config) ->
	% rechten nachbarnamen aufl�sen
	Config#config.namensDPID ! {self(),{lookup, Config#config.leftN}},
	LeftName = Config#config.leftN,
	receive
		not_found -> werkzeug:logging(Config#config.logfile, nice_format("~p Linker Nachbar ~p konnte nicht aufgelost werden.", [util:timestamp(),Config#config.leftN])), LeftNTupel = {};
		LeftNTupel = {LeftName, Node1} -> werkzeug:logging(Config#config.logfile, nice_format("~p Linker Nachbar ~p befindet sich auf node ~p.", [util:timestamp(),LeftName, Node1]))
	end,

	% linken nachbarnamen aufl�sen
	Config#config.namensDPID ! {self(),{lookup, Config#config.rightN}},
	RightName = Config#config.rightN,
	receive
		not_found -> werkzeug:logging(Config#config.logfile, nice_format("~p Rechter Nachbar ~p konnte nicht aufgelost werden.", [util:timestamp(),Config#config.rightN])), RightNTupel = {};
		RightNTupel = {RightName, Node2} -> werkzeug:logging(Config#config.logfile, nice_format("~p Rechter Nachbar ~p befindet sich auf node ~p.", [util:timestamp(),RightName, Node2]))
	end,
	Config#config{leftN = LeftNTupel, rightN = RightNTupel}.

init(KoordPID,Arbeitszeit, TermZeit,ProzessNR, StarterNR,NamensDPID, Praktikumsgruppe, TeamID)->
	Config = #config{koordPID=KoordPID, arbeitszeit=Arbeitszeit, termZeit=TermZeit, prozessNR=ProzessNR, starterNR=StarterNR, namensDPID=NamensDPID, praktikumsgruppe=Praktikumsgruppe, teamID=TeamID},
	NewConfig2 = Config#config{myID=list_to_atom(gen_myid(Config#config.praktikumsgruppe, Config#config.teamID, Config#config.prozessNR, Config#config.starterNR))},
	NewConfig = NewConfig2#config{logfile="GGTP_" ++ atom_to_list(NewConfig2#config.myID) ++ "@" ++ net_adm:localhost() ++ ".log"},
	
	%registierung allgemein
	register(NewConfig#config.myID,self()),
	
	%registrierung beim Namensdienst.
	%io:format(" at NS at ~p~n", [NewConfig#config.namensDPID]),
	Config#config.namensDPID !{self(), {rebind, NewConfig#config.myID, node()}},
	receive 
		ok -> werkzeug:logging(NewConfig#config.logfile, nice_format("~p bindet to: ~p", [util:timestamp(),Config#config.namensDPID]));
		kill	-> killingstuff(Config)
	after 5000000 -> werkzeug:logging(NewConfig#config.logfile, nice_format("~p no respomnse from nameservice: ~p", [util:timestamp(),Config#config.namensDPID]))
	end,

	NewerConfig = NewConfig, 

	werkzeug:logging(NewConfig#config.logfile, nice_format("~p send hello to koord (~p) with myID (~p)", [util:timestamp(), NewerConfig#config.koordPID, NewConfig#config.myID])),
	NewerConfig#config.koordPID ! {hello, NewConfig#config.myID},
	% auf die nachbar informationen vom koordinator warten
	receive 
		{setneighbors, LeftN, RightN} -> 
			werkzeug:logging(NewConfig#config.logfile, nice_format("~p got Neighbours ~p and ~p", [util:timestamp(),LeftN, RightN])),
			EvenNewerConfig=NewerConfig#config{leftN=LeftN, rightN=RightN};
		kill	->EvenNewerConfig=Config, killingstuff(Config)
	
	after 600000000 ->	werkzeug:logging(NewConfig#config.logfile, nice_format("~p got no answer from the boss of it all", [util:timestamp()])), EvenNewerConfig = NewerConfig
	end,

	NewestConfig = resolve_neighbors(EvenNewerConfig),
	
	% auf initialen Mi wert vom koordinator warten
	receive 
		{setpm,MiNeu} -> Mi=MiNeu,
			werkzeug:logging(NewConfig#config.logfile, nice_format("~p got new Mi: ~p", [util:timestamp(),MiNeu])),
			MostNewConfig=NewestConfig#config{lastSendy=now()};
		kill	-> MostNewConfig=NewestConfig, Mi=0, killingstuff(NewestConfig)
	end,
	loop(MostNewConfig,Mi).

loop(Config,Mi)->
	Temp=timer:now_diff(now(), Config#config.lastSendy),
	if 
		Temp > Config#config.termZeit*1000 -> werkzeug:logging(Config#config.logfile, nice_format("~p DIfferenz: ~p initiate abstimmung",[util:timestamp(),Temp])), 
			Config#config.rightN !{abstimmung,Config#config.myID},
			abstimmung(Config,Mi);
		true -> foo
	end,
	receive 

		% Rekursiver Aufruf der ggT Berechnung.
		{sendy,Y}  -> 	werkzeug:logging(Config#config.logfile, nice_format("~p received sendy : ~p",[util:timestamp(),Y])),
				Mi2=algo(Mi, Y, Config#config.arbeitszeit,Config) , 
				NewConfig=Config#config{lastSendy=now()},
				if 
					Mi2 =/= Mi -> sendToNs(NewConfig,Mi),
						NewConfig#config.koordPID ! {briefmi, {NewConfig#config.myID, Mi2, werkzeug:timeMilliSecond()}},
						werkzeug:logging(Config#config.logfile, nice_format("~p Mi (~p) did change to ~p",[util:timestamp(), Mi,Mi2])),
						loop(NewConfig, Mi2);
					true -> werkzeug:logging(Config#config.logfile, nice_format("~p Mi didn't change. Mi: ~p Mi2: ~p",[util:timestamp(),Mi,Mi2])), loop(NewConfig, Mi)
				end;
		
		{abstimmung, From} when From == Config#config.myID ->
			werkzeug:logging(Config#config.logfile, nice_format("~p send end of algorithm (briefterm) to koordinator. Mi: ~p",[util:timestamp(),Mi])),
			Config#config.koordPID ! {briefterm,{Config#config.myID,Mi,werkzeug:timeMilliSecond()}}, abstimmung(Config, Mi);

		% behandlung der abstimmungsnachricht
		{abstimmung, From} ->Temp2=timer:now_diff(now(), Config#config.lastSendy),
				 if	(Temp2 > (Config#config.termZeit/2)*1000) -> werkzeug:logging(Config#config.logfile, nice_format("~p forward abstimmung. Mi: ~p",[util:timestamp(),Mi])),
						Config#config.rightN!{abstimmung,From},
						loop(Config,Mi);%,abstimmung(Config,Mi);
				 	true -> werkzeug:logging(Config#config.logfile, nice_format("~p ignore abstimmung; not finished yet. Mi: ~p",[util:timestamp(),Mi])),
						loop(Config,Mi)
				 end;
					 
					 

		% Sendet das aktuelle "Mi" an "From"
		{tellmi, From} -> From ! Mi, loop(Config, Mi);
					
		kill	-> Config#config.namensDPID ! {self(),{unbind, Config#config.myID}}, killingstuff(Config);
		{setpm,MiNeu} ->
			werkzeug:logging(Config#config.logfile, nice_format("~p got new Mi: ~p", [util:timestamp(),MiNeu])),
			MostNewConfig=Config#config{lastSendy=now()},
			loop(MostNewConfig,MiNeu)

		%Bla	-> werkzeug:logging(Config#config.logfile, nice_format("got stupid message: ~p",[Bla]))
	after Config#config.termZeit 	-> 
		werkzeug:logging(Config#config.logfile, nice_format("~p initiate abstimmung (after block). Mi: ~p",[util:timestamp(),Mi])), 
		Config#config.rightN ! {abstimmung, Config#config.myID}, 
		abstimmung(Config, Mi)
	
	end
	.
 
algo(Mi, Y, Arbeitszeit,Config) ->  % 21.
			werkzeug:logging(Config#config.logfile,nice_format("~p algo(Mi: ~p, Y: ~p, Arbeitszeit: ~p)",[util:timestamp(),Mi, Y, Arbeitszeit])),
			timer:sleep(Arbeitszeit),
 	 		case Y < Mi of
   			 	true -> Mi2=((Mi-1) rem Y) + 1,
						werkzeug:logging(Config#config.logfile,nice_format("~p Mi2: ~p )",[util:timestamp(),Mi2]));
   			 	_ -> Mi
  			end.
			
sendToNs(Config, Mi)	-> werkzeug:logging(Config#config.logfile, nice_format("~p send sendy (~p) to ~p and ~p",[util:timestamp(), Mi, Config#config.leftN, Config#config.rightN])), Config#config.leftN ! {sendy, Mi}, Config#config.rightN ! {sendy, Mi}.

abstimmung(Config, Mi)	->  
	receive 
		kill	-> Config#config.namensDPID ! {self(),{unbind, Config#config.myID}}, killingstuff(Config);
		{abstimmung, MYID} when Config#config.myID==MYID ->
			werkzeug:logging(Config#config.logfile, nice_format("~p send end of algorithm (briefterm) to koordinator. Mi: ~p",[util:timestamp(),Mi])),
			Config#config.koordPID ! {briefterm,{Config#config.myID,Mi,werkzeug:timeMilliSecond()}}, abstimmung(Config, Mi);
		
		{abstimmung, OtherID} ->  
			werkzeug:logging(Config#config.logfile, nice_format("~p (~p ) forward abstimmung. Mi: ~p",[util:timestamp(),OtherID,Mi])),
			Config#config.rightN ! {abstimmung, OtherID}, abstimmung(Config, Mi);

		% Aufruf der ggT Berechnung + zustands�nderung in "aktiv berechnen", zur�ck zu loop(...)
		{sendy,Y}  -> Mi2=algo(Mi, Y, Config#config.arbeitszeit,Config) , 
				NewConfig=Config#config{lastSendy=now()},
				if 
					Mi2 =/= Mi -> sendToNs(NewConfig,Mi),
						NewConfig#config.koordPID ! {briefmi, {NewConfig#config.myID, Mi2, werkzeug:timeMilliSecond()}},
						loop(NewConfig, Mi2);
					true -> loop(NewConfig, Mi)
				end;
		{tellmi, From} -> From ! Mi, abstimmung(Config, Mi);
		{setpm,MiNeu} ->
			werkzeug:logging(Config#config.logfile, nice_format("~p got new Mi: ~p", [util:timestamp(),MiNeu])),
			MostNewConfig=Config#config{lastSendy=now()},
			loop(MostNewConfig,MiNeu)
	end.
		
killingstuff(Config) -> Config#config.namensDPID! {self(),{unbind, Config#config.myID}}, werkzeug:logging(Config#config.logfile, nice_format("~p ~p is DEAD!", [util:timestamp(),Config#config.myID])).
