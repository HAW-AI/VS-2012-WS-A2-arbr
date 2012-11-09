-module(ggT).
-export([start/8, init/8]).
-record(config,
    {	teamID=03,
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


%-define(Log(FromatS, ArgsS), werkzeug:logging("GGTP_" ++ Config#config.myID ++ "@" ++ net_adm:localhost() ++ ".log", nice_format(FormatS, ArgsS))).

nice_format(Format, Args) ->
	lists:flatten(io_lib:format("~s ~n",[lists:flatten(io_lib:format(Format, Args))])).	

gen_myid(PG, TID, PNR, SNR) ->
	lists:flatten(io_lib:format("~B~B~B~B", [ PG, TID, PNR, SNR ])).

% parameterreihenfolge nicht mehr ändern
start(KoordName,Arbeitzeit, TermZeit,ProzessNR, StarterNR,NamensDPID, Praktikumsgruppe, TeamID) ->
	spawn(ggT, init, [KoordName,Arbeitzeit, TermZeit,ProzessNR, StarterNR,NamensDPID, Praktikumsgruppe, TeamID]).

resolve_neighbors(Config) ->
	% rechten nachbarnamen auflösen
	Config#config.namensDPID ! {self(),{lookup, Config#config.leftN}},
	LeftName = Config#config.leftN,
	receive
		not_found -> werkzeug:logging(Config#config.logfile, nice_format("Linker Nachbar ~p konnte nicht aufgelost werden.", [Config#config.leftN])), LeftNTupel = {};
		LeftNTupel = {LeftName, Node1} -> werkzeug:logging(Config#config.logfile, nice_format("Linker Nachbar ~p befindet sich auf node ~p.", [LeftName, Node1]))
	end,

	% linken nachbarnamen auflösen
	Config#config.namensDPID ! {self(),{lookup, Config#config.rightN}},
	RightName = Config#config.rightN,
	receive
		not_found -> werkzeug:logging(Config#config.logfile, nice_format("Rechter Nachbar ~p konnte nicht aufgelost werden.", [Config#config.rightN])), RightNTupel = {};
		RightNTupel = {RightName, Node2} -> werkzeug:logging(Config#config.logfile, nice_format("Rechter Nachbar ~p befindet sich auf node ~p.", [RightName, Node2]))
	end,
	Config#config{leftN = LeftNTupel, rightN = RightNTupel}.

init(KoordPID,Arbeitszeit, TermZeit,ProzessNR, StarterNR,NamensDPID, Praktikumsgruppe, TeamID)->
	Config = #config{koordPID=KoordPID, arbeitszeit=Arbeitszeit, termZeit=TermZeit, prozessNR=ProzessNR, starterNR=StarterNR, namensDPID=NamensDPID, praktikumsgruppe=Praktikumsgruppe, teamID=TeamID},
	NewConfig2 = Config#config{myID=list_to_atom(gen_myid(Config#config.praktikumsgruppe, Config#config.teamID, Config#config.prozessNR, Config#config.starterNR))},
	NewConfig = NewConfig2#config{logfile="GGTP_" ++ atom_to_list(NewConfig2#config.myID) ++ "@" ++ net_adm:localhost() ++ ".log"},
	
	%registierung allgemein
	register(NewConfig#config.myID,self()),
	
	%registrierung beim Namensdienst.
	io:format(" at NS at ~p~n", [NewConfig#config.namensDPID]),
	Config#config.namensDPID !{self(), {rebind, NewConfig#config.myID, node()}},
	receive 
		ok -> werkzeug:logging(NewConfig#config.logfile, nice_format("bindet to: ~p", [Config#config.namensDPID]));
		kill	-> killingstuff(Config)
	after 5000 -> werkzeug:logging(NewConfig#config.logfile, nice_format("no respomnse from nameservice: ~p", [Config#config.namensDPID]))
	end,

	NewerConfig = NewConfig, 

	werkzeug:logging(NewConfig#config.logfile, nice_format("send hello to koord (~p) with myID (~p)", [NewerConfig#config.koordPID, NewConfig#config.myID])),
	NewerConfig#config.koordPID ! {hello, NewConfig#config.myID},
	% auf die nachbar informationen vom koordinator warten
	receive 
		{setneighbors, LeftN, RightN} -> 
			werkzeug:logging(NewConfig#config.logfile, nice_format("got Neighbours ~p and ~p", [LeftN, RightN])),
			EvenNewerConfig=NewerConfig#config{leftN=LeftN, rightN=RightN};
		kill	->EvenNewerConfig=Config, killingstuff(Config)
	
	after 600000 ->	werkzeug:logging(NewConfig#config.logfile, nice_format("got no answer from the boss of it all", [])), EvenNewerConfig = NewerConfig
	end,

	NewestConfig = resolve_neighbors(EvenNewerConfig),
	
	% auf initialen Mi wert vom koordinator warten
	receive 
		{setpm,MiNeu} -> Mi=MiNeu,
			werkzeug:logging(NewConfig#config.logfile, nice_format("got new Mi: ~p", [MiNeu])),
			MostNewConfig=NewestConfig#config{lastSendy=now()};
		kill	-> MostNewConfig=NewestConfig, Mi=0, killingstuff(NewestConfig)
	end,
	loop(MostNewConfig,Mi).

loop(Config,Mi)->
	Temp=timer:now_diff(now(), Config#config.lastSendy),
	if 
		Temp > Config#config.termZeit*1000 -> werkzeug:logging(Config#config.logfile, nice_format("initiate abstimmung",[])), 
			Config#config.rightN !{abstimmung,Config#config.myID},
			abstimmung(Config,Mi);
		true -> foo
	end,
	receive 

		% Rekursiver Aufruf der ggT Berechnung.
		{sendy,Y}  -> 	werkzeug:logging(Config#config.logfile, nice_format("received sendy : ~p",[Y])),
				Mi2=algo(Mi, Y, Config#config.arbeitszeit) , 
				NewConfig=Config#config{lastSendy=now()},
				if 
					Mi2 =/= Mi -> sendToNs(NewConfig,Mi),
						NewConfig#config.koordPID ! {briefmi, {NewConfig#config.myID, Mi2, werkzeug:timeMilliSecond()}},
						werkzeug:logging(Config#config.logfile, nice_format("Mi did change to ~p",[Mi2])),
						loop(NewConfig, Mi2);
					true -> werkzeug:logging(Config#config.logfile, nice_format("Mi didn't change",[])), loop(NewConfig, Mi)
				end;
		
		{abstimmung, From} when From == Config#config.myID ->
			werkzeug:logging(Config#config.logfile, nice_format("send end of algorithm (briefterm) to koordinator",[])),
			Config#config.koordPID ! {briefterm,{Config#config.myID,Mi,werkzeug:timeMilliSecond()}}, abstimmung(Config, Mi);

		% behandlung der abstimmungsnachricht
		{abstimmung, From} ->Temp2=timer:now_diff(now(), Config#config.lastSendy),
				 if	(Temp2 > (Config#config.termZeit/2)*1000) -> werkzeug:logging(Config#config.logfile, nice_format("forward abstimmung",[])),
						Config#config.rightN!{abstimmung,From},
						loop(Config,Mi);%,abstimmung(Config,Mi);
				 	true -> werkzeug:logging(Config#config.logfile, nice_format("ignore abstimmung; not finished yet",[])),
						loop(Config,Mi)
				 end;
					 
					 

		% Sendet das aktuelle "Mi" an "From"
		{tellmi, From} -> From ! Mi, loop(Config, Mi);
					
		kill	-> Config#config.namensDPID ! {self(),{unbind, Config#config.myID}}, killingstuff(Config)
		%Bla	-> werkzeug:logging(Config#config.logfile, nice_format("got stupid message: ~p",[Bla]))
	after Config#config.termZeit 	-> 
		werkzeug:logging(Config#config.logfile, nice_format("initiate abstimmung",[])), 
		Config#config.rightN ! {abstimmung, Config#config.myID}, 
		abstimmung(Config, Mi)
	
	end
	.

algo(Mi, Y, Arbeitszeit) ->  % 21.
			io:format(nice_format("algo(~p, ~p, ~p)",[Mi, Y, Arbeitszeit])),
			timer:sleep(Arbeitszeit),
			if 
				Y<Mi	->	(trunc(Mi-1) rem trunc(Y))+1;
				true 	-> Mi
			end
			.
			
sendToNs(Config, Mi)	-> werkzeug:logging(Config#config.logfile, nice_format("send sendy (~p) to ~p and ~p",[Mi, Config#config.leftN, Config#config.rightN])), Config#config.leftN ! {sendy, Mi}, Config#config.rightN ! {sendy, Mi}.

abstimmung(Config, Mi)	->  
	receive 
		kill	-> Config#config.namensDPID ! {self(),{unbind, Config#config.myID}}, killingstuff(Config);
		{abstimmung, MYID} when Config#config.myID==MYID ->
			werkzeug:logging(Config#config.logfile, nice_format("send end of algorithm (briefterm) to koordinator",[])),
			Config#config.koordPID ! {briefterm,{Config#config.myID,Mi,werkzeug:timeMilliSecond()}}, abstimmung(Config, Mi);
		
		{abstimmung, OtherID} ->  
			werkzeug:logging(Config#config.logfile, nice_format("forward abstimmung",[])),
			Config#config.rightN ! {abstimmung, OtherID}, abstimmung(Config, Mi);

		% Aufruf der ggT Berechnung + zustandsänderung in "aktiv berechnen", zurück zu loop(...)
		{sendy,Y}  -> Mi2=algo(Mi, Y, Config#config.arbeitszeit) , 
				NewConfig=Config#config{lastSendy=now()},
				if 
					Mi2 =/= Mi -> sendToNs(NewConfig,Mi),
						NewConfig#config.koordPID ! {briefmi, {NewConfig#config.myID, Mi2, werkzeug:timeMilliSecond()}},
						loop(NewConfig, Mi2);
					true -> loop(NewConfig, Mi)
				end;
		{tellmi, From} -> From ! Mi, abstimmung(Config, Mi)
	end.
		
killingstuff(Config) -> Config#config.namensDPID! {self(),{unbind, Config#config.myID}}, werkzeug:logging(Config#config.logfile, nice_format("~p is DEAD!", [Config#config.myID])).
