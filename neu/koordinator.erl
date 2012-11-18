%%% --------------------------------------------------------------------
%%% @author Fachher Syed, Eduard Weigandt
%%% @copyright 2011 Creative Commons
%%% @doc Manager for greatest common divisor Algo.
%%% 	This Server manages a finite number of processes in a ring for gcd calculation.
%%% @end
%%% --------------------------------------------------------------------

 
-module(koordinator).
-export([start/0,loop/2]).
-import(math,[pow/2]).
-import(random,[uniform/1]).
-import(werkzeug,[get_config_value/2,logging/2,timeMilliSecond/0]).

-record(koord_conf,
    {arbeitszeit,		% Verzögerungszeit der ggT-Prozesse als simulierter Arbeitsaufwand
    termzeit,           % Terminierungszeit nach der ein ggT-Prozess eine Terminierungsabstimmung durchführt.
    ggtprozessanzahl, 	% Anzahl der ggT-Prozesse pro Starter
    nameservicenode, 	% Node des Namensdienst
	nameservice,
    koordinatorname, 	% Name des Koordinator Prozesses.
	ggtprozesse=[],		% Angemeldete ggT-Prozesse
	logdatei			% Log-Datei
    }).
    

start()	->	{ok, ConfigListe} = file:consult("koordinator.cfg"),
	        {ok, Arbeitszeit} = get_config_value(arbeitszeit, ConfigListe),
	        {ok, Termzeit} = get_config_value(termzeit, ConfigListe),
	        {ok, GGTProzessanzahl} = get_config_value(ggtprozessanzahl, ConfigListe),
	        {ok, Nameservicenode} = get_config_value(nameservicenode, ConfigListe),
	        {ok, Koordinatorname} = get_config_value(koordinatorname, ConfigListe),
	        {ok, HostName} = inet:gethostname(),
			
			net_adm:ping(Nameservicenode),
			timer:sleep(1000),
			Nameservice = global:whereis_name(nameservice),
			
			% Koordinator registriert sich beim Namenserver
			Nameservice	! {self(),{rebind, Koordinatorname, node()}},
			
	        Config = #koord_conf{arbeitszeit = Arbeitszeit, 
	                            termzeit = Termzeit, 
	                            ggtprozessanzahl = GGTProzessanzahl, 
	                            nameservicenode = Nameservicenode,
								nameservice = Nameservice,
	                            koordinatorname = Koordinatorname,
								logdatei = lists:concat(["Koordinator@", HostName, ".log"])
	                           },
	                           
	        PID = spawn(koordinator, loop, [Config#koord_conf{}, initial]),
			register(Koordinatorname, PID),
			logging_format("Startzeit ~p: Koordinator Prozess wurde unter der PID ~p gestartet und ist als ~s regestriert.\n", [timeMilliSecond(),PID, Koordinatorname]),
			PID.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Main Loop %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

loop(Config=#koord_conf{},State)->
	LogDatei = Config#koord_conf.logdatei,
	case State of
		% Koordinator geht in den Zustand "initial" über
		initial    ->
			receive
				% Anfrage nach den steuernden Werten durch den Starter.
				{getsteeringval, From} -> From ! {steeringval,
				                                 Config#koord_conf.arbeitszeit,
				                                 Config#koord_conf.termzeit,
				                                 Config#koord_conf.ggtprozessanzahl
				                                 },
										  logging(LogDatei,lists:concat(["getsteeringval von ",
										                                 pid_to_list(From),
										                                 " mit den Werten Arbeitszeit=",
										                                 Config#koord_conf.arbeitszeit,
										                                 "; TermZeit=",
										                                 Config#koord_conf.termzeit,
										                                 "; GGTProzessanzahl=",
										                                 Config#koord_conf.ggtprozessanzahl,
										                                 " um ",timeMilliSecond(),".\n"])),
										  loop(Config#koord_conf{}, initial);
										  
				% ggT Prozess meldet sich mit seinem Namen an.	
				{hello, Clientname}      -> logging(LogDatei, lists:concat(["hello von ", Clientname," um ", timeMilliSecond(),".\n"])),
											Config#koord_conf.nameservice ! {self(), {lookup, Clientname}},
											receive
												{Clientname, ClientNode} -> 
													NewggTProcessList = [{Clientname, ClientNode}|Config#koord_conf.ggtprozesse],
													logging(LogDatei,lists:concat(["ggT-Prozess ", Clientname,
													                               " auf ", ClientNode,
													                               " gebunden um ", timeMilliSecond(),".\n"]));
												not_found -> 												
													NewggTProcessList = Config#koord_conf.ggtprozesse,
													logging(LogDatei,lists:concat(["ggT-Prozess Namensauflösung von ",Clientname,
													                               " gescheitert um ",timeMilliSecond(),".\n"]))
											end,
											loop(Config#koord_conf{ggtprozesse=NewggTProcessList}, initial);
											
				% Koordinator manuell in den Zustand "ready" versetzen.
				ready				  -> logging(LogDatei,"Erstelle Ring aus ggT-Prozessen: " ++ tuplelist_to_string(Config#koord_conf.ggtprozesse) ++ ".\n"),
										 sendNeighborToPids(createcircle(shuffle_list(Config#koord_conf.ggtprozesse))), 
				                         logging(LogDatei,"Anmeldefrist für ggT-Prozesse abgelaufen.\n"),
										 loop(Config#koord_conf{},ready);
										 
				% Der Koordinator wird beendet und sendet allen ggT-Prozessen das kill-Kommando.
				kill -> kill_ggt_processes(Config#koord_conf.ggtprozesse),
						global:whereis_name(nameservice) ! {self(),{unbind, Config#koord_conf.koordinatorname}}
			end;
		
		% Koordinator geht in den Zustand "ready" über	
		ready      ->
			receive
				% ggT Berechnung manuell beginnen
				{calculate,Ggt}	-> WishedGGT = Ggt,
							   SeedValues = createNumbers(length(Config#koord_conf.ggtprozesse), WishedGGT),
							   send_mi(SeedValues, Config#koord_conf.ggtprozesse),
							   send_y(SeedValues, Config#koord_conf.ggtprozesse),
							   logging(LogDatei, lists:concat(["Beginne eine neue ggT-Berechnung mit Ziel ", integer_to_list(WishedGGT), ".\n"])),
							   loop(Config#koord_conf{},ready);
			
				% ggT Prozess "Clientname" übermittel sein neues "CMi" zur Zeit "CZeit"
				{briefmi,{Clientname,CMi,CZeit}} -> logging_format("~w meldet neues Mi ~w um ~p <(~p<).\n", [Clientname,CMi,CZeit,timeMilliSecond()]),
													loop(Config#koord_conf{},ready);
				
				% ggT Prozess "Clientname" informiert über die Terminierung der Berechnung mit Ergebnis CMi um CZeit Uhr.
				{briefterm,{Clientname,CMi,CZeit}} -> 
				                        logging_format("~w meldet Terminierung mit ggT ~w um ~p <(~p<).\n",[Clientname,CMi,CZeit,timeMilliSecond()]),
										loop(Config#koord_conf{},ready);
				 
				% Koordinator sendet allen ggT-Prozessen das kill-Kommando und bringt sich selbst in den initialen Zustand, 
				% indem sich Starter wieder melden können.
				reset -> kill_ggt_processes(Config#koord_conf.ggtprozesse),
						 loop(Config#koord_conf{ggtprozesse=[]},initial);
				
				% Der Koordinator wird beendet und sendet allen ggT-Prozessen das kill-Kommando.
				kill -> kill_ggt_processes(Config#koord_conf.ggtprozesse),
						global:whereis_name(nameservice) ! {self(),{unbind, Config#koord_conf.koordinatorname}}
			end
	end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% HELPER FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

	
logging_format(Format, FormatArgs) ->
	Text = lists:flatten(io_lib:format(Format, FormatArgs)),
	{ok, HostName} = inet:gethostname(),
	Filename = lists:flatten(["Koordinator@", HostName, ".log"]),
	logging(Filename, Text).


% Name: kill_ggt_processes
% Beschreibung: Sendet an alle ggT-Prozesse die kill Nachricht zu, damit sie sich beenden.
% Beispiel: -
% Vorbedingung: List == list()	

kill_ggt_processes(List) ->
	lists:foreach(fun(GGTProcess) -> GGTProcess ! kill end, List).

% Name: send_mi
% Beschreibung: Sendet an alle ggT-Prozesse ihre Mi Werte zu.
% Beispiel: 
% Vorbedingung: length(Mi-Liste) == length(ggT-Prozess-Liste)

send_mi([], []) -> true;
send_mi([Mi|MiNumbers], [Process|Processes]) -> 
	{Name,Node} = Process,
	Process ! {setpm, Mi},
	logging_format("ggT-Prozess ~p (~p) initiales Mi ~p gesendet.\n",[Name,Node,Mi]),
	send_mi(MiNumbers, Processes).
	
% Name: send_y
% Beschreibung: Sendet an 15% (min. 2) aller ggT-Prozesse einen bestimmten Berechnungs Wert Y zu.
% Beispiel: 
% Vorbedingung: MiNumbers =:= list() and length(MiNumbers) > 0 and length(Processes) >= 2

send_y(MiNumbers, Processes) ->
	% SmallestMi = lists:nth(random:uniform(length(MiNumbers)), MiNumbers), 
	SmallestMi = lists:min(MiNumbers),
	case round((length(Processes)/100) * 15) of
		Choice when Choice >= 2 ->
			Range = Choice;
		_Else ->
			Range = 2
	end,
	CalcProcesses = random_elements(Range, Processes),
	logging_format("ggT-Prozessen (~p) wird der Y Wert (~p) gesendet.\n",[length(CalcProcesses),SmallestMi]),
	lists:foreach(fun(Process)-> Process ! {sendy, SmallestMi} end, CalcProcesses).

% Name: random_elements
% Beschreibung: Liefert eine Liste mit einer bestimmten Anzahl von zufällig ausgewählten Elementen zurück.
% Beispiel: random_elements(3, [1,2,3,4,5,6]) -> [3,5,1]
% Vorbedingung: -

random_elements(Range, List) -> random_elements(Range, List, []).
random_elements(0, _List, RandomElems) -> RandomElems;
random_elements(_Range, [], RandomElems) -> RandomElems;
random_elements(Range, List, RandomElems) -> 
	 NewRandomElems = [RandomEl=lists:nth(random:uniform(length(List)), List)|RandomElems], 
	 NewList = lists:delete(RandomEl, List),
	 random_elements(Range-1, NewList, NewRandomElems).

% Name: createcircle
% Beschreibung: Erstellt einen Kreis aus einer Liste.
% Example: createcircle([1,2,3,4])   ->   [{4,1,2},{1,2,3},{2,3,4},{3,4,1}].
% Precondition: lenght(Listin) > 2

createcircle(Listin)	when length(Listin) > 2  -> createcircle(Listin,[],hd(Listin),hd(Listin),lists:last(Listin),length(Listin)).
createcircle([],Temp,_,_,_,_)  -> Temp;
createcircle([Head|Tail],Temp,_,Firstelem,Lastelem,Listlen) when length(Tail)+1 == Listlen 
             -> createcircle(Tail,[{Lastelem,Head,hd(Tail)}|Temp],Head,Firstelem,Lastelem,Listlen);
createcircle([Head|Tail],Temp,Prev,Firstelem,Lastelem,Listlen) when length(Tail) == 0 
             -> createcircle(Tail,[{Prev,Head,Firstelem}|Temp],Head,Firstelem,Lastelem,Listlen);
createcircle([Head|Tail],Temp,Prev,Firstelem,Lastelem,Listlen) -> createcircle(Tail,[{Prev,Head,hd(Tail)}|Temp],Head,Firstelem,Lastelem,Listlen).

sendNeighborToPids([]) -> true;
sendNeighborToPids([{{LeftN,_},Middle,{RightN,_}}|Tail]) -> Middle ! {setneighbors,LeftN,RightN}, sendNeighborToPids(Tail).


% Name: createNumbers
% Beschreibung: 
% Beispiel: 
% Vorbedingung:

createNumbers(0,_WishedGGT) -> [];
createNumbers(Range, WishedGGT) when WishedGGT > 0 -> createNumbers(Range, WishedGGT, []).
createNumbers(0, _WishedGGT, List) -> List;
createNumbers(Range, WishedGGT, List) -> createNumbers(Range - 1, WishedGGT,[createNumber(WishedGGT)|List]).

% Name: createNumber
% Beschreibung: Erstellt eine Zahl, die sich nur durch die eingegebene Zahl WishedGGT restlos teilen lässt.
% Beispiel: createNumber(5) -> 5 * 3^2 * 5^1 * 11^2 * 13^0 * 23^2 * 37^1 = 532874925
% Vorbedingung: WishedGGT > 0 und WishedGGT == number()

createNumber(WishedGGT)	->
	trunc(WishedGGT * 
	pow(3,uniform(3)-1) * 
	pow(5,uniform(3)-1) * 
	pow(11,uniform(3)-1) * 
	pow(13,uniform(3)-1) * 
	pow(23,uniform(3)-1) *
	pow(37,uniform(3)-1)).		

% Name: shuffle_list
% Beschreibung: Verschiebt die Elemente einer Liste zufällig in ihrer Position und gibt die neue Liste aus
% Beispiel: shuffle_list([1,2,3,4]) -> [2, 4, 1, 3]
% Vorbedingung: List == list()	

shuffle_list(List) ->                                          
   random:seed(now()),
   {NewList, _} = lists:foldl( fun(_El, {Acc,Rest}) ->          
       RandomEl = lists:nth( random:uniform(length(Rest)), Rest),
       {[RandomEl|Acc], lists:delete(RandomEl, Rest)}            
   end, {[],List}, List),                                        
   NewList.

% Name: tuplelist_to_string
% Beschreibung: Erstellt ein String aus einer Liste mit zweier Tupeln.
% Beispiel: tuplelist_to_string([{atom1,atom2}]) -> "[{atom1, atom2}]"
% Vorbedingung:
tuplelist_to_string(L) ->
    tuplelist_to_string(L,[]).

tuplelist_to_string([],Acc) ->
    lists:flatten(["[",
    	   string:join(lists:reverse(Acc),","),
    	   "]"]);
tuplelist_to_string([{X,Y}|Rest],Acc) ->
	S = lists:flatten(io_lib:format("{~s, ~s}", [X,Y])),
    % S = "{" ++ atom_to_list(X) ++ ", " ++ atom_to_list(Y) ++ "}",
    tuplelist_to_string(Rest,[S|Acc]).
