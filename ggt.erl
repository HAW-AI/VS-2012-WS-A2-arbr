%% Author: anton
%% Created: 01.11.2012
%% Description: TODO: Add description to ggt
-module(ggt).

-record(ggt_config, {arbeitszeit,termzeit,prozessnr,starternr,nameservicepid,praktikumsgruppe, team,koordinatorpid,leftN,rightN,lastSendy}).
%%
%% Include files
%%
-import(util, [timestamp/0]).
%%
%% Exported Functions
%%
-compile([export_all]).

%%
%% API Functions
%%
start(KoordinatorPID, Arbeitszeit, TermZeit, ProzessNr, StarterNr, NameservicePID, Praktikumsgruppe, Team) ->
	Config = #ggt_config{
						 arbeitszeit = Arbeitszeit,
						 termzeit = TermZeit,
						 prozessnr = ProzessNr,
						 starternr = StarterNr,
						 nameservicepid = NameservicePID,
						 praktikumsgruppe = Praktikumsgruppe,
						 team = Team,
						 koordinatorpid = KoordinatorPID
						 },
	log("Name: ~p",Config,[toString(name(Config))]),
	register(name(Config),self()),
	%Verbindung mit dem Nameserver aufbauen
	Config#ggt_config.nameservicepid ! {self(), {rebind, name(Config), node()}},
	receive
		ok -> log("Verbunden mit ~p",[Config#ggt_config.nameservicepid],Config);
		kill -> ok %TODO auf kill reagieren
	after 5000 ->
			log("Namensdienst ~p antwortet nicht",[Config#ggt_config.nameservicepid],Config)
	end,
	% Hello an Koordinator sende
	Config#ggt_config.koordinatorpid ! {hello, name(Config)},
	% Infos über die Nachbarn erhalten
	receive
		{setneighbors, LeftN, RightN}->
			log("Ich habe die Nachbarn kennengelernt(L: ~p , R: ~p)",Config,[LeftN,RightN]),
			NewConfig = Config#ggt_config{leftN = LeftN, rightN=RightN};
		kill ->
			ok
	after 60000 ->
			log("Ich habe keine Infos bezüglich der Nachbarn erhalten",Config)
	end,
	ConfigWithNeighbors = resolve_neighbors(NewConfig), %TODO resolve_neighbors
	% Mi-Wert vom Koordinator erhalten
	receive
		{setpm, MiNeu} -> Mi = MiNeu,
						  log("Der neu Mi-Wert ist ~p",ConfigWithNeighbors,[MiNeu]),
						  NewerConfig = ConfigWithNeighbors#ggt_config{lastSendy=now()};
		kill -> NewerCofig = ConfigWithNeighbors,
				Mi=0 %TODO kill-routine
	end,
	spawn(fun() -> log("GGTP ~p (~p) gestartet",NewerConfig,[self(), name(NewerConfig)]), loop(NewerConfig,Mi) end).


%%
%% Local Functions
%%
loop(Config,Mi)->
	ok.

name(Config) ->
	%
	Stringname = toString(Config#ggt_config.praktikumsgruppe)++toString(Config#ggt_config.team)++toString(Config#ggt_config.prozessnr)++toString(Config#ggt_config.starternr),
	{Name,_}=string:to_integer(Stringname),
	Name.

log(Message,Config) ->
  log(Message,Config, []).
log(Message,Config, Data) ->
  util:log(logfile(Config), Message, Data).

logfile(Config) ->
  "GGTP_"++toString(name(Config))++".log".
toString(Integer) ->
	lists:flatten(io_lib:format("~p", [Integer])).