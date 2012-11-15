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
		kill -> unbind(Config)
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
			unbind(Config)
	after 60000 ->
			log("Ich habe keine Infos bezüglich der Nachbarn erhalten",Config)
	end,
	ConfigWithNeighbors = resolve_neighbors(NewConfig),
	% Mi-Wert vom Koordinator erhalten
	receive
		{setpm, MiNeu} -> Mi = MiNeu,
						  log("Der neu Mi-Wert ist ~p",ConfigWithNeighbors,[MiNeu]),
						  NewerConfig = ConfigWithNeighbors#ggt_config{lastSendy=now()};
		kill -> NewerCofig = ConfigWithNeighbors,
				Mi=0,
				unbind(ConfigWithNeighbors)
	end,
	spawn(fun() -> log("GGTP ~p (~p) gestartet",NewerConfig,[self(), name(NewerConfig)]), loop(NewerConfig,Mi) end).


%%
%% Local Functions
%%
loop(Config,Mi)->
	ok.

resolve_neighbors(Config) ->
	%Der rechte Nachbarname wird aufgelöst
	Config#ggt_config.nameservicepid ! {self(), {lookup, Config#ggt_config.leftN}},
	receive
		not_found ->
			log("Linker Nachbar ~p wurde nicht aufgelöst",Config,[Config#ggt_config.leftN]),
			LeftNTupel = {};
		{LeftName, LeftNode} ->
			log("Linker Nachbar ~p ist auf dem Node ~p",Config,[LeftName,LeftNode]),
			LeftNTupel = {LeftName, LeftNode}
	end,
	%Der linke Nachbarname wird aufgelöst
	Config#ggt_config.nameservicepid ! {self(), {lookup, Cofig#ggt_config.rightN}},
	receive
		not_found ->
			log("Der rechte Nachbar ~p wurde nicht aufgelöst",Config,[Config#ggt_config.rightN]),
			RightNTupel = {};
		{RightName, RightNode} ->
			log("Rechter Nachbar ~p ist auf dem Node ~p",Config,[RightName,RightNode]),
			RightNTupel = {RightName, RightNode}
	end,
	Config#ggt_config{leftN=LeftNTupel, rightN=RightNTupel}.
		
unbind(Config)->
	Config#config.namensDPID! {self(),{unbind, name(Config)}},
	log("~p - Verbindung getrennt",Config,[toString(name(Config))]).

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