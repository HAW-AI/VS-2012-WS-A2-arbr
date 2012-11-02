%% Author: anton
%% Created: 01.11.2012
%% Description: TODO: Add description to starter
-module(starter).


-record(starter_config, {praktikumsgruppe,teamnummer,nameservicenode,koordinatorname}).

%%
%% Include files
%%
-import(util, [timestamp/0]).
-import(werkzeug,[get_config_value/2]).
%%
%% Exported Functions
%%
-compile([export_all]).

%%
%% API Functions
%%

start(Nummer) ->
	Config = get_configs(),
	net_adm:ping(Config#starter_config.nameservicenode),
	Nameservice = global:whereis_name(Config#starter_config.nameservicenode),
	if
		Nameservice == undefined -> log("Koordinator wurde nicht gefunden");
		true -> Nameservice ! {self(), {lookup, Config#starter_config.koordinatorname}}
	end,
	receive
		{KoordinatorName, KoordinatorNode} ->
			log("Koordinator ~p in der Node ~p gebunden",[KoordinatorName,KoordinatorNode]),
			{KoordinatorName, KoordinatorNode} ! {getsteeringval, self()},
			receive
				{steeringval, ArbeitsZeit, TermZeit, GGTProzessAnzahl} ->
					log("Erstelle GGT-Prozess (Steeringval:~p , Arbeitszeit: ~p , TermZeit: ~p , GGTProzessAnzahl: ~p)",[steeringval, ArbeitsZeit, TermZeit, GGTProzessAnzahl]),
					Fun = fun(ProzessNr) ->
								  ggt:start({KoordinatorName, KoordinatorNode},ArbeitsZeit*1000,TermZeit*1000,ProzessNr,Nummer,Nameservice,Config#starter_config.praktikumsgruppe,Config#starter_config.teamnummer),
								  log("GGT-Prozess Nr: ~p wurde gestartet",[ProzessNr])
						  end,
					lists:foreach(Fun, lists:seq(1, GGTProzessAnzahl))
			end;
		not_found -> log("Koordinator node konnte nicht gebunden werden")
	end.

%%
%% Local Functions
%%
get_configs()->
	{ok, ConfigListe} = file:consult("ggt.cfg"),
	{ok, Praktikumsgruppe} = get_config_value(praktikumsgruppe, ConfigListe),
	{ok, Teamnummer} = get_config_value(teamnummer, ConfigListe),
	{ok, Nameservicenode} = get_config_value(nameservicenode, ConfigListe),
	{ok, Koordinatorname} = get_config_value(koordinatorname, ConfigListe),
	#starter_config{
					praktikumsgruppe = Praktikumsgruppe,
					teamnummer = Teamnummer,
					nameservicenode = Nameservicenode,
					koordinatorname=Koordinatorname
					}.

log(Message) ->
  log(Message, []).
log(Message, Data) ->
  util:log(logfile(), Message, Data).

logfile() ->
	{ok, Hostname} = inet:gethostname(),
  "Starter@"+[Hostname]+".log".
