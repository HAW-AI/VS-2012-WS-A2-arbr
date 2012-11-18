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
	global:sync(),
	Nameservice = global:whereis_name(nameservice),
	if
		Nameservice == undefined -> log("Nameservice wurde nicht gefunden",Nummer), exit("Nameservice wurde nicht gefunden");
		true -> Nameservice ! {self(), {lookup, Config#starter_config.koordinatorname}}
	end,
	receive
		{KoordinatorName, KoordinatorNode} ->
			log("Koordinator ~p in der Node ~p gebunden",[KoordinatorName,KoordinatorNode],Nummer),
			{KoordinatorName, KoordinatorNode} ! {getsteeringval, self()},
			receive
				{steeringval, ArbeitsZeit, TermZeit, GGTProzessAnzahl} ->
					log("Erstelle GGT-Prozess (Steeringval:~p , Arbeitszeit: ~p , TermZeit: ~p , GGTProzessAnzahl: ~p)",[steeringval, ArbeitsZeit, TermZeit, GGTProzessAnzahl],Nummer),
					Fun = fun(ProzessNr) ->
								  ggt:start({KoordinatorName, KoordinatorNode},ArbeitsZeit*1000,TermZeit*1000,ProzessNr,Nummer,Nameservice,Config#starter_config.praktikumsgruppe,Config#starter_config.teamnummer),
								  log("GGT-Prozess Nr: ~p wurde gestartet",[ProzessNr],Nummer)
						  end,
					lists:foreach(Fun, lists:seq(1, GGTProzessAnzahl))
			end;
		not_found -> log("Koordinator node konnte nicht gebunden werden",Nummer)
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

log(Message,Nummer) ->
  log(Message, [],Nummer).
log(Message, Data,Nummer) ->
  util:log(logfile(Nummer), Message, Data).

logfile(Nummer) ->
	{ok, Hostname} = inet:gethostname(),
	StringNummer = lists:flatten(io_lib:format("~p", [Nummer])),
  "Starter_"++StringNummer++"@"++Hostname++".log".
