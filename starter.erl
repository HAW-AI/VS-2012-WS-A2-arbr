%% Author: anton
%% Created: 01.11.2012
%% Description: TODO: Add description to starter
-module(starter).


-record(starter_config, {nummer,praktikumsgruppe,teamnummer,nameservicenode,koordinatorname}).

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
start(Nummer)->
	{ok, ConfigListe} = file:consult("ggt.cfg"),
	Praktikumsgruppe = proplists:get_value(praktikumsgruppe, ConfigListe),
	Teamnummer = poplists:get_value(teamnummer, ConfigListe),
	Nameservicenode = poplists:get_value(nameservicenode, ConfigListe),
	Koordinatorname = poplists:get_value(koordinatorname, ConfigListe),
	Config = #starter_config{nummer=Nummer,
							 praktikumsgruppe = Praktikumsgruppe,
							 teamnummer = Teamnummer,
							 nameservicenode = Nameservicenode,
							 koordinatorname=Koordinatorname
							},
	spawn(fun() -> log("Starter ~p (~p)gestartet",[self(), Nummer]), loop(Config) end).

loop(Config) ->
	Config#starter_config.koordinatorname ! getsteeringval,
	receive
		{steeringval,ArbeitsZeit,TermZeit,GGTProzessnummer} -> ok;
		Any -> log("Eine unbekannte Nachricht ~p wurde empfange", [Any])
	end,
	ok.

%%
%% Local Functions
%%
log(Message) ->
  log(Message, []).
log(Message, Data) ->
  util:log(logfile(), Message, Data).

logfile() ->
  "Starter.log".
