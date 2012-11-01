%% Author: anton
%% Created: 01.11.2012
%% Description: TODO: Add description to ggt
-module(ggt).

-record(ggt_config, {delay,terminatetime,startnummer,starternummer,praktikumsgruppe,teamnummer, koordinatorname,nameservicenode}).
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
start(Delay, Terminatetime,Startnummer, Starternummer,Praktikumsgruppe,Teamnummer, Koordinatorname, Nameservicenode) ->
	Config = #ggt_config{
						 delay = Delay,
						 terminatetime = Terminatetime,
						 startnummer = Startnummer,
						 starternummer = Starternummer,
						 koordinatorname = Koordinatorname,
						 praktikumsgruppe = Praktikumsgruppe,
						 teamnummer = Teamnummer,
						 nameservicenode = Nameservicenode
						 },
	log("Name: ~p",Config,[toString(name(Config))]),
	spawn(fun() -> log("GGTP ~p (~p) gestartet",Config,[self(), name(Config)]), loop(Config) end).


%%
%% Local Functions
%%
loop(Config)->
	%Config#ggt_config.koordinatorname ! {hello, name(Config)},
	%Config#ggt_config.nameservicenode ! {rebind, name(Config)},
	register(list_to_atom(toString(name(Config))), self()),
	receive
		{setneighbors,LeftN,RightN} -> ok
	end,
	ok.

name(Config) ->
	%
	Stringname = toString(Config#ggt_config.praktikumsgruppe)++toString(Config#ggt_config.teamnummer)++toString(Config#ggt_config.startnummer)++toString(Config#ggt_config.starternummer),
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