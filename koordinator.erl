%         ...                                                 ..          .                                   s
%     .xH8%"```"%.                                          dF           @88>                                :8
%    x888~ xnHhx. ".         u.          u.      .u    .   '88bu.        %8P      u.    u.                  .88           u.      .u    .
%   X888X 8**8888k `.  ...ue888b   ...ue888b   .d88B :@8c  '*88888bu      .     x@88k u@88c.       u       :888ooo  ...ue888b   .d88B :@8c
%   8888X<~  `8888L !  888R Y888r  888R Y888r ="8888f8888r   ^"*8888N   .@88u  ^"8888""8888"    us888u.  -*8888888  888R Y888r ="8888f8888r
%   88888!   .!8*"" `  888R I888>  888R I888>   4888>'88"   beWE "888L ''888E`   8888  888R  .@88 "8888"   8888     888R I888>   4888>'88"
%   `88888!"*888x      888R I888>  888R I888>   4888> '     888E  888E   888E    8888  888R  9888  9888    8888     888R I888>   4888> '
%    `*8888  8888L     888R I888>  888R I888>   4888>       888E  888E   888E    8888  888R  9888  9888    8888     888R I888>   4888>
%   .x.`888X X888X    u8888cJ888  u8888cJ888   .d888L .+    888E  888F   888E    8888  888R  9888  9888   .8888Lu= u8888cJ888   .d888L .+
%  '888> %8X !8888..-  "*888*P"    "*888*P"    ^"8888*"    .888N..888    888&   "*88*" 8888" 9888  9888   ^%888*    "*888*P"    ^"8888*"
%  '888   8  '8888%`     'Y"         'Y"          "Y"       `"888*""     R888"    ""   'Y"   "888*""888"    'Y"       'Y"          "Y"
%    "*=="     ""                                              ""         ""                  ^Y"   ^Y'

-module(koordinator).
-author("Ben Rexin <benjamin.rexin@haw-hamburg.de>").

-export([start/0]).
-compile([export_all]).

-import(util, [log/3]).
-import(shuffle, [list/1]).

-record(state,{
    config,
    clients=[]
  }).

% Der Koordinator, der den verteilten Algorithmus verwaltet.
% Dazu gehören das hochfahren des Systems, den Start einer ggT-Berechnung und das herunterfahren des Systems.
% Der Koordinator verfügt über eine GUI (Textausgabe reicht aus), in der der Ablauf des Algorithmus beobachtet werden kann.
start() ->
  {ok, Config} = file:consult('koordinator.cfg'),
  State = #state{config=Config},
  KoordinatorPid = spawn(fun() -> log("starting koordinator"), loop(State) end),
  register(proplists:get_value(koordinatorname, Config), KoordinatorPid),
  ok.

% * Die benötigten steuernden Werte sind aus der Datei koordinator.cfg auszulesen und umfassen:
%     die Erlang-Node des Namensdienstes, die Anzahl der ggT-Prozesse pro Starter, die Verzögerungszeit der ggT-Prozesse als
%     simulierter Arbeitsaufwand für die Berechnung sowie die Terminierungszeit **, nach der ein ggT-Prozess eine
%     Terminierungsabstimmung durchführt.
% * Nach dem Start des Koordinators können sich Starter und/oder ggT-Prozesse bei ihm melden.
% * Ist der Koordinator im Zustand "initial", gibt er den Startern auf Anfrage die benötigten Informationen über Anzahl der
%     zu startenden ggT-Prozesse, deren jeweilige Verzögerungszeit und deren **-Terminierungszeit.
% * In seiner GUI (Textausgabe reicht) und seiner Logdatei werden die sich angemeldeten ggT-Prozesse angezeigt.
% * Wird manuell der Koordinator in den Zustand "bereit" versetzt, gibt er keinem Starter mehr Auskunft und registriert
%     keine ggT-Prozesse mehr! Er baut nun den Ring auf, indem er per Zufall die ggT-Prozesse in einem Ring anordnet.
%     Erst danach geht er in den Zustand "bereit".
% * Ist der Koordinator im Zustand "bereit" wird per manuellem Befehl (z.B. Knopfdruck oder Nachricht) eine ggT-Berechnung
%     gestartet. Der Koordinator informiert dann alle ggT-Prozesse über deren jeweilige Startwerte #Mi und startet die
%     Berechnung. Für die Startwerte #Mi erhält er aus der GUI oder per Zufallswert den gewünschten ggT gewünschter_ggT
%     (dieser dient zur Kontrolle der Berechnung!) und multipliziert ihn wie folgt: aus der Menge der Primzahlen
%     {3, 5, 11, 13, 23, 37} wird ein Produkt erstellt mit zufällig gewählten Exponenten aus der Menge {0, 1, 2}:  z.B.
%     Mi = gewünschter_ggT * 32 * 51 * 110 * 132 * 231 * 371 oder z.B. Mj = gewünschter_ggT * 31 * 51 * 111 * 132 * 232 * 370
% * Er wählt dann per Zufall 15% aller ggT-Prozesse aus, denen er zum Start der Berechnung eine Zahl per sendy sendet, die
%     sich analog zu Punkt 6. berechnet
% * In seiner GUI (Textausgabe reicht) und seiner Logdatei zeigt der Koordinator alle Nachrichten der ggT-Prozesse und
%     Starter an.
% * Per manueller Eingabe (Knopfdruck oder Nachricht senden) kann der Koordinator in den Zustand "beenden" versetzt werden
%     oder eine neue ggt-Berechnung starten.
% * Ist der Koordinator im Zustand "beenden" informiert er die ggT-Prozesse über die Beendigung (kill).
% * Der Koordinator ist in Erlang/OTP zu implementieren und muss auf jedem Rechner im Labor startbar sein!

loop(State) -> loop_initial(State).

loop_initial(State) ->
  receive
    % Die Anfrage nach den steuernden Werten durch den Starter Prozess.
    { getsteeringval, Sender } ->
      log("(~p) getsteeringval", [Sender]),
      % {steeringval,ArbeitsZeit,TermZeit,GGTProzessnummer}
      ArbeitsZeit = proplists:get_value(arbeitszeit, State#state.config),
      TermZeit = proplists:get_value(termzeit, State#state.config),
      GGTProzessnummer = proplists:get_value(ggtprozessnummer, State#state.config),
      Sender ! { steeringval, ArbeitsZeit, TermZeit, GGTProzessnummer },
      loop_initial(State);

    % Ein ggT-Prozess meldet sich beim Koordinator mit Namen Clientname an (Name ist der lokal registrierte Name!).
    { hello, SenderName } ->
      log("(~s) hello", [SenderName]),
      Clients = lists:append(SenderName, State#state.clients),
      loop_initial(State#state{clients=Clients});

    start ->
      log("start"),
      Clients = shuffle:list(State#state.clients),
      loop_work(State#state{clients=Clients})
  end.

loop_work(State) ->
  receive
    % Ein ggT-Prozess mit Namen Clientname informiert über sein neues Mi CMi um CZeit Uhr.
    { briefmi, { Clientname, CMi, CZeit } } ->
      log("(~s) briefmi", [Clientname]),
      ok;

    % Ein ggT-Prozess mit Namen Clientname informiert über über die Terminierung der Berechnung mit Ergebnis CMi um CZeit Uhr.
    { briefterm, { Clientname, CMi, CZeit } } ->
      log("(~s) briefterm", [Clientname]),
      ok;

    %: Der Koordinator sendet allen ggT-Prozessen das kill-Kommando und bringt sich selbst in den initialen Zustand, indem sich Starter wieder melden können.
    reset ->
      log("reset"),
      lists:foreach(
        % kill: der ggT-Prozess wird beendet.
        fun(client) -> client ! kill end,
        State#state.clients
      ),
      loop_initial(State);

    % Der Koordinator wird beendet und sendet allen ggT-Prozessen das kill-Kommando.
    kill ->
      log("kill"),
      lists:foreach(
        % kill: der ggT-Prozess wird beendet.
        fun(client) -> client ! kill end,
        State#state.clients
      ),
      ok
  end.


log(Message) ->
  log(Message, []).
log(Message, Data) -> spawn(fun() ->
  util:log(logfile(), Message, Data)
  end).

logfile() ->
  { ok, Hostname } = inet:gethostname(),
  lists:flatten(io_lib:format("~s@~s.~s", ["Koordinator", Hostname, "log"])).




