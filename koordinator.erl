-module(koordinator).
-author("Ben Rexin <benjamin.rexin@haw-hamburg.de>").

-export([start/0]).

-record(state,{
    config,
    clients=orddict:new()
  }).

% Der Koordinator, der den verteilten Algorithmus verwaltet.
% Dazu gehören das hochfahren des Systems, den Start einer ggT-Berechnung und das herunterfahren des Systems.
% Der Koordinator verfügt über eine GUI (Textausgabe reicht aus), in der der Ablauf des Algorithmus beobachtet werden kann.
start() ->
  {ok, Config} = file:consult('koordinator.cfg'),
  State = #state{config=Config}
  spawn(fun() -> loop(State) end),
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
      ok;

    % Ein ggT-Prozess meldet sich beim Koordinator mit Namen Clientname an (Name ist der lokal registrierte Name!).
    { hello, SenderName } ->
      ok;

  end.

loop_work(State) ->
  receive
    % Ein ggT-Prozess mit Namen Clientname informiert über sein neues Mi CMi um CZeit Uhr.
    { briefmi, { Clientname, CMi, CZeit } } ->
      ok;

    % Ein ggT-Prozess mit Namen Clientname informiert über über die Terminierung der Berechnung mit Ergebnis CMi um CZeit Uhr.
    { briefterm, { Clientname, CMi, CZeit } } ->
      ok;

    %: Der Koordinator sendet allen ggT-Prozessen das kill-Kommando und bringt sich selbst in den initialen Zustand, indem sich Starter wieder melden können.
    reset ->
      loop_initial(State);

    % Der Koordinator wird beendet und sendet allen ggT-Prozessen das kill-Kommando.
    kill ->
      ok
  end.
