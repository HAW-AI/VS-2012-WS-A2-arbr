VS-2012-WS-A2-arbr
==================
Verteilte Systeme - Winter Semester 2012 - Aufgabe 2

0. Folgende Dateien müssen kompiliert werden: koordinator.erl, util.erl, werkzeug.erl, ggT.erl, starter.erl

1. Nameservice starten:
	> erl -(s)name ns -cookie vsp  
	
	``` nameservice:start(). ```
	
2. Koordinator starten:
	> erl -(s)name koordinator -cookie vsp  
	
	``` koordinator:start(). ```
	
3. Starter starten:
	> erl -(s)name starter -cookie vsp  
	
	``` starter:start(N). ```

	N - Anzahl der ggT-Prozesse, die erzeugt werden müssen
	
4. Koordinator in Ready-Zustand versetzen:
	``` chef ! ready. ``` 
	
5. Über Koordinator den Start-Befehl erteilen:
	``` chef ! {calculate, N} ```
	N - ggT
	
6. Koordinator neustarten:
	``` chef ! reset. ```
	
7. Beenden:
	``` chef ! kill. ``` 

