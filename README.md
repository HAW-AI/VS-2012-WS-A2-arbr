VS-2012-WS-A2-arbr
==================
Verteilte Systeme - Winter Semester 2012 - Aufgabe 2

## Verwendung
**Hinweis: ** Erfolgreich unter "Erlang R15B02" getestet. 

0. Folgende Dateien müssen kompiliert werden: nameservice.erl, coordinator.erl, util.erl, werkzeug.erl, ggt.erl, starter.erl

	```c(nameservice). c(util). c(werkzeug). c(ggt). c(starter). c(coordinator).```

1. Nameservice starten:
	> erl -(s)name ns -cookie vsp  
	
	``` nameservice:start(). ```
	
2. Koordinator starten:
	> erl -(s)name coordinator -cookie vsp  
	
	``` coordinator:start(). ```
	
3. Starter starten:
	> erl -(s)name starter -cookie vsp  
	
	``` starter:start(N). ```

	N - Nummer des Starters
	
4. Koordinator in Ready-Zustand versetzen:
	``` chef ! get_ready. ``` 
	oder 
	``` coordinator:get_ready(). ```
	
5. Über Koordinator den Start-Befehl erteilen:
	``` chef ! {start_distributed_gcd_calculation, N} ```
	oder 
	``` coordinator:start_distributed_gcd_calculation(N). ```
	N - ggT
	
6. Koordinator zurücksetzen:
	Es werden alle ggt-Prozesse beendet und der Koordinator in den Anfangszustand zurück versetzt.
	``` chef ! reset. ```
	oder
	``` coordinator:reset(). ```
	
7. Beenden:
	``` chef ! kill. ``` 
	oder
	``` coordinator:kill(). ```
	

## Probleme
Wir haben die Aufgabenstellung unter "Erlang R15B02" erfolgreich testen können, ein Protokoll des Ablaufs kann in ```log_ben_local``` nachvollzogen werden. Leider kommt es unter "Erlang R14B02" zu einem für uns nicht nachvollziehbarem Problem, so das die ggt Prozesse frühzeitig die Berechnung beenden. Nach Recherche verwendet wir keine "außergewöhnlichen"/Versionsabhängigen Funktionalitäten. 