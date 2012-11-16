VS-2012-WS-A2-arbr
==================
Verteilte Systeme - Winter Semester 2012 - Aufgabe 2

0. Folgende Dateien müssen kompiliert werden: coordinator.erl, util.erl, werkzeug.erl, ggT.erl, starter.erl

1. Nameservice starten:
	> erl -(s)name ns -cookie vsp  
	
	``` nameservice:start(). ```
	
2. Koordinator starten:
	> erl -(s)name coordinator -cookie vsp  
	
	``` coordinator:start(). ```
	
3. Starter starten:
	> erl -(s)name starter -cookie vsp  
	
	``` starter:start(N). ```

	N - Anzahl der ggT-Prozesse, die erzeugt werden müssen
	
4. Koordinator in Ready-Zustand versetzen:
	``` chef ! get_ready. ``` 
	oder 
	``` coordinator:get_ready(). ```
	
5. Über Koordinator den Start-Befehl erteilen:
	``` chef ! {start_distributed_gcd_calculation, N} ```
	oder 
	``` coordinator:start_distributed_gcd_calculation(N). ```
	N - Anzahl
	
6. Koordinator neustarten:
	``` chef ! reset. ```
	oder
	``` coordinator:reset(). ```
	
7. Beenden:
	``` chef ! kill. ``` 
	oder
	``` coordinator:kill(). ```
	

