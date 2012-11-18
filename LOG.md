# Log
## coordinator.erl
* Die Anzahl der Nameserver Abfragen wurde reduziert, in dem die PID im ```gcd_client``` mitgeführt wird.
* Aufrufe von ```send_message_to_service``` werden vermieden, und dafür direkt die ```#gcd_client.servicepid``` verwendet.

## ggt.erl
* Zeitstempel zu Logs hinzugefügt.
* Fehlerhafters Logging, durch übergabe des falschen ```MI```, in sendToNs korrigiert.
* Reagiert nun auf ```{setpm, NewMi}``` in Arbeitsschleife. 
* ```Config``` wird an ```algo```, für's logging, übergeben.

## starter.erl
* Es wurde ein ```global:sync()``` nach dem pingen des Nameservice-Nodes eingefügt. 