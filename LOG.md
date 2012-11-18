# Log
## coordinator.erl
* Die Anzahl der Nameserver Abfragen wurde reduziert, in dem die PID im ```gcd_client``` mitgeführt wird.
* Aufrufe von ```send_message_to_service``` werden vermieden, und dafür direkt die ```#gcd_client.servicepid``` verwendet.
* Den ggt-Prozessen werden die Nachbarn nun als Atom der Namen, anstelle von PID, gesendet. 

## ggt.erl
* Zeitstempel zu Logs hinzugefügt.
* Fehlerhafters Logging, durch übergabe des falschen ```MI```, in sendToNs korrigiert.
* Reagiert nun auf ```{setpm, NewMi}``` in Arbeitsschleife. 
* ```Config``` wird an ```algo```, für's logging, übergeben.
* Wartezeit für ```after```-Blöcke wurde vervielfacht.
* Es wird nun in ```algo``` korrekt der neu errechnete ```MI``` zurückgegeben.
* {sendy,Y} blöcke in ```loop``` und ```Abstimmung``` angeblichen (Logging). 

## starter.erl
* Es wurde ein ```global:sync()``` nach dem pingen des Nameservice-Nodes eingefügt. 