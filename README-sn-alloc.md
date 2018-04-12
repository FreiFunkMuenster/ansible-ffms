
## Verteilung der Domänen auf die einzelnen Hosts

Skript ausführen: `./sn-alloc.R`, benötigte R-Pakete: `jsonlite`, `operators` und `combinat`:

Mit dem Skript `sn-alloc.R` werden die Domänen möglichst gleichmäßig auf die einzelnen Hosts verteilt. Dies erfolgt vorranging anhand der Anzahl an L2TP-Verbindungen und nachrangig anhand des Datendurchsatzes. In der `hosts`-Datei ist für jeden Host eine Kennzahl für dessen Leistungsfähigkeit hinterlegt (`capacity=xx`). Falls mehrere Gateways auf dem selben physischen Gerät liegen, ist zusätzlich die Angabe `server=xx` nötig, damit einer Domäne nicht zwei VM auf demselben Gerät zugeordnet werden. Als Datenbasis werden die Zeitreihen der L2TP-Verbindungen und Datendurchsatzraten genutzt. Diese werden automatisch von https://graphite.freifunk-muensterland.de geholt.

    #./hosts
     [...]
    [gateways]
    tj01   ansible_ssh_host=46.4.122.254   ansible_ssh_port=22   # capacity=0.70
     [...]
    remue-09   ansible_ssh_host=148.251.208.170   ansible_ssh_port=22   # capacity=1.00 server=remue
    
Nach der Verteilung werden die Listen in die `host_vars/`-Dateien geschrieben. Daber wird der DHCP-Bereich aus Domänennummer und `server_id` berechet: .64.0 bis .127.255 für `server_id: 2` und .128.0 bis .191.255 für `server_id: 3`. Weiter wird das "Partner-Gateway" für die Domäne angegeben.

    domaenenliste:
     [...]
       "34":
          dhcp_start: 10.34.128.0 
          dhcp_end: 10.34.191.255
          server_id: 3
          partner:"des2"
          
### Manuelle Zuordnung von Gateways

Einer Domäne können die Gateways auch manuell zugeordnet werden. Die manuelle Zuordnung ist in der Datei `sn-alloc-manual.csv` hinterlegt, diese verarbeitet das Skript zuerst und verteilt dann die weiteren Domänen entsprechend der verfügbaren Kapazität.
    
    dom	gw1     gw2
    16	parad0x c1024
