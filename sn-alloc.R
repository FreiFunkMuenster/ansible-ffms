#! /usr/bin/Rscript

# R-skript zu Verteilung der einzelnen Domänen auf die Supernodes. Ziel ist eine möglichst gleichmäßige Auslastung.

library("jsonlite")
library("operators")
library("combinat")

# Daten zu L2TP-Verbindungen und Tx/Rx der letzten sieben Tage
download.file("https://graphite.freifunk-muensterland.de/render/?target=aliasByNode(aliasSub(sumSeriesWithWildcards(gateways.*.l2tp.if_count-br*,%201),%20%27if_count-br%27,%20%27domaene-%27),%202)&format=json&from=NOW-15d&until=NOW-1h", "ffms-l2tp.tmp.json")
download.file("https://graphite.freifunk-muensterland.de/render/?&target=aliasByNode(aliasSub(sumSeriesWithWildcards(perSecond(scale(gateways.*.interface-bat*.if_octets.*,%208)),%201,%204),%20%27interface-bat%27,%20%27domaene-%27),%201)&format=json&from=NOW-15d&until=NOW-1h", "ffms-tx-rx.tmp.json")

l2tp <- fromJSON("ffms-l2tp.tmp.json", flatten=TRUE)
tx.rx <- fromJSON("ffms-tx-rx.tmp.json", flatten=TRUE)

for (i in 1:length(l2tp$target)) { l2tp$target[i] <- strsplit(l2tp$target[i], "-")[[1]][2] }
for (i in 1:length(tx.rx$target)) { tx.rx$target[i] <- strsplit(tx.rx$target[i], "-")[[1]][2] }

# Manuelle Zuordnung einlesen
if (file.exists("sn-alloc-manual.csv")) { 
	domains.manual <- read.table("sn-alloc-manual.csv", colClasses=c("character", "character", "character"), sep="\t", header=TRUE)
	message("Manual domain allocation file found:\n")
	print(domains.manual)
}

# Bestehende Allokationstabelle (zzgl. ggf. neuer Domains) auslesen, falls Datei vorhanden
if (file.exists("domains.csv")) { 
	domains.tab <- read.csv2("domains.csv", colClasses=c("character", "factor", "factor", "numeric", "numeric")) 
} else {
	domains.tab <- data.frame(dom=l2tp$target, l2tp=0, tx.rx=0)
}

jct.l2tp <- match(domains.tab$dom, l2tp$target)
jct.tx.rx <- match(domains.tab$dom, tx.rx$target)

# Supernodes aus der hosts-Datei auslesen
sn.raw <- read.table("./hosts", sep="\n", colClasses="character", comment.char="")
sn <- data.frame(name="", srv="", perf=0, vm.id=0)[-1,]
sn$name <- as.character(sn$name)
sn$srv <- as.character(sn$srv)

# Kommentarzeilen entfernen
sn.rows <- numeric()
for (i in 1:dim(sn.raw)[1]) { if(strsplit(sn.raw[i,], "")[[1]][1] != "#") sn.rows <<- c(sn.rows, i) }
sn.raw <- data.frame(sn.raw[sn.rows,1])

for (i in row(sn.raw)) {
	if(sn.raw[i,1] %~% "\\[gateways\\]") {
		j <- 0
		while(TRUE) {
			i <- i + 1
			j <- j + 1
			if(substr(sn.raw[i,1], 1, 1) %~% "[A-Za-z0-9]")  {
				a <- strsplit(gsub("\t", " ", sn.raw[i,1]), split=" ")[[1]]
				sn[j,]$name <- a[1]
				sn[j,]$srv <- a[1]
				if(length(a[which(a %~% "server")] > 0)) { 
					sn[j,]$srv <- strsplit(a[which(a %~% "server")], "=")[[1]][2]
					}
				sn[j,]$perf <- as.numeric(strsplit(a[which(a %~% "capacity")], "=")[[1]][2])
			}
			else break
		}
	}
}

# vm_id aus den host_vars-Dateien auslesen
host.vars.raw <- list()

for (i in sn$name) {
	host.vars.raw[[i]] <- read.table(paste("./host_vars/",i,sep=""), sep="\n", colClasses="character", comment.char="", quote="", blank.lines.skip=FALSE)
	sn$vm.id[which(sn$name == i)] <- as.numeric(strsplit(host.vars.raw[[i]][which(host.vars.raw[[i]][[1]] %~% "vm_id"),], " ")[[1]][2])
}

# Funktion zur Berechnung der DHCP-Bereiche
calc.dhcp <- function(dom, length=63, offset=0, id) { 
	range <- c(paste("      dhcp_start: ",paste("10",as.numeric(dom),offset,0,sep="."),sep=""), paste("      dhcp_ende: ",paste("10",as.numeric(dom),offset+length,255,sep="."),sep=""))
	return(c(paste("   ",dom,":",sep="\""),range, paste("      server_id: ",id,sep="")))
}

# data frames für die Verarbeitung anpassen
# sn = Tabelle der Supernodes
# domains = Tabelle der Domänen
# com = Liste der Gateway-Kombinationen

sn$name <- as.factor(sn$name)
sn$srv <- as.factor(sn$srv)
sn$perf = sn$perf/sum(sn$perf)

sn <- cbind(sn, l2tp=sn$perf, tx.rx=sn$perf)

domains <- data.frame(dom=domains.tab$dom, l2tp=domains.tab$l2tp, tx.rx=domains.tab$tx.rx)

for (i in 1:length(domains$dom)) {
	if (!is.na(jct.l2tp[i])) {
		domains$l2tp[i] <- quantile(l2tp$datapoints[[jct.l2tp[i]]][,1], 0.8, na.rm=TRUE)[[1]]
		domains$tx.rx[i] <- mean(tx.rx$datapoints[jct.tx.rx[i]][[1]][,1], na.rm=TRUE)
	}
}

domains <- domains[!is.na(domains$l2tp),]
total.l2tp <- sum(domains$l2tp)
total.tx.rx <- sum(domains$tx.rx)
domains$l2tp <- domains$l2tp/total.l2tp*sum(sn$perf)/2
domains$tx.rx <- domains$tx.rx/total.tx.rx*sum(sn$perf)/2

domains <- cbind(domains, rank=(domains$l2tp+domains$tx.rx)/2)
domains <- domains[order(domains$rank, decreasing=TRUE),]

domains <- cbind(domains, gw1="", gw2="")
levels(domains$gw1) <- levels(sn$name)
levels(domains$gw2) <- levels(sn$name)

# Verteilung der Domänen

com <- data.frame(t(combn(sn$name, m=2)))
com <- rbind(com, data.frame(X1=com[,2],X2=com[,1]))
names(com) <- c("gw1","gw2")
com <- cbind(com, data.frame(srv1=sn$srv, srv2=sn$srv, l2tp=0, tx.rx=0, count=0))

for (i in row(com)[,1]) { 
	com$srv1[i] <- sn$srv[which(sn$name == com$gw1[i])]
	com$srv2[i] <- sn$srv[which(sn$name == com$gw2[i])]
}

# Alle Paare entfernen, die auf dem gleich physischen Gerät liegen
com <- com[com$srv1 != com$srv2,]

calc.perf <- function(srv,l2tp,tx.rx) { 
	com[which(com$gw1 == srv),]$l2tp <<- com[which(com$gw1 == srv),]$l2tp + l2tp
	com[which(com$gw2 == srv),]$l2tp <<- com[which(com$gw2 == srv),]$l2tp + l2tp
	com[which(com$gw1 == srv),]$tx.rx <<- com[which(com$gw1 == srv),]$tx.rx + tx.rx
	com[which(com$gw2 == srv),]$tx.rx <<- com[which(com$gw2 == srv),]$tx.rx + tx.rx
}
	
for (i in sn$name) {
	calc.perf(i, sn$l2tp[which(sn$name == i)], sn$tx.rx[which(sn$name == i)])
}

# Manuelle Domänenzuordnung zuweisen
for (i in row(domains.manual)[,1]) {
	dom.m <- domains$dom == domains.manual$dom
	com.m <- (as.character(com$gw1) == domains.manual$gw1[i]) & (as.character(com$gw2) == domains.manual$gw2[i])
	domains$gw1[dom.m] <- domains.manual$gw1[i]
	domains$gw2[dom.m] <- domains.manual$gw2[i]
	com$count[com.m] <- com$count[com.m] + 1
	num.l2tp <- domains$l2tp[dom.m]
	num.tx.rx <- domains$tx.rx[dom.m]
	calc.perf(com$gw1[com.m], -num.l2tp, -num.tx.rx)
	calc.perf(com$gw2[com.m], -num.l2tp, -num.tx.rx)
	sn[sn$name == com$gw1[com.m],]$l2tp <- sn[sn$name == com$gw1[com.m],]$l2tp - num.l2tp
	sn[sn$name == com$gw2[com.m],]$l2tp <- sn[sn$name == com$gw2[com.m],]$l2tp - num.l2tp
	sn[sn$name == com$gw1[com.m],]$tx.rx <- sn[sn$name == com$gw1[com.m],]$tx.rx - num.tx.rx
	sn[sn$name == com$gw2[com.m],]$tx.rx <- sn[sn$name == com$gw2[com.m],]$tx.rx - num.tx.rx
}

# Restliche Domänen automatisch verteilen
for (i in row(domains)[,1]) {
	if (!(domains$dom[i] %in% domains.manual$dom)) {
		com <- com[order(com$l2tp, decreasing=TRUE),]
	 	com <- com[order(com$count),]
		domains$gw1[i] <- com$gw1[1]
		domains$gw2[i] <- com$gw2[1]
		com$count[1] <- com$count[1] + 1
		num.l2tp <- domains$l2tp[i]
		num.tx.rx <- domains$tx.rx[i]
		calc.perf(com$gw1[1], -num.l2tp, -num.tx.rx)
		calc.perf(com$gw2[1], -num.l2tp, -num.tx.rx)
		sn[sn$name == com$gw1[1],]$l2tp <- sn[sn$name == com$gw1[1],]$l2tp - num.l2tp
		sn[sn$name == com$gw2[1],]$l2tp <- sn[sn$name == com$gw2[1],]$l2tp - num.l2tp
		sn[sn$name == com$gw1[1],]$tx.rx <- sn[sn$name == com$gw1[1],]$tx.rx - num.tx.rx
		sn[sn$name == com$gw2[1],]$tx.rx <- sn[sn$name == com$gw2[1],]$tx.rx - num.tx.rx
	}
}

domains$l2tp <- round(domains$l2tp*2*total.l2tp)
domains$tx.rx <- round(domains$tx.rx*2*total.tx.rx)

#Ergebnis in separate Dateien schreiben
#write.csv2(sn, file="sn-perf.csv")
write.csv2(domains[c("dom","gw1","gw2","l2tp","tx.rx")], file="domains.csv", row.names=FALSE)

# Domänenlisten in die host_vars-Dateien schreiben

list.domains <- function(dom) {
	domlist.new <- as.character()
	domlist <- as.character(domains$dom[c(which(domains$gw1 == dom), which(domains$gw2 == dom))])
	domlist[order(domlist)]
}

host.vars.new <- list()

for (i in sn$name) {
	domlist <- list.domains(i)
	domlist.new <- ""[-1]
	if (length(domlist) != 0) { 
		for (j in domlist) { 
			serverlist <-  c(sn[sn$name == domains[domains$dom == j,]$gw1,]$vm.id, sn[sn$name == domains[domains$dom == j,]$gw2,]$vm.id)	
			if (sn$vm.id[sn$name == i] == min(serverlist)) { dhcp.set <- calc.dhcp(j,offset=64,id=2) } else dhcp.set <- calc.dhcp(j,offset=128,id=3)
			domlist.new <- c(domlist.new, dhcp.set, 
				paste("      partner: \"",as.character(sn$name[which(sn$vm.id == serverlist[serverlist != sn$vm.id[sn$name == i]])]),"\"",sep=""))
		}
	}
	host.vars.new[[i]] <- c(host.vars.raw[[i]][1:which(host.vars.raw[[i]] == "domaenenliste:"),],
	domlist.new)
	# Schlussteil der Datei nur anfügen, wenn er auch existiert
	if(length(which(host.vars.raw[[i]][-(1:which(host.vars.raw[[i]] == "domaenenliste:")),] == "")) != 0) {	
		host.vars.new[[i]] <- c(host.vars.new[[i]], host.vars.raw[[i]][(min(which(host.vars.raw[[i]][-(1:which(host.vars.raw[[i]] == "domaenenliste:")),] == "")) + which(host.vars.raw[[i]] == "domaenenliste:")):dim(host.vars.raw[[i]])[1],])
	}
	fileConn <- file(paste("./host_vars/",i,sep=""))
	writeLines(host.vars.new[[i]], fileConn)
	close(fileConn)
}

print(domains)

message("\nAllocation table written to the file 'domains.csv'.\nTo add or remove domains, modifiy the file 'domains.csv' and run the script again.\nWhen adding domains, the value in the column 'L2TP' can be either 0 or a best guess but must not be empty.\nIf the L2TP-value is empty (=NA), the domain will be ignored.\nTo re-initialize the file, remove it and run the script again.\n")
