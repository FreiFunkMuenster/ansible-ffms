# R-skript zu Verteilung der einzelnen Domänen auf die Supernodes. Ziel ist eine möglichst gleichmäßige Auslastung.

library("jsonlite")
library("operators")

# Daten zu L2TP-Verbindungen und Tx/Rx der letzten sieben Tage
download.file("https://graphite.freifunk-muensterland.de/render/?target=aliasByNode(aliasSub(sumSeriesWithWildcards(gateways.*.l2tp.if_count-br*,%201),%20%27if_count-br%27,%20%27domaene-%27),%202)&format=json&from=NOW-7d&to=NOW", "ffms-l2tp.json")
download.file("https://graphite.freifunk-muensterland.de/render/?width=586&height=308&target=aliasByNode(aliasSub(sumSeriesWithWildcards(perSecond(scale(gateways.*.interface-bat*.if_octets.*,%208)),%201,%204),%20%27interface-bat%27,%20%27domaene-%27),%201)&format=json&from=NOW-7d&to=NOW", "ffms-tx-rx.json")

l2tp <- fromJSON("ffms-l2tp.json", flatten=TRUE)
tx.rx <- fromJSON("ffms-tx-rx.json", flatten=TRUE)
jct <- match(l2tp$target, tx.rx$target)

# Supernodes aus der hosts-Datei auslesen
sn.raw <- read.table("./hosts", sep="\n", colClasses="character", comment.char="")
sn <- data.frame(name="", srv="", perf=0, vm.id=0)[-1,]
sn$name <- as.character(sn$name)
sn$srv <- as.character(sn$srv)

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

# DHCP-Bereiche aus den host-vars-Dateien auslesen
dhcp <- list()

for (i in sn$name) {
	begin.list <- which(host.vars.raw[[i]] == "domaenenliste:")+1
	end.list <- which(host.vars.raw[[i]][-(1:begin.list),] == "")[1]+begin.list-1
	for (j in begin.list:end.list) {
		if (host.vars.raw[[i]][j,] %!~% "[a-z]") {
			dhcp.set <- host.vars.raw[[i]][j:(j+3),]
			dom <- strsplit(rev(strsplit(as.character(strsplit(host.vars.raw[[i]][j,], ":")[[1]]), " ")[[1]])[1], "\"")[[1]][2]
			srv <- as.character(rev(strsplit(dhcp.set[as.numeric(which(dhcp.set %~% "server_id"))], split=" ")[[1]])[1])
			dhcp[[dom]][[srv]] <- dhcp.set
		}
	}
}

# data frames für die Verarbeitung anpassen
sn$name <- as.factor(sn$name)
sn$srv <- as.factor(sn$srv)
sn$perf = sn$perf/sum(sn$perf)

sn <- cbind(sn, l2tp=sn$perf, tx.rx=sn$perf)

total <- data.frame(dom=as.character(), l2tp=as.numeric(), tx.rx=as.numeric())

for (i in 1:length(jct)) {
	total <- rbind(total, data.frame(
		dom=strsplit(l2tp$target[i], "-")[[1]][2], 
		l2tp=mean(l2tp$datapoints[[i]][,1], na.rm=TRUE), 
		tx.rx=mean(tx.rx$datapoints[jct[i]][[1]][,1], na.rm=TRUE))
	)
}

total <- total[!is.na(total$l2tp),]
total$l2tp <- jitter(total$l2tp/sum(total$l2tp)*sum(sn$perf)/2)
total$tx.rx <- jitter(total$tx.rx/sum(total$tx.rx)*sum(sn$perf)/2)

total <- cbind(total, rank=(total$l2tp+total$tx.rx)/2)

total <- total[order(total$rank, decreasing=TRUE),]

total <- cbind(total, gw1="", gw2="")
levels(total$gw1) <- levels(sn$name)
levels(total$gw2) <- levels(sn$name)

# Verteilung der Domänen
for (i in row(total)[,1]) {
	if (max(sn$l2tp) > max(sn$tx.rx)) {
		sn <- sn[order(sn$l2tp, decreasing=TRUE),]
		total$gw1[i] <- sn$name[1]
		sn$l2tp[1] <- sn$l2tp[1] - total$l2tp[i]
		sn$tx.rx[1] <- sn$tx.rx[1] - total$tx.rx[i]
		if (sn$srv[1] != sn$srv[3]) {
		total$gw2[i] <- sn$name[3] 
		sn$l2tp[3] <- sn$l2tp[3] - total$l2tp[i]
		sn$tx.rx[3] <- sn$tx.rx[3] - total$tx.rx[i]
		}
		else { total$gw2[i] <- sn$name[2]
		sn$l2tp[2] <- sn$l2tp[2] - total$l2tp[i]
		sn$tx.rx[2] <- sn$tx.rx[2] - total$tx.rx[i]
		}
	}
	else {
		sn <- sn[order(sn$tx.rx, decreasing=TRUE),]
		total$gw1[i] <- sn$name[1]
		sn$l2tp[1] <- sn$l2tp[1] - total$l2tp[i]
		sn$tx.rx[1] <- sn$tx.rx[1] - total$tx.rx[i]
		if (sn$srv[1] != sn$srv[2]) {
		total$gw2[i] <- sn$name[2] 
		sn$l2tp[2] <- sn$l2tp[2] - total$l2tp[i]
		sn$tx.rx[2] <- sn$tx.rx[2] - total$tx.rx[i]
		}
		else { total$gw2[i] <- sn$name[3]
		sn$l2tp[3] <- sn$l2tp[3] - total$l2tp[i]
		sn$tx.rx[3] <- sn$tx.rx[3] - total$tx.rx[i]
		}
	}
}

#Ergebnis in separate Dateien schreiben
#write.csv2(sn, file="sn-perf.csv")
#write.csv2(total, file="sn-alloc.csv")

host.vars.new <- list()

# Domänenlisten in die host_vars-Dateien schreiben
for (i in sn$name) {
	domlist.new <- as.character()
	domlist <- as.character(total$dom[c(which(total$gw1 == i), which(total$gw2 == i))])
	domlist <- domlist[order(domlist)]
	for (j in 1:length(domlist)) { 
		serverlist <-  c(sn[sn$name == total[total$dom == domlist[j],]$gw1,]$vm.id, sn[sn$name == total[total$dom == domlist[j],]$gw2,]$vm.id)	
		if (sn$vm.id[sn$name == i] == min(serverlist)) { dhcp.set <- dhcp[[domlist[j]]]["2"][[1]] } else dhcp.set <- dhcp[[domlist[j]]]["3"][[1]]
		domlist.new <- c(domlist.new, dhcp.set)
	}
	
	host.vars.new[[i]] <- c(host.vars.raw[[i]][1:which(host.vars.raw[[i]] == "domaenenliste:"),],
	domlist.new,	
	host.vars.raw[[i]][(min(which(host.vars.raw[[i]][-(1:which(host.vars.raw[[i]] == "domaenenliste:")),] == "")) + which(host.vars.raw[[i]] == "domaenenliste:")):dim(host.vars.raw[[i]])[1],])
	fileConn <- file(paste("./host_vars/",i,sep=""))
	writeLines(host.vars.new[[i]], fileConn)
	close(fileConn)
}
