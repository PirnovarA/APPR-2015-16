# 2. faza: Uvoz podatkov

library(XML)
library(RCurl)
library(gdata)
library(dplyr)
#### Funkcije

#####
# Zapišimo podatke v razpredelnico national, podatki o poklicih in njihovih povprečnih plačah v ZDA
national <- read.csv2("podatki/national_2014.csv",
                      col.names = c("OCC_CODE","OCC_TITLE","OCC_GROUP","TOT_EMP","EMP_PRSE","H_MEAN","A_MEAN","MEAN_PRSE","H_PCT10","H_PCT25","H_MEDIAN","H_PCT75","H_PCT90","A_PCT10","A_PCT25","A_MEDIAN","A_PCT75","A_PCT90","ANNUAL","HOURLY"),
                      stringsAsFactors = FALSE,
                      fileEncoding = "UTF-8")
#Poimenoval vrstice
national <- (subset(national, select = -c(OCC_CODE,OCC_GROUP,HOURLY,ANNUAL,EMP_PRSE,MEAN_PRSE)))[-1,]
national <- national[!duplicated(national$OCC_TITLE),]
national <- (data.frame(national[,-1], row.names=national[,1]))[-1,]

#  # pomeni, da je urna placa>90$ oz letna>187200$ ... Za prakticne namene spremenimo to v stevilke
urne.place <- names(national)[match("H_PCT10",names(national)):match("H_PCT90",names(national))]
national[urne.place] <- lapply(national[urne.place], function(x) (gsub("[#]", "90", x)))

letne.place <- names(national)[match("A_PCT10",names(national)):match("A_PCT90",names(national))]
national[letne.place] <- lapply(national[letne.place], function(x) (gsub("[#]", "187200", x)))

# * pomeni, da podatki niso bili na voljo ali pa poklici,ki so plačani letno, ne na uro. * bomo spremenili v NA
national[] <- lapply(national[], function(x) (gsub("[*]","NA",x)))

# Sedaj odstranimo . in spremenimo vejice v decimalne pike, hkrati pa stolpce spremenimo v numeric
national[] <- lapply(national[], function(x) (gsub("[.]","",x)))
national[] <- lapply(national[], function(x) as.numeric(gsub("[,]",".",x)))

write.csv2(national, "podatki/national.csv", fileEncoding = "UTF-8")

##############################################################################
#Uvoz tabele s povprečnim prihodkom na državljana v zveznih državah
u= "http://www.infoplease.com/ipa/A0104652.html"
kategorije=c(1980, 1990, 1995, 2000, 2003, 2006, 2009, 2012)  #Imena stolpcev
tables = readHTMLTable(u, fileEncoding="UTF-8")   #Prebral HTML tabelce

per_capita= tables[[2]] #Izbral ta pravo tabelo
per_capita <- (data.frame(per_capita[,-1], row.names=per_capita[,1]))[-1,] #Poimenovanje vrstic, odstranil prvo vrstico, ker jo nadomestim z imensko
colnames(per_capita) <- kategorije #Poimenovanje stolpcev
per_capita <- per_capita[complete.cases(per_capita[,length(names(per_capita))]),]

#Odstranim dolarje, , nadomestim s . in spremenim v numeric
indx <- sapply(per_capita, is.factor) 
per_capita[indx] <- lapply(per_capita[indx], function(x) as.numeric(gsub("[,$]", "", x)))


write.csv2(per_capita, "podatki/per_capita.csv", fileEncoding = "UTF-8")
###########################################################################
#Uvoz tabele s povprecnim prihodkom na državljana v ZDA

u=getURL("https://en.wikipedia.org/wiki/List_of_U.S._states_by_income",.opts = list(ssl.verifypeer = FALSE) )
tables = readHTMLTable(u, fileEncoding = "UTF-8")
per_capita2 = tables[[3]]

kategorije <- c(names(per_capita2))[-1:-2] #Imena stolpcev, odstranil prvo kategorijo, ker bo 1. stolpec izbrisan, drugi pa uporabljen za imena
kategorije <- lapply(kategorije, function(x) (gsub("[\n]"," ", x)))  #Znebil se obveznih prelomov vrstic
per_capita2 <- per_capita2[,-1] #Odstranimo brezvezni stolpec kategorij
per_capita2 <- data.frame(per_capita2[,-1], row.names = per_capita2[,1]) #Damo imena vrsticam, imenujemo po prvem stolpcu
colnames(per_capita2) <- kategorije   #Prejšnje poimenovanje pokvari imena stolpcev, sedaj jih popravimo
#Odstranim dolarje, , nadomestim s . in spremenim v numeric
indx <- sapply(per_capita2, is.factor) 
per_capita2[indx] <- lapply(per_capita2[indx], function(x) as.numeric(gsub("[,$]", "", x)))

write.csv2(per_capita2, "podatki/per_capita2.csv", fileEncoding = "UTF-8")
############################################################################
