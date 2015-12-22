# 2. faza: Uvoz podatkov

library(XML)
library(RCurl)
library(gdata)
library(dplyr)

# Zapišimo podatke v razpredelnico national, podatki o poklicih in njihovih povprečnih plačah v ZDA
orig.imena <- c("OCC_CODE","OCC_TITLE","OCC_GROUP","TOT_EMP","EMP_PRSE","H_MEAN","A_MEAN","MEAN_PRSE","H_PCT10","H_PCT25","H_MEDIAN","H_PCT75","H_PCT90","A_PCT10","A_PCT25","A_MEDIAN","A_PCT75","A_PCT90","ANNUAL","HOURLY")
national <- read.csv2("podatki/national_2014.csv",
                      col.names = c("OCC_CODE","Occupation","OCC_GROUP","Total_employment","EMP_PRSE","Hourly_mean","Annual_mean","MEAN_PRSE","H_PCT10","H_PCT25","Hourly_median","H_PCT75","H_PCT90","A_PCT10","A_PCT25","Annual_median","A_PCT75","A_PCT90","ANNUAL","HOURLY"),
                      stringsAsFactors = FALSE,
                      fileEncoding = "UTF-8")
#Poimenoval vrstice
national <- (subset(national, select = -c(OCC_CODE,OCC_GROUP,HOURLY,ANNUAL,EMP_PRSE,MEAN_PRSE)))
national <- national[!duplicated(national$Occupation),]

#  # pomeni, da je urna placa>90$ oz letna>187200$ ... Za prakticne namene spremenimo to v stevilke
urne.place <- names(national)[match("H_PCT10",names(national)):match("H_PCT90",names(national))]
national[urne.place] <- lapply(national[urne.place], function(x) (gsub("[#]", "90", x)))

letne.place <- names(national)[match("A_PCT10",names(national)):match("A_PCT90",names(national))]
national[letne.place] <- lapply(national[letne.place], function(x) (gsub("[#]", "187200", x)))

# * pomeni, da podatki niso bili na voljo ali pa poklici,ki so plačani letno, ne na uro. * bomo spremenili v NA
national[] <- lapply(national[], function(x) (gsub("[*]","",x)))

# Sedaj odstranimo . in spremenimo vejice v decimalne pike, hkrati pa stolpce spremenimo v numeric
national[] <- lapply(national[], function(x) (gsub("[.]","",x)))
national[2:14] <- lapply(national[2:14], function(x) as.numeric(gsub("[,]",".",x)))

##############################################################################
#Uvoz tabele s povprečnim prihodkom na državljana v zveznih državah

u= "http://www.infoplease.com/ipa/A0104652.html"
kategorije=as.character(c("State", 1980, 1990, 1995, 2000, 2003, 2006, 2009, 2012))  #Imena stolpcev
tables = readHTMLTable(u, fileEncoding="UTF-8")   #Prebral HTML tabelce

per_capita= tables[[2]] #Izbral ta pravo tabelo
colnames(per_capita) <- kategorije #Poimenovanje stolpcev
per_capita <- per_capita[complete.cases(per_capita[,length(names(per_capita))]),][-1,]

#Odstranim dolarje, , nadomestim s . in spremenim v numeric
per_capita$State <- sapply(per_capita$State, function(x) as.character(gsub("[$]", "",x)))
indx <- sapply(per_capita, is.factor)
per_capita[indx] <- lapply(per_capita[indx], function(x) as.numeric(gsub("[,$]", "", x)))
per_capita <- replace(per_capita,per_capita=="DC","Washington, D.C.")  #Zamenjam ime zv. države

###########################################################################
#Uvoz tabele s povprecnim prihodkom na državljana v ZDA

u=getURL("https://en.wikipedia.org/wiki/List_of_U.S._states_by_income",.opts = list(ssl.verifypeer = FALSE) )
tables = readHTMLTable(u, fileEncoding = "UTF-8")
per_capita2 = tables[[3]]

kategorije <- c(names(per_capita2))[-1] #Imena stolpcev, odstranil prvo kategorijo, ker bo 1. stolpec izbrisan
kategorije <- lapply(kategorije, function(x) (gsub("[\n]"," ", x)))  #Znebil se obveznih prelomov vrstic
per_capita2 <- per_capita2[,-1] #Odstranimo brezvezni stolpec kategorij
colnames(per_capita2) <- kategorije   #Prejšnje poimenovanje je imelo obvezni prelom vrstic, to smo sedaj odstranili
#Odstranim dolarje, , nadomestim s . in spremenim v numeric
per_capita2$State <- sapply(per_capita2$State, function(x) as.character(gsub("[$]", "",x)))
indx <- sapply(per_capita2, is.factor) 
per_capita2[indx] <- lapply(per_capita2[indx], function(x) as.numeric(gsub("[,$]", "", x)))

############################################################################

source("uvoz/uredi.r", encoding = "UTF-8")
source("uvoz/grafi.r", encoding = "UTF-8")
