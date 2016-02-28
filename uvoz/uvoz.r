# 2. faza: Uvoz podatkov
source("uvoz/funkcije.r", encoding = "UTF-8")
##############################################################################################
#National in states
source("uvoz/uvoz_national.r", encoding = "UTF-8")

### Mean per capita in States ###########################################################################################
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

#Tidy data
per_capita_tidy <- melt(per_capita,id="State")
per_capita_tidy <- dplyr::rename(per_capita_tidy,Year=variable,Wage=value)
### Mean per capita ########################################################################
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

#Tidy data
per_capita2_tidy <- melt(per_capita2, id=c("State","Population","Number of households"))
per_capita2_tidy <-dplyr::rename(per_capita2_tidy,Wage=value, Type=variable)
### Cost of living #########################################################################
#S https://www.missourieconomy.org/indicators/cost_of_living/index.stm dobimo tabelco z indeksi
#za cost of living za posamezen state.. Ker je to (vsaj zastonjsko) težje najti kot spodobnega 
#republikanskega kandidata, se zadovoljimo s tem in tudi za samo eno leto(2015), ker.. pac ja...
u=GET("https://www.missourieconomy.org/indicators/cost_of_living/index.stm")  #Dobimo link, ker je https se malo "pomatramo"
tables = readHTMLTable(content(u), fileEncoding = "UTF-8",stringsAsFactors=FALSE) #Dobimo tabele z linka
cost_of_living = tables[[1]]  #Izberemo ta pravo tabelo
names(cost_of_living) <- cost_of_living[1,]    #Prvo vrstico uporabimo za imena stolpcev
cost_of_living <- (cost_of_living[-1,])[-2]    #Znebimo se prve vrstice in stolpca Rank
cost_of_living[c(2:8)] <- lapply((cost_of_living[c(2:8)]), function(x) as.numeric(x))  #Indekse spremenimo v numeric

write.csv2(cost_of_living, "podatki/cost_of_living.csv",fileEncoding="UTF-8",row.names = FALSE)

#Tidy data 
cost_of_living_tidy <- melt(cost_of_living, id=c("State"))
cost_of_living_tidy <- dplyr::rename(cost_of_living_tidy, Cost.of.Living=value, Type= variable)
############################################################################
source("uvoz/uredi_percapita.r", encoding = "UTF-8")
source("uvoz/grafi.r", encoding = "UTF-8")
