#Uvoz csv2 v UTF-8
uvoz.csv <- function(path,imena){
  tabela <- read.csv2(path,col.names = imena,stringsAsFactors = FALSE, fileEncoding = "UTF-8")
  return(tabela)
}
# Uvoz national in uvoz state tabel, predoločeno, ker so vse isto narejene
uvoz.national <- function(path,leto="2014"){
  if(leto!="2002"){
    orig.imena <- c("OCC_CODE","Occupation","OCC_GROUP","Total_employment","EMP_PRSE","Hourly_mean","Annual_mean","MEAN_PRSE","H_PCT10","H_PCT25","Hourly_median","H_PCT75","H_PCT90","A_PCT10","A_PCT25","Annual_median","A_PCT75","A_PCT90","ANNUAL","HOURLY")
  }else if(leto=="2002"){
    orig.imena <- c("OCC_CODE","Occupation","OCC_GROUP","Total_employment","EMP_PRSE","Hourly_mean","Annual_mean","MEAN_PRSE","H_PCT10","H_PCT25","Hourly_median","H_PCT75","H_PCT90","A_PCT10","A_PCT25","Annual_median","A_PCT75","A_PCT90","ANNUAL")
  }
  return(uvoz.csv(path,orig.imena))
}

uvoz.state <- function(path, leto="2014"){
  if(leto=="2014" | leto=="2013" | leto=="2012" | leto=="2011" | leto=="2010"){
    orig.imena <- c("AREA","ST","State","OCC_CODE","Occupation","OCC_GROUP","Total_employment","EMP_PRSE","JOBS_1000","LOC_Q","Hourly_mean","Annual_mean","MEAN_PRSE","H_PCT10","H_PCT25","Hourly_median","H_PCT75","H_PCT90","A_PCT10","A_PCT25","Annual_median","A_PCT75","A_PCT90","ANNUAL","HOURLY")
  }else if(leto=="2009" | leto=="2008" | leto=="2007" | leto=="2006" |leto=="2005" | leto=="2004"){
    orig.imena <- c("AREA","ST","State","OCC_CODE","Occupation","OCC_GROUP","Total_employment","EMP_PRSE","Hourly_mean","Annual_mean","MEAN_PRSE","H_PCT10","H_PCT25","Hourly_median","H_PCT75","H_PCT90","A_PCT10","A_PCT25","Annual_median","A_PCT75","A_PCT90","ANNUAL","HOURLY")
  }else if(leto=="2002"){
    orig.imena <- c("AREA","ST","State","OCC_CODE","Occupation","OCC_GROUP","Total_employment","EMP_PRSE","Hourly_mean","Annual_mean","MEAN_PRSE","H_PCT10","H_PCT25","Hourly_median","H_PCT75","H_PCT90","A_PCT10","A_PCT25","Annual_median","A_PCT75","A_PCT90","ANNUAL")
  }
  return(uvoz.csv(path,orig.imena))
}
#Funkcija, v kateri izberemo, kateri tip tabele uvazamo
uvozi <- function(path,tip="national",leto="2014"){
  if(tip=="national"){
    return(uvoz.national(path,leto))
  }else{
    return(uvoz.state(path,leto))
  }
}
#Odstranitev nepotrebnih stolpcev, so predoloceni, ker imajo vse tabele iste
odstrani <- function(podatki,tip="national"){
  if(tip=="national"){
    podatki <- subset(podatki, select = c(Occupation,Total_employment,Hourly_mean,Annual_mean,H_PCT10,H_PCT25,Hourly_median,H_PCT75,H_PCT90,A_PCT10,A_PCT25,Annual_median,A_PCT75,A_PCT90))
  }else{
    podatki <- subset(podatki, select= c(State,Occupation,Total_employment,Hourly_mean,Annual_mean,H_PCT10,H_PCT25,Hourly_median,H_PCT75,H_PCT90,A_PCT10,A_PCT25,Annual_median,A_PCT75,A_PCT90))
  }
  return(podatki)
}
#To funkcijo lahko uporabimo na obeh, a če imamo state, moramo dati to kot argument
#Določi max urno in letno plačo, popuca podatke nepotrebnih simbolov
uredi <- function(podatki,tip="national",urna="90", letna="187200"){
  if(tip=="national"){pod=c(2)}
  else if(tip=="state"){pod=c(3)}
  urne.place <- names(podatki)[c((match("Hourly_mean",names(podatki))),(match("H_PCT10",names(podatki)):match("H_PCT90",names(podatki))))]
  podatki[urne.place] <- lapply(podatki[urne.place], function(x) (gsub("[#]", urna, x)))
  
  letne.place <- names(podatki)[c((match("Annual_mean",names(podatki))),(match("A_PCT10",names(podatki)):match("A_PCT90",names(podatki))))]
  podatki[letne.place] <- lapply(podatki[letne.place], function(x) (gsub("[#]", letna, x)))

  podatki[] <- lapply(podatki[], function(x) (gsub("[*]","",x)))
  podatki[] <- lapply(podatki[], function(x) (gsub("[**]","",x)))
  podatki[] <- lapply(podatki[], function(x) (gsub("[***]","",x)))
  podatki[pod] <- lapply(podatki[pod], function(x) (gsub("[.]","",x)))
  
  if(tip=="national"){
    podatki[2:14] <- lapply(podatki[2:14], function(x) as.numeric(gsub("[,]",".",x)))
    podatki <- podatki[!duplicated(podatki$Occupation),]
  }else{
    podatki[3:15] <- lapply(podatki[3:15], function(x) as.numeric(gsub("[,]",".",x)))
  }
  return(podatki)
}
#Tabele bomo opremili z inflacijo in priredili letu 2014 (2012 in 2010 zaradi razlogov isto kot 2014)
priredi.inflaciji <- function(podatki, tip="national", leto="2014"){
  if(tip=="national"){
    pod=c(3:8)
  }else if(tip=="state"){
    pod=c(4:9)
  }
  if(leto=="2014"){
    podatki<-podatki
  }else if(leto=="2012"){
    #podatki[pod] <- round(data.frame(mapply(`*`,podatki[pod],1.031)),2)
  }else if(leto=="2010"){
    #podatki[pod] <- round(data.frame(mapply(`*`,podatki[pod],1.086)),2)
  }else if(leto=="2008"){
    podatki[pod] <- round(data.frame(mapply(`*`,podatki[pod],1.1)),2)
  }else if(leto=="2006"){
    podatki[pod] <- round(data.frame(mapply(`*`,podatki[pod],1.174)),2)
  }else if(leto=="2004"){
    podatki[pod] <- round(data.frame(mapply(`*`,podatki[pod],1.253)),2)
  }else if(leto=="2002"){
    podatki[pod] <- round(data.frame(mapply(`*`,podatki[pod],1.316)),2)
  }
  return(podatki)
}
#Ko uporabimo to, dobimo semi urejeno tabelo
#Pokličemo prejšnje funkcije, se pravi, uvozimo,, odtranimo nepomembne stolpce, določimo vrednosti plač(določene v uradni dokumentaciji podatkov), priredimo inflaciji
naredi <- function(path,tip="national", leto="2014"){
  if(leto=="2014" | leto=="2013" | leto=="2012" | leto=="2011" | leto=="2010"){
    urna="90"
    letna="187200"
  }else if(leto=="2010" | leto=="2009" | leto=="2008" | leto=="2007"){
    urna="80"
    letna="166400"
  }else if(leto=="2006" | leto=="2005" | leto=="2004" | leto=="2003" | leto=="2002"){
    urna="70"
    letna="145600"
  }
  podatki <- uvozi(path,tip,leto)
  podatki <- odstrani(podatki,tip)
  podatki <- uredi(podatki,tip, urna, letna)
  return(podatki)
}
#Naredimo urne podatke oz. izberemo samo urne podatke
naredi.urne <- function(podatki,tip="national",leto="2014"){
  if(tip=="national"){
    podatki <- na.omit(subset(podatki, select = names(podatki)[c((1:3),(5:9))]))
    podatki <- arrange(podatki, desc(Hourly_mean), desc(Hourly_median), desc(H_PCT10))
    podatki <- priredi.inflaciji(podatki,tip,leto)
  }else{
    podatki <- na.omit(subset(podatki, select = names(podatki)[c((1:4),(6:10))]))
    podatki <- arrange(podatki,State, desc(Hourly_mean), desc(Hourly_median), desc(H_PCT10))
    podatki <- priredi.inflaciji(podatki,tip,leto)
  }
  podatki$Year <- as.numeric(leto)
  return(podatki)
}
#Funkcija se za letne, ceprav je verjetno ne bom uporabil, ampak boljse bit pripravljen
naredi.letne <- function(podatki,tip="national"){
  if(tip=="national"){
    podatki <- na.omit(subset(podatki, select = names(podatki)[c((1:3),(5:9))]))
    podatki <- arrange(podatki, desc(Annual_mean), desc(Annual_median), desc(A_PCT10))
    podatki <- priredi.inflaciji(podatki,tip,leto)
  }else{
    podatki <- na.omit(subset(podatki, select = names(podatki)[c((1:4),(6:10))]))
    podatki <- arrange(podatki,State, desc(Annual_mean), desc(Annual_median), desc(A_PCT10))
    podatki <- priredi.inflaciji(podatki,tip,leto)
  }
  podatki$Year <- as.numeric(leto)
  return(podatki)
}

# Izberemo st_poklicev top in low poklicev
filtriraj_poklice <- function(st_poklicev){
  poklici <- (c(national_2014$Occupation))
  poklici <- poklici[c((1:st_poklicev),((length(poklici)-(st_poklicev-1)):length(poklici)))]
  
  poklici_top <- poklici[1:st_poklicev]
  poklici_low <- rev(poklici)[1:st_poklicev]
  return(list(poklici_top,poklici_low))
}
#Filtriramo podatke tako, da imamo samo prej izbrane poklice
buc.buc <- function(podatki, st_poklicev){
  poklici <- filtriraj_poklice(st_poklicev)
  poklici <- unlist(poklici)
  
  podatki <- filter(podatki, Occupation %in% poklici)
  return(podatki)
}

zaokrozi <- function(df, digits) {
  nums <- vapply(df, is.numeric, FUN.VALUE = logical(1))
  
  df[,nums] <- round(df[,nums], digits = digits)
  
  (df)
}