# 2. faza: Uvoz podatkov
source("uvoz/funkcije.r", encoding = "UTF-8")
### National and states ##################################################################
source("uvoz/uvoz_national.r", encoding = "UTF-8")

### Mean per capita in States ######################################################################
#Uvoz tabele s povprečnim prihodkom na državljana v zveznih državah

if(file.exists("podatki/per_capita.csv")){
  per_capita <- read.csv2("podatki/per_capita.csv", fileEncoding = "UTF-8", stringsAsFactors = FALSE)
  names(per_capita) <- gsub("X","",names(per_capita))
}else{
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
  
  #Tabelco per_capita priredimo za inflacijo, da bomo lahko primerjali vrednosti skozi leta med sabo(NPV bo v 2015)
  inflacije = c(2.880,1.816,1.557,1.378,1.290,1.177,1.106,1.034)
  per_capita[2:9] <- round(data.frame(mapply(`*`,per_capita[2:9],inflacije)),0)
  
  #Uredimo tabelco per_capita
  per_capita <- arrange(per_capita,desc(`2012`),desc(`2009`))
  
  write.csv2(per_capita, "podatki/per_capita.csv", fileEncoding = "UTF-8",row.names = FALSE)
}

#Tidy data
per_capita_tidy <- melt(per_capita,id="State")
per_capita_tidy <- dplyr::rename(per_capita_tidy,Year=variable,Wage=value)

### Cost of living #########################################################################
#S https://www.missourieconomy.org/indicators/cost_of_living/index.stm dobimo tabelco z indeksi
#za cost of living za posamezen state.. Ker je to (vsaj zastonjsko) težje najti kot spodobnega 
#republikanskega kandidata, se zadovoljimo s tem in tudi za samo eno leto(2015), ker.. pac ja...
if(file.exists("podatki/cost_of_living.csv")){
  cost_of_living<- read.csv2("podatki/cost_of_living.csv",encoding ="UTF-8",stringsAsFactors = FALSE)
}else{
  u=GET("https://www.missourieconomy.org/indicators/cost_of_living/index.stm")  #Dobimo link, ker je https se malo "pomatramo"
  tables = readHTMLTable(content(u), fileEncoding = "UTF-8",stringsAsFactors=FALSE) #Dobimo tabele z linka
  cost_of_living = tables[[1]]  #Izberemo ta pravo tabelo
  names(cost_of_living) <- cost_of_living[1,]    #Prvo vrstico uporabimo za imena stolpcev
  cost_of_living <- (cost_of_living[-1,])[-2]    #Znebimo se prve vrstice in stolpca Rank
  cost_of_living[c(2:8)] <- lapply((cost_of_living[c(2:8)]), function(x) as.numeric(x))  #Indekse spremenimo v numeric
  
  write.csv2(cost_of_living, "podatki/cost_of_living.csv",fileEncoding="UTF-8",row.names = FALSE)
}

#Tidy data 
cost_of_living_tidy <- melt(cost_of_living, id=c("State"))
cost_of_living_tidy <- dplyr::rename(cost_of_living_tidy, Cost.of.Living=value, Type= variable)

### Uvoz ISO kod ############

if(file.exists("podatki/iso_state.csv")){
  iso_state <- read.csv2("podatki/iso_state.csv", fileEncoding = "UTF-8", stringsAsFactors = FALSE)
}else{
  iso_state <- read.csv("http://www.fonz.net/blog/wp-content/uploads/2008/04/states.csv",encoding="UTF-8")
  iso_state <- dplyr::rename(iso_state,Code=Abbreviation)
  
  write.csv2(iso_state,"podatki/iso_state.csv",fileEncoding ="UTF-8",row.names = FALSE)
}

############################################################################
source("uvoz/grafi.r", encoding = "UTF-8")
