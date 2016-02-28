# 4. faza: Analiza podatkov

source("analiza/functions.r", encoding = "UTF-8")

### Poklici, primerni za finančne matematike ################################################
financni <- c("Marketing Managers","Financial Managers","Top Executives","Actuaries",
              "Personal Financial Advisors","Economists","Mathematicians","Financial Analysts and Advisors",
              "Financial Analysts","Financial Examiners","Statisticians","Financial Specialists",
              "Credit Analysts")

financni_nat <- filter(national, Occupation %in% financni)
financni_nat_tidy <- filter(national_tidy, Occupation %in% financni)
financni_st <- filter(state, Occupation %in% financni)
financni_st_tidy <- filter(state_tidy, Occupation %in% financni)

### Cost of living by state #################################################################
#Ker imamo podatke za samo 2015, se delamo... naivne in uporabimo te indekse za state_2014 ... torej je vse skupi samo za priblizni zgled
COL_state <- subset(state_2014,select=c(State,Occupation,Hourly_mean))  #Prvo dobimo iz state_2014 tabelo z samo povprečno plačo
COL_state_all <- merge(COL_state,cost_of_living)   #Združimo skupi z indeksi COL (vsemi)
COL_state <- subset(COL_state_all, select=c(State, Occupation, Hourly_mean, Index))
COL_state$Real_pay <- (COL_state$Hourly_mean/COL_state$Index)*10   #Sedaj je plača deljena z Indeksom in množena z 10 (zaradi preglednosti). To nam pove, koliko je plača v tisti državi dejanjsko "vredna" v primerjavi z drugimi
COL_state$Index <- NULL   #Odstranimo indekse, jih ne potrebujemo več

#Kakšna je realna povprečna plača po statesih
COL_state_avg <- filter(COL_state, Occupation=='All Occupations')

#Kakšna je povprečna realna plača po statesih za poklice finančnih matematikov
COL_state_fin <- filter(COL_state, Occupation %in% financni)
COL_state_fin_mean <- subset(COL_state_fin,select=c(State, Hourly_mean, Real_pay))
COL_state_fin_mean <- aggregate(. ~ State, data = COL_state_fin_mean, mean)
COL_state_fin_mean <- zaokrozi(COL_state_fin_mean,2)

### Razrstitev plač v plačilne razrede ######################################################
#Prvo uporabimo na vseh scale   (PS: napredovanje od zacetka, s for zanko cez vsa imena, namesto rocno ;) 
# for(i in seq(2,15,2)){
#   if(i<10){ 
#     assign(paste0("national_200",i,"_norm"),scale(col.to.name(get(paste0("national_200",i)))))
#     k <- kmeans(get(paste0("national_200",i,"_norm")),3,nstart = 1000)
#     assign(paste0("k_200",i),k)
#     assign("pomozna", get(paste0("national_200",i)))
#     pomozna$Class <- factor(k$cluster)
#     assign(paste0("national_200",i),pomozna)
#   }else{  
#     assign(paste0("national_20",i,"_norm"),scale(col.to.name(get(paste0("national_20",i)))))
#     k <- kmeans(get(paste0("national_20",i,"_norm")),3,nstart = 1000)
#     assign(paste0("k_20",i),k)
#     assign("pomozna", get(paste0("national_20",i)))
#     pomozna$Class <- factor(k$cluster)
#     assign(paste0("national_20",i),pomozna)
#   }
#   rm(pomozna)
#   rm(k)
# }
#Dodali smo tabelam national skupine, h katerim pasejo. Analiza bo graficna, ker se skupine spreminjajo
#ob vsakem zagonu
### hclust ureditev v drevesa in skupine ###############################################
#Uredimo v 6 razredov, ker je drevo, je high,medium,low, ki se delijo se na dva vsak
for(i in seq(2,15,2)){
  if(i<10){
    assign(paste0("national_200",i,"_tree"),drevesa(get(paste0("national_200",i))))
    assign(paste0("national_200",i),obrezi(get(paste0("national_200",i,"_tree")),get(paste0("national_200",i))))
  }else{
    assign(paste0("national_20",i,"_tree"),drevesa(get(paste0("national_20",i))))
    assign(paste0("national_20",i),obrezi(get(paste0("national_20",i,"_tree")),get(paste0("national_20",i))))
  }
}
#Naredimo novo national tabelo, ki ima sedaj dopolnjen tudi placilni razred
national <- rbind(national_2014,national_2012,national_2010,national_2008,national_2006,national_2004,national_2002)
national$Total_employment <- NULL
national_tidy <- as.data.frame(melt(national, id=c("Occupation","Year","Pay_grade")))
national_tidy <- dplyr::rename(national_tidy,Wage=value,Type=variable)
#Povprecna placa placilnega razreda
for(i in seq(2,15,2)){
  if(i<10){
    zacasni <- subset(get(paste0("national_200",i)),select=c(Hourly_mean,Pay_grade))
    zacasni <- aggregate(. ~ Pay_grade, data = zacasni, mean)
    zacasni$Year <- as.numeric(paste0("200",i))
    zacasni <- zaokrozi(zacasni,2)
    assign(paste0("national_200",i,"_grade"),zacasni)
  }else{
    zacasni <- subset(get(paste0("national_20",i)),select=c(Hourly_mean,Pay_grade))
    zacasni <- aggregate(. ~ Pay_grade, data = zacasni, mean)
    zacasni$Year <- as.numeric(paste0("20",i))
    zacasni <- zaokrozi(zacasni,2)
    assign(paste0("national_20",i,"_grade"),zacasni)
  }
  rm(zacasni)
}
#Tukaj so sedaj za vsak placilni razred povprecne place za vsa leta
national_grade <- rbind(national_2014_grade,national_2012_grade,national_2010_grade,national_2008_grade,national_2006_grade,national_2004_grade,national_2002_grade)
#Povprecna placa za financne poklice za vsa leta
financni_nat_mean <- subset(financni_nat,select=c(Hourly_mean,Year))
financni_nat_mean <- aggregate(. ~ Year, data=financni_nat_mean, mean)
financni_nat_mean <- zaokrozi(financni_nat_mean,2)
financni_nat_mean$Pay_grade <- "FIN"
#Damo v data frame z razredi se financne poklice
national_grade <- rbind(national_grade,financni_nat_mean)

### Razvrstitev zv. drzav (glede na realno plačo) v 3 skupine ######################
zacasna <- subset(COL_state_avg,select = c(State,Real_pay))
row.names(zacasna) <- COL_state_avg$State
zacasna$State <- NULL
COL_state_avg_tree <- hclust(dist(zacasna,method="euclidian"),method="ward.D2")
COL_state_avg <- obrezi(COL_state_avg_tree,subset(COL_state_avg,select=c(State,Hourly_mean,Real_pay)),n=3)
COL_state_avg <- zaokrozi(COL_state_avg,2)

### Razvrstitev zv. drzav glede na realno placo financnih poklicev v 3 skupine ################
zacasna <- subset(COL_state_fin_mean, select=c(State, Real_pay))
row.names(zacasna) <- COL_state_fin_mean$State
zacasna$State <- NULL
COL_state_fin_tree <- hclust(dist(zacasna,method="euclidian"),method="ward.D2")
COL_state_fin_mean <- obrezi(COL_state_fin_tree,COL_state_fin_mean,n=3)
COL_state_fin_mean <- zaokrozi(COL_state_fin_mean,2)
rm(zacasna)

### Poiskanje povprečne plače za finančne poklice za vsako leto za vsak state ############
financni_st_tidy_mean <- filter(financni_st_tidy, Type %in% "Hourly_mean")
financni_st_tidy_mean <- zaokrozi(aggregate(. ~ State + Year, data=financni_st_tidy_mean[c(-2,-4)], mean),2)  #Sedaj imamo povprecno placo financnih poklicev po letih in drzavah

### Tabeli z poklici v zveznih državah ##########################################
state <- merge(state, subset(national, select=c(Occupation,Year,Pay_grade)),by=c("Occupation","Year"))
state <- dplyr::arrange(state,desc(Year),desc(Hourly_mean))



source("analiza/grafi.r", encoding = "UTF-8")