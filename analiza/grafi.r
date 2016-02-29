### Graf primerjave spreminjanja povprecne plače finančnih poklicev v primerjavi z nacionalnim povprecjem ######
napoved_fin_nat.lo <- loess(Hourly_mean ~ Year, financni_nat_mean,control=loess.control(surface="direct"))
napoved_fin_nat <- predict(napoved_fin_nat.lo, data.frame(Year=2016))

napoved_nat.lo <- loess(Wage ~ Year, national_tidy_mean[-1,-3], control=loess.control(surface = "direct"))
napoved_nat <- predict(napoved_nat.lo, data.frame(Year=2016))

graf_national_fin <- ggplot() + geom_smooth(data=national_tidy_mean, aes(x=Year, y=Wage, text="National"), method="loess", color="red") +
  geom_smooth(data=filter(financni_nat_tidy,Type=="Hourly_mean"), aes(x=Year, y=Wage,text ="Finančni"), method="loess", color="blue") + 
  geom_point(aes(x=2016, y=napoved_fin_nat, text="Napoved za fin. za 2016")) +
  geom_point(aes(x=2016, y=napoved_nat, text="Napoved za 2016"))

graf_national_fin_plotly <- ggplotly(graf_national_fin)



### Zemljevid s povprečnimi plačami finančnih poklicev v letu 2014 ##################
financni_st_tidy_mean$region <- tolower(financni_st_tidy_mean$State)    #Ker so v states_map imena z majhnimi zacetnicami, dodamo semkaj se stolpec z malimi imeni drzav
financni_st_tidy_mean <- merge(financni_st_tidy_mean,iso_state,by="State")   #Dodamo iso kratice zveznim drzavam
financni_st_tidy_mean_2014 <- filter(financni_st_tidy_mean, Year == "2014")  #Izberemo samo tiste, ki so za leto 2014
financni_st_tidy_mean_2014_map <- merge(states_map, financni_st_tidy_mean_2014, by="region") #Zdruzena tabelca z tisto o podatkih o krajih -> pripravljenost na graf

#Graf ZDA, barvno obarvani statesi, glede na povprecno placo 
# graf_financni_mean_2014 <- ggplot() + geom_polygon(data=financni_st_tidy_mean_2014_map, aes(x=long, y=lat, group = State, fill=financni_st_tidy_mean_2014_map$Wage),colour="white") +
#   scale_fill_continuous(low ="lightblue" , high = "darkblue",guide="colorbar") + 
#   labs(fill = "Mean financial wage in 2014" ,title = "Mean financial wage in 2014", x="", y="") +
#   theme(axis.text.x=element_blank(),axis.text.y=element_blank())
graf_financni_mean_2014 <- plot_ly(financni_st_tidy_mean_2014, z = `Wage`, text = State, locations = Code, type = 'choropleth',
         locationmode = 'USA-states', color = `Wage`, colors = 'PuRd',
         marker = list(line = meje), colorbar = list(title = "Wage")) %>%
          layout(title = 'Wage for financial professions', geo = parametri)


### Zemljevid zveznih držav glede na realno plačo v 2014 ###################################
#DF za plotly mapo
COL_state_fin_mean_for_plotly <- merge(COL_state_fin_mean,iso_state, by="State")   #Ustvarimo nov df, dodamo iso kode
COL_state_fin_mean_for_plotly <- dplyr::rename(COL_state_fin_mean_for_plotly,Wage_FIN=Hourly_mean, Real_FIN=Real_pay, Grade_FIN=Pay_grade)  #Preimenujemo stolpce
COL_state_fin_mean_for_plotly <- cbind(COL_state_fin_mean_for_plotly, COL_state_avg[!names(COL_state_avg) %in% names(COL_state_fin_mean_for_plotly)])   #Zdruzimo skupaj, pazimo, da kaksnih stvari ne zdruzimo dvakrat
COL_state_fin_mean_for_plotly <- dplyr::rename(COL_state_fin_mean_for_plotly, Wage_nat=Hourly_mean, Real_nat=Real_pay, Grade_nat=Pay_grade)
#Priprava na graf, nastimamo, kaj se pokaze, ko damo cez misko
COL_state_fin_mean_for_plotly$hover <- with(COL_state_fin_mean_for_plotly,paste(State,'<br>',
                                                                                "Wage in $ for fin:", Wage_FIN, '<br>','<br>',
                                                                                "Real pay for state:", Real_nat, '<br>',
                                                                                "Wage in $ for state:", Wage_nat, '<br>','<br>',
                                                                                "Pay grade for FIN:", Grade_FIN,'<br>',
                                                                                "Pay grade for state:", Grade_nat))
#Nastavitve za mapo
graf_COL_state_fin_mean_plotly <- plot_ly(COL_state_fin_mean_for_plotly, z = `Real_FIN`, text = hover, locations = Code, type = 'choropleth',
                                        locationmode = 'USA-states', color = `Real_FIN`, colors = 'Purples',
                                        marker = list(line = meje), colorbar = list(title = "Real wage")) %>%
  layout(title = 'Real pay for financial professions', geo = parametri)

### Graf spremembe višine plač v plačilnih razredih #########################################
for(i in unique(national_grade$Pay_grade)){
  if(exists("napoved_nat_grade")){napoved_nat_grade <- rbind(napovej_za_grade(i), napoved_nat_grade)}
  else{napoved_nat_grade <- napovej_za_grade(i)}
}
za_graf <- rbind(national_grade, napoved_nat_grade)
graf_national_grade <- ggplot() + geom_smooth(data=filter(za_graf,Year!=2016), aes(x=Year, y= Hourly_mean,
                                                                       group=Pay_grade,color=Pay_grade),se=FALSE) + 
  geom_point(data=filter(za_graf,Year!=2016), aes(x=Year, y= Hourly_mean, group=Pay_grade,color=Pay_grade)) +
  geom_point(data=filter(za_graf,Year==2016),aes(x=Year, y=Hourly_mean, group=Pay_grade, color=Pay_grade))

graf_national_grade
graf_national_grade_plotly <- ggplotly(graf_national_grade+guides(color=FALSE))

### Drevo za financne poklice za leto 2014 #################################################
financni_nat_2014 <- filter(financni_nat, Year==2014)
financni_nat_drevo <- drevesa(financni_nat_2014)
graf_financni_nat_drevo <- as.dendrogram(financni_nat_drevo)
financni_nat_2014 <- obrezi(financni_nat_drevo,financni_nat_2014,n=3)

### Zemljevid zv držav glede na njihov plačilni razred ##########
COL_state_avg_plotly <- merge(COL_state_avg,iso_state, by="State")
COL_state_avg_plotly$hover <- with(COL_state_avg,paste(State,"<br>","Real pay:",`Real_pay`))

graf_COL_state_grade <- plot_ly(COL_state_avg_plotly, z = `Pay_grade`, text = hover, locations = Code, type = 'choropleth',
 locationmode = 'USA-states', color = `Pay_grade`, colors = 'Dark2',
 marker = list(line = meje), colorbar = list(title = "Grade"),showscale=FALSE) %>%
  layout(title = 'States sorted by grade', geo = parametri)







