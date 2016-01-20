# 3. faza: Izdelava zemljevida
source("lib/uvozi.zemljevid.r",encoding="UTF-8")
# Uvozimo zemljevid.
#Urejamo podatke, ustvarjamo zemljevide, na splosno, imamo se fajn(ha-ha)
state_tidy_mean <- filter(state_tidy, Type=="Hourly_mean")      #Samo povprecne place v tabelci
povp_placa_state <- filter(state_tidy_mean, Occupation=="All Occupations")    #Za vsak state samo povprecno plačo

national_tidy_mean <- filter(national_tidy, Type=="Hourly_mean")      #Samo povprecne place v tabelci
povp_placa_national <- filter(national_tidy_mean, Occupation=="All Occupations")    #Za cele ZDA povprecna plača

povp_placa_top_low <- filter(povp_placa_state, State %in% c("Mississippi","South Dakota", "District of Columbia","Massachusetts","New York","West Virginia")) #3 z najvisjo in 3 z najnizjo povprecno placo
#Spreminljanje povprečne plače v zveznih državah
graf_national_tidy_mean <- ggplot() + geom_line(data=povp_placa_top_low,aes(x=Year,y=Wage,group=State,colour=State))+
  geom_smooth(data=povp_placa_state, aes(x=Year, y=Wage),method="loess",color="blue")  + geom_smooth(data=povp_placa_national, aes(x=Year, y=Wage), method="loess", color="red")      #Glajeno krivuljo, ki odraža spreminjanje povprecja v USA


#Povprečne plače v zveznih državah v 2014-GRAF
states_map <- map_data("state")           #Tabela, v kateri so states in njihove lat in long, ter podobni podtki, za graf
povp_placa_state$region <- tolower(povp_placa_state$State)    #Ker so v states_map imena z majhnimi zacetnicami, dodamo semkaj se stolpec z malimi imeni drzav
povp_placa_state <- merge(povp_placa_state,iso_state,by="State")   #Dodamo iso kratice zveznim drzavam, odstranijo se avtomatsko tiste, ki niso tretirane kot drzave (otoki, districty ipd)
povp_placa_state_2014 <- filter(povp_placa_state, Year==2014)     #Tabelca povprecnih plac za samo 2014
povp_placa_state_2014_map <- merge(states_map, povp_placa_state_2014, by="region") #Zdruzena tabelca z tisto o podatkih o krajih -> pripravljenost na graf
#Graf ZDA, barvno obarvani statesi, glede na povprecno placo v 2014
graf_povp_placa_state_2014 <- ggplot() + geom_polygon(data=povp_placa_state_2014_map, aes(x=long, y=lat, group = State, fill=povp_placa_state_2014_map$Wage),colour="white") +
  scale_fill_continuous(low ="lightblue" , high = "darkblue",guide="colorbar") + 
  labs(fill = "Mean wage in 2014" ,title = "Mean wage in 2014", x="", y="") +
  theme(axis.text.x=element_blank(),axis.text.y=element_blank())

#Delanje razpredelnice za plotly mapo
povp_placa_state_for_plotly <- subset(povp_placa_state_2014,select=-c(Year,Type,Occupation,Wage,region))
povp_placa_state_for_plotly$`2014` <- (filter(povp_placa_state, Year==2014))$Wage
povp_placa_state_for_plotly$`2012` <- (filter(povp_placa_state, Year==2012))$Wage
povp_placa_state_for_plotly$`2010` <- (filter(povp_placa_state, Year==2010))$Wage
povp_placa_state_for_plotly$`2008` <- (filter(povp_placa_state, Year==2008))$Wage
povp_placa_state_for_plotly$`2006` <- (filter(povp_placa_state, Year==2006))$Wage
povp_placa_state_for_plotly$`2004` <- (filter(povp_placa_state, Year==2004))$Wage
povp_placa_state_for_plotly$`2002` <- (filter(povp_placa_state, Year==2002))$Wage
#Zdej pa graf, v plotly
povp_placa_state_for_plotly$hover <- with(povp_placa_state_for_plotly,paste
                                          (State,'<br>', 
                                          "2012", `2012`,'<br>', "2010", `2010`,'<br>',
                                          "2008", `2008`,"<br>", "2006", `2006`,'<br>',
                                          "2004", `2004`,"<br>","2002",`2002`))
#Nastavitve/opcije za prikaz mape
meje <- list(color = toRGB("white"), width = 2)
parametri <- list(scope = 'usa',projection = list(type = 'albers usa'),showlakes = TRUE,lakecolor = toRGB('white'))

graf_povp_placa_state_plotly <- plot_ly(povp_placa_state_for_plotly, z = `2014`, text = hover, locations = Code, type = 'choropleth',
        locationmode = 'USA-states', color = `2014`, colors = 'Reds',
        marker = list(line = meje), colorbar = list(title = "Dollars per hour")) %>%
  layout(title = 'Mean wages by states', geo = parametri)


#Volitve 2012
zda <- uvozi.zemljevid("http://baza.fmf.uni-lj.si/states_21basic.zip", "states")
capitals <- read.csv("podatki/uscapitals.csv")
row.names(capitals) <- capitals$state
capitals <- preuredi(capitals, zda, "STATE_NAME")
capitals$US.capital <- capitals$capital == "Washington"

## Dodamo podatke o predsedniških volitvah v zemljevid
zda$vote.2012 <- capitals$vote.2012
zda$electoral.votes <- capitals$electoral.votes
usa <- pretvori.zemljevid(zda)

usa.cont <- usa %>% filter(! STATE_NAME %in% c("Alaska", "Hawaii"))
capitals.cont <- capitals %>% filter(! state %in% c("Alaska", "Hawaii"))

volitve_2012 <- ggplot() + geom_polygon(data = usa.cont,
                                aes(x = long, y = lat,
                                    group = group, fill = vote.2012)) +
  scale_fill_manual(values = c("blue", "red")) +
  guides(fill = guide_legend("Volitve 2012"))

