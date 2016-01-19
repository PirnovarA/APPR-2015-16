# 3. faza: Izdelava zemljevida
source("lib/uvozi.zemljevid.r",encoding="UTF-8")
# Uvozimo zemljevid.
#Urejamo podatke, ustvarjamo zemljevide, na splosno, imamo se fajn(ha-ha)
state_tidy_mean <- filter(state_tidy, Type=="Hourly_mean")      #Samo povprecne place v tabelci
povp_placa_state <- filter(state_tidy_mean, Occupation=="All Occupations")    #Za vsak state samo povprecno plačo

#Spreminljanje povprečne plače v zveznih državah
graf_national_tidy_mean <- ggplot(data=povp_placa_state, aes(x=Year, y=Wage)) + 
  geom_smooth(method="loess")         #Glajeno krivuljo, ki odraža spreminjanje povprecja v USA
#graf_national_tidy_mean_plotly <- ggplotly(graf_national_tidy_mean)

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

povp_placa_state_for_plotly$hover <- with(povp_placa_state_for_plotly,paste
                                          (state, '<br>', "2014", `2014`,'<br>', 
                                          "2012", `2012`,'<br>', "2010", `2010`,'<br>',
                                          "2008", `2008`,"<br>", "2006", `2006`,'<br>',
                                          "2004", `2004`,"<br>","2002",`2002`))
#Poskus s plotly mapo
df <- read.csv("https://raw.githubusercontent.com/plotly/datasets/master/2011_us_ag_exports.csv")
df$hover <- with(df, paste(state, '<br>', "Beef", beef, "Dairy", dairy, "<br>",
                           "Fruits", total.fruits, "Veggies", total.veggies,
                           "<br>", "Wheat", wheat, "Corn", corn))
# give state boundaries a white border
l <- list(color = toRGB("white"), width = 2)
# specify some map projection/options
g <- list(
  scope = 'usa',
  projection = list(type = 'albers usa'),
  showlakes = TRUE,
  lakecolor = toRGB('white')
)

plot_ly(df, z = total.exports, text = hover, locations = code, type = 'choropleth',
        locationmode = 'USA-states', color = total.exports, colors = 'Purples',
        marker = list(line = l), colorbar = list(title = "Millions USD")) %>%
  layout(title = '2011 US Agriculture Exports by State<br>(Hover for breakdown)', geo = g)
