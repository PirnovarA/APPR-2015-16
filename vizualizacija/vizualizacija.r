# 3. faza: Izdelava zemljevida
source("lib/uvozi.zemljevid.r",encoding="UTF-8")
# Uvozimo zemljevid.


national_tidy_mean <- filter(national_tidy, Type=="Hourly_mean")
state_tidy_mean <- filter(state_tidy, Type=="Hourly_mean")
povp_placa_state <- filter(state_tidy_mean, Occupation=="All Occupations") 
#Spreminljanje povprečne plače v zveznih državah
graf_state_tidy_mean <- ggplot(data=povp_placa_state, aes(x=Year, y=Wage)) + 
  geom_smooth(method="loess")
graf_state_tidy_mean
test <- ggplotly(graf_state_tidy_mean)

all_states <- map_data("state")
test_state <- povp_placa_state
test_state$region <- tolower(povp_placa_state$State)
Total <- merge(all_states, test_state, by="region")
