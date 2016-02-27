#ggplot(per_capita) + aes(x = reorder(State,`2012`), y= `2012`) +xlab("State")+geom_bar(stat="identity") + coord_flip()

#Spreminjanje per capita plač skozi leta (per_capita)
top_5 <- arrange(per_capita,desc(per_capita[,9]))
drzave <- (c(top_5$State))[c((1:5),(length(row.names(top_5))-4):length(row.names(top_5)))]

top_5 <- filter(per_capita_tidy, State %in% drzave)
graf_top_5 <- ggplot(top_5, main="Growth of per capita income through the years") + aes(x=Year, y = Wage ,colour=State) +
  geom_line(aes(group=State)) + xlab("Year") + ylab("$ per capita") + geom_point() 
########################################################################################
national_tidy1 <- buc.buc(national_tidy,5)


