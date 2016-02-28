#Funkcija da Occupation v imena vrstic, zbrise se Year, Total_employment
col.to.name <- function(vhodni){
  row.names(vhodni) <- vhodni$Occupation
  vhodni <- (vhodni[-1])
  vhodni$Year <- NULL
  try(vhodni$Total_employment <- NULL)
  return(vhodni)
}

drevesa <- function(df,razdalja="euclidian", metoda="ward.D2"){
  df <- col.to.name(df)
  df_razd <- dist(df, method = razdalja)
  df_tree <- hclust(df_razd,method = metoda)
  return(df_tree)
}

obrezi <- function(drevo, izhodni="izhodni", n=6){
  obrezano <- cutree(drevo, k=n)
  izhodni$Pay_grade <- obrezano
  return(izhodni)
}

napovej_za_grade <- function(i){
  zacasna.lo <-loess(Hourly_mean ~ Year, filter(national_grade, Pay_grade %in% i ) ,control=loess.control(surface="direct"))
  Hourly_mean <- predict(zacasna.lo, data.frame(Year=2016))
  zacasna <- data.frame(Hourly_mean)
  zacasna$Pay_grade <- i
  zacasna$Year <- 2016
  return(zacasna)
}

povp.za.plotly <- function(df){
  za_plotly <- subset(filter(df,Year==2014),select=c(State, Code))
  za_plotly$`x2014` <- (filter(df, Year==2014))$Wage
  za_plotly$`x2012` <- (filter(df, Year==2012))$Wage
  za_plotly$`x2010` <- (filter(df, Year==2010))$Wage
  za_plotly$`x2008` <- (filter(df, Year==2008))$Wage
  za_plotly$`x2006` <- (filter(df, Year==2006))$Wage
  za_plotly$`x2004` <- (filter(df, Year==2004))$Wage
  za_plotly$`x2002` <- (filter(df, Year==2002))$Wage
  za_plotly$hover <- with(za_plotly,paste0(State))
  return(za_plotly)
}
