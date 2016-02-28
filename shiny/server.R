library(shiny)

if ("server.R" %in% dir()) {
  setwd("..")
}
source("lib/libraries.r", encoding = "UTF-8")
source("uvoz/uvoz.r", encoding = "UTF-8")
source("vizualizacija/vizualizacija.r", encoding = "UTF-8")
source("analiza/analiza.r", encoding = "UTF-8")

shinyServer(function(input, output) {
  
  muu <- reactive({
    drzava <- input$variable
    df <- filter(povp_placa_state)
    graf <- ggplot() + geom_line(df, aes(x=Wage, y=Year, group=State, Color=State))
    graf
  })
  
  output$test <- renderPlot({
    muu
  })
})
