library(shiny)

shinyUI(fluidPage(
  
  titlePanel("Plače v Ameriki"),
  
  tabsetPanel(
      tabPanel("Velikost družine",
               DT::dataTableOutput("druzine")),
      
      tabPanel("Število naselij",
               sidebarPanel(
                  uiOutput("pokrajine")
                ),
               mainPanel(plotOutput("naselja"))),
      
      tabPanel("Zemljevid",
               plotOutput("zemljevid")),
      
      tabPanel("Število naselij in površina",
               plotOutput("povrsina"))
    )
))
