library(shiny)

shinyUI(fluidPage(
  #Ime aplikacije
  titlePanel("Plače v Ameriki"),
  
  tabsetPanel(
      tabPanel("Velikost družine",
               fluidRow(column(6,
                               plotOutput("test"))),
               div(fluidRow(column(10,offset=1, align="center",
                                   checkboxGroupInput("variable","States:",inline=TRUE,selected=c("Washington"),choices=c(unique(povp_placa_state$State)))))
               )),
      
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
