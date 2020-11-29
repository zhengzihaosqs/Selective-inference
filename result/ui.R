

library(shiny)
library(plotly)
# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Presentation of Project Simulation result"),
  tabsetPanel(
    tabPanel("fixedlinear",
             fluidRow(12,
                      column(6, plotlyOutput("fixedlinearfdrbeta1")),
                      column(6,plotlyOutput("fixedlinearfdrbeta35"))
             ),
             fluidRow(12,
                      column(6, tableOutput("fixedlinearfdrsdbeta1")),
                      column(6, tableOutput("fixedlinearfdrsdbeta35"))
             ),
             fluidRow(12,
                      column(6, plotlyOutput("fixedlinearpowerbeta1")),
                      column(6,plotlyOutput("fixedlinearpowerbeta35"))
             ),
             fluidRow(12,
                      column(6, tableOutput("fixedlinearpowersdbeta1")),
                      column(6, tableOutput("fixedlinearpowersdbeta35"))
             )
    ),
    tabPanel("modellinear",
             fluidRow(12,
                      column(6, plotlyOutput("modellinearfdrbeta1")),
                      column(6,plotlyOutput("modellinearfdrbeta35"))
             ),
             fluidRow(12,
                      column(6, tableOutput("modellinearfdrsdbeta1")),
                      column(6, tableOutput("modellinearfdrsdbeta35"))
             ),
             fluidRow(12,
                      column(6, plotlyOutput("modellinearpowerbeta1")),
                      column(6,plotlyOutput("modellinearpowerbeta35"))
             ),
             fluidRow(12,
                      column(6, tableOutput("modellinearpowersdbeta1")),
                      column(6, tableOutput("modellinearpowersdbeta35"))
             )
    ),
    tabPanel("modelbin",
             fluidRow(12,
                      column(6, plotlyOutput("modelbinfdrbeta1")),
                      column(6,plotlyOutput("modelbinfdrbeta35"))
             ),
             fluidRow(12,
                      column(6, tableOutput("modelbinfdrsdbeta1")),
                      column(6, tableOutput("modelbinfdrsdbeta35"))
             ),
             fluidRow(12,
                      column(6, plotlyOutput("modelbinpowerbeta1")),
                      column(6,plotlyOutput("modelbinpowerbeta35"))
             ),
             fluidRow(12,
                      column(6, tableOutput("modelbinpowersdbeta1")),
                      column(6, tableOutput("modelbinpowersdbeta35"))
             )
    ),
    tabPanel("modelsin",
             fluidRow(12,
                      column(6, plotlyOutput("modelsinfdrbeta1")),
                      column(6,plotlyOutput("modelsinfdrbeta35"))
             ),
             fluidRow(12,
                      column(6, tableOutput("modelsinfdrsdbeta1")),
                      column(6, tableOutput("modelsinfdrsdbeta35"))
             ),
             fluidRow(12,
                      column(6, plotlyOutput("modelsinpowerbeta1")),
                      column(6,plotlyOutput("modelsinpowerbeta35"))
             ),
             fluidRow(12,
                      column(6, tableOutput("modelsinpowersdbeta1")),
                      column(6, tableOutput("modelsinpowersdbeta35"))
             )
    )
  )
  
))
