

library(shiny)
library(plotly)
library(DT)
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
                      column(6, plotlyOutput("fixedlinearpowerbeta1")),
                      column(6,plotlyOutput("fixedlinearpowerbeta35"))
             ),
             fluidRow(12,
                      column(6, DTOutput("fixedlinearfdrsdbeta1")),
                      column(6, DTOutput("fixedlinearfdrsdbeta35"))
             ),
             fluidRow(12,
                      column(6, DTOutput("fixedlinearpowersdbeta1")),
                      column(6, DTOutput("fixedlinearpowersdbeta35"))
             )
    ),
    tabPanel("modellinear",
             fluidRow(12,
                      column(6, plotlyOutput("modellinearfdrbeta1")),
                      column(6,plotlyOutput("modellinearfdrbeta35"))
             ),
             
             fluidRow(12,
                      column(6, plotlyOutput("modellinearpowerbeta1")),
                      column(6,plotlyOutput("modellinearpowerbeta35"))
             ),
             fluidRow(12,
                      column(6, DTOutput("modellinearfdrsdbeta1")),
                      column(6, DTOutput("modellinearfdrsdbeta35"))
             ),
             fluidRow(12,
                      column(6, DTOutput("modellinearpowersdbeta1")),
                      column(6, DTOutput("modellinearpowersdbeta35"))
             )
    ),
    tabPanel("modelbin",
             fluidRow(12,
                      column(6, plotlyOutput("modelbinfdrbeta1")),
                      column(6,plotlyOutput("modelbinfdrbeta35"))
             ),
             
             fluidRow(12,
                      column(6, plotlyOutput("modelbinpowerbeta1")),
                      column(6,plotlyOutput("modelbinpowerbeta35"))
             ),
             fluidRow(12,
                      column(6, DTOutput("modelbinfdrsdbeta1")),
                      column(6, DTOutput("modelbinfdrsdbeta35"))
             ),
             fluidRow(12,
                      column(6, DTOutput("modelbinpowersdbeta1")),
                      column(6, DTOutput("modelbinpowersdbeta35"))
             )
    ),
    tabPanel("modelsin",
             fluidRow(12,
                      column(6, plotlyOutput("modelsinfdrbeta1")),
                      column(6,plotlyOutput("modelsinfdrbeta35"))
             ),
            
             fluidRow(12,
                      column(6, plotlyOutput("modelsinpowerbeta1")),
                      column(6,plotlyOutput("modelsinpowerbeta35"))
             ),
             fluidRow(12,
                      column(6, DTOutput("modelsinfdrsdbeta1")),
                      column(6, DTOutput("modelsinfdrsdbeta35"))
             ),
             fluidRow(12,
                      column(6, DTOutput("modelsinpowersdbeta1")),
                      column(6, DTOutput("modelsinpowersdbeta35"))
             )
    )
  )
  
))
