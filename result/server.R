

library(shiny)
library(plotly)
# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  #################################################################################################
  #                                    Fixed linear    start
  output$fixedlinearfdrbeta1<- renderPlotly({
   
  })
  output$fixedlinearfdrbeta35<- renderPlotly({

  })
  output$fixedlinearpowerbeta1<- renderPlotly({
    
  })
  output$fixedlinearpowerbeta35<- renderPlotly({
    
  })
  output$fixedlinearfdrsdbeta1<-renderTable({
   
  })
  output$fixedlinearfdrsdbeta35<-renderTable({
   
  })
  output$fixedlinearpowersdbeta1<-renderTable({
    
  })
  output$fixedlinearpowersdbeta35<-renderTable({
    
  })
  #################################################################################################
  #                                    Fixed linear    finish
  
})
