

library(shiny)
library(plotly)
# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  #################################################################################################
  #                                    Fixed linear    start
  # p= ggplot(mydata,aes(x=K,y=FDR1,col=knockstat))+geom_line()+geom_point()
  #   +ggtitle("testing for title")+ylim(0,0.15)+ylab("asdsa")+geom_hline(yintercept =  0.1,linetype = "dotdash")   
  # ggplotly(p)
  output$fixedlinearfdrbeta1<- renderPlotly({
   
  })
  # ggplot(mydata,aes(x=K,y=FDR1,col=knockstat))+geom_line()+geom_point()
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
