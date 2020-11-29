

library(shiny)
library(plotly)
library(latex2exp)
library(DT)
# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  #################################################################################################
  #                                    Fixed linear    start
  fixedlineark<-read.csv("fixedlinearK.csv")
  
  output$fixedlinearfdrbeta1<- renderPlotly({
    p= ggplot(fixedlineark,aes(x=K,y=FDP1,col=knockstat))+geom_line()+geom_point()+
      ggtitle("beta =1,Fixed-X,linear model---FDP")+ylim(0,0.15)+ylab("FDP")+geom_hline(yintercept =  0.1,linetype = "dotdash") 
    return(ggplotly(p))
  })
  output$fixedlinearfdrbeta35<- renderPlotly({
    p= ggplot(fixedlineark,aes(x=K,y=FDP35,col=knockstat))+geom_line()+geom_point()+
      ggtitle("beta =3.5,Fixed-X,linear model---FDP")+ylim(0,0.15)+ylab("FDP")+geom_hline(yintercept =  0.1,linetype = "dotdash") 
    return(ggplotly(p))

  })
  output$fixedlinearpowerbeta1<- renderPlotly({
    p= ggplot(fixedlineark,aes(x=K,y=POWER1,col=knockstat))+geom_line()+geom_point()+
      ggtitle("beta =1,Fixed-X,linear model---power")+ylab("power")+geom_hline(yintercept =  1,linetype = "dotdash") 
    return(ggplotly(p))
  })
  output$fixedlinearpowerbeta35<- renderPlotly({
    p= ggplot(fixedlineark,aes(x=K,y=POWER35,col=knockstat))+geom_line()+geom_point()+
      ggtitle("beta =3.5,Fixed-X,linear model---power")+ylab("power")+geom_hline(yintercept =  1,linetype = "dotdash") 
    return(ggplotly(p))
  })
  fixedlinearsd<-read.csv("fixedlinearsd.csv")
  output$fixedlinearfdrsdbeta1<-renderDT({ 
    datatable(fixedlinearsd[c(1,3,5,7,9),],rownames = F,caption = tags$caption('beta =1,Fixed-X,linear model---standard deviation for FDP',style='text-align: center'))
  })
  output$fixedlinearfdrsdbeta35<-renderDT({
    datatable(fixedlinearsd[c(2,4,6,8,10),],rownames = F,caption = tags$caption('beta =3.5,Fixed-X,linear model---standard deviation for FDP',style='text-align: center'))
  })
  output$fixedlinearpowersdbeta1<-renderDT({
    datatable(fixedlinearsd[c(11,13,15,17,19),],rownames = F,caption = tags$caption('beta =1,Fixed-X,linear model---standard deviation for power',style='text-align: center'))
  })
  output$fixedlinearpowersdbeta35<-renderDT({
    datatable(fixedlinearsd[c(12,14,16,18,20),],rownames = F,caption = tags$caption('beta =3.5,Fixed-X,linear model---standard deviation for power',style='text-align: center'))
  })
  #################################################################################################
  #                                    Fixed linear    finish
  #################################################################################################
  #                                    Model linear    start
  modellineark<-read.csv("modellinearK.csv")
  
  output$modellinearfdrbeta1<- renderPlotly({
    p= ggplot(modellineark,aes(x=K,y=FDP1,col=knockstat))+geom_line()+geom_point()+
      ggtitle("beta =1,Model-X,linear model---FDP")+ylim(0,0.15)+ylab("FDP")+geom_hline(yintercept =  0.1,linetype = "dotdash") 
    return(ggplotly(p))
  })
  output$modellinearfdrbeta35<- renderPlotly({
    p= ggplot(modellineark,aes(x=K,y=FDP35,col=knockstat))+geom_line()+geom_point()+
      ggtitle("beta =3.5,Model-X,linear model---FDP")+ylim(0,0.15)+ylab("FDP")+geom_hline(yintercept =  0.1,linetype = "dotdash") 
    return(ggplotly(p))
    
  })
  output$modellinearpowerbeta1<- renderPlotly({
    p= ggplot(modellineark,aes(x=K,y=POWER1,col=knockstat))+geom_line()+geom_point()+
      ggtitle("beta =1,Model-X,linear model---power")+ylab("power")+geom_hline(yintercept =  1,linetype = "dotdash") 
    return(ggplotly(p))
  })
  output$modellinearpowerbeta35<- renderPlotly({
    p= ggplot(modellineark,aes(x=K,y=POWER35,col=knockstat))+geom_line()+geom_point()+
      ggtitle("beta =3.5,Model-X,linear model---power")+ylab("power")+geom_hline(yintercept =  1,linetype = "dotdash") 
    return(ggplotly(p))
  })
  modellinearsd<-read.csv("modellinearsd.csv")
  output$modellinearfdrsdbeta1<-renderDT({ 
    datatable(modellinearsd[c(1,3,5,7,9),],rownames = F,caption = tags$caption('beta =1,Model-X,linear model---standard deviation for FDP',style='text-align: center'))
  })
  output$modellinearfdrsdbeta35<-renderDT({
    datatable(modellinearsd[c(2,4,6,8,10),],rownames = F,caption = tags$caption('beta =3.5,Model-X,linear model---standard deviation for FDP',style='text-align: center'))
  })
  output$modellinearpowersdbeta1<-renderDT({
    datatable(modellinearsd[c(11,13,15,17,19),],rownames = F,caption = tags$caption('beta =1,Model-X,linear model---standard deviation for power',style='text-align: center'))
  })
  output$modellinearpowersdbeta35<-renderDT({
    datatable(modellinearsd[c(12,14,16,18,20),],rownames = F,caption = tags$caption('beta =3.5,Model-X,linear model---standard deviation for power',style='text-align: center'))
  })
  #################################################################################################
  #                                    Fixed linear    finish
  #################################################################################################
  #                                    Model sin    start
  modelsink<-read.csv("modelsinK.csv")
  
  output$modelsinfdrbeta1<- renderPlotly({
    p= ggplot(modelsink,aes(x=K,y=FDP1,col=knockstat))+geom_line()+geom_point()+
      ggtitle("beta =1,Model-X,Y=sin(X)beta---FDP")+ylim(0,0.15)+ylab("FDP")+geom_hline(yintercept =  0.1,linetype = "dotdash") 
    return(ggplotly(p))
  })
  output$modelsinfdrbeta35<- renderPlotly({
    p= ggplot(modelsink,aes(x=K,y=FDP35,col=knockstat))+geom_line()+geom_point()+
      ggtitle("beta =3.5,Model-X,Y=sin(X)beta---FDP")+ylim(0,0.15)+ylab("FDP")+geom_hline(yintercept =  0.1,linetype = "dotdash") 
    return(ggplotly(p))
    
  })
  output$modelsinpowerbeta1<- renderPlotly({
    p= ggplot(modelsink,aes(x=K,y=POWER1,col=knockstat))+geom_line()+geom_point()+
      ggtitle("beta =1,Model-X,Y=sin(X)beta---power")+ylab("power")+geom_hline(yintercept =  1,linetype = "dotdash") 
    return(ggplotly(p))
  })
  output$modelsinpowerbeta35<- renderPlotly({
    p= ggplot(modelsink,aes(x=K,y=POWER35,col=knockstat))+geom_line()+geom_point()+
      ggtitle("beta =3.5,Model-X,Y=sin(X)beta---power")+ylab("power")+geom_hline(yintercept =  1,linetype = "dotdash") 
    return(ggplotly(p))
  })
  modelsinsd<-read.csv("modelsinsd.csv")
  output$modelsinfdrsdbeta1<-renderDT({ 
    datatable(modelsinsd[c(1,3,5,7,9),],rownames = F,caption = tags$caption('beta =1,Model-X,Y=sin(X)beta---standard deviation for FDP',style='text-align: center'))
  })
  output$modelsinfdrsdbeta35<-renderDT({
    datatable(modelsinsd[c(2,4,6,8,10),],rownames = F,caption = tags$caption('beta =3.5,Model-X,Y=sin(X)beta---standard deviation for FDP',style='text-align: center'))
  })
  output$modelsinpowersdbeta1<-renderDT({
    datatable(modelsinsd[c(11,13,15,17,19),],rownames = F,caption = tags$caption('beta =1,Model-X,Y=sin(X)beta---standard deviation for power',style='text-align: center'))
  })
  output$modelsinpowersdbeta35<-renderDT({
    datatable(modelsinsd[c(12,14,16,18,20),],rownames = F,caption = tags$caption('beta =3.5,Model-X,Y=sin(X)beta---standard deviation for power',style='text-align: center'))
  })
  #################################################################################################
  #                                    Fixed sin    finish
  #################################################################################################
  #                                    Model bin    start
  modelbink<-read.csv("modelbinK.csv")
  
  output$modelbinfdrbeta1<- renderPlotly({
    p= ggplot(modelbink,aes(x=K,y=FDP1,col=knockstat))+geom_line()+geom_point()+
      ggtitle("beta =1,Model-X,logistics model---FDP")+ylim(0,0.15)+ylab("FDP")+geom_hline(yintercept =  0.1,linetype = "dotdash") 
    return(ggplotly(p))
  })
  output$modelbinfdrbeta35<- renderPlotly({
    p= ggplot(modelbink,aes(x=K,y=FDP35,col=knockstat))+geom_line()+geom_point()+
      ggtitle("beta =3.5,Model-X,logistics model---FDP")+ylim(0,0.15)+ylab("FDP")+geom_hline(yintercept =  0.1,linetype = "dotdash") 
    return(ggplotly(p))
    
  })
  output$modelbinpowerbeta1<- renderPlotly({
    p= ggplot(modelbink,aes(x=K,y=POWER1,col=knockstat))+geom_line()+geom_point()+
      ggtitle("beta =1,Model-X,logistics model---power")+ylab("power")+geom_hline(yintercept =  1,linetype = "dotdash") 
    return(ggplotly(p))
  })
  output$modelbinpowerbeta35<- renderPlotly({
    p= ggplot(modelbink,aes(x=K,y=POWER35,col=knockstat))+geom_line()+geom_point()+
      ggtitle("beta =3.5,Model-X,logistics model---power")+ylab("power")+geom_hline(yintercept =  1,linetype = "dotdash") 
    return(ggplotly(p))
  })
  modelbinsd<-read.csv("modelbinsd.csv")
  output$modelbinfdrsdbeta1<-renderDT({ 
    datatable(modelbinsd[c(1,3,5,7,9),],rownames = F,caption = tags$caption('beta =1,Model-X,logistics model---standard deviation for FDP',style='text-align: center'))
  })
  output$modelbinfdrsdbeta35<-renderDT({
    datatable(modelbinsd[c(2,4,6,8,10),],rownames = F,caption = tags$caption('beta =3.5,Model-X,logistics model---standard deviation for FDP',style='text-align: center'))
  })
  output$modelbinpowersdbeta1<-renderDT({
    datatable(modelbinsd[c(11,13,15,17,19),],rownames = F,caption = tags$caption('beta =1,Model-X,logistics model---standard deviation for power',style='text-align: center'))
  })
  output$modelbinpowersdbeta35<-renderDT({
    datatable(modelbinsd[c(12,14,16,18,20),],rownames = F,caption = tags$caption('beta =3.5,Model-X,logistics model---standard deviation for power',style='text-align: center'))
  })
  #################################################################################################
  #                                    Fixed bin    finish
})
