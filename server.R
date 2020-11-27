

library(shiny)
library(knockoff)
library(wordcloud2)
knock_to_dataframe<-function(selected,X){
  col_name=NULL
  if(is.null(colnames(X)))
    col_name=paste("X",as.integer(selected),sep = '')
  else
    col_name=colnames(X)[as.integer(selected)]
  selected=as.integer(selected)
  dataf=data.frame(col_name,selected)
  colnames(dataf)<-c("selected_variables","selected_index")
  return(dataf)
}
knockoff_result<-function(X, y, Fdr=0.1, relation='linear', method){
  if(method!="Fixed-X")
    return(knockoff.filter(X,y,fdr=Fdr))
  if(method=="Fixed-X")
    return(knockoff.filter(X,y,knockoffs=create.fixed,fdr=Fdr))
    
}












# Define server logic required to draw a histogram
server=function(input, output) {
  
  
  #sss=read.csv("BHword.csv",header = T)
 # output$mywordcloud<-renderWordcloud2({
  #  wordcloud2(sss)
 # })
  #userdata <- reactive({read.csv(input$file1$datapath,header = input$header)})
  set.seed(1)
  p=100; n=200; k=20
  mu = rep(0,p); Sigma = diag(p)
  X = round(matrix(rnorm(n*p),n),3)
  nonzero = sample(p, k)
  beta = 3.5 * (1:p %in% nonzero)
  y = round(X %*% beta + rnorm(n),3)
  default_data=cbind(data.frame(y),data.frame(X))
  #########################################################download##################################################
  output$downloadData <- downloadHandler(
    filename = function() { 
      paste("Demo", ".csv", sep="")
    },
    content = function(file) {
      write.csv(default_data, file,row.names = FALSE)
    })
  
  output$mydata <- DT::renderDT({
    
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.
    if(is.null(input$file1)){
      if(input$disp == "head") {
        return(head(default_data))
      }
      else {
        return(default_data)
      }
    }
    
    
    req(input$file1)
    
    # when reading semicolon separated files,
    # having a comma separator causes `read.csv` to error
    tryCatch(
      {
        df <- read.csv(input$file1$datapath,
                       header = input$header)
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )
    
    if(input$disp == "head") {
      return(head(df))
    }
    else {
      return(df)
    }
    
  })
  ##################################################download_finsih############################################
  nofinding<-data.frame(result="No discovery! Try a higher fdr level")
  data_thissstep<-reactive({

    if(is.null(input$file1))
      return(default_data)
    else
      return(as.data.frame(read.csv(input$file1$datapath,
                             header = input$header)))
  })
  
  result_userinput<-reactive({
    data_thisstep<-data_thissstep()
    X1=data_thisstep[,-1]
    y1=data_thisstep[,1]
    
    #result_users<-knockoff_result(X1, y1, input$alpha, relation='linear', input$fixedX)
    showNotification(
      paste("You choose ",input$fixedX,"with fdr level = ",input$alpha)
    )
    method=input$fixedX
    if(method=="Model-X")
      reu=knockoff.filter(X1,y1,fdr=input$alpha)
    if(method=="Fixed-X")
      reu=knockoff.filter(X1,y1,knockoffs=create.fixed,fdr=input$alpha)
    #reu=result_users
    thres=reu$threshold
    if(thres==Inf)
      return(nofinding)
    else
      return(knock_to_dataframe(reu$selected,X1))
  })
  output$knock_selected_covariate<-DT::renderDT({result_userinput()})
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  #############################################################################################################
  ##### final stage: create a pdf report using "report.Rmd"
  output$downloadResult<-downloadHandler(
    filename = function() { 
      paste("Selected", ".csv", sep="")
    },
    content = function(file) {
      write.csv(result_selected(), file,row.names = FALSE)
    })
  
  output$downloadReport <- downloadHandler(
    filename = function() {
      paste('myreport', sep = '.', switch(
        input$format, PDF = 'pdf', HTML = 'html', Word = 'docx'
      ))
    },
    
    content = function(file) {
      src <- normalizePath('report.Rmd')
      
      # temporarily switch to the temp dir, in case you do not have write
      # permission to the current working directory
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      file.copy(src, 'report.Rmd', overwrite = TRUE)
      
      library(rmarkdown)
      out <- render('report.Rmd', switch(
        input$format,
        PDF = pdf_document(), HTML = html_document(), Word = word_document()
      ))
      file.rename(out, file)
    }
  )
  
}




