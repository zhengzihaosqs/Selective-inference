

library(shiny)
library(knockoff)
library(wordcloud2)
# Define server logic required to draw a histogram
server=function(input, output) {
  sss=read.csv("BHword.csv",header = T)
  output$mywordcloud<-renderWordcloud2({
    wordcloud2(sss)
  })
  userdata <- reactive({read.csv(input$file1$datapath)})
  set.seed(1)
  p=200; n=100; k=15
  mu = rep(0,p); Sigma = diag(p)
  X = matrix(rnorm(n*p),n)
  nonzero = sample(p, k)
  beta = 3.5 * (1:p %in% nonzero)
  y = X %*% beta + rnorm(n)
  default_data=cbind(data.frame(y),data.frame(X))
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
  
  
  
  #############################################################################################################
  ##### final stage: create a pdf report using "report.Rmd"
  output$downloadReport <- downloadHandler(
    filename = function() {
      paste('my-report', sep = '.', switch(
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