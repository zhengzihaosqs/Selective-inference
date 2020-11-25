# interactive website for user to apply knockoff method
# in the report we can add BH part as comparsion
# Add wordcloud at last time
# 
#
#

library(shiny)
library(wordcloud2)
# Define UI for application that draws a histogram
ui=fluidPage(
  wordcloud2Output('mywordcloud'),
  titlePanel ( 'Knockoff: based on user perference'),
  tags$text ("make sure first column "),
  tags$em("This text is emphasized."),
 
  sidebarLayout(
    sidebarPanel(
      helpText(""),
      textInput("name","Enter your name: ",value = "Paul Li(subject to change)"),
      
      fileInput("file1", "Upload data, choose CSV File",
                multiple = FALSE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),
      checkboxInput("header", "Header", TRUE),
      radioButtons("disp", "Display my dataset",
                   choices = c(Head = "head",
                               All = "all"),
                   selected = "head"),
      downloadButton('downloadData', label="download default Data"),
      tags$hr(style="border-color: purple"),
      selectInput('alpha', 'Enter your FDR level:',
                  choices = c(0.01,0.05,0.1,0.15),selected = 0.1),
      
      
      
      tags$hr(style="border-color: purple"),
      tags$h4("Advanced usage with custom arguments:"),
      selectInput(
        inputId = "knockoffstat",
        label = "Variable Importance Statistic", 
        choices = c("lasso coefdiff", "lasso lambdadiff", "lasso lambdasmax", "Correlation","Kendall","Spearman","random forest")
      ),
      
      
      tags$hr(style="border-color: purple"),
      radioButtons('format', 'Document format', c('PDF', 'HTML', 'Word'),
                   inline = TRUE),
      downloadButton('downloadReport')
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel('My dataset',DT::DTOutput('mydata')),
        tabPanel('knockoff: Selected covariates', DT::DTOutput('knock_selected_covariate'))
        
      )
    )
   
  ),
  # WHERE YOUR FOOTER GOES
  hr(),
  tags$h4("Reference:"),
  textOutput("refer1"),tags$a(href="https://projecteuclid.org/euclid.aos/1438606853", "[link]"),
  textOutput("refer2"),tags$a(href="https://arxiv.org/abs/1610.02351", "[link]")
  
  
)


