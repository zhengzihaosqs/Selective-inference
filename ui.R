# interactive website for user to apply knockoff method
# in the report we can add BH part as comparsion
# Add wordcloud at last time
# 
#
#

library(shiny)

# Define UI for application that draws a histogram
ui=fluidPage(
  wordcloud2Output('mywordcloud'),
  titlePanel ( 'Download a PDF report'),
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
      radioButtons("disp", "Display",
                   choices = c(Head = "head",
                               All = "all"),
                   selected = "head"),
      tags$hr(style="border-color: purple"),
      selectInput('alpha', 'Enter your FDR level:',
                  choices = c(0.01,0.05,0.1,0.15),selected = 0.1),
      
      
      
      tags$hr(style="border-color: purple"),
      
      
      radioButtons('format', 'Document format', c('PDF', 'HTML', 'Word'),
                   inline = TRUE),
      downloadButton('downloadReport')
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel('knockoff: Selected covariates', DT::DTOutput('knock_selected_covariate')), 
        tabPanel('BH: Selected covariates', DT::DTOutput('BH_selected_covariate')),
        tabPanel('My dataset',DT::DTOutput('mydata'))
      )
    )
  )
  
)


