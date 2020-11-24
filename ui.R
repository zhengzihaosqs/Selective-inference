# interactive website for user to apply knockoff method
# Add wordcloud at last time
# 
#
#

library(shiny)

# Define UI for application that draws a histogram
ui=fluidPage(
  title = 'Download a PDF report',
  sidebarLayout(
    sidebarPanel(
      helpText(),
      textInput("name","Enter your name: "),
      selectInput('x', 'Build a regression model of mpg against:',
                  choices = names(mtcars)[-1]),
      radioButtons('format', 'Document format', c('PDF', 'HTML', 'Word'),
                   inline = TRUE),
      downloadButton('downloadReport')
    ),
    mainPanel(
      plotOutput('regPlot')
    )
  )
)


