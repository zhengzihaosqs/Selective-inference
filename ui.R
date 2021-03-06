# interactive website for user to apply knockoff method
# Add wordcloud at last time
# 
#
#

library(shiny)
library(shinyWidgets)

library(ECharts2Shiny)
# Define UI for application that draws a histogram
ui=fluidPage(
 
  titlePanel ( 'Knockoff: variable selection based on user perference'),
 
  sidebarLayout(
    sidebarPanel(
      helpText(""),
     
      fileInput("file1", "Upload data, choose CSV File",
                multiple = FALSE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),
      checkboxInput("header", "Header", TRUE),
      radioButtons("disp", "Display my dataset",
                   choices = c(Head = "head",
                               All = "all"),
                   selected = "all"),
      downloadButton('downloadData', label="Download Default Data"),
      tags$hr(style="border-color: purple"),
      selectInput('alpha', 'Select your FDR level:',
                  choices = c(0.01,0.05,0.1,0.15,0.2,0.25),selected = 0.01),
      
      
      
      tags$hr(style="border-color: purple"),
      downloadButton('downloadResult', label="Download Result")
  ),
    
    mainPanel(
      tabsetPanel(
        tabPanel('knockoff: Selected covariates', DT::DTOutput('knock_selected_covariate')),
        tabPanel('My dataset',DT::DTOutput('mydata'))
      )
    )
   
  ),
  
 # tags$div(tags$h6("The default setting is approximate model-X Gaussian knockoffs, ",style="color:red")
 #          ,tags$h6("if you want to use classical fixed-X knockoff, make sure ",style="color:red"),
 #          tags$h6("n>2p(safe) or n>p and click the below button",style="color:red")),
  radioGroupButtons(
    inputId = "fixedX",label="",
    choices = c("Model-X", 
                "Fixed-X"),selected = "Model-X",status = "primary",checkIcon = list(
                  yes = icon("ok", 
                             lib = "glyphicon"),
                  no = icon("remove",
                            lib = "glyphicon"))
  ),
 tags$a(href="https://rpubs.com/selectiveinferencepaul/699241", "[user manual]",target="_blank"),
  tags$h4("Advanced usage with custom arguments:"),
  selectInput(
    inputId = "knockoffstat",
    label = "Knockoff Statistic", 
    choices = c("lasso coefdiff", "lasso lambdadiff", "lasso lambdasmax", "Correlation difference","Kendall","Spearman"),
    selected = "lasso coefdiff"
  ),
  tags$h5("If you want to model your data with GLM, you can specify the Corresponding family and knockoff statistics:"),
  radioGroupButtons(
    inputId = "expfamily",
    label = "Choices of exponential family:", 
    choices = c("None", "binomial", "poisson", "multinomial"),selected ="None" 
  ), 
  
  radioGroupButtons(
    inputId = "expfamilyknockstat",
    label = "Choices of knockoff statistics for exponential family:", 
    choices = c("coefficient difference", "lambda difference","lambda max"),selected ="coefficient difference"
  ), 
  tags$h4("Extra Method to copute knockoff statistic",style="color:blue"),
  radioGroupButtons(
    inputId = "extraMethod",
    choices = c("None","random forest", "sqrt lasso","stability selection"),selected ="None"
  ), 
  tags$h4("Want a new knockoff copy?",style="color:red"),
  actionButton("do", "A new Knockoff Copy"),
  
  # WHERE YOUR FOOTER GOES
  hr(),
  tags$h4("Reference:"),
  tags$div(
    "Barber, R. F., & Candes, E. J. (2015). Controlling the false discovery rate via knockoffs. The Annals of Statistics, 43(5), 2055-2085.",
    tags$a(href="https://projecteuclid.org/euclid.aos/1438606853", "[link]")
  ),
  tags$div(
    "Candes, E., Fan, Y., Janson, L., & Lv, J. (2016). Panning for gold: Model-X knockoffs for high-dimensional controlled variable selection.",
    tags$a(href="https://arxiv.org/abs/1610.02351", "[link]")
  )
 
  
)


