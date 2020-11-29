

library(shiny)
library(knockoff)
library(wordcloud2)
library(ranger)
library(stabs)
library(RPtests)
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


# Define server logic required to draw a histogram
server=function(input, output) {
  showModal(modalDialog("This online app is designed to run knockoff method for a user",footer = modalButton("Get it!")))
  
  sss=read.csv("BHword.csv",header = T)
  output$mywordcloud<-renderWordcloud2({
    wordcloud2(sss)
  })
  #userdata <- reactive({read.csv(input$file1$datapath,header = input$header)})
  #set.seed(1)
  p=200; n=500; k=40
  mu = rep(0,p); Sigma = diag(p)
  Xd = round(matrix(rnorm(n*p),n),3)
  nonzero = sample(p, k)
  beta = 3.5 * (1:p %in% nonzero)
  yd = round(Xd %*% beta + rnorm(n),3)
  default_data=cbind(data.frame(yd),data.frame(Xd))
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
  nofinding<-data.frame(result="No discovery! Try a higher fdr level OR Chnage knockoff statistic")
  data_for_analysis<-reactive({

    if(is.null(input$file1))
      return(default_data)
    else
      return(as.data.frame(read.csv(input$file1$datapath,
                             header = input$header)))
  })
  
  # look whether result_selected change or not : to make sure it is reactive#########
  ########################################
  
  
  
  return_X_Xk<-reactive({
    input$do
    observeEvent(input$do,{showModal(modalDialog("The knockoff copy for model-X changes!"))})
    data_thisstep<-data_for_analysis()
   
    X1=data_thisstep[,-1]
    y1=data_thisstep[,1]
    X_k_second=create.second_order(as.matrix(X1))
    temp = create.fixed(X1)
    
    return(list(ori_X=X1,X_fixed=temp$X,Xk_fixed=temp$Xk,Xk_modelX=X_k_second,y=y1))
   
  })
  
  
  result_userinput<-reactive({
    data_need=return_X_Xk()
    original_X=data_need$ori_X
    X=NULL
    X_k=NULL
    if(input$fixedX=="Model-X"){
      X=as.matrix(data_need$ori_X)
      X_k=as.matrix(data_need$Xk_modelX)
    }
    else{
      X=as.matrix(data_need$X_fixed)
      X_k=as.matrix(data_need$Xk_fixed)
    }
    y=data_need$y
    FDR=input$alpha
    #   inputId = "expfamily",  inputId = "knockoffstat", inputId = "expfamilyknockstat",   inputId = "extraMethod",
    if(input$extraMethod=="None"){
      if(input$expfamily=="None"){
        #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
        # run default here with different importance measure start
        if(input$knockoffstat=="lasso coefdiff"){
          showNotification("Using coefficient difference to measure variable importance under LASSO")
          k_stat=stat.lasso_coefdiff(X,X_k,y)
          thres=knockoff.threshold(k_stat,fdr=FDR)
          if(thres>10000)
            return(nofinding)
          else{
            discoveries_index = which(k_stat > thres)
            return(knock_to_dataframe(discoveries_index,original_X))
          }
        }
        if(input$knockoffstat=="lasso lambdadiff"){
          showNotification("Using difference of entering time to measure variable importance under LASSO")
          k_stat=stat.lasso_lambdadiff(X,X_k,y)
          thres=knockoff.threshold(k_stat,fdr=FDR)
          if(thres>10000)
            return(nofinding)
          else{
            discoveries_index = which(k_stat > thres)
            return(knock_to_dataframe(discoveries_index,original_X))
          }
        }
        if(input$knockoffstat=="lasso lambdasmax"){
          showNotification("Using signed maximum entering time to measure variable importance under LASSO")
          k_stat=stat.lasso_lambdasmax(X,X_k,y)
          thres=knockoff.threshold(k_stat,fdr=FDR)
          if(thres>10000)
            return(nofinding)
          else{
            discoveries_index = which(k_stat > thres)
            return(knock_to_dataframe(discoveries_index,original_X))
          }
        }
        if(input$knockoffstat=="Correlation difference"){
          showNotification("Using pairwise sample correlation to measure variable importance")
          k_stat=sapply(1:ncol(X),function(j) {abs(sum(y*X[,j]))-abs(sum(y*X_k[,j]))})
          thres=knockoff.threshold(k_stat,fdr=FDR)
          if(thres>10000)
            return(nofinding)
          else{
            discoveries_index = which(k_stat > thres)
            return(knock_to_dataframe(discoveries_index,original_X))
          }
        }
        if(input$knockoffstat=="Correlation difference"){
          showNotification("Using pairwise sample correlation to measure variable importance")
          k_stat=sapply(1:ncol(X),function(j) {abs(sum(y*X[,j]))-abs(sum(y*X_k[,j]))})
          thres=knockoff.threshold(k_stat,fdr=FDR)
          if(thres>10000)
            return(nofinding)
          else{
            discoveries_index = which(k_stat > thres)
            return(knock_to_dataframe(discoveries_index,original_X))
          }
        }
        if(input$knockoffstat=="Kendall"){
          showNotification("Using nonparametric correlation Kendall tau to measure variable importance")
          k_stat=sapply(1:ncol(X),function(j) {abs(cor(y,X[,j],method = c("kendall")))-abs(cor(y,X_k[,j],method = c("kendall")))})
          thres=knockoff.threshold(k_stat,fdr=FDR)
          if(thres>10000)
            return(nofinding)
          else{
            discoveries_index = which(k_stat > thres)
            return(knock_to_dataframe(discoveries_index,original_X))
          }
        }
        if(input$knockoffstat=="Spearman"){
          showNotification("Using nonparametric Spearman correlation to measure variable importance")
          k_stat=sapply(1:ncol(X),function(j) {abs(cor(y,X[,j],method = c("spearman")))-abs(cor(y,X_k[,j],method = c("spearman")))})
          thres=knockoff.threshold(k_stat,fdr=FDR)
          if(thres>10000)
            return(nofinding)
          else{
            discoveries_index = which(k_stat > thres)
            return(knock_to_dataframe(discoveries_index,original_X))
          }
        }
        
        #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
        # run default here with different importance measure finish
      }
      else{
        #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
        # run expfamily here with different importance measure start
        if((input$expfamily=="binomial")&(input$expfamilyknockstat=="coefficient difference")){
          showNotification("Using coefficient difference to measure variable importance under logistics regression")
          k_stat=stat.glmnet_coefdiff(X,X_k,y,family = "binomial")
          thres=knockoff.threshold(k_stat,fdr=FDR)
          if(thres>10000)
            return(nofinding)
          else{
            discoveries_index = which(k_stat > thres)
            return(knock_to_dataframe(discoveries_index,original_X))
          }
        }
        if((input$expfamily=="binomial")&(input$expfamilyknockstat=="lambda difference")){
          showNotification("Using difference of entering time to measure variable importance under logistics regression")
          k_stat=stat.glmnet_lambdadiff(X,X_k,y,family = "binomial")
          thres=knockoff.threshold(k_stat,fdr=FDR)
          if(thres>10000)
            return(nofinding)
          else{
            discoveries_index = which(k_stat > thres)
            return(knock_to_dataframe(discoveries_index,original_X))
          }
        }
        if((input$expfamily=="binomial")&(input$expfamilyknockstat=="lambda max")){
          showNotification("Using signed maximum entering time to measure variable importance under logistics regression")
          k_stat=stat.glmnet_lambdadiff(X,X_k,y,family = "binomial")
          thres=knockoff.threshold(k_stat,fdr=FDR)
          if(thres>10000)
            return(nofinding)
          else{
            discoveries_index = which(k_stat > thres)
            return(knock_to_dataframe(discoveries_index,original_X))
          }
        }
        if((input$expfamily=="poisson")&(input$expfamilyknockstat=="coefficient difference")){
          showNotification("Using coefficient difference to measure variable importance for non-negative counts")
          k_stat=stat.glmnet_coefdiff(X,X_k,y,family = "poisson")
          thres=knockoff.threshold(k_stat,fdr=FDR)
          if(thres>10000)
            return(nofinding)
          else{
            discoveries_index = which(k_stat > thres)
            return(knock_to_dataframe(discoveries_index,original_X))
          }
        }
        if((input$expfamily=="poisson")&(input$expfamilyknockstat=="lambda difference")){
          showNotification("Using difference of entering time to measure variable importance for non-negative counts")
          k_stat=stat.glmnet_lambdadiff(X,X_k,y,family = "poisson")
          thres=knockoff.threshold(k_stat,fdr=FDR)
          if(thres>10000)
            return(nofinding)
          else{
            discoveries_index = which(k_stat > thres)
            return(knock_to_dataframe(discoveries_index,original_X))
          }
        }
        if((input$expfamily=="poisson")&(input$expfamilyknockstat=="lambda max")){
          showNotification("Using signed maximum entering time to measure variable importance for non-negative counts")
          k_stat=stat.glmnet_lambdadiff(X,X_k,y,family = "poisson")
          thres=knockoff.threshold(k_stat,fdr=FDR)
          if(thres>10000)
            return(nofinding)
          else{
            discoveries_index = which(k_stat > thres)
            return(knock_to_dataframe(discoveries_index,original_X))
          }
        }
        if((input$expfamily=="multinomial")&(input$expfamilyknockstat=="coefficient difference")){
          showNotification("Using coefficient difference to measure variable importance for multiple levels factor")
          k_stat=stat.glmnet_coefdiff(X,X_k,y,family = "multinomial")
          thres=knockoff.threshold(k_stat,fdr=FDR)
          if(thres>10000)
            return(nofinding)
          else{
            discoveries_index = which(k_stat > thres)
            return(knock_to_dataframe(discoveries_index,original_X))
          }
        }
        if((input$expfamily=="multinomial")&(input$expfamilyknockstat=="lambda difference")){
          showNotification("Using difference of entering time to measure variable importance for multiple levels factor")
          k_stat=stat.glmnet_lambdadiff(X,X_k,y,family = "multinomial")
          thres=knockoff.threshold(k_stat,fdr=FDR)
          if(thres>10000)
            return(nofinding)
          else{
            discoveries_index = which(k_stat > thres)
            return(knock_to_dataframe(discoveries_index,original_X))
          }
        }
        if((input$expfamily=="multinomial")&(input$expfamilyknockstat=="lambda max")){
          showNotification("Using signed maximum entering time to measure variable importance for multiple levels factor")
          k_stat=stat.glmnet_lambdadiff(X,X_k,y,family = "multinomial")
          thres=knockoff.threshold(k_stat,fdr=FDR)
          if(thres>10000)
            return(nofinding)
          else{
            discoveries_index = which(k_stat > thres)
            return(knock_to_dataframe(discoveries_index,original_X))
          }
        }
      
        #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
        # run expfamily here with different importance measure finish
      }
    }
    else{
      #@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
      # EXTA METHOD part begin
      if(input$extraMethod=="random forest"){
        showNotification("Using random forest to compute variable importance.")
        k_stat=stat.random_forest(X,X_k,y)
        thres=knockoff.threshold(k_stat,fdr=FDR)
        if(thres>10000)
          return(nofinding)
        else{
          discoveries_index = which(k_stat > thres)
          return(knock_to_dataframe(discoveries_index,original_X))
        }
      }
      if(input$extraMethod=="sqrt lasso"){
        showNotification("Using signed maximum lambda in square root lasso to compute variable importance.")
        k_stat=stat.sqrt_lasso(X,X_k,y)
        thres=knockoff.threshold(k_stat,fdr=FDR)
        if(thres>10000)
          return(nofinding)
        else{
          discoveries_index = which(k_stat > thres)
          return(knock_to_dataframe(discoveries_index,original_X))
        }
      }
      if(input$extraMethod=="stability selection"){
        showNotification("Using stability selection to measure variable importance.")
        k_stat=stat.stability_selection(X,X_k,y)
        thres=knockoff.threshold(k_stat,fdr=FDR)
        if(thres>10000)
          return(nofinding)
        else{
          discoveries_index = which(k_stat > thres)
          return(knock_to_dataframe(discoveries_index,original_X))
        }
      }
      #@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
      # EXTA METHOD part finish
    }
    
    
    
    
      
    result_users<-knockoff_result(X1, y1, input$alpha, relation='linear', input$fixedX)
    showNotification(
      paste("You choose ",input$fixedX,"with fdr level = ",input$alpha)
    )
    reu=result_users
    thres=reu$threshold
    if(thres>10000)
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
      write.csv(result_userinput(), file,row.names = FALSE)
    })
  
  
  
}




