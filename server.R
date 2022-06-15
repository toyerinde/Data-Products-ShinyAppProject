#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(shiny)
library(mlbench)
library(caret)
library(glmnet)
library(randomForest)
library(ggplot2)
library(dplyr)
data("PimaIndiansDiabetes2")
df_db<-PimaIndiansDiabetes2
# Preprocessing raw data to remove all rows with NAs
df_dbcln<-df_db[complete.cases(df_db),]

#Creating training and validation dataset from original training data
set.seed(2122)
inTrain_BC<-createDataPartition(df_dbcln$diabetes,p=0.75,list=FALSE)
training_datadb<-df_dbcln[inTrain_BC,]
validation_datadb<-df_dbcln[-inTrain_BC,]
# Set cross validation specs for the training dataset
ctrlspecs_tr<-trainControl(method="cv",number=10,savePredictions="all",classProbs = TRUE)
lambda_vector<-10^seq(5,-5,length=300)
set.seed(2122)

#LASSO regression model fitting
modfit_lassodb<-train(diabetes~.,data=training_datadb,preProcess=c("center","scale"),
                      method="glmnet",tuneGrid=expand.grid(alpha=1,lambda=lambda_vector),
                      trControl=ctrlspecs_tr)

predict_lassodb<-predict(modfit_lassodb,newdata=validation_datadb)
model_lassodbperf<-confusionMatrix(predict_lassodb,validation_datadb$diabetes)

#Random Forest regression model fitting 
set.seed(2122)
modfit_rfdb<-train(diabetes~.,data=training_datadb,preProcess=c("center","scale"),
                   method="rf",tuneLength=8,
                   trControl=ctrlspecs_tr)

predict_rfdb<-predict(modfit_rfdb,newdata=validation_datadb)
model_rfperfdb<-confusionMatrix(predict_rfdb,validation_datadb$diabetes)


# Define server logic required to draw a histogram
shinyServer(function(input, output,session) {
observeEvent(input$Run,{
  modelinput<-input$radio
  if (modelinput=="LASSO") {
  
  #LASSO regression model variable importance plot
  
  output$ImpPlot<-renderPlot({
    ggplot(varImp(modfit_lassodb)) + labs(title="LASSO Variable Importance Plot")
    
  })
    output$accu1<-renderText({
   paste("LASSO Model Accuracy = ",model_lassodbperf$overall[[1]])
    })  
    
    output$kappa1<-renderText({
    paste("LASSO Model Kappa = " , model_lassodbperf$overall[[2]])
    })
  }
     else {
 
    
    output$ImpPlot <- renderPlot({
     ggplot(varImp(modfit_rfdb))+ labs(title = "Random Forest Variable Importance Plot")
  
    })
    output$accu2<-renderText({
    paste("Random Forest Accuracy =",  model_rfperfdb$overall[[1]])
      })  
      
    output$kappa2<-renderText({
    paste("Random Forest Kappa =",  model_rfperfdb$overall[[2]])
      })
     }
})
    
  
 # Reset  
  observeEvent(input$Reset,{
    output$ImpPlot<-renderPlot({})
    output$accu1<-renderText({})
    output$kappa1<-renderText({})
    output$accu2<-renderText({})
    output$kappa2<-renderText({})
  })
  

  
  # Comparison
    comp_perfdb<-matrix(c(model_lassodbperf$overall[[1]],model_lassodbperf$overall[[2]],
                          model_rfperfdb$overall[[1]],model_rfperfdb$overall[[2]]),
                        ncol=2,byrow= TRUE)
    #Name the columns and rows of comparison matrix
    colnames(comp_perfdb)<-c("Accuracy","Kappa")
    rownames(comp_perfdb)<-c("LASSO","Random Forest")
    comp_perfdbdf<-as.data.frame(comp_perfdb)
    Model<-c("LASSO","Random Forest")
    comp_df<-cbind(Model,comp_perfdbdf)
    observeEvent(input$compare, {
      output$Comp<-renderTable({
      comp_df
      
    })
      # Recommendation
      rec_lasso<-model_lassodbperf$overall[[1]]
      rec_rf<-model_rfperfdb$overall[[1]]
      if(rec_lasso>rec_rf) {
        output$recomm<- renderText({
          paste("The recommended predictive model is LASSO")
        })
      }
      else {
        output$recomm<- renderText({
          paste("The recommended predictive model is Random Forest") 
        })
      } 
    
      
    })
    
    # Exit App 
    observeEvent(input$Exit,{
      stopApp()
    })
    


})
