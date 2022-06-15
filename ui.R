#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(

    # Application title
    titlePanel(h4("Determination of the Best Model for Predicting Onset of Diabetes in Female Pima Indians")),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
          wellPanel(
          tags$a(href= "http://rpubs.com/Troyerinde/914802", " App Documentation"), 
          tags$p(radioButtons("radio", label = h3("Select Predictive Model"),
                       choices = c("LASSO" = "LASSO", "Random Forest" = "RF"), 
                       selected = "")),
          fluidRow(actionButton("Run", "Run Selected Model")),
          
          fluidRow(actionButton("Reset","Clear Model Results")),
          
          fluidRow(actionButton("Exit","Exit App")),
          fluidRow(actionButton("compare",label= "Compare Model(Run both models first)"))
        )), 
        
        
        
        

        # Show a Variable Importance plot and model results
        mainPanel(
            plotOutput("ImpPlot"),
            textOutput("accu1"),
            textOutput("kappa1"),
            textOutput("accu2"),
            textOutput("kappa2"),
            fluidRow(column(5,offset= 1,tableOutput("Comp"))),
            h4(textOutput("recomm"))
        )
    )
))
