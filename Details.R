library(DT)
library(shinyWidgets)
library(lubridate)
library(shinycustomloader)
library(stringr)
library(openxlsx)



DetailsUI <- function(id) {
  
  ns <- NS(id)
  fluidPage(
    box(title = "Details of the application",status = "primary",solidHeader = TRUE,collapsible = TRUE,collapsed = TRUE ,width = 8,
        h4("This application is designed to build a model to predict the target value (or) target class. By taking the user input csv and algorithm to build a model."),
        p(h4("it works only for numerical data "))
    ),
    
    box(title = "How To Use", status = "primary", solidHeader = TRUE,
        collapsible = TRUE,collapsed = TRUE  ,width = 8,
        p(h3("DATA PREPARATION")),
        tags$hr(),
        h4("Step 1: Upload Dataset"),
        h5("Ideally any csv file is useable.  It is recommended to perform cleaning and munging methods prior to the upload though. We intend to apply data munging/cleaning methods in this app in the near future."),
        h4("Step 2: Select  Feature variables in the select feature variables"),
        h5("you can see those selected variables summary in the file tab under summary of the features."),
        h4("Step 3: Select  one target variable which has to predict from select target variable tab"),
        h5("The target variable tab contains the values of the variable with complete summary of the target variable. "),
        h4("Step 4: Train the  Model"),
        h5("Choose algorithm to train the model after selecting the features and target variables .")
    ),
    tags$hr(),
    box(title = "Train the model",status = "primary", solidHeader = TRUE,
        collapsible = TRUE, collapsed = TRUE ,width = 8,
        tags$hr(),
        h4("Step 1: Train the model to predict the target value."),
        h4("Step 2: Click on the model which is in the colour green."),
        tags$hr(),
        h4(" Choose algorithm based on your targets "),
        h4("Example:Regression : Target variable"), 
        h4("Classification : Target Classes")
    ),
    tags$hr(),
    box(title="Prediction",status = "primary", solidHeader = TRUE,
        collapsible = TRUE,collapsed = TRUE,width = 8,
        tags$hr(),
        h4("step 1: Click on MSE  if the model run for regression probelm."),
        h4("Stpe 2: Click on Prediction values to the prediction values for new data.")))
}
print("done before server")
Detailsserver <- function(input, output, session) {
  ns <- session$ns
  print("entered into server of details")
  
  values <- reactiveValues()
  plot   <- reactiveValues()
}
