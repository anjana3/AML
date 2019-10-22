
library(shiny)
library(DataExplorer)
library(UpSetR)
library(e1071)
library(tensorflow)
library(mice)
library(purrr)
library(keras)
library(caret)
library(rpart.plot)
library(ggplot2)

library(shinydashboard)
library(shinythemes)
library(ggplot2)
library(shinyjs)
library(DT)
library(dplyr)
library(data.table)
library(rsample)
library(shinyalert)
library(corrplot)
library(shinyWidgets)
library(shinyFeedback)

source("Details.R")
source("Data_Exploration.R")
source("Data_Prediction.R")
source("Data_Preparation.R")
source("Data_Visualization.R")
source("Model_Building.R")
source("Model_Evalution.R")
header <- dashboardHeaderPlus(title = "AML", disable = FALSE)

sidebar <- dashboardSidebar(width = 240,
                            tags$head(tags$style(HTML('.shiny-server-account {display: none; }'))),
                            
                            sidebarMenu(
                              id = "tabs",
                               menuItem("Details",
                                        tabName = "Detailstab",
                                        # selected = TRUE,
                                        icon = icon("blackboard",lib = "glyphicon")
                               ),
                              menuItem("Data Preparation", 
                                       tabName = "Data_Preparationtab",
                                       icon = icon("wrench",lib = "glyphicon")
                              ),
                              menuItem("Data Visualization", 
                                       tabName = "Data_Visualizationtab",
                                       icon = icon("picture",lib = "glyphicon")
                              ),
                              menuItem("Data Exploration", 
                                       tabName = "Data_Explorationtab",
                                       icon = icon("picture",lib = "glyphicon")             
                              ),
                              menuItem("Model Building", 
                                       tabName = "Model_Buildingtab",
                                       icon = icon("cogs")           
                              ),
                              menuItem("Model Evalution", 
                                       tabName = "Model_Evalutiontab",
                                       icon = icon("ok-sign",lib = "glyphicon")
                              ),
                              menuItem("Data Prediction", 
                                       tabName = "Data_Predictiontab",
                                       icon = icon("tasks",lib="glyphicon")   
                              )
                              
                            )
)

body <- dashboardBody(useShinyjs(),
                      useShinyFeedback(),
                      useShinyalert(),
                      tags$head(
                        # Custom shiny to javascript binding
                        # scrolls "outDiv" to bottom once called
                        tags$script(
                          '
                          Shiny.addCustomMessageHandler("scrollCallback",
                          function(color) {
                          var objDiv = document.getElementById("summary_main_id");
                          objDiv.scrollIntoView();
                          }
                          );'
                        )
                        ),
                      tags$head(
                        # Custom shiny to javascript binding
                        # scrolls "outDiv" to bottom once called
                        tags$script(
                          '
                          Shiny.addCustomMessageHandler("scrollCallback",
                          function(color) {
                          var objDiv = document.getElementById("summary_main_id");
                          objDiv.scrollIntoView();
                          }
                          );'
                        )
                        ),
                      
                      tags$head(tags$style(HTML('
                                                .skin-blue .main-header .logo {
                                                background-color: #3c8dbc;
                                                },
                                                .skin-blue .main-header .logo:hover {
                                                background-color: #3c8dbc;
                                                }'
                                               )
                      )
                      ),
                      tabItems(
                        tabItem(tabName = "Detailstab",
                                 DetailsUI("Detailsserver")
                         ),
                        tabItem(tabName = "Data_Preparationtab",
                                Data_PreparationUI("DataPreparationserver")
                        ),
                        tabItem(tabName = "Data_Visualizationtab",
                                Data_VisualizationUI("DataVisualizationserver") 
                                                      
                        ),
                        tabItem(tabName = "Data_Exploration",
                                Data_ExplorationUI("DataExplorationserver")
                        ),
                        tabItem(tabName = "Model_Building",
                                Model_BuildingUI("ModelBuildingserver")
                        ),
                        tabItem(tabName = "Model_Evalution",
                                Model_EvalutionUI("ModelEvalutionserver")
                        ),
                        tabItem(tabName = "Data_Prediction",
                                Data_PredictionUI("DataPredictionserver")
                        )
                      ))
ui <- fluidPage( useShinyFeedback(),
                 includeCSS("www/main.css"),
                 
                 dashboardPage(
                   header, 
                   sidebar, 
                   body)
)

server <- function(input, output, session) {
  callModule(Detailsserver, "Detailsserver")
  callModule(Data_Preparationserver, "DataPreparationserver")
  callModule(Data_Visualizationserver, "DataVisualizationserver")  
  callModule(Data_Explorationserver, "DataExplorationserver")
  callModule(Model_Buildingserver, "ModelBuildingserver")
  callModule(Model_Evalutionserver, "ModelEvalutionserver")
  callModule(Data_Predictionserver, "DataPredictionserver")


}

shinyApp(ui, server)
