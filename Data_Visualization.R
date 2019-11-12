library(readr)
library(tidyverse)
library(stringr)
library(DT)
library(shinyWidgets)
library(lubridate)
library(shinycustomloader)
library(stringr)
library(openxlsx)
library(shinydashboard)
library(shinydashboardPlus)

Data_VisualizationUI <- function(id) {
  ns <- NS(id)
  
  fluidPage(
    fluidRow(
    box(
      width = 12,
      class = "dataloading_class",
      title = "Data Loading",
      solidHeader = TRUE,
      status = "warning",
      column(
        3,
        tags$b("Please upload a data set for visualization"),
        tags$br(),
        
        fileInput(
          ns("uploaded_file"),
          "Choose CSV File",
          multiple = TRUE,
          accept = c(
            "text/csv",
            "text/comma-separated-values,text/plain",
            ".csv"
          )
        )
        
      ),
      column(
        width = 9,
        tags$div(class = "info_class",
                 infoBoxOutput(ns(
                   "info_details_dataset"
                 ))),
        actionButton(
          ns("View_dataset_visual"),
          label = "View Dataset",
          class = "view_button_class"
        )
      )
    )
  ),
  sidebarLayout(
    sidebarPanel(width = 3,
      box(width=15,title = "Ggplot2 and Highcharts",solidHeader = T,status = 'primary',
          
      radioButtons(ns("typeplot_radio"),label = "Please Select Type:",inline=T,choices = c("ggplot2","highchart"),selected = character(0))),

      uiOutput(ns("typeplot_text")),
      tags$hr(),
      uiOutput(ns("drop_xvariable")),
      uiOutput(ns("drop_yvariable"))
    ),
    mainPanel(
      fluidPage(
      navbarPage("Choose graph",
                 
                 navbarMenu(tags$b("LINE CHARTS"), 
                            tabPanel(tags$b("Basic Line"), 
                                     plotOutput(ns("Basicline_plot_output"))
                            ), 
                            tabPanel(tags$b("Time Series"), plotOutput(ns("Timeseries_plot_output")))
                 ),
                 
                 navbarMenu(tags$b("BARCHARTS"), 
                            tabPanel(tags$b("Basic Bar"), plotOutput(ns("basicbar_plot_ouput"))), 
                            tabPanel(tags$b("Stacked Bar"), plotOutput(ns("stackedbar_plot_output")))),
                 id = "navbar"
      )
    )
  )
  ))
}


Data_Visualizationserver <- function(input, output, session) {
  ns <- session$ns
  
  values <- reactiveValues()
  plot   <- reactiveValues()
  observe(
  print(is.null(input$Basicline_plot_output)))
  
  uploaded_Data <- reactive({
    file_to_read <- input$uploaded_file
    if (is.null(file_to_read))
      return()
    data_frame <-
      read.csv(file_to_read$datapath,
               sep = ",",
               stringsAsFactors = FALSE)
    data_frame
  })
  
  
  
  observeEvent(input$View_dataset_visual, {
    showModal(
      modalDialog(
        title = "Data View",
        
        DT::dataTableOutput("view_output"),
        style = "height:500px; overflow-y: scroll;overflow-x: scroll;",
        
        output$view_output <- renderDataTable({
          datatable(uploaded_Data(),
                    escape = FALSE,
                    class = "cell-border",
                    selection = 'single',
                    options = list(
                                   
                                   aLengthMenu = list(c(5, 10, 20, 100,-1), list('5', '10', '20', '100', 'All')),
                                   iDisplayLength = 5
                                   
                    )
          ) 
          
        })
      )
    )
  })
  
  
  output$info_details_dataset <- renderInfoBox({
    uploaded <- uploaded_Data()
    if (is_empty(uploaded)) {
      details_dataset <- paste("Please choose a dataset")
      
    } else{
      rows <- nrow(uploaded)
      columns <- ncol(uploaded)
      total <- rows * columns
      
      details_dataset <-
        paste("Total Observations: ",
              total,
              "No of rows: ",
              rows,
              "     ",
              "No of Columns:",
              columns)
      
      
    }
    infoBox(
      width = 6,
      "Details of Dataset",
      details_dataset,
      icon = icon("list"),
      color = "purple"
    )
  })
  
  # Render ouput for the radio button selected text
  
  output$typeplot_text<-renderUI({
    values$radio_type_plot <- input$typeplot_radio
    if(is_empty(values$radio_type_plot)){
    }
    
      else if (values$radio_type_plot == "highchart" ) 
      {
      statement <-paste0(values$radio_type_plot,"package enables the creation of Highcharts type plots within R.")      } 
      else  
          statement<-paste0(values$radio_type_plot," is based on the grammar of graphics, the
      idea that you can build every graph from the same
                            few components: a data set, a set of geoms-visual
                            marks that represent data points, and a coordinate
                            system.")
        
  })  # End of the render function
  
  output$graph_type<-renderUI({
    selectInput(ns("select_type_graph"),label = "Select type of Graph",choices = c("","",""))
  })
  # render output for the xvarible in sidebar for the plot
  
  output$drop_xvariable <-renderUI({
    
    if(is.null(uploaded_Data())){
      print("executing choose a dataseet2")
      selectInput(ns("dataset_xcolumn"),label = "Select Variable(X):",choices = c("Choose a Dataset"))
    }else{
      print("3")
        selectInput(ns("dataset_xcolumn"),label = "Select Variable(X):",choices = names(uploaded_Data()))
   print(input$dataset_xcolumn)
        print("checking for condition")
         }
  })  # End of the render output
  
  # render output for the y varible in sidebar for the plot
  
  output$drop_yvariable <-renderUI({
    
    if(is_empty(uploaded_Data())){
      selectInput(ns("dataset_ycolumn"),label = "Select Variable(Y):",choices = c("Choose a Dataset"))
    }else{
      selectInput(ns("dataset_ycolumn"),label = "Select Variable(Y):",choices = names(uploaded_Data()))
    }
  })

  observe({
  if(is.null(uploaded_Data())){
    Example_df<-mtcars
    values$dataset_inputgraph <- data.frame(x = Example_df$hp,y = Example_df$carb)
    
      }else{
    
    dataset_xcolumn_input <- uploaded_Data() %>% select(input$dataset_xcolumn)
    print("checking for 2")
    dataset_ycolumn_input <- uploaded_Data() %>% select(input$dataset_ycolumn)
    print("executed y column")
    values$dataset_inputgraph <- data.frame(x = dataset_xcolumn_input,y = dataset_ycolumn_input)
    
    }
    })
  # 
  # dataset_xcolumn_input <- reactive({
  # 
  #   req(input$dataset_xcolumn)
  # 
  #   print(uploaded_Data())
  # 
  #   dataset_xcolumn_input <- uploaded_Data() %>% select(input$dataset_xcolumn)
  # 
  # })
  # 
  # dataset_ycolumn_input <- reactive({
  # 
  #   req(input$dataset_ycolumn)
  # 
  #   dataset_ycolumn_input <- uploaded_Data() %>% select(input$dataset_ycolumn)
  # 
  # })
  # 
  # observe({


  output$Basicline_plot_output<-renderPlot({

          ggplot(data = values$dataset_inputgraph, aes(x = values$dataset_inputgraph[,1], y = values$dataset_inputgraph[,2])) +
            geom_line()
  })
  #})

 output$Timeseries_plot_output<-renderPlot({
   
 })
  
}  # End of the server
