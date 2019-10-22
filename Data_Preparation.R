Data_PreparationUI <- function(id) {
  ns <- NS(id)
              fluidPage(
                sidebarLayout(position = "left",
                              sidebarPanel(
                                p(h4("Please upload a CSV formatted file.")),
                                fileInput(ns("uploaded_file"), "Choose CSV File",
                                          multiple = TRUE,
                                          accept = c("text/csv",
                                                     "text/comma-separated-values,text/plain",
                                                     ".csv")),
                                tags$hr(),
                                checkboxInput(ns('header'), 'Header', TRUE),
                                
                                radioButtons(ns("sep"), "Separator",
                                             choices = c(Semicolon = ";",
                                                         Comma = ",",
                                                         Tab = "\t"),
                                             selected = ","),
                                actionButton(ns('btn_viewData'),label = 'View Data',style="color: #fff; background-color: teal;border-color: #2e6da4")
                                
                              ),
                              mainPanel(
                                
                                uiOutput("checkbos")
                                
                              )    
                ),
                column( width =12,
                        sidebarLayout(
                          sidebarPanel(
                            prettyCheckboxGroup(inputId = ns("clean"),
                                                label = "Data Cleaning", br(),icon = icon("check"),
                                                choices = c("Missing Values" ="miss", "NULL Values"="null"),
                                                animation = "tada", status = "success")),
                          mainPanel(
                            uiOutput(ns("check"))
                          )
                        )
                )
              )
      

}

Data_Preparationserver <- function(input, output, session) {
  
  
  ns <- session$ns
  
  values <- reactiveValues()
  plot   <- reactiveValues()

df <- reactive({
    req(input$uploaded_file)
    read.csv(input$uploaded_file$datapath,
             header = input$header,
             sep = input$sep)})
  
  
  observe({
    observeEvent(input$btn_viewData,{
      showModal(modalDialog(
        title = "view of the data",
        
        output$btn_viewData<-renderDataTable({
          df()
        })
      ))
    })})
  output$summary<-renderDataTable({
    df()})  
  output$missingval<-renderDataTable({
    filter_all(df(),any_vars(is.na(.)))
  })
  
  output$mis<-renderPrint({
    sapply(df(), function(x) sum(is.na(x)))  
  })
  
  output$check<-renderTable({
    clean<-switch(input$clean,
                  miss = showModal(modalDialog(title = "Result page for the missing values in the data frame","Total number of missing values in a data frame is:",tags$hr(),sum(is.na.data.frame(df())),
                                               tags$hr(),
                                               fluidPage(
                                                 tabsetPanel(
                                                   tabPanel(h4("Missing Variables"),
                                                            dataTableOutput("missingval"),
                                                            verbatimTextOutput("mis"))
                                                   
                                                 )
                                               ),
                                               tags$hr(),
                                               HTML("Do you want to replace the missing values ? "),
                                               actionButton("BUTyes", "Yes"),
                                               actionButton("BUTno", "No")
                  )),
                  null =sum(is.na(df())))})
  
  observeEvent(input$BUTyes, {
    showModal(modalDialog(title ="Choose methods to replace the missing values according to the column type",
                          checkboxGroupInput(inputId = "select_var", 
                                             label = "Select Feature variables", 
                                             choices = names(df()),
                                             selected = names(df())),tableOutput("select")))
  })
  
  output$select<-renderDataTable({
    var<-switch(input$var,
                median = showModal(modalDialog(title = "Replaced missing values with median value of the data frame")),
                mean=showModal(modalDialog(title = "Missing values replaced with mean of the data frame")),
                
                dynamic = showModal(modalDialog(title = "Place your dynamic value")),
                delete=showModal(modalDialog(title = "Removing the values")))
  })
  
  observeEvent(input$BUTno, {
    showModal(modalDialog("you clicked no! means you dont want replace missing values"))
  })  
  
}
