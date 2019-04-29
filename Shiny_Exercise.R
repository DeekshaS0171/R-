library(shiny)
library(shinyWidgets)
if(interactive()){
  
  ui <- fluidPage(
    
    fluidRow(
      column(width = 6,
             h2('The uploaded file data'),
             
             dataTableOutput('mytable1'),
             
             fileInput('file1', 'Choose info-file to upload',
                       accept = c(
                         'text/csv',
                         'text/comma-separated-values',
                         'text/tab-separated-values',
                         'text/plain',
                         '.csv',
                         '.tsv'
                       )
             )
      ),
      column(width = 6,
             h2('The uploaded file data'),
             
             dataTableOutput('mytable2'),
             
             fileInput('file2', 'Choose info-file to upload',
                       accept = c(
                         'text/csv',
                         'text/comma-separated-values',
                         'text/tab-separated-values',
                         'text/plain',
                         '.csv',
                         '.tsv'
                       )
             )
      )
    )
  )
  server <- function(input,output){
    output$contents <- renderTable({
      # input$file1 will be NULL initially. After the user selects
      # and uploads a file, it will be a data frame with 'name',
      # 'size', 'type', and 'datapath' columns. The 'datapath'
      # column will contain the local filenames where the data can
      # be found.
      inFile <- input$file1
      
      if (is.null(inFile))
        return(NULL)
      
      read.csv(inFile$datapath, header = input$header)
    })
  }
  
  shinyApp(ui = ui,server = server)
}
##2222#########################################################
if(interactive()){
  
  ui <- fluidPage( 
    
    dropdownButton(
      label = "Check some boxes", status = "default", width = 450,
      tags$label("Choose :"),
      
    fluidRow(
      fileInput("file1", "Choose CSV File",
                accept = c(
                  "text/csv",
                  "text/comma-separated-values,text/plain",
                  ".csv")
      )
      #tags$hr(),
      #checkboxInput("header", "Header", TRUE)
    )
  ),
    fluidRow(
      fileInput("file1", "Choose CSV File",
                accept = c(
                  "text/csv",
                  "text/comma-separated-values,text/plain",
                  ".csv")
       #tags$hr(),
       #checkboxInput("header", "Header", TRUE)
      ),
      
    
    mainPanel(
      tableOutput("contents")
    
    )
  
  )
)
  
  server <- function(input,output){
    output$contents <- renderTable({
      # input$file1 will be NULL initially. After the user selects
      # and uploads a file, it will be a data frame with 'name',
      # 'size', 'type', and 'datapath' columns. The 'datapath'
      # column will contain the local filenames where the data can
      # be found.
      inFile <- input$file1
      
      if (is.null(inFile))
        return(NULL)
      
      read.csv(inFile$datapath, header = input$header)
    })
  }
  
  shinyApp(ui = ui,server = server)
}
##3333#################################################
ui <- fluidPage(
  tags$h1("Example dropdown button"),
  br(),
  fluidRow(
    column(
      width = 6,
      dropdownButton(
        label = "Check some boxes", status = "default", width = 80,
        checkboxGroupInput(inputId = "check1", label = "Choose", choices = paste(1:26, ") Choice ", LETTERS))
      ),
      verbatimTextOutput(outputId = "res1")
    ),
    column(
      width = 6,
      dropdownButton(
        label = "Check some boxes", status = "default", width = 80,
        actionButton(inputId = "a2z", label = "Sort A to Z", icon = icon("sort-alpha-asc")),
        actionButton(inputId = "z2a", label = "Sort Z to A", icon = icon("sort-alpha-desc")),
        br(),
        actionButton(inputId = "all", label = "(Un)select all"),
        checkboxGroupInput(inputId = "check2", label = "Choose", choices = paste(1:26, ") Choice ", LETTERS))
      ),
      verbatimTextOutput(outputId = "res2")
    )
  )
)
server <- function(input, output, session) {
  output$res1 <- renderPrint({
    input$check1
  })
  
  # Sorting asc
  observeEvent(input$a2z, {
    updateCheckboxGroupInput(
      session = session, inputId = "check2", choices = paste(1:26, ") Choice ", LETTERS), selected = input$check2
    )
  })
  # Sorting desc
  observeEvent(input$z2a, {
    updateCheckboxGroupInput(
      session = session, inputId = "check2", choices = paste(26:1, ") Choice ", rev(LETTERS)), selected = input$check2
    )
  })
  output$res2 <- renderPrint({
    input$check2
  })
  # Select all / Unselect all
  observeEvent(input$all, {
    if (is.null(input$check2)) {
      updateCheckboxGroupInput(
        session = session, inputId = "check2", selected = paste(1:26, ") Choice ", LETTERS)
      )
    } else {
      updateCheckboxGroupInput(
        session = session, inputId = "check2", selected = ""
      )
    }
  })
}
shinyApp(ui = ui, server = server)