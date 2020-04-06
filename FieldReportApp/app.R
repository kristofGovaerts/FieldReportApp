#Application was created using Shiny. Run in RStudio using the 'Run App' button.
#Currently only able to load a field map and visualize series in two dimensions.
#Intent is to create a dynamic field report generator.
#
#April 2020.
#Kristof Govaerts, SESVanderhave.

library(shiny)
library(readxl)

options(shiny.maxRequestSize = 30*1024^2) #upload size = 30MB
source('visualization_functions.R')

ui <- shinyUI(fluidPage(
  tags$style(type="text/css",
             ".shiny-output-error { visibility: hidden; }",
             ".shiny-output-error:before { visibility: hidden; }"
  ), #suppress warning messages
  titlePanel("Field report"),
             sidebarLayout(
               sidebarPanel(
                 fileInput('file1', 'Field ebook',
                           accept=c('.xlsx', '.xls', '.csv', '.txt')),
                 fileInput('file2', 'Plot data',
                           accept=c('.xlsx', '.xls', '.csv', '.txt')),
                 tags$br(),
                 checkboxGroupInput('seriesL', 'Series: ', choices = NULL, selected = NULL, inline=TRUE),
                 tags$br(),
                 checkboxGroupInput('parsL', 'Parameters: ', choices = NULL, selected = NULL, inline=TRUE)
               ),
               mainPanel(
                 tabsetPanel(
                   tabPanel("Fieldmap", plotOutput("fragplot"))
                 )
                 )
               )
   )
  )

server <- shinyServer(function(input, output, session) {
  # added "session" because updateSelectInput requires it
  
  data <- reactive({ 
    req(input$file1) ## ?req #  require that the input is available
    
    inFile <- input$file1 
    
    if (tools::file_ext(inFile$datapath) %in% c('xls', 'xlsx')) {
      df <- read_excel(inFile$datapath)
    } else if (tools::file_ext(inFile$datapath) %in% c('txt', 'csv')) {
        df <- read.table(inFile$datapath, sep="", dec=".", 
                         header=TRUE, stringsAsFactors = FALSE)
      } else {print('Wrong file type.')}

    series <- levels(as.factor(df$'Series Id'))
    updateCheckboxGroupInput(session, inputId = 'seriesL',
                             choices = series, selected = series, inline=TRUE)
    
    return(df)
  })
  
   output$fragplot <- renderPlot({
     fm <- data()
     sl <- input$seriesL
     
     plot_fieldmap(fm, sl)
  }, width=1200, height=800)
   })

shinyApp(ui, server)