#Application was created using Shiny. Run in RStudio using the 'Run App' button.
#Currently only able to load a field map and visualize series in two dimensions.
#Intent is to create a dynamic field report generator.
#Output will be saved in the same folder as the second input file
#
#April 2020.
#Kristof Govaerts, SESVanderhave.

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
      fluidRow(column(4,
                      checkboxGroupInput('seriesL', 'Series: ', choices = NULL, selected = NULL),
                      checkboxGroupInput('timesL', 'Timepoints: ', choices = NULL, selected = NULL)
      ),
      column(4, offset=2,
             checkboxGroupInput('parsL', 'Parameters: ', choices = NULL, selected = NULL),
             selectInput('typesL', 'Statistic: ', choices = NULL, selected = NULL)
      )
      ),
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Fieldmap", plotOutput("fragplot")),
        tabPanel("Field data", 
                 selectInput('ppar', 'Parameter to plot:', choices = NULL, selected = NULL),
                 selectInput('stime', 'Timepoint to plot:', choices = NULL, selected = NULL),
                 plotOutput("Fdataplot"),
                 plotOutput("Fcheckplot")),
        tabPanel("Data processing",
                 checkboxInput("GaR", "Genotype as random?", value=TRUE),
                 htmlOutput("spatstext"),
                 tags$br(),
                 actionButton("sspats", "Start SpATS"),
                 tags$br()),
        tabPanel("SPATS output",
                 tags$br(),
                 selectInput('spatspar', "Parameter:", choices = NULL, selected = NULL),
                 selectInput('spatstime', "Timepoint:", choices = NULL, selected = NULL),
                 plotOutput("spatsplots"),
                 tags$br(),
                 textOutput("herit"),
                 tags$br(),
                 downloadButton('downl', label='Save predicted values'))
      )
    )
  )
)
)
