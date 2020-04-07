#Application was created using Shiny. Run in RStudio using the 'Run App' button.
#Currently only able to load a field map and visualize series in two dimensions.
#Intent is to create a dynamic field report generator.
#
#April 2020.
#Kristof Govaerts, SESVanderhave.

server <- shinyServer(function(input, output, session) {
  # added "session" because updateSelectInput requires it
  adata <- reactiveValues()
  
  observe({ 
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
                             choices = series, selected = series)
    adata$df1 <- df
  })
  
  observe({ 
    req(input$file2)
    inFile2 <- input$file2
    if (tools::file_ext(inFile2$datapath) %in% c('xls', 'xlsx')) {
      df2 <- read_excel(inFile2$datapath)
    } else if (tools::file_ext(inFile2$datapath) %in% c('txt', 'csv')) {
      df2 <- read.table(inFile2$datapath, sep="\t", dec=".", 
                        header=TRUE, stringsAsFactors = FALSE)
    } else {print('Wrong file type.')}
    
    adata$df2 <- df2
  })
  
  observe({
    req(adata$df1)
    series <- levels(as.factor(adata$df1$'Series Id'))
    updateCheckboxGroupInput(session, inputId = 'seriesL',
                             choices = series, selected = series)
  })
  
  observe({
    req(adata$df2)
    df2 <- adata$df2
    times <- levels(as.factor(df2$time))
    pl <- data_columns(df2)
    pars <- pl[[1]]
    types <- pl[[2]]
    
    updateCheckboxGroupInput(session, inputId = 'timesL',
                             choices = times, selected = times)
    updateCheckboxGroupInput(session, inputId = 'parsL',
                             choices = pars, selected = pars)
    updateSelectInput(session, inputId = 'typesL',
                      choices = types, selected = c('mean'))
    adata$times <- times
    adata$pars <- pars
    adata$types <- types
  })
  
  observe({
    req(adata$pars)
    c_pars <- paste(adata$pars, input$typesL, sep='_')
    updateSelectInput(session, inputId = 'ppar', 
                      choices = c_pars, selected=c_pars[1])
    updateSelectInput(session, inputId = 'stime',
                      choices = adata$times, selected=adata$times[1])
  })
  
  observe({
    req(adata$df1, adata$df2)
    adata$ddata <- prepare_data(adata$df1, adata$df2)
  })
  
  output$fragplot <- renderPlot({
    req(adata$df1)
    fm <- adata$df1
    sl <- input$seriesL
    
    plot_fieldmap(fm, sl)
  }, width=1200, height=800)
  
  output$Fdataplot <- renderPlot({
    req(adata$ddata)
    ddata <- adata$ddata
    print(input$ppar)
    print(input$stime)
    print(colnames(ddata))
    
    plot_data_column(ddata, input$ppar, as.numeric(input$stime))
  }, width=400, height=400)
  
  output$Fcheckplot <- renderPlot({
    req(adata$ddata)
    ddata <- subset(adata$ddata, time %in% input$timesL)
    plot_checks(ddata, input$ppar)
  }, width=400, height=400)
})