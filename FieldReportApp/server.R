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
      df <- read.table(inFile$datapath, sep="\t", dec=".", 
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
    dims <-  c(max(adata$ddata$X) - min(adata$ddata$X), max(adata$ddata$Y) - min(adata$ddata$Y))
    adata$dims <- dims
    adata$segs <- sapply(dims, function(x) if (x<9) {3} else if (x>45) {15} else {round(x/3)})
  })
  
  observe({
    req(adata$spats)
    pl <- paste(input$parsL, input$typesL, sep='_')
    updateSelectInput(session, inputId = 'spatspar',
                      choices = pl, selected=pl[1])
    updateSelectInput(session, inputId = 'spatstime',
                      choices = input$timesL, selected=input$timesL[1])
  })
  
  output$fragplot <- renderPlot({
    req(adata$df1)
    fm <- adata$df1
    sl <- input$seriesL
    
    plot_fieldmap(fm, sl)
  }, width=1200, height=800)
  
  output$spatstext <- renderText({
    s0 <- paste("Number of unique seednames:", length(levels(adata$ddata$Seed)))
    s1 <- paste("Timepoints selected:", list(input$timesL))
    s2 <- paste("Parameters selected:", list(input$parsL))
    s3 <- paste("Field dimensions:", adata$dims[1], "x", adata$dims[2])
    s4 <- paste("SpATS segments:", list(adata$segs))
    s5 <- paste("Total SpATS analyses:", length(input$timesL) * length(input$parsL))
    HTML(paste(s0, s1,s2,s3,s4,s5, sep = '<br/>'))
  })
  
  output$Fdataplot <- renderPlot({
    req(adata$ddata)
    ddata <- adata$ddata

    plot_data_column(ddata, input$ppar, as.numeric(input$stime))
  }, width=400, height=400)
  
  output$Fcheckplot <- renderPlot({
    req(adata$ddata)
    ddata <- subset(adata$ddata, (time %in% input$timesL) & (series %in% as.numeric(input$seriesL)))
    plot_checks(ddata, input$ppar)
  }, width=400, height=400)
  
  observeEvent(input$revX, {
    req(adata$df2)
    adata$df2$X <- reverse(adata$df2$X)
  })
  
  observeEvent(input$revY, {
    req(adata$ddata)
    adata$df2$Y <- reverse(adata$df2$Y)
  })
  
  observeEvent(input$sspats, {
    req(adata$ddata)
    pl <- paste(input$parsL, input$typesL, sep='_')
    print("Running SpATS")
    print(input$timesL)
    print(pl)
    print(input$seriesL)
    print(input$GaR)
    if (length(input$seriesL) != length(levels(as.factor(adata$ddata$series)))) {
      ss <- subset(adata$ddata, series %in% as.numeric(input$seriesL))} else {
        ss <- data.frame(adata$ddata)}
    
    if (input$perSeries == "Yes") {
      print("Combining series and seedname for SpATS.")
      ss$Seed <- paste(ss$Seed, ss$Series, sep='_')}
    
    adata$spats <- spats_all(ss, input$timesL, pl, gar = input$GaR, nseg = adata$segs)
  })
  
  output$spatsplots <- renderPlot({
    req(adata$spats)
    plot(adata$spats[[input$spatspar]][[input$spatstime]])
  })
  
  output$herit <- renderText({
    if (input$GaR == TRUE) {
      paste("Heritability:", getHeritability(adata$spats[[input$spatspar]][[input$spatstime]]))
    } else {paste("Can't calculate heritability if genotypes not included as random effects.")}
  })
  
  outputfile <- reactive({
    req(adata$spats)
    spatslist <- adata$spats
    if (input$perSeries == "Yes") {
      spats_raw <- consolidate_spatslist(spatslist, bySeries=TRUE)
    } else{
      spats_raw <- consolidate_spatslist(spatslist)
    }
    spats_temp <- to_long(spats_raw)
    spats_temp <- cbind(spats_temp, rescale_pars(spats_temp[,4:length(colnames(spats_temp))]))
    spats_temp <- spats_temp[with(spats_temp, order(seedname, time)),]
    spats_auc <- to_aucs(spats_raw)
    spats_auc <- cbind(spats_auc, rescale_pars(spats_auc[,2:length(colnames(spats_auc))]))
    ofn <- paste(dirname(input$file1$datapath), 'SPATS_output.xlsx', sep='/')
    print("Saving to:")
    print(ofn)
    olist <- list("Raw" = spats_raw, "Preds" = spats_temp, "AUDPC" = spats_auc)
    return(olist)
  })
  
  output$downl <- downloadHandler(
    filename = 'SPATS_output.xlsx',
    content = function(file) {
      olist <- outputfile()
      write.xlsx(olist, file)
    })
})
