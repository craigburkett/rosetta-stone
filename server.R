# options(warn = -1) # For suppressing plotly warnings
library(shiny)
library(shinydashboard)

# TODO: Features
## Modeling: Add options to imputation step
## Modeling: Add options to Normalization step other than range
## Modeling: Feature importance plot with caret using featurePlot()
## Recursive feature elimination using rfeControl() and rfe()
# https://topepo.github.io/caret/recursive-feature-elimination.html
# https://www.machinelearningplus.com/machine-learning/caret-package/

server <- function(input, output, session){
  rv = set_reactive_values()
  
  ### ----------------- ###
  ### Reactive elements ###
  ### ----------------- ###
  
  rawDataSet = reactive({
    req(c(input$dataSource))
    
    readRDS(paste0(input$dataSource, ".rds")) %>% mutate_all(list(~binarize_levels(.)))
  })

  ## Selected data set (with non-desired columns removed)
  selectData = reactive({
    
    # Remove columns with too many missing values
    minPercent = input$predCompLimit / 100
    indCols2keep = colMeans(is.na(rawDataSet())) <= (1-minPercent)
    nameList = names(rawDataSet())[indCols2keep]
    
    dateVars = get_dates(sapply(sapply(rawDataSet(), class), "[[", 1)) # Try to guess which vars are dates
    
    if(input$iRemDates)  nameList = setdiff(nameList, dateVars) # Remove dates if asked
    if(input$iRemIDs)    nameList = remove_IDs(nameList, idNamesNotPickedUp) # Remove IDs if asked
    # if(input$iRemFuture) nameList = remove_manual(nameList) # Remove future vars if asked
    
    rawDataSet() %>% select(all_of(nameList))
  })
  
  ## Figure out which variables are dates, factors, numeric, and binary
  varClasses = reactive({    sapply(sapply(selectData(), class), "[[", 1)    })
  
  dateList   = reactive({    names(varClasses())[varClasses() %in% c("Date", "POSIXct")]    })
  factorList = reactive({    names(varClasses())[varClasses() %in% c("factor", "character")]    })
  numberList = reactive({    names(varClasses())[varClasses() %in% c("numeric", "integer", "difftime")]    })
  binaryList = reactive({    names(varClasses())[varClasses() %in% c("logical")]    })
  
  ## Filtered data set
  filterData = reactive({
    req(input$filterDataVar1, input$filterDataSample)
    
    selectData() %>%
      ungroup() %>%
      
      {if(input$filterDataVar1 %in% factorList()) filter(., UQ(sym(input$filterDataVar1)) %in% input$filterDataLevel1) else .} %>%
      {if(input$filterDataVar1 %in% numberList() & !is.null(input$filterDataMin1)) filter(., UQ(sym(input$filterDataVar1)) >= input$filterDataMin1) else .} %>%
      {if(input$filterDataVar1 %in% numberList() & !is.null(input$filterDataMax1)) filter(., UQ(sym(input$filterDataVar1)) <= input$filterDataMax1) else .} %>%
      
      {if(input$filterDataVar2 %in% factorList()) filter(., UQ(sym(input$filterDataVar2)) %in% input$filterDataLevel2) else .} %>%
      {if(input$filterDataVar2 %in% numberList() & !is.null(input$filterDataMin2)) filter(., UQ(sym(input$filterDataVar2)) >= input$filterDataMin2) else .} %>%
      {if(input$filterDataVar2 %in% numberList() & !is.null(input$filterDataMax2)) filter(., UQ(sym(input$filterDataVar2)) <= input$filterDataMax2) else .} %>%
      
      {if(input$filterDataVar3 %in% factorList()) filter(., UQ(sym(input$filterDataVar3)) %in% input$filterDataLevel3) else .} %>%
      {if(input$filterDataVar3 %in% numberList() & !is.null(input$filterDataMin3)) filter(., UQ(sym(input$filterDataVar3)) >= input$filterDataMin3) else .} %>%
      {if(input$filterDataVar3 %in% numberList() & !is.null(input$filterDataMax3)) filter(., UQ(sym(input$filterDataVar3)) <= input$filterDataMax3) else .} %>%
      
      {if(input$filterDataSample != "All") sample_n(., min(as.numeric(input$filterDataSample), nrow(.))) else .}
  })
  
  
  ## Dataset used specifically for modeling - ie. not for exploration graphs and statistics
  modelData = reactive({
    req(input$tabs == "Classification" && !is.null(input$predictorsClass) || input$tabs == "Regression" && !is.null(input$predictorsReg))
    
    filterData() %>%
      {if(input$tabs == "Classification") select(., all_of(c(input$responseClass, input$predictorsClass))) else .} %>%
      {if(input$tabs == "Regression")     select(., all_of(c(input$responseReg,   input$predictorsReg)))   else .} %>% 
      drop_na() %>% # TODO: Remove after imputation feature made
      mutate_if(is.character, as.factor) %>%
      mutate_if(is.logical, as.integer) %>%
      # Convert to factor if doing classification - needed for caret
      {if(input$tabs == "Classification") mutate_at(., input$responseClass, function(x){factor(paste0("Class_", x))}) else .} %>% 
      # droplevels() %>%
      mutate_if(is.factor, collapse_levels, input$numLevels)
  })
  
  
  ## Summary descriptive stats
  summaryStats = reactive({
    req(input$tabs == "Bivariate", input$multivarX %in% names(filterData()), input$multivarY %in% names(filterData()))
    
    filterData() %>% select(all_of(c(input$multivarX, input$multivarY))) %>% get_summary_stats()
  })
    
  nTotal = reactive(  nrow(filterData())   ) # Printed text

  processedData = reactive({
    
    ## Training and Testing data
    req(modelData(), input$tabs %in% c("Classification", "Regression"))
    
    yVarStr = if(input$tabs == "Classification") input$responseClass else if(input$tabs == "Regression") input$responseReg else NULL
    
    if(nrow(modelData()) < 2) return(NULL)
    
    trainRowNumbers = createDataPartition(modelData()[[yVarStr]], p=1-input$testPerc/100, list=FALSE)
    
    trainData = modelData() %>% slice(trainRowNumbers)
    trainX <- trainData %>% select(-all_of(yVarStr))
    trainY <- trainData %>% select( all_of(yVarStr))
    
    ## Imputation Step
    if(input$iImpute){
      preProcess_missingdata_model <- preProcess(trainData, method='knnImpute')
      trainData = predict(preProcess_missingdata_model, trainData)
    } 

    ## One-Hot Encoding Step
    if(input$tabs == "Classification") form = reformulate(input$predictorsClass)
    if(input$tabs == "Regression")     form = reformulate(input$predictorsReg)
    dummies_model <- dummyVars(form, data=trainData)
    trainData = predict(dummies_model, trainData) %>% data.frame()
    
    ## Pre-processing (scaling) step
    if(input$iScaling){
      preProcess_range_model <- preProcess(trainData, method='range')
      trainData = predict(preProcess_range_model, trainData)
    } 
    
    ## Put Y back in
    trainData = trainData %>% bind_cols(trainY)
    
    ## Pre-process test data in the same way
    testY = modelData() %>% slice(-trainRowNumbers) %>% select(all_of(yVarStr))
    
    testData <- modelData() %>% 
      slice(-trainRowNumbers) %>%
      {if(input$iImpute) predict(preProcess_missingdata_model, .) else .} %>%
      predict(dummies_model, .) %>%
      data.frame() %>%
      {if(input$iScaling) predict(preProcess_range_model, .) else .} %>%
      bind_cols(testY)
    
    list(trainData = trainData, testData = testData)
  })
  
  trainData = reactive({    processedData()$trainData  })
  testData  = reactive({    processedData()$testData   })
  
  fracRetained = reactive({   (nrow(trainData()) + nrow(testData())) / nTotal()   }) # Used in gauge
  nRetained    = reactive({   (nrow(trainData()) + nrow(testData()))   })
  
  
  ## Action buttons
  buttonTimesReg   = reactive(input$noPredsReg + input$allPredsReg + input$luckyPredsReg)
  buttonTimesClass = reactive(input$noPredsClass + input$allPredsClass + input$luckyPredsClass)
  buttonTimesPreds = reactive(input$noPredsPreds + input$allPredsPreds)
  
  ## Classification Model
  observeEvent(input$fitClassModel, {
    
    # Check for a response variable
    if(input$responseClass == ""){
      showNotification("Choose a binary response variable.", duration = 5, type = "error")
      return()
    } 
    
    # Check for any predictors
    if(length(input$predictorsClass) < 1){
      showNotification("You need to select some predictors.", duration = 5, type = "error")
      return()
    } 
    
    # Check for data in dataset
    if(nrow(trainData()) < 2){
      showNotification("No data in dataset. Perhaps you have filtered it all out?", duration = 5, type = "error")
      return()
    } 
    
    if(is.null(trainData())) return(NULL)
    
    withProgress(message = 'Modeling: ', value = 0, {
      setProgress(0.1, detail = 'Training Model')
    
      ## Get caret-internal model name from lookup table
      thisMethod = modelLookup$modelNameCaret[modelLookup$modelNameUI == input$classModelType]
      
      rv$caretModel = train(
        form = reformulate(names(trainData()), response = input$responseClass),
        data = trainData(),
        trControl = trainControl(method = "repeatedcv", number = 3, repeats = 2), # TODO: Input
        method = thisMethod[1]
      )
        
      setProgress(0.7, detail = 'Generating Predictions') 
      rv$actualsClass = testData() %>% pull(input$responseClass) # Actual response values
      rv$yesClass = if(is.logical(rv$actualsClass)) "Class_TRUE" else "Class_1" # What is the 'ON' level of binary variable?
      
      rv$probs = predict(rv$caretModel, newdata = testData(), type="prob")[[rv$yesClass]] # Predictions as class probability
      
      rv$rocPred = prediction(as.numeric(rv$probs), as.numeric(rv$actualsClass))
      # setProgress(0.8, detail = 'Updating graphics')

      setProgress(0.9, detail = 'Calculating Fit Stats')
      
      rv$cutoff = get_optimal_cutoff(rv$rocPred, input$fAlpha)
    }) # END progress
    
    # if(!rv$fitClass$converged) showNotification("GLM algorithm did not converge.", duration = 5, type = "warning") # TODO: Test this, have never seen it pop up
    # TODO: How to generalize specific model arguments?
  })
  
  ## Update cutoff if alpha changes
  observeEvent(input$fAlpha, {
    rv$cutoff = if(!is.null(rv$rocPred)) get_optimal_cutoff(rv$rocPred, input$fAlpha) else 0.5
  })
  
  # Model classification stats
  classStats = reactive({
    req(input$thresholdClass, input$tabs == "Classification", !is.null(rv$probs))

    # Use optimal F-score to find threshold if asked
    if(input$iOptimalThreshold) updateNumericInput(session, "thresholdClass", value = rv$cutoff)

    rv$predsClass = factor(paste0("Class_", as.integer(rv$probs >= input$thresholdClass))) # Predictions as class label based on shiny input or optimal threshold

    get_class_stats(rv$predsClass, rv$actualsClass, rv$probs, rv$yesClass, mean(rv$actualsClass == rv$yesClass), alpha = input$fAlpha)
  })
  
  
  ## Regression Model
  observeEvent(input$fitRegModel, {

    # Check for a response variable
    if(input$responseReg == ""){
      showNotification("Choose a numeric response variable.", duration = 5, type = "error")
      return()
    }

    # Check for any predictors
    if(length(input$predictorsReg) < 1){
      showNotification("You need to select some predictors.", duration = 5, type = "error")
      return()
    }

    # Check for data in dataset
    if(nrow(trainData()) < 2){
      showNotification("No data in dataset. Perhaps you have filtered it all out?", duration = 5, type = "error")
      return()
    }

    if(is.null(trainData())) return(NULL)

    withProgress(message = 'Modeling: ', value = 0, {
      setProgress(0.1, detail = 'Training Model')

      ## Get caret-internal model name from lookup table
      thisMethod = modelLookup$modelNameCaret[modelLookup$modelNameUI == input$regModelType]

      rv$caretModel = train(
        form = reformulate(setdiff(names(trainData()), input$responseReg), response = input$responseReg),
        data = trainData(),
        trControl = trainControl(method = "repeatedcv", number = 3, repeats = 1),
        method = thisMethod
      )

      setProgress(0.7, detail = 'Generating Predictions')
      rv$actualsReg = testData() %>% pull(input$responseReg) # Actual response values

      rv$preds = predict(rv$caretModel, newdata = testData())

      # setProgress(0.8, detail = 'Updating graphics')

      setProgress(0.9, detail = 'Calculating Fit Stats')
    }) # END progress

  })
  
  # Model regression stats
  regStats = reactive({  req(input$tabs == "Regression", !is.null(rv$preds)); get_reg_stats(rv$preds, rv$actualsReg)  })
  
  ### -------------- ###
  ### Input elements ###
  ### -------------- ###
  
  ## Sidebar
  output$filterDataVars   = renderUI(  selectInput("filterDataVar", "Filter by:", c("None", factorList(), numberList()), selected = "None")  )
  
  output$filterDataLevels = renderUI({
    req(input$filterDataVar)
    if(input$filterDataVar == "None" | !input$filterDataVar %in% factorList()) return(NULL)
    
    selectInput("filterDataLevel", "Level:",  choices = get_levels(selectData(), input$filterDataVar), multiple = T)
  })
  
  output$filterDataVars1 = renderUI({   make_filter(factorList(), numberList(), 1)   })
  output$filterDataVars2 = renderUI({   make_filter(factorList(), numberList(), 2)   })
  output$filterDataVars3 = renderUI({   make_filter(factorList(), numberList(), 3)   })
  
  output$filterDataLevels1 = renderUI({   make_filter_levels(selectData(), factorList(), input, 1)   })
  output$filterDataLevels2 = renderUI({   make_filter_levels(selectData(), factorList(), input, 2)   })
  output$filterDataLevels3 = renderUI({   make_filter_levels(selectData(), factorList(), input, 3)   })
 
  output$filterDataMin1 = renderUI({   make_filter_minmax(selectData(), numberList(), "Min", input, 1)   })
  output$filterDataMax1 = renderUI({   make_filter_minmax(selectData(), numberList(), "Max", input, 1)   })
  output$filterDataMin2 = renderUI({   make_filter_minmax(selectData(), numberList(), "Min", input, 2)   })
  output$filterDataMax2 = renderUI({   make_filter_minmax(selectData(), numberList(), "Max", input, 2)   })
  output$filterDataMin3 = renderUI({   make_filter_minmax(selectData(), numberList(), "Min", input, 3)   })
  output$filterDataMax3 = renderUI({   make_filter_minmax(selectData(), numberList(), "Max", input, 3)   })
  
  ## Uni tab
  output$selectFac <- renderUI(  selectInput("facVar", "Factor Variable:",   choices = intersect(factorList(), names(selectData())))  )
  output$selectNum <- renderUI(  selectInput("numVar", "Numeric Variable:",  choices = intersect(c(numberList(), dateList()), names(selectData())))  )
  output$selectDat <- renderUI(  selectInput("datVar", "DateTime Variable:", choices = intersect(dateList(), names(selectData())))  )
  output$selectBin <- renderUI(  selectInput("binVar", "Binary Variable:",   choices = intersect(binaryList(), names(selectData())))  )
  
  ## Bi tab
  output$selectMultivarX <- renderUI(    selectInput("multivarX", "X Variable:",  choices = names(selectData()))   )
  output$selectMultivarY <- renderUI(    selectInput("multivarY", "Y Variable:",  choices = intersect(c(factorList(), numberList()), names(selectData())))  )
  output$selectMultivarG <- renderUI(    selectInput("multivarG", "Grouping Variable (Boxplot & Scatter):",   choices = c("NONE", intersect(c(factorList(), binaryList()), names(selectData()))))  )
  
  observeEvent(input$switchVarsBi, {
    tempX = input$multivarX
    tempY = input$multivarY
    
    output$selectMultivarX <- renderUI(    selectInput("multivarX", "X Variable:",  choices = names(selectData()), selected = tempY)   )
    output$selectMultivarY <- renderUI({
      selectInput("multivarY", "Y Variable:",  choices = intersect(c(factorList(), numberList()), names(selectData())), selected = tempX)
    })
  })
  
  ## Long tab
  output$selectLonXvar     <- renderUI(   selectInput("lonXvar", "X variable", choices = c(dateList(), factorList()))   )
  output$selectLonYvar     <- renderUI(   selectInput("lonYvar", "Y variable", choices = numberList())   )
  output$selectLonIdVar    <- renderUI(   selectInput("lonIdVar", "ID variable", choices = factorList())   )
  output$selectLonGroupVar <- renderUI(   selectInput("lonGroupVar", "Group by:", choices = c("", factorList()))   )
  output$selectLonFacetVar <- renderUI(   selectInput("lonFacetVar", "Facet by:", choices = c("", factorList()))   )
  
  observeEvent(input$switchVarsLon, {
    tempG = input$lonGroupVar
    tempF = input$lonFacetVar
    
    output$selectLonGroupVar <- renderUI(    selectInput("lonGroupVar", "Group by:",  choices = c("", factorList()), selected = tempF)   )
    output$selectLonFacetVar <- renderUI(    selectInput("lonFacetVar", "Facet by:",  choices = c("", factorList()), selected = tempG)   )
  })
  
  
  ## Modeling tabs
  output$responseClassOut = renderUI(  selectInput("responseClass", "Response Variable:", binaryList())  )
  output$responseRegOut   = renderUI(  selectInput("responseReg",   "Response Variable:", numberList())  )
  # output$responseForeOut  = renderUI(  selectInput("responseFore",  "Response Variable:", numberList())  )
  
  # output$effectPredClassOut = renderUI(  selectInput("effectPredClass", "Investigate:", input$predictorsClass)  )
  # output$effectPredRegOut   = renderUI(  selectInput("effectPredReg",   "Investigate:", input$predictorsReg  )  )
  
  
  ### Checkboxes ###
  output$inputVarsClass = renderUI(   make_checkBoxGroup(selectData(), input$responseClass, "predictorsClass", buttonTimesClass(), input$last_btn)  )
  output$inputVarsReg   = renderUI(   make_checkBoxGroup(selectData(), input$responseReg,   "predictorsReg",   buttonTimesReg(),   input$last_btn)  )
  output$inputVarsPreds = renderUI(   make_checkBoxGroup(selectData(), NULL,                "predictorsPreds", buttonTimesPreds(), input$last_btn)  )
  
  
  
  
  ### --------------- ###
  ### Output elements ###
  ### --------------- ###
  
  output$mseReg  = renderText(regStats()$MSE)
  output$mapeReg = renderText(scales::percent(regStats()$MAPE, accuracy = 0.1))

  ### Tables ###
  ### ------ ###
  
  # Update Data Table
  output$tablePreds = DT::renderDataTable({
    make_data_table(filterData(), input$predictorsPreds, rv$rowIndexTest, rv$predsClass, rv$predsReg)
  })
    
  
  ### Plots ###
  ### ----- ###
  
  ## Uni tab
  plotBar = reactive({
    req(input$facVar %in% names(selectData()))
    
    filterData() %>% select(all_of(input$facVar)) %>%
      gg_count(type= "Bar", x.lab = input$facVar, maxLevels = input$maxBarsUni, maxLabelL = input$maxLabelUni, 
               iLog = input$iLogBarUni, iLabel = input$iLabelBarUni, iPareto = input$iParetoUni, iRotateX = input$iRotateXUni)
  })
  
  plotHist = reactive({
    req(input$numVar %in% names(selectData()))
    
    filterData() %>% select(all_of(input$numVar)) %>%
      gg_hist(x.lab = input$numVar, myBins = input$numBinsHistUni, iLog = input$iLogHistUni)
  })
  
  plotDate = reactive({
    req(input$datVar %in% names(selectData()))
    
    filterData() %>% select(all_of(input$datVar)) %>% gg_polar_time()
  })
  
  plotPie = reactive({
    req(input$binVar %in% names(selectData()))
    
    filterData() %>% pull(input$binVar) %>% mean(na.rm = T) %>% make_gauge()
  })
  
  output$plotBar  = renderPlot(plotBar(), bg= "transparent")
  output$plotHist = renderPlot(plotHist(), bg= "transparent")
  output$plotDate = renderPlot(plotDate(), bg= "transparent")
  output$plotPie = renderGauge(plotPie())
  
  ## Bi tab
  plotMulti = reactive({
    req(input$multivarX %in% names(filterData()), input$multivarY %in% names(filterData())) # , input$tabs == "Bivariate"
    
    filterData() %>%
      {if(input$multivarG == "NONE") select(., all_of(c(input$multivarX, input$multivarY))) else select(., all_of(c(input$multivarX, input$multivarY, input$multivarG)))} %>%
      make_multi_plot(input$catXcatType, input$maxLevelsMulti, input$maxGroupsMulti, input$maxLabelMulti, input$cutoffMulti, input$iParetoMulti, input$iLogMulti, 
                      input$iRotateXMulti, input$iFreqPoly, input$numBinsMulti, input$iLinearMulti, input$iXYMulti, input$iJitterMulti)
  })
  
  output$plotMulti = renderPlot(plotMulti(), bg= "transparent")
  output$plotCorr = renderPlot({  req(input$iShowCorrs == TRUE);     make_corrplot(filterData())  })
  
  ## Lon tab
  plotTime = reactive({
    req(input$lonXvar, input$lonYvar, input$lonIdVar, input$tabs == "Longitudinal", input$lonYvar %in% names(filterData()))
    
    filterData() %>%
      gg_time2(input$lonXvar, input$lonYvar, input$lonIdVar, input$lonGroupVar, input$lonFacetVar, input$lonAggFlag, input$lonAggType, input$lonFreeScaleFlag, input$lonErrorBarFlag)
  })

  output$plotTime = renderPlot(plotTime(), bg= "transparent")
  
  
  ## Class tab
  # Histogram of prediction probabilities
  output$classProbs = renderPlot({
    req(!is.null(rv$probs))
    
    gg_hist(rv$probs, dashPt = input$thresholdClass)
  }, bg="transparent")
  
  # Effects plot
  # output$effectPlotClass = renderPlot({
  #   req(input$tabs == "Classification", rv$fitClass)
  #   
  #   testData = isolate(testData()) # Don't update effects just by clicking on checkboxes
  #   
  #   rv$fitClass %>% plot_effects(testData, input$responseClass, input$effectTypeClass, input$effectPredClass, input$topNEffectsClass) 
  # })
  
  # ROC, PR and F-Score diagnostic plots
  output$plotROC = renderPlot({
    req(input$iShowPlots == TRUE, !is.null(rv$rocPred))
    
    withProgress(message = 'Updating ROC Curve: ', value = 0.7, {
      
      make_PROC_plot(rv$rocPred, "ROC")
    }) # END progress
  })
  output$plotPR = renderPlot({
    req(input$iShowPlots == TRUE, !is.null(rv$rocPred))
    
    withProgress(message = 'Updating PR Curve: ', value = 0.8, {
      
      make_PROC_plot(rv$rocPred, "PR")
    }) # END progress
  })
  output$plotFscore = renderPlot({
    req(input$iShowPlots == TRUE, !is.null(rv$rocPred))
    
    withProgress(message = 'Updating F-Score Curve: ', value = 0.9, {
      
      make_PROC_plot(rv$rocPred, "Fscore")
    }) # END progress
  })
  
  
  ### Downloads ###
  ### --------- ###
  output$downPlotBar = downloadHandler(
    filename = function() {paste(input$facVar, "barplot.png", sep = "-")},
    
    content = function(file) {      ggsave(file, plot = plotBar() + theme(axis.text = element_text(size = 11)), width = 11, units = "in")    }
  )
  output$downPlotHist = downloadHandler(
    filename = function() {paste(input$numVar, "histogram.png", sep = "-")},
    
    content = function(file) {      ggsave(file, plot = plotHist() + theme(axis.text = element_text(size = 11)), width = 11, units = "in")    }
  )
  output$downPlotMulti = downloadHandler(
    filename = function() {paste(input$multivarY, "by", input$multivarX, "plot.png", sep = "-")},
    
    content = function(file) {      ggsave(file, plot = plotMulti() + theme(axis.text = element_text(size = 11)), width = 11, units = "in")    }
  )
  
  
  ### Summary Text ###
  ### ------------ ###

  ## Sidebar
  output$dataNtotal  = renderText(  nTotal()  )
  output$dataN = renderText(  nRetained()  )
  
  ## Bi tab
  output$summStatsAvgX  = renderText(   make_summ_text(summaryStats(), "avg",  input$multivarX, input$tabs)   )
  output$summStatsSdX   = renderText(   make_summ_text(summaryStats(), "sd",   input$multivarX, input$tabs)   )
  output$summStatsSkewX = renderText(   make_summ_text(summaryStats(), "skew", input$multivarX, input$tabs)   )
  output$summStatsKurtX = renderText(   make_summ_text(summaryStats(), "kurt", input$multivarX, input$tabs)   )
  
  output$summStatsAvgY  = renderText(   make_summ_text(summaryStats(), "avg",  input$multivarY, input$tabs)   )
  output$summStatsSdY   = renderText(   make_summ_text(summaryStats(), "sd",   input$multivarY, input$tabs)   )
  output$summStatsSkewY = renderText(   make_summ_text(summaryStats(), "skew", input$multivarY, input$tabs)   )
  output$summStatsKurtY = renderText(   make_summ_text(summaryStats(), "kurt", input$multivarY, input$tabs)   )
  
  output$summStatsCorr = renderText({
    req(input$multivarX %in% names(filterData()), input$multivarY %in% names(filterData()), input$tabs == "Bivariate")
    
    if(input$multivarX == input$multivarY) return(1)
    
    filterData() %>%
      select(all_of(c(input$multivarX, input$multivarY))) %>%
      get_cor()
  })
    
  
  ### Gauges ###
  ### ------ ###
  
  output$gaugeN = renderGauge(make_gauge(fracRetained(), yellowPoint = 0.75, redPoint = 0.5))
  
  ## Univariate tab
  output$gaugeFac = renderGauge({   req(input$facVar %in% names(selectData())); render_gauge(input$facVar, filterData())   })
  output$gaugeNum = renderGauge({   req(input$numVar %in% names(selectData())); render_gauge(input$numVar, filterData())   })
  output$gaugeDat = renderGauge({   req(input$datVar %in% names(selectData())); render_gauge(input$datVar, filterData())   })
  output$gaugeBin = renderGauge({   req(input$binVar %in% names(selectData())); render_gauge(input$binVar, filterData())   })
  
  ## Bivariate tab
  output$gaugeBiComp = renderGauge({
    req(input$multivarX %in% names(filterData()), input$multivarY %in% names(filterData()), input$tabs == "Bivariate")
    
    filterData() %>%
      select(all_of(c(input$multivarX, input$multivarY))) %>% 
      complete.cases() %>% mean() %>% make_gauge()
  })
  
  ## Classification tab
  output$gaugeSkillClass = renderGauge(  make_gauge(classStats()$skill, yellowPoint = 0.5, redPoint = 0.25)  )
  output$gaugeAcc        = renderGauge(  make_gauge(classStats()$accuracy)  )
  output$gaugeRecall     = renderGauge(  make_gauge(classStats()$recall)  )
  output$gaugePrec       = renderGauge(  make_gauge(classStats()$precision)  )
  output$gaugeFscore     = renderGauge(  make_gauge(classStats()$Fscore)  )
  output$gaugeFalphaScore= renderGauge(  make_gauge(classStats()$FalphaScore)  )
  output$gaugeMCC        = renderGauge(  make_gauge(classStats()$MCC, yellowPoint = 0.6, redPoint = 0.4)  )
  output$gaugeAUC        = renderGauge(  make_gauge(classStats()$AUC, yellowPoint = 0.7, redPoint = 0.6)  )
  output$gaugeInform     = renderGauge(  make_gauge(classStats()$inform, yellowPoint = 0.6, redPoint = 0.4)  )
  output$gaugeMarked     = renderGauge(  make_gauge(classStats()$marked, yellowPoint = 0.6, redPoint = 0.4)  )
  
  
  ## Regression tab
  # output$gaugeSkillReg   = renderGauge(  make_gauge(regStats()$skill, yellowPoint = 0.5, redPoint = 0.25)  )
  
  # output$testText1 = renderPrint(processedData()) # Testing - remove
  # output$testText2 = renderPrint(nrow(modelData())) # Testing - remove
  # output$testText3 = renderPrint(nrow(trainData())) # Testing - remove
  # output$testText4 = renderPrint(rv$yesClass) # Testing - remove
  
  # session$onSessionEnded(stopApp) # Uncomment to have R stop on browser close
  
} # END server

