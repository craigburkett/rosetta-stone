library(tidyverse)
library(magrittr, warn.conflicts = F)
library(rlang, warn.conflicts = F) # For sym() parsing text programmatically
library(shiny)
library(shinydashboard, warn.conflicts = F)
library(flexdashboard, warn.conflicts = F) # For gauges on shiny dashboard
library(DT, warn.conflicts = F) # For more attractive data tables in UI
library(moments) # For skew, kurtosis calcs
library(lattice) # Needed for corrplot
library(corrplot) # For bubble correlation plot
library(ROCR)  # For ROC / PR curves
library(caret, warn.conflicts = F) # Generalized modeling
# library(ada) # For ADABoost modeling
# library(randomForest) # For RF classification and forecasting - seems to be handled by caret somehow ...
# library(iml) # For generalized effects
library(shinycssloaders) # For waiting UI
if(file.exists("GGgraphs.R")) source("GGgraphs.R")
if(file.exists("../GGgraphs.R")) source("../GGgraphs.R")
  
options(dplyr.summarise.inform = F)
### STUFF UNIQUE TO THIS DATASET ###

idNamesNotPickedUp = c("") # Any ID vars that do not end in 'id' you want removed
manualRemoveList = c("")   # Any other vars you want removed from predictor list (like leakage from the future)

### GENERAL ###
modelLookup = data.frame(
  modelMode = c("Class", "Class", "Class", "Class", 
                "Reg", "Reg", "Reg", "Reg")
  , modelNameUI = c("Logistic Regression", "Random Forest", "K-Nearest Neighbours", "XGBoost", 
                    "Linear Regression", "K-Nearest Neighbours", "Random Forest", "XGBoost") # , "ADABoost", "C5.0"
  , modelNameCaret = c("glm", "rf", "knn", "xgbTree", 
                       "lm", "rf", "knn", "xgbTree") # names(getModelInfo()) # , "ada", "C5.0"
)

### -------------- ###
### Data Wrangling ###
### -------------- ###
lmp <- function (modelobject) {
  if (class(modelobject)[1] != "lm") return(NULL)#stop("Not an object of class 'lm' ")
  f <- summary(modelobject)$fstatistic
  
  unname(pf(f[1],f[2],f[3],lower.tail=F))
}

get_levels <- function(dfr, col) {
  dfr %>%
    select(col) %>%
    pull() %>%
    unique()
}

check_inputs <- function(resp, preds, typeStr = "binary") {
  ### Call only within shiny app
  
  iProblem = FALSE
  
  # Check for a response variable
  if(resp == ""){
    showNotification(paste("Choose a", typeStr, "response variable."), duration = 5, type = "error")
    iProblem = TRUE
  } 
  
  # Check for any predictors
  if(length(preds) < 1){
    showNotification("You need to select some predictors.", duration = 5, type = "error")
    iProblem = TRUE
  }  
  
  iProblem
}

collapse_levels <- function(col, numLevels = 10) {
  ### Collapses a factor with more than numLevels levels
  ### Result has numLevels + 1 (Other) levels
  
  if(!is.factor(col)) return(col)
    
  fct_lump(col, n = numLevels)
}

binarize_levels <- function(var, iFacConvert = F) {
  ### Converts an integer, or string, factor variable (if asked) into logical if it has exactly two levels

  # # Factor, 2 levels
  if(iFacConvert & class(var)[1] %in% c("factor", "character") & length(unique(var)) == 2){
    var = as.logical(as.numeric(factor(var)) - 1)
  }

  # Int, 2 levels, 0s and 1s
  if(class(var)[1] == "integer" & length(unique(var)) == 2 & all(unique(var) %in% c(0, 1))) {
    var = as.logical(var)
  } 

  var
}

get_cor <- function(dfr) {
  ### dfr: two column DF
  if(nrow(dfr) < 1) return(NULL)
  
  if(!all(lapply(dfr, class) %in% c("integer", "numeric", "difftime"))) return(NULL)
  
  dfr %>%
    mutate_all(as.numeric) %>%
    cor(use = "pairwise") %>%
    extract(1, 2)
}

get_dates <- function(classes) {
  ## Input: Vector of classes
  
  names(classes[classes %in% c("POSIXct", "Date")])
}

get_range <- function(dfr, col) {
  dfr %>%
    select(col) %>%
    pull() %>%
    range(na.rm = T)
}

get_summary_stats <- function(dfr, roundDec = 2) {
  ### Returns numeric summary stats for any numeric columns in dfr including dates
  # TODO: dates
  
  dd = dfr[sapply(dfr, class) %in% c("numeric", "integer", "difftime")]
  if(ncol(dd) < 1) return(list(avg= "", sd= "", skew= "", kurt= "")) # Get outta dodge if no numerics
  
  get_col_stats <- function(col) {
    col = as.numeric(col)
    
    avg  = round(mean(col, na.rm= T), roundDec)
    sd   = round(sd(col, na.rm= T), roundDec)
    skew = round(moments::skewness(col, na.rm= T), roundDec)
    kurt = round(kurtosis(col, na.rm= T), roundDec)
    
    list(avg= avg, sd= sd, skew= skew, kurt= kurt)
  }
  
  lapply(dd, get_col_stats) # Compute stats for every numeric column
}

get_loss <- function(pred, actual, method="MSE") {
  stopifnot(method %in% c("MSE", "MAPE"))
  
  actual = actual[!is.na(pred)]
  pred = pred[!is.na(pred)]
  if(length(actual) > length(pred)) actual = sample(actual, length(pred))
  
  if(class(pred) == "factor")  pred = ifelse(pred == "TRUE", 1, 0)
  
  if(method=="MSE")  return(mean((pred - actual)^2))
  if(method=="MAPE") return(mean(abs((actual - pred)/actual)))
}

remove_IDs <- function(vec, addThese = "") {
  ## Input: Character vector, optional vector containing ID vars that don't get picked up automatically ('id' as last two characters)
  
  x = vec[!grepl("id$", vec, ignore.case = T)]
  x[!x %in% addThese]
}

remove_manual <- function(vec) {
  ## Input: Character vector, optional vector of names to remove manually
  
  vec[!vec %in% manualRemoveList] # Defined in global
}

### ----------- ###
### UI Elements ###
### ----------- ###

make_filter <- function(facs, nums, iter) {
  renderUI({
    selectInput(paste0("filterDataVar", iter), "Filter by:", c("None", facs, nums), selected = "None")
  })
}

make_filter_levels <- function(dfr, facs, input, iter) {
  
  renderUI({
    req(input[[paste0("filterDataVar", iter)]])
    
    if(input[[paste0("filterDataVar", iter)]] == "None" | !input[[paste0("filterDataVar", iter)]] %in% facs) return(NULL)
    
    selectInput(paste0("filterDataLevel", iter), "Level:",  choices = get_levels(dfr, input[[paste0("filterDataVar", iter)]]), multiple = T)
  })
}

make_filter_minmax <- function(dfr, nums, type, input, iter) {
  
  ind = 1
  if(type == "Max") ind = 2
  
  renderUI({
    req(input[[paste0("filterDataVar", iter)]])
    
    if(input[[paste0("filterDataVar", iter)]] == "None" | !input[[paste0("filterDataVar", iter)]] %in% nums) return(NULL)
    
    thisRange = get_range(dfr, input[[paste0("filterDataVar", iter)]])
    
    numericInput(paste0("filterData", type, iter), paste0(type, ":"), thisRange[ind], thisRange[1], thisRange[2])
  })
}

make_gauge <- function(number, yellowPoint = 0.75, redPoint = 0.5, invert = F) {
  ### Renders a percent on a gauge
  if(invert) number = 1.00 - number
  
  number %>% 
    round(3) %>%
    multiply_by(100) %>% 
    gauge(min = 0, max = 100, symbol = "%", 
          gaugeSectors(success = c(100*yellowPoint, 100), warning = c(100*redPoint, 100*yellowPoint), danger = c(0, 100*redPoint)))
  
}

make_checkBoxGroup <- function(dfr, y, inputName, times, buttonLabel = NULL) {
  ### Renders a checkboxgroup element using raw variable names 
  # dfr: Raw data set
  # y: Outcome (response) variable chosen
  # inputName: Shiny internal labelId reference for this UI element
  # buttonLabel: text label for which button was pressed
  # # times: Counter for action buttons (to redraw group whenever one changes)
  
  nameList = setdiff(names(dfr), y) # Don't include the response variable in list of predictors
  if(is.null(buttonLabel)) predList = ""
  else if(grepl("^noPreds.*", buttonLabel)) predList = ""
  else if(grepl("^allPreds.*", buttonLabel)) predList = nameList
  else if(grepl("^luckyPreds.*", buttonLabel)) predList = sample(nameList, sample(1:length(nameList), 1))
  else predList = ""
  
  # Custom alignment of box - 3 columns, variable height, see CSS in UI.R for 'multicol' details - needs Chrome (Firefox doesn't render cols)
  list(tags$div(
    id= letters[(times %% length(letters)) + 1], # Hack to update with repeated action button presses
    align = 'left', class = 'multicol', 
    checkboxGroupInput(inputName, "", choices = nameList, selected = predList, inline = F)
  )) 
}

set_reactive_values <- function() {
  reactiveValues(
    stats = list(accuracy= 0, recall= 0, precision= 0, Fscore= 0, MCC= 0, inform= 0, marked= 0, AUC= 0, skill= 0) # nData= 0, 
    
    , cutoff= 0.5
    , fitClass     = NULL
    , predsClass   = NULL
    , actualsClass = NULL
    , yesClass     = NULL
    , probs        = NULL
    , rocPred      = NULL
    
    , fitReg       = NULL
    , predsReg     = NULL
    , actualsReg   = NULL
    
    , fitFore      = NULL
    , predsFore    = NULL
    , actualsFore  = NULL
    
    , rowIndexTest = NULL
    , caretModel = NULL
  )
}

render_gauge <- function(var, dfr) {
  # One-line wrapper function to renderGauge
  
  dfr %>% 
      select(all_of(var)) %>% 
      pull(var) %>% 
      is.na() %>% 
      mean() %>% 
      make_gauge(invert = T)
}

### ---------------- ###
### Plots and Tables ###
### ---------------- ###

make_multi_plot <- function(dfr, catType="Bar", maxLevels=8, maxGroups=4, maxLabelL=20, cutoff=Inf, iPareto=T, iLog=F, 
                            iRotateX=F, iFreqPoly=F, numBins=30, linearFit=F, xy=F, jitter=F) {
  ### 5 cases, depending on type of inputs
  # TODO: Improve doc'n of this function
  # TODO: error checking on inputs
  if(any(apply(is.na(dfr), 2, all))) return(NULL) # If any col has all missings
  
  if(ncol(dfr) < 2 | ncol(dfr) > 3 | nrow(dfr) < 2 | is.null(dfr)) return(NULL)
  
  xName = names(dfr)[1]
  yName = names(dfr)[2]
  gName = names(dfr)[3] # In case it's here
  
  xClass = lapply(dfr, class)[1]
  yClass = lapply(dfr, class)[2]
  
  catClass = c("character", "factor", "logical")
  numClass = c("numeric", "integer", "difftime")
  datClass = c("Date", "POSIXct")
  
  # Convert logicals to factor for now
  if(xClass == "logical") dfr[1] = factor(dfr[1])
  if(yClass == "logical") dfr[2] = factor(dfr[2])
  
  # Case 5: CAT x CAT x CAT (with "Jitter" request)
  if(ncol(dfr) == 3 & catType == "Jitter") {
    gClass = lapply(dfr, class)[3]
    
    # If they are not all categorical, return empty plot
    if(!(xClass[1] %in% catClass & yClass[1] %in% catClass & gClass[1] %in% catClass)) return(NULL)
    
    return(gg_jitter(dfr, maxLevels = maxLevels, maxGroups = maxGroups, iPareto = iPareto, iRotateX = iRotateX, maxLabelL = maxLabelL, manualColors = T, x.lab = xName, y.lab = yName, g.lab = gName))
  }
  
  # Case 1: CAT x CAT
  if(xClass[1] %in% catClass & yClass %in% catClass) return(gg_count(dfr[,1:2], type = catType, maxLevels = maxLevels, maxGroups = maxGroups, maxLabelL = maxLabelL, iLog = iLog, iPareto = iPareto, iRotateX = iRotateX, x.lab = xName, y.lab = yName))
  
  # Case 2: CAT x NUM (x CAT)
  if(xClass[1] %in% catClass & yClass %in% numClass) return(gg_box(dfr, x.lab = xName, y.lab = yName, maxBoxes = maxLevels, maxGroups = maxGroups, maxLabelL = maxLabelL, cutoff = cutoff, iLog = iLog, iPareto = iPareto, iRotateX = iRotateX))
  
  # Case 3: NUM x CAT 
  if(xClass[1] %in% numClass & yClass %in% catClass) return(gg_hist(dfr[,1:2], iFreqPoly = iFreqPoly, myBins = numBins, maxGroups = maxGroups, cutoff = cutoff, iLog = iLog, x.lab = xName, y.lab = yName))
  
  # Case 4: NUM x NUM (x CAT)
  if(xClass[1] %in% numClass & yClass %in% numClass) return(gg_scatter(dfr, x.lab = xName, y.lab = yName, cutoff = cutoff, maxGroups = maxGroups, maxPoints = Inf, iSmooth=linearFit, iXY=xy, iJitter=jitter, iLog = iLog))
  
  return(NULL)
}

make_corrplot <- function(dfr, method= "spearman") {
  ### Draws a correlation plot with bubbles
  # if(is.null(dfr) | nrow(dfr) == 0 | ncol(dfr) == 0) return(NULL)
  
  dfr %>% 
    setNames(abbreviate(colnames(dfr), 10)) %>%
    Filter(is.numeric, .) %>%
    # mutate_if(is.Date, as.numeric) %>% # Convert any dates to numeric
    cor(use = "pairwise", method = "spearman") %>%
    corrplot()
}

make_summ_text <- function(summStats, statistic, var, tab) {
  if(is.null(var) | tab != "Bivariate") return(NULL)
  
  thisSummStat = extract(summStats, var)[[1]]
  
  as.numeric(extract(thisSummStat, statistic))
}

make_data_table <- function(dfr, predictors, rowNums, classPreds, regPreds, roundingN=1) {
  if(is.null(predictors)) return(NULL)
  if(predictors[1] == "") return(NULL)
  
  if(is.null(regPreds) & is.null(classPreds)) return(NULL) # No predictions, get outta dodge
  
  # Do we have any regression predictions? Do they match up with the data? If so, use them
  if(!is.null(regPreds) & length(rowNums) == length(regPreds)) Preds = round(regPreds, roundingN)
  
  # Do we have any classification predictions? Do they match up with the data? If so, use them
  else if(!is.null(classPreds) & length(rowNums) == length(classPreds)) Preds = classPreds
  
  else return(NULL)
  
  stopifnot(length(rowNums) == length(Preds))
  
  dfr %>%
    select(predictors) %>%
    slice(rowNums) %>%
    cbind(Preds) %>%
    DT::datatable(options = list(pageLength = 15), rownames = F)
}

make_PROC_plot <- function(rocPred, type = "ROC") {
  # http://scg.sdsu.edu/rf_r/
  if(is.null(rocPred)) return()
  
  # Make the respective plot asked for, or nothing at all
  if(type == "ROC") {
    AUROC = round(as.numeric(performance(rocPred, "auc")@y.values), 3) # AUC
    
    rocPerf = performance(rocPred, "tpr", "fpr")
    myPlot  = plot(rocPerf, main= paste(type, "Curve"), col= 2, lwd= 2, colorize = T)
    abline(a=0,b=1,lwd=2,lty=2,col="gray")
    
    text(x= 0.8, y= 0.2, labels = paste("AUC =", AUROC)) # Add AUC to plot
  }
  else if(type == "PR") {
    rocPerf = performance(rocPred, "prec", "rec")
    myPlot  = plot(rocPerf, main= paste(type, "Curve"), col= 2, lwd= 2, colorize = T)
  }
  else if(type == "Fscore") {
    rocPerf = performance(rocPred, "f")
    myPlot  = plot(rocPerf, main= paste(type, "Curve"), col= 2, lwd= 2) # No colorize on F
  }
  else return()
  
  myPlot
}

plot_effects <- function(fit, testData, responseName, effectType, effectVar, topN = 10) {
  ### Returns plot of generalized model effects for 'any' model
  ### Needs at least two features to work properly
  
  X = testData %>% select(-c(responseName))
  y = testData %>% pull(responseName)
  
  if(ncol(X) < 2) return(NULL) # Get outta dodge if you have less than two features - nothing to display, fool!
  if(!is.null(fit$type)) { # Check for RF object
    if(fit$type == "classification") return(NULL) # TODO: Currently only works with RF regression objects)
  } 
  
  # Need to convert all logicals back to numerics here, or iml shits it's pants
  X = X %>% mutate_if(is.logical, as.integer)
  
  predictor = Predictor$new(fit, data = X, y = y)
  
  if(effectType == "Importance"){
    plotDat = FeatureImp$new(predictor, loss = "mae")
    plotDat$results = plotDat$results %>% arrange(desc(importance)) %>% head(topN)
  }
  else if (effectType == "Interactions"){
    plotDat = Interaction$new(predictor)
    plotDat$results = plotDat$results %>% arrange(desc(.interaction)) %>% head(topN)
  }
  else if (effectType == "Shapley"){
    plotDat = Shapley$new(predictor, x.interest = X[1,])
    plotDat$results = plotDat$results %>% arrange(desc(abs(phi))) %>% head(topN) %>% arrange(desc(phi))
  }
  else if (effectType == "ALE (by var)"){
    plotDat = FeatureEffect$new(predictor, feature = effectVar)
  }
  else if (effectType == "Interactions (by var)"){
    plotDat = Interaction$new(predictor, feature = effectVar)
  }
  else return(NULL)
  
  plot(plotDat)
  
  # # Particular data points
  # shapley$explain(x.interest = X[2,])
  # shapley$plot()
  
  # data.frame(Variable, Effect, row.names = NULL) %>%
  #   # data.frame(Variable, Effect, Direction, row.names = NULL) %>%
  #   arrange(desc(abs(Effect))) %>% # Sort by absolute value
  #   dplyr::slice(1:topN) %>% # Just take the top topN effects
  #   mutate(Variable = fct_reorder(Variable, Effect)) # Order factor for plotting
}


### ------------- ###
### Data Modeling ###
### ------------- ###

train_test_split <- function(dfr, testPerc = 0.2, imbalanceMethod = "Nothing", ordered = FALSE) {
  ### Returns a list of two data frames, containing training and test data. Class is of type logical
  ## Need to do imputation before calling this, if you want it
  ## If ordered = T, need to have dfr sorted by date already (oldest at the top)
  # TODO: Fix up- and down-sampling ability
  if(nrow(dfr) < 1 | ncol(dfr) < 2) return(NULL)
  # if(nrow(dfr) < 1) return(NULL)
  
  dfr = dfr[complete.cases(dfr),] # Remove missing values rows, as most models won't allow them
  
  nCases  = nrow(dfr) # Total 'experimental units'
  nCasesTrain = round((1 - testPerc) * nCases) # Number of training cases
  
  # Training/Testing split
  if(ordered) trainIndex = 1:nCasesTrain # Data are sorted by timestamp, so this is first (100 - xx)% of data for training
  else        trainIndex = sample(1:nCases, nCasesTrain)    # Random cases for training
  
  testIndex  = setdiff(1:nCases, trainIndex) # Last xx% (or random cases) for testing
  
  train = dfr[trainIndex, ] # Actually divide up the data here
  test  = dfr[testIndex, ]
  
  # Deal with levels in test that don't appear in train
  all_levels = lapply(test, levels)
  for(col in names(train)) if(is.factor(train[,col])) levels(train[,col]) = all_levels[[col]]
  
  # TODO: Fix this up to be more general - ie. not use $Class or rename or something
  # trainPosIndex = which(train$Class)
  # trainNegIndex = which(!train$Class)
  # nPosCases = length(trainPosIndex)
  # nNegCases = length(trainNegIndex)
  # 
  # ## Down or Up sampling, pick one or neither ##
  # iMoreTcases = nPosCases > nNegCases
  # 
  # newTrainPosIndex = trainPosIndex
  # newTrainNegIndex = trainNegIndex
  # 
  # if(imbalanceMethod == "Down-sampling"){
  #   if(iMoreTcases)  newTrainPosIndex = sample(trainPosIndex, size = nNegCases, replace = F) 
  #   if(!iMoreTcases) newTrainNegIndex = sample(trainNegIndex, size = nPosCases, replace = F)
  # } else if(imbalanceMethod == "Up-sampling"){
  #   if(iMoreTcases)  newTrainNegIndex = sample(trainNegIndex, size = nPosCases, replace = T) 
  #   if(!iMoreTcases) newTrainPosIndex = sample(trainPosIndex, size = nNegCases, replace = T)
  # }
  # 
  # train = train %>% dplyr::slice(c(newTrainPosIndex, newTrainNegIndex))
  
  list(train= train, test= test, testIndex= testIndex)
}

get_class_stats <- function(preds, actuals, probs, yesClass, propPos, alpha=1) {
  ### 
  # preds: predictions as a class
  # actuals: true values as a class
  # probs: predictions as a probability
  # yesClass: class label for the 'yes' class
  # propPos: proportion of baseline that is positive (for baseline random guessing comparison)
  
  rocPred = prediction(probs, actuals)
  
  tp = as.numeric(sum((preds == actuals) & (actuals == yesClass), na.rm=T))
  tn = as.numeric(sum((preds == actuals) & (actuals != yesClass), na.rm=T))
  fp = as.numeric(sum((preds != actuals) & (preds == yesClass), na.rm=T))
  fn = as.numeric(sum((preds != actuals) & (preds != yesClass), na.rm=T))
  
  mccDenom = max((tp+fp)*(tp+fn)*(tn+fp)*(tn+fn), 1) # If zero make it one - gives proper convergence below
  
  acc  = (tp + tn) / (tp + fp + tn + fn) # accuracy
  
  tpr  = tp / (tp + fn) # sensitivity or recall
  tnr  = tn / (tn + fp) # specificity or 1 - FPR
  tpa  = tp / (tp + fp) # PPV or precision
  tna  = tn / (tn + fn) # NPV
  
  FOR = fn / (fn + tn) # False omission rate
  fdr = fp / (fp + tp) # False discovery rate
  
  f    = 2 * tp / (2*tp + fp + fn) # F-score, harmonic mean of tpr and tpa
  # fBeta= (1 + beta^2) * (tpa*tpr)/(beta^2*tpa + tpr) # Generalized F-score
  fAlpha= 1 / (alpha*1/tpa + (1-alpha)*1/tpr) # Generalized F-score
  mcc  = (tp*tn - fp*fn) / sqrt(mccDenom) # Matthew's Corr. Co-eff
  inf  = tpr + tnr - 1 # Bookmaker Informedness
  mar  = tpa + tna - 1 # Markedness
  
  auc  = round(as.numeric(performance(rocPred, "auc")@y.values), 3) # AUC
  n    = sum(!is.na(preds))
  
  randPreds = rbernoulli(n, propPos)
  skill = 1 - Mse(preds == yesClass, actuals == yesClass) / Mse(randPreds, actuals == yesClass)
  
  list(accuracy= acc, recall= tpr, precision= tpa, Fscore= f, FalphaScore= fAlpha, MCC= mcc, inform= inf, marked= mar, AUC= auc, skill= skill) # nData= n, tp=tp, tn=tn, fp=fp, fn=fn, 
}

get_reg_stats <- function(preds, actuals) {
  MSE  = get_loss(preds, actuals, "MSE")
  MAPE = get_loss(preds, actuals, "MAPE")
  
  # skillMSE  = get_skill()
  # skillMAPE = get_skill()
  
  list(MSE = MSE, MAPE = MAPE)
}

# get_skill <- function(preds, testActs, trainActs, n) {
#   ### Pretty much same as above, just skill and for regression
#   error = get_loss(preds, actuals, "MSE")
#   
#   naivePreds = rep(mean(trainActs), n) # Mean value naive prediction
#   # naivePreds = rnorm(n, mean(rv$actualsFore), sd(rv$actualsFore)) # Based on Normal distribution assuming deltas
#   naiveError = get_loss(naivePreds, testActs, "MSE")
#   
#   1 - error/naiveError # skill
# }

get_optimal_cutoff <- function(rocPred, thisAlpha) {
  p = performance(rocPred, "f", alpha = thisAlpha)
  fScores = unlist(p@y.values)
  maxF = max(fScores, na.rm = T)
  unlist(p@x.values)[which(fScores == maxF)][1]
}