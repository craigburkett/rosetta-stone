library(tidyverse)
library(caret)

adult = readRDS("adult.rds")
yVarStr = "hrsPerWeek"

trainRowNumbers = createDataPartition(adult[[yVarStr]], p=0.8, list=FALSE)

trainData = adult %>% slice(trainRowNumbers)
testData  = adult %>% slice(-trainRowNumbers)
trainX <- trainData %>% select(-all_of(yVarStr))
trainY <- trainData %>% select( all_of(yVarStr))
testY  <-  testData %>% select( all_of(yVarStr))

## Imputation Step
preProcess_missingdata_model <- preProcess(trainData, method='knnImpute')
trainData = predict(preProcess_missingdata_model, trainData)


## One-Hot Encoding Step
form = reformulate(c("age", "relationship", "race"))
dummies_model <- dummyVars(form, data=trainX)
trainData = predict(dummies_model, trainData) %>% data.frame()


## Pre-processing (scaling) step
preProcess_range_model <- preProcess(trainData, method='range')
trainData = predict(preProcess_range_model, trainData)

## Put Y back in
trainData = trainData %>% bind_cols(trainY)

## Pre-process test data in the same way
testData <- adult %>%
  slice(-trainRowNumbers) %>%
  predict(preProcess_missingdata_model, .) %>%
  predict(dummies_model, .) %>%
  data.frame() %>%
  predict(preProcess_range_model, .) %>%
  bind_cols(testY)

### MODELING

## CARET
default_lm_mod = train(
  form = reformulate(names(trainData), response = yVarStr),
  data = trainData,
  trControl = trainControl(method = "repeatedcv", number = 3, repeats = 2),
  method = "lm"
)

xgb_mod = train(
  form = reformulate(setdiff(names(trainData), yVarStr), response = yVarStr),
  data = trainData,
  trControl = trainControl(method = "repeatedcv", number = 3, repeats = 2),
  method = "xgbTree"
)

preds = predict(default_lm_mod, newdata = testData)
preds = predict(xgb_mod, newdata = testData)
actuals = testData[[yVarStr]]
(MSE = sum((preds-actuals)^2)/length(preds))

### FEATURE SELECTION
# subsets <- c(1:5, 10, 15, 18)
subsets <- c(1, 3, 6)

ctrl <- rfeControl(functions = rfFuncs,
                   method = "repeatedcv",
                   repeats = 5,
                   verbose = FALSE)

# lmProfile <- rfe(x=trainX, 
#                  y=trainData[[yVarStr]],
#                  sizes = subsets,
#                  rfeControl = ctrl)

lmProfile

### TUNING
