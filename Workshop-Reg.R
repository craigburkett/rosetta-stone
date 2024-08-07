library(tidyverse)
library(caret)

adult = readRDS("adult.rds") %>% mutate(over50K_Flag = factor(paste0("Class_", over50K_Flag)))
yVarStr = "hrsPerWeek"

trainRowNumbers = createDataPartition(adult[[yVarStr]], p=0.8, list=FALSE)

trainData = adult %>% slice(as.numeric(trainRowNumbers))
testData  = adult %>% slice(-as.numeric(trainRowNumbers))
trainX <- trainData %>% select(-all_of(yVarStr))
trainY <- trainData %>% select( all_of(yVarStr))
testY  <-  testData %>% select( all_of(yVarStr))

## Imputation Step
preProcess_missingdata_model <- preProcess(trainData, method='knnImpute')
trainData = predict(preProcess_missingdata_model, trainData)


## One-Hot Encoding Step
form = reformulate(c("age", "sex", "educationNum"))
dummies_model <- dummyVars(form, data=trainX)
trainData = predict(dummies_model, trainData) %>% data.frame()


## Pre-processing (scaling) step
preProcess_range_model <- preProcess(trainData, method='range')
trainData = predict(preProcess_range_model, trainData)

## Put Y back in
trainData = trainData %>% bind_cols(trainY)

## Pre-process test data in the same way
testData <- adult %>%
  slice(-as.numeric(trainRowNumbers)) %>%
  predict(preProcess_missingdata_model, .) %>%
  predict(dummies_model, .) %>%
  data.frame() %>%
  predict(preProcess_range_model, .) %>%
  bind_cols(testY)

#### MODELING

### CARET
## GLM
default_glm_mod = train(
  form = reformulate(setdiff(names(trainData), yVarStr), response = yVarStr),
  data = trainData,
  trControl = trainControl(method = "repeatedcv", number = 3, repeats = 2),
  method = "glm"
)

preds = predict(default_glm_mod, newdata = testData)
actuals = testData[[yVarStr]]
(mape = mean(abs((actuals - preds)/actuals)) * 100)

## kNN
knn_model = train(
  form = reformulate(setdiff(names(trainData), yVarStr), response = yVarStr),
  data = trainData[1:1000,],
  trControl = trainControl(method = "repeatedcv", number = 3, repeats = 2),
  method = "knn"
)

preds = predict(knn_model, newdata = testData)
actuals = testData[[yVarStr]]
(mape = mean(abs((actuals - preds)/actuals)) * 100)

## RF
rf_model = train(
  form = reformulate(setdiff(names(trainData), yVarStr), response = yVarStr),
  data = trainData[1:1000,],
  trControl = trainControl(method = "repeatedcv", number = 3, repeats = 2),
  method = "rf"
)

preds = predict(rf_model, newdata = testData)
actuals = testData[[yVarStr]]
yesClass = "Class_TRUE"
probs = predict(rf_model, newdata = testData, type = "prob")[[yesClass]]
propPos = 0.5

get_class_stats(preds, actuals, probs, yesClass, propPos)


## XGBoost
xgb_model = train(
  form = reformulate(setdiff(names(trainData), yVarStr), response = yVarStr),
  data = trainData,
  trControl = trainControl(method = "repeatedcv", number = 3, repeats = 2),
  method = "xgbTree"
)

preds = predict(xgb_model, newdata = testData) # Error here, different var names
actuals = testData[[yVarStr]]
yesClass = "Class_TRUE"
probs = predict(xgb_model, newdata = testData, type = "prob")[[yesClass]]
propPos = 0.5

get_class_stats(preds, actuals, probs, yesClass, propPos)


### FEATURE SELECTION
# subsets <- c(1:5, 10, 15, 18)
subsets <- c(1, 3, 6)

ctrl <- rfeControl(functions = rfFuncs,
                   method = "repeatedcv",
                   repeats = 5,
                   verbose = FALSE)

lmProfile <- rfe(x=trainX, 
                 y=trainData[[yVarStr]],
                 sizes = subsets,
                 rfeControl = ctrl)

lmProfile

### TUNING
