library(shiny)
library(shinydashboard)
# library(plotly)

webPageTitle = "DataPOC Exploration Tool"

## Icons here:
# http://fontawesome.io/icons/
# http://getbootstrap.com/components/#glyphicons

## How menu items and dashboards work
# https://rstudio.github.io/shinydashboard/behavior.html
# https://github.com/rstudio/shinydashboard/issues/114

## Key bindings
# http://www.foreui.com/articles/Key_Code_Table.htm
# https://stackoverflow.com/questions/24973549/r-shiny-key-input-binding

## Debugging
# https://shiny.rstudio.com/articles/debugging.html

# tweaks: a list object to set up multicols for checkboxGroupInput AND keep track of recently pressed button
tweaks <- 
  list(tags$head(tags$style(HTML("
                                 .multicol {
                                   height: auto;
                                   -webkit-column-count: 3; /* Chrome, Safari, Opera */ 
                                   -moz-column-count: 3;    /* Firefox */ 
                                   column-count: 3; 
                                   -moz-column-fill: auto;
                                   -column-fill: auto;
                                 } 
                                 ")) 
  ), 
  tags$head(tags$script(HTML("$(document).on('click', '.needed', function () {
                                Shiny.onInputChange('last_btn',this.id);
                             });")))
  )


UIFacColWidth = 4
UINumColWidth = 3
UIDatColWidth = 3
UIBinColWidth = 2

header = dashboardHeader(
  title = span(
    tags$img(height = "45px", alt= "SNAP Logo", src='BSClogoTrans.png')
    , HTML(paste(webPageTitle, "<sup> BETA </sup>"))) # <sub>v0.1</sub>
  , titleWidth = 500
  , dropdownMenu(type = "messages", icon = icon("info-circle"), 
                 messageItem(from = "Nigerian Prince", 
                             message = tags$div("Investment Opportunity"), icon = icon(NULL)   )
  ) # END dropdown
  
) # END Header 


sidebar = dashboardSidebar(
  tags$head(tags$style(HTML('
                            /* menu */
                            .skin-purple .sidebar-menu > li.active > a {
                            background-color: #545096;
                            }
                            #                           '))), # https://www.quackit.com/css/css_color_codes.cfm
  
  # width = 450,
  
  sidebarMenu(id= "menu"
              # , menuItem("menu1",  tabName = "menu1",  icon = icon("random"), expandedName = "", startExpanded = F) # End menuItem
              
              , selectInput("dataSource", "Choose data source:", choices = sub(".rds", "", list.files(pattern = "rds")))
              
              , div(style = "margin-top:-2em", checkboxInput("iRemIDs",    "Remove IDs",   TRUE))
              , div(style = "margin-top:-2em", checkboxInput("iRemDates",  "Remove Dates", TRUE))
              # , div(style = "margin-top:-2em", checkboxInput("iRemFuture", "Remove Future vars (TBC)", TRUE))
              
              , fluidRow(
                  column(6, numericInput("predCompLimit", "Feature min completion %", 0, 0, 100, 10))
                , column(6, numericInput("numLevels", "Collapse factor levels", 10, 1, 30, 1))
              )
              
              , fluidRow(
                column(6, selectInput("filterDataSample", "Sample", c(1000, 10000, 100000, "All"), 10000))
                ,column(6, h3("N = ", textOutput("dataNtotal", inline= T)))
              )
              
              , uiOutput("filterDataVars1")
              , uiOutput("filterDataLevels1")
              , fluidRow(column(6, uiOutput("filterDataMin1"))
                        ,column(6, uiOutput("filterDataMax1")))
              

              , conditionalPanel("input.filterDataVar1 !== 'None'", uiOutput("filterDataVars2"))
              , uiOutput("filterDataLevels2")
              , fluidRow(column(6, uiOutput("filterDataMin2"))
                         ,column(6, uiOutput("filterDataMax2")))
              
              , conditionalPanel("input.filterDataVar2 !== 'None'", uiOutput("filterDataVars3"))
              , uiOutput("filterDataLevels3")
              , fluidRow(column(6, uiOutput("filterDataMin3"))
                         ,column(6, uiOutput("filterDataMax3")))
              
              , h1("Modeling")
              
              , h3("N = ", textOutput("dataN", inline= T))
              , h4("% of Data Used", gaugeOutput("gaugeN"))
              
              , checkboxInput("iScaling", "Scale Inputs", FALSE)
              , checkboxInput("iImpute", "Impute Missing Data", FALSE)
              , sliderInput("testPerc", "Test %", 0, 100, 20, 1)
              # , radioButtons("imbalance", "Imbalance Adjust: (TBC)", choices = c("Nothing", "Down-sampling", "Up-sampling"), selected = "Nothing")
              

              , verbatimTextOutput("testText1") # Testing - remove
              , verbatimTextOutput("testText2") # Testing - remove
              , verbatimTextOutput("testText3") # Testing - remove
              , verbatimTextOutput("testText4") # Testing - remove
), width = 300) # END Sidebar 

body = dashboardBody(
  tags$head(
    tags$style(
      HTML(".shiny-notification {position: fixed; top: calc(80%); left: calc(50%); font-size: 32px;}")
    )), 
  
  tabPanel(""
           , tabsetPanel(id = "tabs",
                         tabPanel("Univariate",
                                  fluidPage(
                                    fluidRow(
                                      column(UIFacColWidth, uiOutput("selectFac") )
                                      ,column(UINumColWidth, uiOutput("selectNum") )
                                      ,column(UIDatColWidth, uiOutput("selectDat") )
                                      ,column(UIBinColWidth, uiOutput("selectBin") )
                                    ) # END row
                                    
                                    ,fluidRow(
                                      column(UIFacColWidth, 
                                             fluidRow(
                                               column(3, numericInput("maxLabelUni", "Max Length", 20, 6, 40, 2))
                                               ,column(3, numericInput("maxBarsUni", "Max Bars", 8, 2, 40, 2))
                                               
                                               ,column(3, div(style = "margin-top:-0em", checkboxInput("iParetoUni", "Pareto order",  value= TRUE))
                                                       , div(style = "margin-top:-1em", checkboxInput("iLogBarUni", "log transform", value= FALSE)))
                                               
                                               ,column(3, div(style = "margin-top:-0em", checkboxInput("iRotateXUni", "Rotate labels", value= FALSE))
                                                       , div(style = "margin-top:-1em", checkboxInput("iLabelBarUni", "Add labels", value= FALSE)))
                                             ) # END row
                                      ) # END col
                                      
                                      ,column(UINumColWidth, 
                                              fluidRow(
                                                column(6, numericInput("numBinsHistUni", "Hist Bins", 30, 6, 50, 4))
                                                ,column(6, checkboxInput("iLogHistUni", "log transform", value= FALSE))
                                              )
                                      ) # END col
                                      ,column(UIBinColWidth, offset = UIDatColWidth, h3("Percent Yes"))
                                    ) # END row above graphs
                                    
                                    ,fluidRow(
                                      column(UIFacColWidth, plotOutput("plotBar"), downloadButton("downPlotBar", "Download Plot", style="color: #fff; background-color: #4C4CB2; border-color: #2e6da4"))
                                      ,column(UINumColWidth, plotOutput("plotHist"), downloadButton("downPlotHist", "Download Plot", style="color: #fff; background-color: #4C4CB2; border-color: #2e6da4"))
                                      ,column(UIDatColWidth, plotOutput("plotDate"))
                                      ,column(UIBinColWidth, gaugeOutput("plotPie"))
                                    ) # END row
                                    
                                    ,fluidRow(
                                      column(UIFacColWidth, h3("Percent Complete", gaugeOutput("gaugeFac")))
                                      ,column(UINumColWidth, h3("Percent Complete", gaugeOutput("gaugeNum")))
                                      ,column(UIDatColWidth, h3("Percent Complete", gaugeOutput("gaugeDat")))
                                      ,column(UIBinColWidth, h3("Percent Complete", gaugeOutput("gaugeBin")))
                                    ) # END row
                                    
                                  ) # END FluidPage
                         ) # END Tab Uni
                         
                         , tabPanel("Bivariate",
                                    fluidPage(
                                      fluidRow(
                                        column(10 
                                               ,fluidRow(
                                                 column(3, uiOutput("selectMultivarX"))
                                                 ,column(1, br(), actionButton("switchVarsBi", "", icon = icon("exchange-alt")))
                                                 ,column(3, uiOutput("selectMultivarY"))
                                                 ,column(3, uiOutput("selectMultivarG"))
                                                 # ,column(2, conditionalPanel("input.catXcatType == 'Jitter'", uiOutput("selectMultivarG"))) # TODO: Make this show up at other times too
                                               )
                                               
                                               ,fluidRow(
                                                 column(3
                                                        , h5("Avg:  ",   textOutput("summStatsAvgX",  inline = T))
                                                        , h5("SD:   ",   textOutput("summStatsSdX",   inline = T))
                                                        , h5("Skew: ",   textOutput("summStatsSkewX", inline = T))
                                                        # , h5("Kurt: ",   textOutput("summStatsKurtX", inline = T))
                                                 )
                                                 ,column(1, h5("Corr: ",   textOutput("summStatsCorr", inline = T)))
                                                 ,column(3
                                                         , h5("Avg:  ",   textOutput("summStatsAvgY",  inline = T))
                                                         , h5("SD:   ",   textOutput("summStatsSdY",   inline = T))
                                                         , h5("Skew: ",   textOutput("summStatsSkewY", inline = T))
                                                         # , h5("Kurt: ",   textOutput("summStatsKurtY", inline = T))
                                                 )
                                                 ,column(3
                                                         , radioButtons("catXcatType", "Plot Type", c("Bar", "Circle", "Tile", "Jitter"), inline = T)
                                                         , numericInput("maxLabelMulti", "Max Label Length", 20, 6, 40, 2)
                                                 )
                                               ) # END row
                                        ) # END col
                                        ,column(2, h3("Complete Data", gaugeOutput("gaugeBiComp")))
                                      ) # END row
                                      
                                      ,fluidRow(
                                        column(1, numericInput("maxLevelsMulti", "Max Levels", 8, 2, 40, 2))
                                        ,column(1, numericInput("maxGroupsMulti", "Max Groups", 4, 1, 10, 1))
                                        ,column(1, div(style = "margin-top:-0em", checkboxInput("iParetoMulti", "Pareto order",  value= TRUE))
                                                , div(style = "margin-top:-1em", checkboxInput("iLogMulti", "log transform", value= FALSE)))
                                        ,column(1, div(style = "margin-top:-0em", checkboxInput("iFreqPoly", "Polygon", value= FALSE))
                                                , div(style = "margin-top:-1em", checkboxInput("iRotateXMulti", "Rotate labels", value= FALSE)))
                                        ,column(1, numericInput("numBinsMulti", "Hist Bins", 30, 6, 50, 4))
                                        ,column(1, numericInput("cutoffMulti", "Numeric Cutoff", 100000, 0, 1000000, 100))
                                        ,column(1, div(style = "margin-top:-0em", checkboxInput("iLinearMulti", "Linear fit",  value= FALSE))
                                                , div(style = "margin-top:-1em", checkboxInput("iXYMulti", "Y = X", value= FALSE)))
                                        ,column(1, checkboxInput("iJitterMulti", "Jitter", value= FALSE))
                                        ,column(1, checkboxInput("iShowCorrs", "Corr Plot", value= FALSE))
                                      )
                                      
                                      ,fluidRow(
                                        column(8, plotOutput("plotMulti", height = 600),
                                               downloadButton("downPlotMulti", "Download Plot",
                                                              style="color: #fff; background-color: #4C4CB2; border-color: #2e6da4"))
                                        ,column(4, plotOutput("plotCorr"))
                                      ) # END row
                                      
                                    ) # END FluidPage
                                    
                         ) # END Tab Multi
                         
                         
                         , tabPanel("Longitudinal",
                                    fluidPage(
                                      fluidRow(
                                        column(2, uiOutput("selectLonXvar"))
                                        ,column(2, uiOutput("selectLonYvar"))
                                        ,column(1, uiOutput("selectLonIdVar"))
                                        ,column(2, uiOutput("selectLonGroupVar"))
                                        ,column(1, br(), actionButton("switchVarsLon", "", icon = icon("exchange-alt")))
                                        ,column(2, uiOutput("selectLonFacetVar"))
                                        ,column(1, 
                                                checkboxInput("lonAggFlag", "Aggregate", value= FALSE)
                                                ,checkboxInput("lonErrorBarFlag", "Error bars", value= FALSE)
                                                ,checkboxInput("lonFreeScaleFlag", "Free Scales", value= FALSE)
                                        )
                                        ,column(1, 
                                                conditionalPanel("input.lonAggFlag == true", 
                                                                 radioButtons("lonAggType", "Aggregation", c("sum", "mean", "median", "min", "max"))
                                                )
                                        )
                                      ) # END row
                                      
                                      ,plotOutput("plotTime", height = 800)
                                      
                                    ) # END FluidPage
                                    
                         ) # END Tab Longitudinal
                         
                         
                         , tabPanel("Classification",
                                    fluidPage(
                                      sidebarLayout(
                                        
                                        sidebarPanel(tweaks
                                                     , h3("Predictors / Features")
                                                     , uiOutput("inputVarsClass")
                                                     , fluidRow(actionButton("allPredsClass",   "Select All Predictors", class= "needed", icon = icon("check-square")))
                                                     , fluidRow(actionButton("noPredsClass",    "Clear All Predictors",  class= "needed", icon = icon("minus-square")))
                                                     , fluidRow(actionButton("luckyPredsClass", "I'm Feeling Lucky",     class= "needed", icon = icon("bitcoin")))
                                                     , width = 3),
                                        
                                        
                                        mainPanel(
                                          fluidPage(
                                            fluidRow(
                                              column(3
                                                     ,uiOutput("responseClassOut")
                                                     ,actionButton("fitClassModel", "Fit Model", 
                                                                   icon = icon("chart-line"),
                                                                   style= paste0("color: #FFF; background-color: #545096"))
                                              )
                                              ,column(3, radioButtons("classModelType", "Model:", modelLookup$modelNameUI[modelLookup$modelMode == "Class"]))
                                            
                                            
                                            ,column(2 
                                                    ,numericInput("thresholdClass", "Prediction Threshold", 0.5, 0, 1, 0.02)
                                                    ,checkboxInput("iOptimalThreshold", "Optimal Threshold", FALSE)
                                                    ,checkboxInput("iShowPlots", "Show ROC / PR plots", FALSE)
                                            )
                                            # ,column(2, radioButtons("effectTypeClass", "Plot Effects", 
                                            #                         choices= c("Importance", "Interactions", "Shapley", "ALE (by var)", "Interactions (by var)")))
                                            # ,column(2
                                            #         , uiOutput("effectPredClassOut")
                                            #         , numericInput("topNEffectsClass", "Limit effects to:", 10, 2, 20, 1)
                                            # )
                                          ) # END row
                                          
                                          # , fluidRow(   column(12, withSpinner(  plotOutput("effectPlotClass")  ))   ) # END row
                                          
                                          , fluidRow(
                                            column(2, h3("Accuracy",    gaugeOutput("gaugeAcc")))
                                            ,column(2, h3("Recall",      gaugeOutput("gaugeRecall")))
                                            ,column(2, h3("Precision",   gaugeOutput("gaugePrec")))
                                            
                                            ,column(5, plotOutput("classProbs", height = "200px"))
                                          ) # END row
                                          
                                          , fluidRow(
                                            column(2, h3("Model Skill", gaugeOutput("gaugeSkillClass")))
                                            # ,column(2, h3("F-Score",     gaugeOutput("gaugeFscore")))
                                            ,column(2, h3("AUC",         gaugeOutput("gaugeAUC")))
                                            ,column(2, h3(HTML("F<sub>&alpha;</sub>-Score"),  gaugeOutput("gaugeFalphaScore")))
                                            ,column(1, numericInput("fAlpha", "", 0.5, 0.1, 0.9, 0.1))
                                            
                                          ) # END row
                                          
                                          # , fluidRow(
                                          #    column(2, h3("MCC",         gaugeOutput("gaugeMCC")))
                                          #   ,column(2, h3("Informedness",gaugeOutput("gaugeInform")))
                                          #   ,column(2, h3("Markedness",  gaugeOutput("gaugeMarked")))
                                          #   
                                          # ) # END row
                                          
                                          , fluidRow(
                                            column(4, plotOutput("plotROC")    )
                                            , column(4, plotOutput("plotPR" )    )
                                            , column(4, plotOutput("plotFscore" )    )
                                          ) # END row
                                          
                                        ) # end fluidPage
                                      ) # END mainPanel
                                    ) # END sidebarLayout
                         ) # END FluidPage
           ) # END Tab Classification
           
           
           , tabPanel("Regression",
                      fluidPage(
                        sidebarLayout(
                          
                          sidebarPanel(tweaks
                                       , h3("Predictors / Features")
                                       , uiOutput("inputVarsReg")
                                       
                                       
                                       , fluidRow(actionButton("allPredsReg",   "Select All Predictors", class= "needed", icon= icon("check-square")))
                                       , fluidRow(actionButton("noPredsReg",    "Clear All Predictors",  class= "needed", icon= icon("minus-square")))
                                       , fluidRow(actionButton("luckyPredsReg", "I'm Feeling Lucky",     class= "needed", icon= icon("bitcoin")))
                                       , width = 3),
                          
                          
                          mainPanel(
                            fluidPage(
                              fluidRow(
                                column(3
                                       ,uiOutput("responseRegOut")
                                       ,actionButton("fitRegModel", "Fit Model", 
                                                     icon = icon("chart-line"),
                                                     style= paste0("color: #FFF; background-color: #545096"))
                                       
                                       ,checkboxInput("iRemNonPos", "Remove <= 0", FALSE)
                                       ,div(style = "margin-top:-1em", checkboxInput("iForecast", "Forecast?", FALSE))
                                       ,div(style = "margin-top:-1em", checkboxInput("iLogYReg", "Log Y", FALSE))
                                )
                                ,column(3, radioButtons("regModelType", "Model:", modelLookup$modelNameUI[modelLookup$modelMode == "Reg"]))
                                
                                # ,column(2 
                                #         , selectInput('numTreesReg', label = "Trees",
                                #                       choices= c("50", "100", "200", "500", "1000", "2000"), selected = "100")
                                #         , numericInput("topNEffectsReg", "Limit effects to:", 10, 2, 20, 1)
                                # ) # END col
                                # ,column(2, radioButtons("effectTypeReg", "Plot Effects", choices= c("Importance", "Interactions", "Shapley", "ALE (by var)", "Interactions (by var)")))
                                ,column(2, uiOutput("effectPredRegOut"))
                                
                              ) # END row
                              ,fluidRow(
                                column(2, h3("MSE",  textOutput("mseReg")))#,  h3("Model Skill", gaugeOutput("gaugeSkillRegMSE"))
                                ,column(2, h3("MAPE", textOutput("mapeReg")))#, h3("Model Skill", gaugeOutput("gaugeSkillRegMAPE"))
                              )
                              
                              # , fluidRow(   column(12, withSpinner(  plotOutput("effectPlotReg")  ))   ) # END row
                              
                              
                              # , fluidRow(
                              #   column(2, radioButtons("compareSkillReg", "Compare against:", c("Historical Avg", "Historical Median", "Normal Dist", "Bootstrap Dist")))
                              #   ,column(2, h3("Model Skill", gaugeOutput("gaugeSkillReg")))
                              #   ,column(2, h3("R2", gaugeOutput("gaugeR2")))
                              #   ,column(2, h3("R2 Adjusted", gaugeOutput("gaugeR2adj")))
                              #   ,column(2, h3("Confidence", gaugeOutput("gaugeConf")))
                              # ) # END row
                              
                              # infoBoxOutput("pvalAlert")
                              # infoBoxOutput("vifAlert")
                              # , fluidRow(column(12, align = "center", h3("Correlation Matrix")))
                              # withSpinner(  plotOutput("corrplot")  )
                              # withSpinner(  plotOutput("vifPlot")  )
                              # withSpinner(  plotOutput("pvalPlot")  )
                              
                            ) # end fluidPage
                          ) # END mainPanel
                        ) # END sidebarLayout
                      ) # END FluidPage
           ) # END Tab Regression
                      
                         
                        
                         , selected = "Univariate"
                         
                         ) # END tabsetPanel
           ) # END tabPanel MAIN
) # END Body

dashboardPage(header, sidebar, body, skin = "purple", title = HTML(webPageTitle))
