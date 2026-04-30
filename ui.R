shinyUI(navbarPage(
  title = "Assignment 3 - Jesse Pilcher | 19717404",
  
  
  
  tabPanel("Data",
           verbatimTextOutput(outputId = "DataSummary"),
           fluidRow(
             column(width = 4,
                    sliderInput(inputId = "Multiplier", label = "IQR multiplier", min = 0, max = 10, step = 0.1, value = 1.5)
             ),
             column(width = 3,
                    checkboxInput(inputId = "Normalise", label = "Standardise chart", value = TRUE)
             )
           ),
           plotOutput(outputId = "BoxPlots"),
           plotOutput(outputId = "Missing"),
           plotOutput(outputId = "Corr"),
           DT::dataTableOutput(outputId = "Table")
  ),
  
  tabPanel("Split",
           sliderInput(inputId = "Split", label = "Train proportion", min = 0, max = 1, value = 0.8),
           verbatimTextOutput(outputId = "SplitSummary")
  ),
  
  tabPanel("Available methods",
           h3("Regression methods in caret"),
           shinycssloaders::withSpinner(DT::dataTableOutput(outputId = "Available"))
  ),
  
  tabPanel("Methods",
           checkboxInput(inputId = "Parallel", label = "Use parallel processing", value = TRUE),
           bsTooltip(id = "Parallel", title = paste("This will utilise all", detectCores(), "available CPUs during training")),
           "The preprocessing steps and their order are important.",
           HTML("See function <code>dynamicSteps</code> in global.R for interpretation of preprocessing options. "),
           "Documentation", tags$a("here", href = "https://www.rdocumentation.org/packages/recipes/versions/0.1.16", target = "_blank"),
           
           tabsetPanel(type = "pills",
                       
                       tabPanel("NULL Model",
                                br(),
                                fluidRow(
                                  column(width = 4),
                                  column(width = 1,
                                         actionButton(inputId = "null_Go", label = "Train", icon = icon("play")),
                                         bsTooltip(id = "null_Go", title = "This will train or retrain your model (and save it)")
                                  ),
                                  column(width = 1,
                                         actionButton(inputId = "null_Load", label = "Load", icon = icon("file-arrow-up")),
                                         bsTooltip(id = "null_Load", title = "This will reload your saved model")
                                  ),
                                  column(width = 1,
                                         actionButton(inputId = "null_Delete", label = "Forget", icon = icon("trash-can")),
                                         bsTooltip(id = "null_Delete", title = "This will remove your model from memory")
                                  )
                                ),
                                hr(),
                                h3("Resampled performance:"),
                                tableOutput(outputId = "null_Metrics")
                       ),
                       
                       tabPanel("Linear Regression",
                                tabsetPanel(
                                  type = "pills",
                                  
                                  tabPanel("GLMnet Model",
                                           verbatimTextOutput(outputId = "glmnet_MethodSummary"),
                                           fluidRow(
                                             column(width = 4,
                                                    selectizeInput(
                                                      inputId = "glmnet_Preprocess",
                                                      label = "Pre-processing",
                                                      choices = unique(c(glmnet_initial, ppchoices)),
                                                      multiple = TRUE,
                                                      selected = glmnet_initial
                                                    ),
                                                    bsTooltip(
                                                      id = "glmnet_Preprocess",
                                                      title = "These entries will be populated in the correct order from a saved model once it loads",
                                                      placement = "top"
                                                    )
                                             ),
                                             column(width = 1,
                                                    actionButton(inputId = "glmnet_Go", label = "Train", icon = icon("play")),
                                                    bsTooltip(id = "glmnet_Go", title = "This will train or retrain your model (and save it)")
                                             ),
                                             column(width = 1,
                                                    actionButton(inputId = "glmnet_Load", label = "Load", icon = icon("file-arrow-up")),
                                                    bsTooltip(id = "glmnet_Load", title = "This will reload your saved model")
                                             ),
                                             column(width = 1,
                                                    actionButton(inputId = "glmnet_Delete", label = "Forget", icon = icon("trash-can")),
                                                    bsTooltip(id = "glmnet_Delete", title = "This will remove your model from memory")
                                             )
                                           ),
                                           hr(),
                                           h3("Resampled performance:"),
                                           tableOutput(outputId = "glmnet_Metrics"),
                                           hr(),
                                           h3("Hyperparameter Tuning:"),
                                           plotOutput(outputId = "glmnet_ModelTune"),
                                           hr(),
                                           h3("Recipe:"),
                                           htmlOutput(outputId = "glmnet_RecipePrint"),
                                           h3("Outputs"),
                                           tableOutput(outputId = "glmnet_RecipeOutput"),
                                           fluidRow(
                                             column(width = 6,
                                                    h3("Training Summary:"),
                                                    verbatimTextOutput(outputId = "glmnet_TrainSummary")
                                             ),
                                             column(width = 6,
                                                    h3("Coefficients"),
                                                    wellPanel(
                                                      tableOutput(outputId = "glmnet_Coef")
                                                    )
                                             )
                                           )
                                  ),
                                  
                                  tabPanel("PLS Model",
                                           verbatimTextOutput(outputId = "pls_MethodSummary"),
                                           fluidRow(
                                             column(width = 4,
                                                    selectizeInput(
                                                      inputId = "pls_Preprocess",
                                                      label = "Pre-processing",
                                                      choices = unique(c(pls_initial, ppchoices)),
                                                      multiple = TRUE,
                                                      selected = pls_initial
                                                    ),
                                                    bsTooltip(
                                                      id = "pls_Preprocess",
                                                      title = "These entries will be populated in the correct order from a saved model once it loads",
                                                      placement = "top"
                                                    )
                                             ),
                                             column(width = 1,
                                                    actionButton(inputId = "pls_Go", label = "Train", icon = icon("play")),
                                                    bsTooltip(id = "pls_Go", title = "This will train or retrain your model (and save it)")
                                             ),
                                             column(width = 1,
                                                    actionButton(inputId = "pls_Load", label = "Load", icon = icon("file-arrow-up")),
                                                    bsTooltip(id = "pls_Load", title = "This will reload your saved model")
                                             ),
                                             column(width = 1,
                                                    actionButton(inputId = "pls_Delete", label = "Forget", icon = icon("trash-can")),
                                                    bsTooltip(id = "pls_Delete", title = "This will remove your model from memory")
                                             )
                                           ),
                                           hr(),
                                           h3("Resampled performance:"),
                                           tableOutput(outputId = "pls_Metrics"),
                                           hr(),
                                           h3("Hyperparameter Tuning:"),
                                           plotOutput(outputId = "pls_ModelTune"),
                                           hr(),
                                           h3("Recipe:"),
                                           htmlOutput(outputId = "pls_RecipePrint"),
                                           h3("Outputs"),
                                           tableOutput(outputId = "pls_RecipeOutput"),
                                           fluidRow(
                                             column(width = 6,
                                                    h3("Training Summary:"),
                                                    verbatimTextOutput(outputId = "pls_TrainSummary")
                                             ),
                                             column(width = 6,
                                                    h3("Coefficients"),
                                                    wellPanel(
                                                      tableOutput(outputId = "pls_Coef")
                                                    )
                                             )
                                           )
                                  ),
                                  
                                  tabPanel("Stepwise OLS Regression",
                                           verbatimTextOutput(outputId = "stepwise_MethodSummary"),
                                           fluidRow(
                                             column(width = 4,
                                                    selectizeInput(
                                                      inputId = "stepwise_Preprocess",
                                                      label = "Pre-processing",
                                                      choices = unique(c(default_initial, ppchoices)),
                                                      multiple = TRUE,
                                                      selected = default_initial
                                                    ),
                                                    bsTooltip(
                                                      id = "stepwise_Preprocess",
                                                      title = "These entries will be populated in the correct order from a saved model once it loads",
                                                      placement = "top"
                                                    )
                                             ),
                                             column(width = 1,
                                                    actionButton(inputId = "stepwise_Go", label = "Train", icon = icon("play")),
                                                    bsTooltip(id = "stepwise_Go", title = "This will train or retrain your model (and save it)")
                                             ),
                                             column(width = 1,
                                                    actionButton(inputId = "stepwise_Load", label = "Load", icon = icon("file-arrow-up")),
                                                    bsTooltip(id = "stepwise_Load", title = "This will reload your saved model")
                                             ),
                                             column(width = 1,
                                                    actionButton(inputId = "stepwise_Delete", label = "Forget", icon = icon("trash-can")),
                                                    bsTooltip(id = "stepwise_Delete", title = "This will remove your model from memory")
                                             )
                                           ),
                                           hr(),
                                           h3("Resampled performance:"),
                                           tableOutput(outputId = "stepwise_Metrics"),
                                           hr(),
                                           h3("Recipe:"),
                                           htmlOutput(outputId = "stepwise_RecipePrint"),
                                           h3("Outputs"),
                                           tableOutput(outputId = "stepwise_RecipeOutput"),
                                           fluidRow(
                                             column(width = 6,
                                                    h3("Training Summary:"),
                                                    verbatimTextOutput(outputId = "stepwise_TrainSummary")
                                             ),
                                             column(width = 6,
                                                    h3("Coefficients"),
                                                    wellPanel(
                                                      tableOutput(outputId = "stepwise_Coef")
                                                    )
                                             )
                                           )
                                  ),
                                  
                                  tabPanel("Principal Component Regression", #method = pcr
                                           verbatimTextOutput(outputId = "pcr_MethodSummary"),
                                           fluidRow(
                                             column(width = 4,
                                                    selectizeInput(
                                                      inputId = "pcr_Preprocess",
                                                      label = "Pre-processing",
                                                      choices = unique(c(default_initial, ppchoices)),
                                                      multiple = TRUE,
                                                      selected = default_initial
                                                    ),
                                                    bsTooltip(
                                                      id = "pcr_Preprocess",
                                                      title = "These entries will be populated in the correct order from a saved model once it loads",
                                                      placement = "top"
                                                    )
                                             ),
                                             column(width = 1,
                                                    actionButton(inputId = "pcr_Go", label = "Train", icon = icon("play")),
                                                    bsTooltip(id = "pcr_Go", title = "This will train or retrain your model (and save it)")
                                             ),
                                             column(width = 1,
                                                    actionButton(inputId = "pcr_Load", label = "Load", icon = icon("file-arrow-up")),
                                                    bsTooltip(id = "pcr_Load", title = "This will reload your saved model")
                                             ),
                                             column(width = 1,
                                                    actionButton(inputId = "pcr_Delete", label = "Forget", icon = icon("trash-can")),
                                                    bsTooltip(id = "pcr_Delete", title = "This will remove your model from memory")
                                             )
                                           ),
                                           hr(),
                                           h3("Resampled performance:"),
                                           tableOutput(outputId = "pcr_Metrics"),
                                           hr(),
                                           h3("Hyperparameter Tuning:"),
                                           plotOutput(outputId = "pcr_ModelTune"),
                                           hr(),
                                           h3("Recipe:"),
                                           htmlOutput(outputId = "pcr_RecipePrint"),
                                           h3("Outputs"),
                                           tableOutput(outputId = "pcr_RecipeOutput"),
                                           fluidRow(
                                             column(width = 6,
                                                    h3("Training Summary:"),
                                                    verbatimTextOutput(outputId = "pcr_TrainSummary")
                                             ),
                                             column(width = 6,
                                                    h3("Coefficients"),
                                                    wellPanel(
                                                      tableOutput(outputId = "pcr_Coef")
                                                    )
                                             )
                                           )
                                  )#end of PCR TAB
                                  )#End of subset panels
                                  ),#end of linear regression tab
                                
                       
                       tabPanel("Decision Trees",
                                tabsetPanel(
                                  type = "pills",
                                  
                                  tabPanel("Rpart Model",
                                           verbatimTextOutput(outputId = "rpart_MethodSummary"),
                                           fluidRow(
                                             column(width = 4,
                                                    selectizeInput(
                                                      inputId = "rpart_Preprocess",
                                                      label = "Pre-processing",
                                                      choices = unique(c(rpart_initial, ppchoices)),
                                                      multiple = TRUE,
                                                      selected = rpart_initial
                                                    ),
                                                    bsTooltip(id = "rpart_Preprocess", title = "These entries will be populated in the correct order from a saved model once it loads", placement = "top")
                                             ),
                                             column(width = 1,
                                                    actionButton(inputId = "rpart_Go", label = "Train", icon = icon("play")),
                                                    bsTooltip(id = "rpart_Go", title = "This will train or retrain your model (and save it)")
                                             ),
                                             column(width = 1,
                                                    actionButton(inputId = "rpart_Load", label = "Load", icon = icon("file-arrow-up")),
                                                    bsTooltip(id = "rpart_Load", title = "This will reload your saved model")
                                             ),
                                             column(width = 1,
                                                    actionButton(inputId = "rpart_Delete", label = "Forget", icon = icon("trash-can")),
                                                    bsTooltip(id = "rpart_Delete", title = "This will remove your model from memory")
                                             )
                                           ),
                                           hr(),
                                           h3("Resampled performance:"),
                                           tableOutput(outputId = "rpart_Metrics"),
                                           hr(),
                                           h3("Hyperparameter Tuning:"),
                                           plotOutput(outputId = "rpart_ModelTune"),
                                           hr(),
                                           h3("Model tree:"),
                                           plotOutput(outputId = "rpart_ModelTree"),
                                           hr(),
                                           h3("Recipe:"),
                                           htmlOutput(outputId = "rpart_RecipePrint"),
                                           h3("Outputs"),
                                           tableOutput(outputId = "rpart_RecipeOutput"),
                                           fluidRow(
                                             column(width = 6,
                                                    h3("Training Summary:"),
                                                    verbatimTextOutput(outputId = "rpart_TrainSummary")
                                             )
                                           )
                                  ),
                                  
                                  tabPanel("Gradient Boosted", #method = xgbTree
                                           verbatimTextOutput(outputId = "xgbTree_MethodSummary"),
                                           fluidRow(
                                             column(width = 4,
                                                    selectizeInput(
                                                      inputId = "xgbTree_Preprocess",
                                                      label = "Pre-processing",
                                                      choices = unique(c(default_initial, ppchoices)),
                                                      multiple = TRUE,
                                                      selected = default_initial
                                                    ),
                                                    bsTooltip(id = "xgbTree_Preprocess", title = "These entries will be populated in the correct order from a saved model once it loads", placement = "top")
                                             ),
                                             column(width = 1,
                                                    actionButton(inputId = "xgbTree_Go", label = "Train", icon = icon("play")),
                                                    bsTooltip(id = "xgbTree_Go", title = "This will train or retrain your model (and save it)")
                                             ),
                                             column(width = 1,
                                                    actionButton(inputId = "xgbTree_Load", label = "Load", icon = icon("file-arrow-up")),
                                                    bsTooltip(id = "xgbTree_Load", title = "This will reload your saved model")
                                             ),
                                             column(width = 1,
                                                    actionButton(inputId = "xgbTree_Delete", label = "Forget", icon = icon("trash-can")),
                                                    bsTooltip(id = "xgbTree_Delete", title = "This will remove your model from memory")
                                             )
                                           ),
                                           hr(),
                                           h3("Resampled performance:"),
                                           tableOutput(outputId = "xgbTree_Metrics"),
                                           hr(),
                                           h3("Hyperparameter Tuning:"),
                                           plotOutput(outputId = "xgbTree_ModelTune"),
                                           hr(),
                                           h3("Variable Importance:"),
                                           plotOutput(outputId = "xgbTree_VarImp"),
                                           hr(),
                                           h3("Recipe:"),
                                           htmlOutput(outputId = "xgbTree_RecipePrint"),
                                           h3("Outputs"),
                                           tableOutput(outputId = "xgbTree_RecipeOutput"),
                                           fluidRow(
                                             column(width = 6,
                                                    h3("Training Summary:"),
                                                    verbatimTextOutput(outputId = "xgbTree_TrainSummary")
                                             )
                                           )),#gradient boosted tree tab end
                                  
                                  tabPanel("Random Forest", #method = ranger
                                           verbatimTextOutput(outputId = "ranger_MethodSummary"),
                                           fluidRow(
                                             column(width = 4,
                                                    selectizeInput(
                                                      inputId = "ranger_Preprocess",
                                                      label = "Pre-processing",
                                                      choices = unique(c(default_initial, ppchoices)),
                                                      multiple = TRUE,
                                                      selected = default_initial
                                                    ),
                                                    bsTooltip(id = "ranger_Preprocess", title = "These entries will be populated in the correct order from a saved model once it loads", placement = "top")
                                             ),
                                             column(width = 1,
                                                    actionButton(inputId = "ranger_Go", label = "Train", icon = icon("play")),
                                                    bsTooltip(id = "ranger_Go", title = "This will train or retrain your model (and save it)")
                                             ),
                                             column(width = 1,
                                                    actionButton(inputId = "ranger_Load", label = "Load", icon = icon("file-arrow-up")),
                                                    bsTooltip(id = "ranger_Load", title = "This will reload your saved model")
                                             ),
                                             column(width = 1,
                                                    actionButton(inputId = "ranger_Delete", label = "Forget", icon = icon("trash-can")),
                                                    bsTooltip(id = "ranger_Delete", title = "This will remove your model from memory")
                                             )
                                           ),
                                           hr(),
                                           h3("Resampled performance:"),
                                           tableOutput(outputId = "ranger_Metrics"),
                                           hr(),
                                           h3("Hyperparameter Tuning:"),
                                           plotOutput(outputId = "ranger_ModelTune"),
                                           hr(),
                                           h3("Variable Importance:"),
                                           plotOutput(outputId = "ranger_VarImp"),
                                           hr(),
                                           h3("Recipe:"),
                                           htmlOutput(outputId = "ranger_RecipePrint"),
                                           h3("Outputs"),
                                           tableOutput(outputId = "ranger_RecipeOutput"),
                                           fluidRow(
                                             column(width = 6,
                                                    h3("Training Summary:"),
                                                    verbatimTextOutput(outputId = "ranger_TrainSummary")
                                             )
                                           )
                                           ) #random forest tree tab end
                                  
                                  
                                )
                       ),
                       
                       tabPanel("KNN Regression", # Method = KNN
                                verbatimTextOutput(outputId = "kknn_MethodSummary"),
                                fluidRow(
                                  column(width = 4,
                                         selectizeInput(
                                           inputId = "kknn_Preprocess",
                                           label = "Pre-processing",
                                           choices = unique(c(default_initial, ppchoices)),
                                           multiple = TRUE,
                                           selected = default_initial
                                         ),
                                         bsTooltip(
                                           id = "kknn_Preprocess",
                                           title = "These entries will be populated in the correct order from a saved model once it loads",
                                           placement = "top"
                                         )
                                  ),
                                  column(width = 1,
                                         actionButton(inputId = "kknn_Go", label = "Train", icon = icon("play")),
                                         bsTooltip(id = "kknn_Go", title = "This will train or retrain your model (and save it)")
                                  ),
                                  column(width = 1,
                                         actionButton(inputId = "kknn_Load", label = "Load", icon = icon("file-arrow-up")),
                                         bsTooltip(id = "kknn_Load", title = "This will reload your saved model")
                                  ),
                                  column(width = 1,
                                         actionButton(inputId = "kknn_Delete", label = "Forget", icon = icon("trash-can")),
                                         bsTooltip(id = "kknn_Delete", title = "This will remove your model from memory")
                                  )
                                ),
                                hr(),
                                h3("Resampled performance:"),
                                tableOutput(outputId = "kknn_Metrics"),
                                hr(),
                                h3("Hyperparameter Tuning:"),
                                plotOutput(outputId = "kknn_ModelTune"),
                                hr(),
                                h3("Recipe:"),
                                htmlOutput(outputId = "kknn_RecipePrint"),
                                h3("Outputs"),
                                tableOutput(outputId = "kknn_RecipeOutput"),
                                fluidRow(
                                  column(width = 6,
                                         h3("Training Summary:"),
                                         verbatimTextOutput(outputId = "kknn_TrainSummary")
                                  ),
                                  column(width = 6,
                                         h3("Coefficients"),
                                         wellPanel(
                                           tableOutput(outputId = "kknn_Coef")
                                         )
                                  )
                                )
                                ),#knn tab end
                       
                       tabPanel("Neural Networks",
                                tabsetPanel(
                                  type = "pills",
                                  tabPanel("Radial Basis Function Network"),
                                  tabPanel("mxNet"),
                                  tabPanel("Extreme Learning Models (ELM)"),
                                  tabPanel("Multi-Layer Perceptron"),
                                  tabPanel("Bayesian Regularized Neural Net"),
                                  tabPanel("Model Averaged Neural Network" #method = avNNet
                                           )
                                )
                       ),
                       
                       tabPanel("Kernel Methods",
                                tabsetPanel(
                                  type = "pills",
                                  tabPanel("Least Squares SVM"),
                                  tabPanel("Linear Kernel"),
                                  tabPanel("Polynomial Kernel"),
                                  tabPanel("Exponential Kernel"),
                                  tabPanel("Radial Basis Kernel"),
                                  tabPanel("Spectrum Spring Kernel"),
                                  tabPanel("Gaussian Process" #method = gaussprLinear
                                           ),
                                  
                                )
                       ),
                       tabPanel("Flexible Nonlinear Models",
                                tabsetPanel(
                                  type = "pills",
                                  
                                  tabPanel("Multivariate Adaptive Regression Splines" #method = gcvEarth
                                  ),
                                  
                                  tabPanel("Boosted Generalized Additive Model" #method = gamboost
                                  ),
                                  
                                  tabPanel("Bayesian Additive Regression Trees" #method = bartMachine
                                  )
                                )
                       ),
                       
                       tabPanel("Rule-Based Models",
                                tabsetPanel(
                                  type = "pills",
                                  
                                  tabPanel("Cubist" #method = cubist
                                  ),
                                  
                                  tabPanel("Bagged Logic Regression" #method = logicBag
                                  )
                                )
                       )
                                
           )
  ),
  
  tabPanel("Model Selection",
           tags$h5("Cross validation results:"),
           checkboxInput(inputId = "Notch", label = "Show notch", value = FALSE),
           checkboxInput(inputId = "NullNormalise", label = "Normalise", value = TRUE),
           checkboxInput(inputId = "HideWorse", label = "Hide models worse than null model", value = TRUE),
           plotOutput(outputId = "SelectionBoxPlot"),
           radioButtons(inputId = "Choice", label = "Model choice", choices = c(""), inline = TRUE)
  ),
  
  tabPanel("Performance",
           htmlOutput(outputId = "Title"),
           verbatimTextOutput(outputId = "TestSummary"),
           fluidRow(
             column(offset = 2, width = 4,
                    plotOutput(outputId = "TestPlot", width = "600", height = "600")
             ),
             column(width = 2,
                    plotOutput(outputId = "TestResiduals", height = "600")
             ),
             column(width = 2,
                    plotOutput(outputId = "TrainResiduals", height = "600")
             )
           ),
           sliderInput(inputId = "IqrM", label = "IQR multiplier", min = 0, max = 5, value = 1.5, step = 0.1)
  )
))