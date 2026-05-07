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
           shinycssloaders::withSpinner(DT::dataTableOutput(outputId = "Available")),
           plotOutput("selected_methods")
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
                                                      choices = unique(c(default_initial, ppchoices)),
                                                      multiple = TRUE,
                                                      selected = default_initial
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
                                                      choices = unique(c(default_initial, ppchoices)),
                                                      multiple = TRUE,
                                                      selected = default_initial
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
                                                      selected = default_initial
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
                                  
                                  tabPanel("Gradient Boosted", #method = gbm
                                           verbatimTextOutput(outputId = "gbm_MethodSummary"),
                                           fluidRow(
                                             column(width = 4,
                                                    selectizeInput(
                                                      inputId = "gbm_Preprocess",
                                                      label = "Pre-processing",
                                                      choices = unique(c(default_initial, ppchoices)),
                                                      multiple = TRUE,
                                                      selected = default_initial
                                                    ),
                                                    bsTooltip(id = "gbm_Preprocess", title = "These entries will be populated in the correct order from a saved model once it loads", placement = "top")
                                             ),
                                             column(width = 1,
                                                    actionButton(inputId = "gbm_Go", label = "Train", icon = icon("play")),
                                                    bsTooltip(id = "gbm_Go", title = "This will train or retrain your model (and save it)")
                                             ),
                                             column(width = 1,
                                                    actionButton(inputId = "gbm_Load", label = "Load", icon = icon("file-arrow-up")),
                                                    bsTooltip(id = "gbm_Load", title = "This will reload your saved model")
                                             ),
                                             column(width = 1,
                                                    actionButton(inputId = "gbm_Delete", label = "Forget", icon = icon("trash-can")),
                                                    bsTooltip(id = "gbm_Delete", title = "This will remove your model from memory")
                                             )
                                           ),
                                           hr(),
                                           h3("Resampled performance:"),
                                           tableOutput(outputId = "gbm_Metrics"),
                                           hr(),
                                           h3("Hyperparameter Tuning:"),
                                           plotOutput(outputId = "gbm_ModelTune"),
                                           hr(),
                                           h3("Variable Importance:"),
                                           plotOutput(outputId = "gbm_VarImp"),
                                           hr(),
                                           h3("Recipe:"),
                                           htmlOutput(outputId = "gbm_RecipePrint"),
                                           h3("Outputs"),
                                           tableOutput(outputId = "gbm_RecipeOutput"),
                                           fluidRow(
                                             column(width = 6,
                                                    h3("Training Summary:"),
                                                    verbatimTextOutput(outputId = "gbm_TrainSummary")
                                             )
                                           )), # gbm tab end
                                  
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
                                  tabPanel("Averaged Neural Network", # method = avNNet
                                           verbatimTextOutput(outputId = "avNNet_MethodSummary"),
                                           fluidRow(
                                             column(
                                               width = 4,
                                               selectizeInput(
                                                 inputId = "avNNet_Preprocess",
                                                 label = "Pre-processing",
                                                 choices = unique(c(default_initial, ppchoices)),
                                                 multiple = TRUE,
                                                 selected = default_initial
                                               ),
                                               bsTooltip(
                                                 id = "avNNet_Preprocess",
                                                 title = "These entries will be populated in the correct order from a saved model once it loads",
                                                 placement = "top"
                                               )
                                             ),
                                             column(
                                               width = 1,
                                               actionButton(inputId = "avNNet_Go", label = "Train", icon = icon("play")),
                                               bsTooltip(id = "avNNet_Go", title = "This will train or retrain your model (and save it)")
                                             ),
                                             column(
                                               width = 1,
                                               actionButton(inputId = "avNNet_Load", label = "Load", icon = icon("file-arrow-up")),
                                               bsTooltip(id = "avNNet_Load", title = "This will reload your saved model")
                                             ),
                                             column(
                                               width = 1,
                                               actionButton(inputId = "avNNet_Delete", label = "Forget", icon = icon("trash-can")),
                                               bsTooltip(id = "avNNet_Delete", title = "This will remove your model from memory")
                                             )
                                           ),
                                           hr(),
                                           h3("Resampled performance:"),
                                           tableOutput(outputId = "avNNet_Metrics"),
                                           hr(),
                                           h3("Hyperparameter Tuning:"),
                                           plotOutput(outputId = "avNNet_ModelTune"),
                                           hr(),
                                           h3("Recipe:"),
                                           htmlOutput(outputId = "avNNet_RecipePrint"),
                                           h3("Outputs"),
                                           tableOutput(outputId = "avNNet_RecipeOutput"),
                                           fluidRow(
                                             column(
                                               width = 6,
                                               h3("Training Summary:"),
                                               verbatimTextOutput(outputId = "avNNet_TrainSummary")
                                             ),
                                             column(
                                               width = 6,
                                               h3("Best Tune"),
                                               wellPanel(
                                                 tableOutput(outputId = "avNNet_Coef")
                                               )
                                             )
                                           )
                                  ),
                                  tabPanel("Radial Basis Function Network", #method = rbf
                                           verbatimTextOutput(outputId = "rbf_MethodSummary"),
                                           fluidRow(
                                             column(
                                               width = 4,
                                               selectizeInput(
                                                 inputId = "rbf_Preprocess",
                                                 label = "Pre-processing",
                                                 choices = unique(c(default_initial, ppchoices)),
                                                 multiple = TRUE,
                                                 selected = default_initial
                                               ),
                                               bsTooltip(
                                                 id = "rbf_Preprocess",
                                                 title = "These entries will be populated in the correct order from a saved model once it loads",
                                                 placement = "top"
                                               )
                                             ),
                                             column(
                                               width = 1,
                                               actionButton(inputId = "rbf_Go", label = "Train", icon = icon("play")),
                                               bsTooltip(id = "rbf_Go", title = "This will train or retrain your model (and save it)")
                                             ),
                                             column(
                                               width = 1,
                                               actionButton(inputId = "rbf_Load", label = "Load", icon = icon("file-arrow-up")),
                                               bsTooltip(id = "rbf_Load", title = "This will reload your saved model")
                                             ),
                                             column(
                                               width = 1,
                                               actionButton(inputId = "rbf_Delete", label = "Forget", icon = icon("trash-can")),
                                               bsTooltip(id = "rbf_Delete", title = "This will remove your model from memory")
                                             )
                                           ),
                                           hr(),
                                           h3("Resampled performance:"),
                                           tableOutput(outputId = "rbf_Metrics"),
                                           hr(),
                                           h3("Hyperparameter Tuning:"),
                                           plotOutput(outputId = "rbf_ModelTune"),
                                           hr(),
                                           h3("Recipe:"),
                                           htmlOutput(outputId = "rbf_RecipePrint"),
                                           h3("Outputs"),
                                           tableOutput(outputId = "rbf_RecipeOutput"),
                                           fluidRow(
                                             column(
                                               width = 6,
                                               h3("Training Summary:"),
                                               verbatimTextOutput(outputId = "rbf_TrainSummary")
                                             ),
                                             column(
                                               width = 6,
                                               h3("Best Tune"),
                                               wellPanel(
                                                 tableOutput(outputId = "rbf_Coef")
                                               )
                                             )
                                           )
                                           ),
                                  
                                  #NOT WORKING ATM BECAUSE BITCH FUCK
                                  # tabPanel("Extreme Learning Models (ELM)", #Nmethod = elm
                                  #          verbatimTextOutput(outputId = "elm_MethodSummary"),
                                  #          fluidRow(
                                  #            column(
                                  #              width = 4,
                                  #              selectizeInput(
                                  #                inputId = "elm_Preprocess",
                                  #                label = "Pre-processing",
                                  #                choices = unique(c(default_initial, ppchoices)),
                                  #                multiple = TRUE,
                                  #                selected = default_initial
                                  #              ),
                                  #              bsTooltip(
                                  #                id = "elm_Preprocess",
                                  #                title = "These entries will be populated in the correct order from a saved model once it loads",
                                  #                placement = "top"
                                  #              )
                                  #            ),
                                  #            column(
                                  #              width = 1,
                                  #              actionButton(inputId = "elm_Go", label = "Train", icon = icon("play")),
                                  #              bsTooltip(id = "elm_Go", title = "This will train or retrain your model (and save it)")
                                  #            ),
                                  #            column(
                                  #              width = 1,
                                  #              actionButton(inputId = "elm_Load", label = "Load", icon = icon("file-arrow-up")),
                                  #              bsTooltip(id = "elm_Load", title = "This will reload your saved model")
                                  #            ),
                                  #            column(
                                  #              width = 1,
                                  #              actionButton(inputId = "elm_Delete", label = "Forget", icon = icon("trash-can")),
                                  #              bsTooltip(id = "elm_Delete", title = "This will remove your model from memory")
                                  #            )
                                  #          ),
                                  #          hr(),
                                  #          h3("Resampled performance:"),
                                  #          tableOutput(outputId = "elm_Metrics"),
                                  #          hr(),
                                  #          h3("Hyperparameter Tuning:"),
                                  #          plotOutput(outputId = "elm_ModelTune"),
                                  #          hr(),
                                  #          h3("Recipe:"),
                                  #          htmlOutput(outputId = "elm_RecipePrint"),
                                  #          h3("Outputs"),
                                  #          tableOutput(outputId = "elm_RecipeOutput"),
                                  #          fluidRow(
                                  #            column(
                                  #              width = 6,
                                  #              h3("Training Summary:"),
                                  #              verbatimTextOutput(outputId = "elm_TrainSummary")
                                  #            ),
                                  #            column(
                                  #              width = 6,
                                  #              h3("Best Tune"),
                                  #              wellPanel(
                                  #                tableOutput(outputId = "elm_Coef")
                                  #              )
                                  #            )
                                  #          )),
                                  # tabPanel("Multi-Layer Perceptron"),
                                  tabPanel("Bayesian Regularized Neural Net", #method = brnn
                                           verbatimTextOutput(outputId = "brnn_MethodSummary"),
                                           fluidRow(
                                             column(
                                               width = 4,
                                               selectizeInput(
                                                 inputId = "brnn_Preprocess",
                                                 label = "Pre-processing",
                                                 choices = unique(c(default_initial, ppchoices)),
                                                 multiple = TRUE,
                                                 selected = default_initial
                                               ),
                                               bsTooltip(
                                                 id = "brnn_Preprocess",
                                                 title = "These entries will be populated in the correct order from a saved model once it loads",
                                                 placement = "top"
                                               )
                                             ),
                                             column(
                                               width = 1,
                                               actionButton(inputId = "brnn_Go", label = "Train", icon = icon("play")),
                                               bsTooltip(id = "brnn_Go", title = "This will train or retrain your model (and save it)")
                                             ),
                                             column(
                                               width = 1,
                                               actionButton(inputId = "brnn_Load", label = "Load", icon = icon("file-arrow-up")),
                                               bsTooltip(id = "brnn_Load", title = "This will reload your saved model")
                                             ),
                                             column(
                                               width = 1,
                                               actionButton(inputId = "brnn_Delete", label = "Forget", icon = icon("trash-can")),
                                               bsTooltip(id = "brnn_Delete", title = "This will remove your model from memory")
                                             )
                                           ),
                                           hr(),
                                           h3("Resampled performance:"),
                                           tableOutput(outputId = "brnn_Metrics"),
                                           hr(),
                                           h3("Hyperparameter Tuning:"),
                                           plotOutput(outputId = "brnn_ModelTune"),
                                           hr(),
                                           h3("Recipe:"),
                                           htmlOutput(outputId = "brnn_RecipePrint"),
                                           h3("Outputs"),
                                           tableOutput(outputId = "brnn_RecipeOutput"),
                                           fluidRow(
                                             column(
                                               width = 6,
                                               h3("Training Summary:"),
                                               verbatimTextOutput(outputId = "brnn_TrainSummary")
                                             ),
                                             column(
                                               width = 6,
                                               h3("Best Tune"),
                                               wellPanel(
                                                 tableOutput(outputId = "brnn_Coef")
                                               )
                                             )
                                           )
                                           ),
                                  tabPanel("Stacked AutoEncoder Deep Neural Network", #method = dnn
                                           verbatimTextOutput(outputId = "dnn_MethodSummary"),
                                           fluidRow(
                                             column(
                                               width = 4,
                                               selectizeInput(
                                                 inputId = "dnn_Preprocess",
                                                 label = "Pre-processing",
                                                 choices = unique(c(default_initial, ppchoices)),
                                                 multiple = TRUE,
                                                 selected = default_initial
                                               ),
                                               bsTooltip(
                                                 id = "dnn_Preprocess",
                                                 title = "These entries will be populated in the correct order from a saved model once it loads",
                                                 placement = "top"
                                               )
                                             ),
                                             column(
                                               width = 1,
                                               actionButton(inputId = "dnn_Go", label = "Train", icon = icon("play")),
                                               bsTooltip(id = "dnn_Go", title = "This will train or retrain your model (and save it)")
                                             ),
                                             column(
                                               width = 1,
                                               actionButton(inputId = "dnn_Load", label = "Load", icon = icon("file-arrow-up")),
                                               bsTooltip(id = "dnn_Load", title = "This will reload your saved model")
                                             ),
                                             column(
                                               width = 1,
                                               actionButton(inputId = "dnn_Delete", label = "Forget", icon = icon("trash-can")),
                                               bsTooltip(id = "dnn_Delete", title = "This will remove your model from memory")
                                             )
                                           ),
                                           hr(),
                                           h3("Resampled performance:"),
                                           tableOutput(outputId = "dnn_Metrics"),
                                           hr(),
                                           h3("Hyperparameter Tuning:"),
                                           plotOutput(outputId = "dnn_ModelTune"),
                                           hr(),
                                           h3("Recipe:"),
                                           htmlOutput(outputId = "dnn_RecipePrint"),
                                           h3("Outputs"),
                                           tableOutput(outputId = "dnn_RecipeOutput"),
                                           fluidRow(
                                             column(
                                               width = 6,
                                               h3("Training Summary:"),
                                               verbatimTextOutput(outputId = "dnn_TrainSummary")
                                             ),
                                             column(
                                               width = 6,
                                               h3("Best Tune"),
                                               wellPanel(
                                                 tableOutput(outputId = "dnn_Coef")
                                               )
                                             )
                                           )
                                           )
                                           
                                )
                       ),
                       
                       tabPanel("Kernel Methods",
                                tabsetPanel(
                                  type = "pills",
                                  
                                  tabPanel("Linear Kernel", # method = svmLinear
                                           verbatimTextOutput(outputId = "svmLinear_MethodSummary"),
                                           fluidRow(
                                             column(
                                               width = 4,
                                               selectizeInput(
                                                 inputId = "svmLinear_Preprocess",
                                                 label = "Pre-processing",
                                                 choices = unique(c(default_initial, ppchoices)),
                                                 multiple = TRUE,
                                                 selected = default_initial
                                               ),
                                               bsTooltip(
                                                 id = "svmLinear_Preprocess",
                                                 title = "These entries will be populated in the correct order from a saved model once it loads",
                                                 placement = "top"
                                               )
                                             ),
                                             column(
                                               width = 1,
                                               actionButton(inputId = "svmLinear_Go", label = "Train", icon = icon("play")),
                                               bsTooltip(id = "svmLinear_Go", title = "This will train or retrain your model (and save it)")
                                             ),
                                             column(
                                               width = 1,
                                               actionButton(inputId = "svmLinear_Load", label = "Load", icon = icon("file-arrow-up")),
                                               bsTooltip(id = "svmLinear_Load", title = "This will reload your saved model")
                                             ),
                                             column(
                                               width = 1,
                                               actionButton(inputId = "svmLinear_Delete", label = "Forget", icon = icon("trash-can")),
                                               bsTooltip(id = "svmLinear_Delete", title = "This will remove your model from memory")
                                             )
                                           ),
                                           hr(),
                                           h3("Resampled performance:"),
                                           tableOutput(outputId = "svmLinear_Metrics"),
                                           hr(),
                                           h3("Hyperparameter Tuning:"),
                                           plotOutput(outputId = "svmLinear_ModelTune"),
                                           hr(),
                                           h3("Recipe:"),
                                           htmlOutput(outputId = "svmLinear_RecipePrint"),
                                           h3("Outputs"),
                                           tableOutput(outputId = "svmLinear_RecipeOutput"),
                                           fluidRow(
                                             column(
                                               width = 6,
                                               h3("Training Summary:"),
                                               verbatimTextOutput(outputId = "svmLinear_TrainSummary")
                                             ),
                                             column(
                                               width = 6,
                                               h3("Best Tune"),
                                               wellPanel(
                                                 tableOutput(outputId = "svmLinear_Coef")
                                               )
                                             )
                                           )
                                  ),
                                  tabPanel("Polynomial Kernel", # method = svmPoly
                                           verbatimTextOutput(outputId = "svmPoly_MethodSummary"),
                                           fluidRow(
                                             column(
                                               width = 4,
                                               selectizeInput(
                                                 inputId = "svmPoly_Preprocess",
                                                 label = "Pre-processing",
                                                 choices = unique(c(default_initial, ppchoices)),
                                                 multiple = TRUE,
                                                 selected = default_initial
                                               ),
                                               bsTooltip(
                                                 id = "svmPoly_Preprocess",
                                                 title = "These entries will be populated in the correct order from a saved model once it loads",
                                                 placement = "top"
                                               )
                                             ),
                                             column(
                                               width = 1,
                                               actionButton(inputId = "svmPoly_Go", label = "Train", icon = icon("play")),
                                               bsTooltip(id = "svmPoly_Go", title = "This will train or retrain your model (and save it)")
                                             ),
                                             column(
                                               width = 1,
                                               actionButton(inputId = "svmPoly_Load", label = "Load", icon = icon("file-arrow-up")),
                                               bsTooltip(id = "svmPoly_Load", title = "This will reload your saved model")
                                             ),
                                             column(
                                               width = 1,
                                               actionButton(inputId = "svmPoly_Delete", label = "Forget", icon = icon("trash-can")),
                                               bsTooltip(id = "svmPoly_Delete", title = "This will remove your model from memory")
                                             )
                                           ),
                                           hr(),
                                           h3("Resampled performance:"),
                                           tableOutput(outputId = "svmPoly_Metrics"),
                                           hr(),
                                           h3("Hyperparameter Tuning:"),
                                           plotOutput(outputId = "svmPoly_ModelTune"),
                                           hr(),
                                           h3("Recipe:"),
                                           htmlOutput(outputId = "svmPoly_RecipePrint"),
                                           h3("Outputs"),
                                           tableOutput(outputId = "svmPoly_RecipeOutput"),
                                           fluidRow(
                                             column(
                                               width = 6,
                                               h3("Training Summary:"),
                                               verbatimTextOutput(outputId = "svmPoly_TrainSummary")
                                             ),
                                             column(
                                               width = 6,
                                               h3("Best Tune"),
                                               wellPanel(
                                                 tableOutput(outputId = "svmPoly_Coef")
                                               )
                                             )
                                           )
                                  ),
                                  tabPanel("Exponential Kernel", # method = svmExpoString
                                           verbatimTextOutput(outputId = "svmExpoString_MethodSummary"),
                                           fluidRow(
                                             column(
                                               width = 4,
                                               selectizeInput(
                                                 inputId = "svmExpoString_Preprocess",
                                                 label = "Pre-processing",
                                                 choices = unique(c(default_initial, ppchoices)),
                                                 multiple = TRUE,
                                                 selected = default_initial
                                               ),
                                               bsTooltip(
                                                 id = "svmExpoString_Preprocess",
                                                 title = "These entries will be populated in the correct order from a saved model once it loads",
                                                 placement = "top"
                                               )
                                             ),
                                             column(
                                               width = 1,
                                               actionButton(inputId = "svmExpoString_Go", label = "Train", icon = icon("play")),
                                               bsTooltip(id = "svmExpoString_Go", title = "This will train or retrain your model (and save it)")
                                             ),
                                             column(
                                               width = 1,
                                               actionButton(inputId = "svmExpoString_Load", label = "Load", icon = icon("file-arrow-up")),
                                               bsTooltip(id = "svmExpoString_Load", title = "This will reload your saved model")
                                             ),
                                             column(
                                               width = 1,
                                               actionButton(inputId = "svmExpoString_Delete", label = "Forget", icon = icon("trash-can")),
                                               bsTooltip(id = "svmExpoString_Delete", title = "This will remove your model from memory")
                                             )
                                           ),
                                           hr(),
                                           h3("Resampled performance:"),
                                           tableOutput(outputId = "svmExpoString_Metrics"),
                                           hr(),
                                           h3("Hyperparameter Tuning:"),
                                           plotOutput(outputId = "svmExpoString_ModelTune"),
                                           hr(),
                                           h3("Recipe:"),
                                           htmlOutput(outputId = "svmExpoString_RecipePrint"),
                                           h3("Outputs"),
                                           tableOutput(outputId = "svmExpoString_RecipeOutput"),
                                           fluidRow(
                                             column(
                                               width = 6,
                                               h3("Training Summary:"),
                                               verbatimTextOutput(outputId = "svmExpoString_TrainSummary")
                                             ),
                                             column(
                                               width = 6,
                                               h3("Best Tune"),
                                               wellPanel(
                                                 tableOutput(outputId = "svmExpoString_Coef")
                                               )
                                             )
                                           )
                                  ),
                                  tabPanel("Radial Basis Kernel", # method = svmRadial
                                           verbatimTextOutput(outputId = "svmRadial_MethodSummary"),
                                           fluidRow(
                                             column(
                                               width = 4,
                                               selectizeInput(
                                                 inputId = "svmRadial_Preprocess",
                                                 label = "Pre-processing",
                                                 choices = unique(c(default_initial, ppchoices)),
                                                 multiple = TRUE,
                                                 selected = default_initial
                                               ),
                                               bsTooltip(
                                                 id = "svmRadial_Preprocess",
                                                 title = "These entries will be populated in the correct order from a saved model once it loads",
                                                 placement = "top"
                                               )
                                             ),
                                             column(
                                               width = 1,
                                               actionButton(inputId = "svmRadial_Go", label = "Train", icon = icon("play")),
                                               bsTooltip(id = "svmRadial_Go", title = "This will train or retrain your model (and save it)")
                                             ),
                                             column(
                                               width = 1,
                                               actionButton(inputId = "svmRadial_Load", label = "Load", icon = icon("file-arrow-up")),
                                               bsTooltip(id = "svmRadial_Load", title = "This will reload your saved model")
                                             ),
                                             column(
                                               width = 1,
                                               actionButton(inputId = "svmRadial_Delete", label = "Forget", icon = icon("trash-can")),
                                               bsTooltip(id = "svmRadial_Delete", title = "This will remove your model from memory")
                                             )
                                           ),
                                           hr(),
                                           h3("Resampled performance:"),
                                           tableOutput(outputId = "svmRadial_Metrics"),
                                           hr(),
                                           h3("Hyperparameter Tuning:"),
                                           plotOutput(outputId = "svmRadial_ModelTune"),
                                           hr(),
                                           h3("Recipe:"),
                                           htmlOutput(outputId = "svmRadial_RecipePrint"),
                                           h3("Outputs"),
                                           tableOutput(outputId = "svmRadial_RecipeOutput"),
                                           fluidRow(
                                             column(
                                               width = 6,
                                               h3("Training Summary:"),
                                               verbatimTextOutput(outputId = "svmRadial_TrainSummary")
                                             ),
                                             column(
                                               width = 6,
                                               h3("Best Tune"),
                                               wellPanel(
                                                 tableOutput(outputId = "svmRadial_Coef")
                                               )
                                             )
                                           )
                                  ),
                                  tabPanel("Spectrum String Kernel", # method = svmSpectrumString
                                           verbatimTextOutput(outputId = "svmSpectrumString_MethodSummary"),
                                           fluidRow(
                                             column(
                                               width = 4,
                                               selectizeInput(
                                                 inputId = "svmSpectrumString_Preprocess",
                                                 label = "Pre-processing",
                                                 choices = unique(c(default_initial, ppchoices)),
                                                 multiple = TRUE,
                                                 selected = default_initial
                                               ),
                                               bsTooltip(
                                                 id = "svmSpectrumString_Preprocess",
                                                 title = "These entries will be populated in the correct order from a saved model once it loads",
                                                 placement = "top"
                                               )
                                             ),
                                             column(
                                               width = 1,
                                               actionButton(inputId = "svmSpectrumString_Go", label = "Train", icon = icon("play")),
                                               bsTooltip(id = "svmSpectrumString_Go", title = "This will train or retrain your model (and save it)")
                                             ),
                                             column(
                                               width = 1,
                                               actionButton(inputId = "svmSpectrumString_Load", label = "Load", icon = icon("file-arrow-up")),
                                               bsTooltip(id = "svmSpectrumString_Load", title = "This will reload your saved model")
                                             ),
                                             column(
                                               width = 1,
                                               actionButton(inputId = "svmSpectrumString_Delete", label = "Forget", icon = icon("trash-can")),
                                               bsTooltip(id = "svmSpectrumString_Delete", title = "This will remove your model from memory")
                                             )
                                           ),
                                           hr(),
                                           h3("Resampled performance:"),
                                           tableOutput(outputId = "svmSpectrumString_Metrics"),
                                           hr(),
                                           h3("Hyperparameter Tuning:"),
                                           plotOutput(outputId = "svmSpectrumString_ModelTune"),
                                           hr(),
                                           h3("Recipe:"),
                                           htmlOutput(outputId = "svmSpectrumString_RecipePrint"),
                                           h3("Outputs"),
                                           tableOutput(outputId = "svmSpectrumString_RecipeOutput"),
                                           fluidRow(
                                             column(
                                               width = 6,
                                               h3("Training Summary:"),
                                               verbatimTextOutput(outputId = "svmSpectrumString_TrainSummary")
                                             ),
                                             column(
                                               width = 6,
                                               h3("Best Tune"),
                                               wellPanel(
                                                 tableOutput(outputId = "svmSpectrumString_Coef")
                                               )
                                             )
                                           )
                                  ),
                                  tabPanel("Gaussian Process", # method = gaussprLinear
                                           verbatimTextOutput(outputId = "gaussprLinear_MethodSummary"),
                                           fluidRow(
                                             column(
                                               width = 4,
                                               selectizeInput(
                                                 inputId = "gaussprLinear_Preprocess",
                                                 label = "Pre-processing",
                                                 choices = unique(c(default_initial, ppchoices)),
                                                 multiple = TRUE,
                                                 selected = default_initial
                                               ),
                                               bsTooltip(
                                                 id = "gaussprLinear_Preprocess",
                                                 title = "These entries will be populated in the correct order from a saved model once it loads",
                                                 placement = "top"
                                               )
                                             ),
                                             column(
                                               width = 1,
                                               actionButton(inputId = "gaussprLinear_Go", label = "Train", icon = icon("play")),
                                               bsTooltip(id = "gaussprLinear_Go", title = "This will train or retrain your model (and save it)")
                                             ),
                                             column(
                                               width = 1,
                                               actionButton(inputId = "gaussprLinear_Load", label = "Load", icon = icon("file-arrow-up")),
                                               bsTooltip(id = "gaussprLinear_Load", title = "This will reload your saved model")
                                             ),
                                             column(
                                               width = 1,
                                               actionButton(inputId = "gaussprLinear_Delete", label = "Forget", icon = icon("trash-can")),
                                               bsTooltip(id = "gaussprLinear_Delete", title = "This will remove your model from memory")
                                             )
                                           ),
                                           hr(),
                                           h3("Resampled performance:"),
                                           tableOutput(outputId = "gaussprLinear_Metrics"),
                                           hr(),
                                           h3("Hyperparameter Tuning:"),
                                           plotOutput(outputId = "gaussprLinear_ModelTune"),
                                           hr(),
                                           h3("Recipe:"),
                                           htmlOutput(outputId = "gaussprLinear_RecipePrint"),
                                           h3("Outputs"),
                                           tableOutput(outputId = "gaussprLinear_RecipeOutput"),
                                           fluidRow(
                                             column(
                                               width = 6,
                                               h3("Training Summary:"),
                                               verbatimTextOutput(outputId = "gaussprLinear_TrainSummary")
                                             ),
                                             column(
                                               width = 6,
                                               h3("Best Tune"),
                                               wellPanel(
                                                 tableOutput(outputId = "gaussprLinear_Coef")
                                               )
                                             )
                                           )
                                  ),
                                  tabPanel("L2 Regularized Support Vector Machines with Linear Kernel", # method = svmLinear3
                                           verbatimTextOutput(outputId = "svmLinear3_MethodSummary"),
                                           fluidRow(
                                             column(
                                               width = 4,
                                               selectizeInput(
                                                 inputId = "svmLinear3_Preprocess",
                                                 label = "Pre-processing",
                                                 choices = unique(c(default_initial, ppchoices)),
                                                 multiple = TRUE,
                                                 selected = default_initial
                                               ),
                                               bsTooltip(
                                                 id = "svmLinear3_Preprocess",
                                                 title = "These entries will be populated in the correct order from a saved model once it loads",
                                                 placement = "top"
                                               )
                                             ),
                                             column(
                                               width = 1,
                                               actionButton(inputId = "svmLinear3_Go", label = "Train", icon = icon("play")),
                                               bsTooltip(id = "svmLinear3_Go", title = "This will train or retrain your model (and save it)")
                                             ),
                                             column(
                                               width = 1,
                                               actionButton(inputId = "svmLinear3_Load", label = "Load", icon = icon("file-arrow-up")),
                                               bsTooltip(id = "svmLinear3_Load", title = "This will reload your saved model")
                                             ),
                                             column(
                                               width = 1,
                                               actionButton(inputId = "svmLinear3_Delete", label = "Forget", icon = icon("trash-can")),
                                               bsTooltip(id = "svmLinear3_Delete", title = "This will remove your model from memory")
                                             )
                                           ),
                                           hr(),
                                           h3("Resampled performance:"),
                                           tableOutput(outputId = "svmLinear3_Metrics"),
                                           hr(),
                                           h3("Hyperparameter Tuning:"),
                                           plotOutput(outputId = "svmLinear3_ModelTune"),
                                           hr(),
                                           h3("Recipe:"),
                                           htmlOutput(outputId = "svmLinear3_RecipePrint"),
                                           h3("Outputs"),
                                           tableOutput(outputId = "svmLinear3_RecipeOutput"),
                                           fluidRow(
                                             column(
                                               width = 6,
                                               h3("Training Summary:"),
                                               verbatimTextOutput(outputId = "svmLinear3_TrainSummary")
                                             ),
                                             column(
                                               width = 6,
                                               h3("Best Tune"),
                                               wellPanel(
                                                 tableOutput(outputId = "svmLinear3_Coef")
                                               )
                                             )
                                           )
                                  )
                                )
                       ), # kernel methods tab end
                       
                       tabPanel("Flexible Nonlinear Models",
                                tabsetPanel(
                                  type = "pills",
                                  
                                  tabPanel("Multivariate Adaptive Regression Splines", # method = gcvEarth
                                           verbatimTextOutput(outputId = "gcvEarth_MethodSummary"),
                                           fluidRow(
                                             column(
                                               width = 4,
                                               selectizeInput(
                                                 inputId = "gcvEarth_Preprocess",
                                                 label = "Pre-processing",
                                                 choices = unique(c(default_initial, ppchoices)),
                                                 multiple = TRUE,
                                                 selected = default_initial
                                               ),
                                               bsTooltip(
                                                 id = "gcvEarth_Preprocess",
                                                 title = "These entries will be populated in the correct order from a saved model once it loads",
                                                 placement = "top"
                                               )
                                             ),
                                             column(
                                               width = 1,
                                               actionButton(inputId = "gcvEarth_Go", label = "Train", icon = icon("play")),
                                               bsTooltip(id = "gcvEarth_Go", title = "This will train or retrain your model (and save it)")
                                             ),
                                             column(
                                               width = 1,
                                               actionButton(inputId = "gcvEarth_Load", label = "Load", icon = icon("file-arrow-up")),
                                               bsTooltip(id = "gcvEarth_Load", title = "This will reload your saved model")
                                             ),
                                             column(
                                               width = 1,
                                               actionButton(inputId = "gcvEarth_Delete", label = "Forget", icon = icon("trash-can")),
                                               bsTooltip(id = "gcvEarth_Delete", title = "This will remove your model from memory")
                                             )
                                           ),
                                           hr(),
                                           h3("Resampled performance:"),
                                           tableOutput(outputId = "gcvEarth_Metrics"),
                                           hr(),
                                           h3("Hyperparameter Tuning:"),
                                           plotOutput(outputId = "gcvEarth_ModelTune"),
                                           hr(),
                                           h3("Recipe:"),
                                           htmlOutput(outputId = "gcvEarth_RecipePrint"),
                                           h3("Outputs"),
                                           tableOutput(outputId = "gcvEarth_RecipeOutput"),
                                           fluidRow(
                                             column(
                                               width = 6,
                                               h3("Training Summary:"),
                                               verbatimTextOutput(outputId = "gcvEarth_TrainSummary")
                                             ),
                                             column(
                                               width = 6,
                                               h3("Best Tune"),
                                               wellPanel(
                                                 tableOutput(outputId = "gcvEarth_Coef")
                                               )
                                             )
                                           )
                                  ),
                                  tabPanel("Boosted Generalized Additive Model", # method = gamboost
                                           verbatimTextOutput(outputId = "gamboost_MethodSummary"),
                                           fluidRow(
                                             column(
                                               width = 4,
                                               selectizeInput(
                                                 inputId = "gamboost_Preprocess",
                                                 label = "Pre-processing",
                                                 choices = unique(c(default_initial, ppchoices)),
                                                 multiple = TRUE,
                                                 selected = default_initial
                                               ),
                                               bsTooltip(
                                                 id = "gamboost_Preprocess",
                                                 title = "These entries will be populated in the correct order from a saved model once it loads",
                                                 placement = "top"
                                               )
                                             ),
                                             column(
                                               width = 1,
                                               actionButton(inputId = "gamboost_Go", label = "Train", icon = icon("play")),
                                               bsTooltip(id = "gamboost_Go", title = "This will train or retrain your model (and save it)")
                                             ),
                                             column(
                                               width = 1,
                                               actionButton(inputId = "gamboost_Load", label = "Load", icon = icon("file-arrow-up")),
                                               bsTooltip(id = "gamboost_Load", title = "This will reload your saved model")
                                             ),
                                             column(
                                               width = 1,
                                               actionButton(inputId = "gamboost_Delete", label = "Forget", icon = icon("trash-can")),
                                               bsTooltip(id = "gamboost_Delete", title = "This will remove your model from memory")
                                             )
                                           ),
                                           hr(),
                                           h3("Resampled performance:"),
                                           tableOutput(outputId = "gamboost_Metrics"),
                                           hr(),
                                           h3("Hyperparameter Tuning:"),
                                           plotOutput(outputId = "gamboost_ModelTune"),
                                           hr(),
                                           h3("Recipe:"),
                                           htmlOutput(outputId = "gamboost_RecipePrint"),
                                           h3("Outputs"),
                                           tableOutput(outputId = "gamboost_RecipeOutput"),
                                           fluidRow(
                                             column(
                                               width = 6,
                                               h3("Training Summary:"),
                                               verbatimTextOutput(outputId = "gamboost_TrainSummary")
                                             ),
                                             column(
                                               width = 6,
                                               h3("Best Tune"),
                                               wellPanel(
                                                 tableOutput(outputId = "gamboost_Coef")
                                               )
                                             )
                                           )
                                  ),
                                  # tabPanel("Bayesian Additive Regression Trees", # method = bartMachine
                                  #          verbatimTextOutput(outputId = "bartMachine_MethodSummary"),
                                  #          fluidRow(
                                  #            column(
                                  #              width = 4,
                                  #              selectizeInput(
                                  #                inputId = "bartMachine_Preprocess",
                                  #                label = "Pre-processing",
                                  #                choices = unique(c(default_initial, ppchoices)),
                                  #                multiple = TRUE,
                                  #                selected = default_initial
                                  #              ),
                                  #              bsTooltip(
                                  #                id = "bartMachine_Preprocess",
                                  #                title = "These entries will be populated in the correct order from a saved model once it loads",
                                  #                placement = "top"
                                  #              )
                                  #            ),
                                  #            column(
                                  #              width = 1,
                                  #              actionButton(inputId = "bartMachine_Go", label = "Train", icon = icon("play")),
                                  #              bsTooltip(id = "bartMachine_Go", title = "This will train or retrain your model (and save it)")
                                  #            ),
                                  #            column(
                                  #              width = 1,
                                  #              actionButton(inputId = "bartMachine_Load", label = "Load", icon = icon("file-arrow-up")),
                                  #              bsTooltip(id = "bartMachine_Load", title = "This will reload your saved model")
                                  #            ),
                                  #            column(
                                  #              width = 1,
                                  #              actionButton(inputId = "bartMachine_Delete", label = "Forget", icon = icon("trash-can")),
                                  #              bsTooltip(id = "bartMachine_Delete", title = "This will remove your model from memory")
                                  #            )
                                  #          ),
                                  #          hr(),
                                  #          h3("Resampled performance:"),
                                  #          tableOutput(outputId = "bartMachine_Metrics"),
                                  #          hr(),
                                  #          h3("Hyperparameter Tuning:"),
                                  #          plotOutput(outputId = "bartMachine_ModelTune"),
                                  #          hr(),
                                  #          h3("Recipe:"),
                                  #          htmlOutput(outputId = "bartMachine_RecipePrint"),
                                  #          h3("Outputs"),
                                  #          tableOutput(outputId = "bartMachine_RecipeOutput"),
                                  #          fluidRow(
                                  #            column(
                                  #              width = 6,
                                  #              h3("Training Summary:"),
                                  #              verbatimTextOutput(outputId = "bartMachine_TrainSummary")
                                  #            ),
                                  #            column(
                                  #              width = 6,
                                  #              h3("Best Tune"),
                                  #              wellPanel(
                                  #                tableOutput(outputId = "bartMachine_Coef")
                                  #              )
                                  #            )
                                  #          )
                                  # )
                                )
                       ), # flexible nonlinear models tab end
                       
                       tabPanel("Rule-Based Models",
                                tabsetPanel(
                                  type = "pills",
                                  
                                  tabPanel("Cubist", # method = cubist
                                           verbatimTextOutput(outputId = "cubist_MethodSummary"),
                                           fluidRow(
                                             column(
                                               width = 4,
                                               selectizeInput(
                                                 inputId = "cubist_Preprocess",
                                                 label = "Pre-processing",
                                                 choices = unique(c(default_initial, ppchoices)),
                                                 multiple = TRUE,
                                                 selected = default_initial
                                               ),
                                               bsTooltip(
                                                 id = "cubist_Preprocess",
                                                 title = "These entries will be populated in the correct order from a saved model once it loads",
                                                 placement = "top"
                                               )
                                             ),
                                             column(
                                               width = 1,
                                               actionButton(inputId = "cubist_Go", label = "Train", icon = icon("play")),
                                               bsTooltip(id = "cubist_Go", title = "This will train or retrain your model (and save it)")
                                             ),
                                             column(
                                               width = 1,
                                               actionButton(inputId = "cubist_Load", label = "Load", icon = icon("file-arrow-up")),
                                               bsTooltip(id = "cubist_Load", title = "This will reload your saved model")
                                             ),
                                             column(
                                               width = 1,
                                               actionButton(inputId = "cubist_Delete", label = "Forget", icon = icon("trash-can")),
                                               bsTooltip(id = "cubist_Delete", title = "This will remove your model from memory")
                                             )
                                           ),
                                           hr(),
                                           h3("Resampled performance:"),
                                           tableOutput(outputId = "cubist_Metrics"),
                                           hr(),
                                           h3("Hyperparameter Tuning:"),
                                           plotOutput(outputId = "cubist_ModelTune"),
                                           hr(),
                                           h3("Recipe:"),
                                           htmlOutput(outputId = "cubist_RecipePrint"),
                                           h3("Outputs"),
                                           tableOutput(outputId = "cubist_RecipeOutput"),
                                           fluidRow(
                                             column(
                                               width = 6,
                                               h3("Training Summary:"),
                                               verbatimTextOutput(outputId = "cubist_TrainSummary")
                                             ),
                                             column(
                                               width = 6,
                                               h3("Best Tune"),
                                               wellPanel(
                                                 tableOutput(outputId = "cubist_Coef")
                                               )
                                             )
                                           )
                                  ),
                                  tabPanel("Bagged Logic Regression", # method = logicBag
                                           verbatimTextOutput(outputId = "logicBag_MethodSummary"),
                                           fluidRow(
                                             column(
                                               width = 4,
                                               selectizeInput(
                                                 inputId = "logicBag_Preprocess",
                                                 label = "Pre-processing",
                                                 choices = unique(c(default_initial, ppchoices)),
                                                 multiple = TRUE,
                                                 selected = default_initial
                                               ),
                                               bsTooltip(
                                                 id = "logicBag_Preprocess",
                                                 title = "These entries will be populated in the correct order from a saved model once it loads",
                                                 placement = "top"
                                               )
                                             ),
                                             column(
                                               width = 1,
                                               actionButton(inputId = "logicBag_Go", label = "Train", icon = icon("play")),
                                               bsTooltip(id = "logicBag_Go", title = "This will train or retrain your model (and save it)")
                                             ),
                                             column(
                                               width = 1,
                                               actionButton(inputId = "logicBag_Load", label = "Load", icon = icon("file-arrow-up")),
                                               bsTooltip(id = "logicBag_Load", title = "This will reload your saved model")
                                             ),
                                             column(
                                               width = 1,
                                               actionButton(inputId = "logicBag_Delete", label = "Forget", icon = icon("trash-can")),
                                               bsTooltip(id = "logicBag_Delete", title = "This will remove your model from memory")
                                             )
                                           ),
                                           hr(),
                                           h3("Resampled performance:"),
                                           tableOutput(outputId = "logicBag_Metrics"),
                                           hr(),
                                           h3("Hyperparameter Tuning:"),
                                           plotOutput(outputId = "logicBag_ModelTune"),
                                           hr(),
                                           h3("Recipe:"),
                                           htmlOutput(outputId = "logicBag_RecipePrint"),
                                           h3("Outputs"),
                                           tableOutput(outputId = "logicBag_RecipeOutput"),
                                           fluidRow(
                                             column(
                                               width = 6,
                                               h3("Training Summary:"),
                                               verbatimTextOutput(outputId = "logicBag_TrainSummary")
                                             ),
                                             column(
                                               width = 6,
                                               h3("Best Tune"),
                                               wellPanel(
                                                 tableOutput(outputId = "logicBag_Coef")
                                               )
                                             )
                                           )
                                  )
                                )
                       ) # rule-based models tab end
                       
                                
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