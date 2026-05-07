shinyServer(function(input, output, session) {
  
  # initialisation ----
  models <- reactiveValues()  # this is a collection of the models

    
  # Ensure the "SavedModels folder exists
  if (!"./SavedModels" %in% list.dirs()) {
    dir.create("./SavedModels")
  }
  
  shiny::onSessionEnded(stopApp)

  
  # reactive getData ----
  getData <- reactive({
    d <- read.csv(file = "Ass3Data.csv", row.names = "Patient", stringsAsFactors = TRUE)  # "Patient" is no longer a variable
    d$ObservationDate <- as.Date(d$ObservationDate, "%Y-%m-%d")
    d
  })
  
  # output BoxPlots ----
  output$BoxPlots <- renderPlot({
    d <- getData()
    numeric <- sapply(d, FUN = is.numeric)
    req(d, input$Multiplier, length(numeric) > 0)
    d <- scale(d[,numeric], center = input$Normalise, scale = input$Normalise)
    boxplot(d, outline = TRUE, main = paste("Boxplot using IQR multiplier of", input$Multiplier), range = input$Multiplier, las = 2)
  })
  
  # output Missing ----
  output$Missing <- renderPlot({
    d <- getData()
    vis_dat(d)
  })
  
  # output Corr ----
  output$Corr <- renderPlot({
    d <- getData()
    numeric <- sapply(d, FUN = is.numeric)
    req(d, length(numeric) > 0)
    corrgram::corrgram(d, order = "OLO", main = "Numeric Data Correlation")
  })
  
  # output DataSummary ----
  output$DataSummary <- renderPrint({
    str(getData())
  })
  
  # output Table ----
  output$Table <- DT::renderDataTable({
    d <- getData()
    numeric <- c(FALSE, sapply(d, is.numeric)) # never round rownames which are the first column (when shown)
    DT::datatable(d) %>%
      formatRound(columns = numeric, digits = 3)
  })
  
  # reactive get Split
  getSplit <- reactive({
    set.seed(199)
    createDataPartition(y = getData()$Response, p = input$Split, list = FALSE)
  })
  
  # reactive getMethods ----
  getMethods <- reactive({
    mi <- caret::getModelInfo()
    Label <- vector(mode = "character", length = length(mi))
    Package <- vector(mode = "character", length = length(mi))
    Hyperparams <- vector(mode = "character", length = length(mi))
    Regression <- vector(mode = "logical", length = length(mi))
    Classification <- vector(mode = "logical", length = length(mi))
    Tags <- vector(mode = "character", length = length(mi))
    ClassProbs <- vector(mode = "character", length = length(mi))
    for (row in 1:length(mi)) {
      Label[row] <- mi[[row]]$label
      libs <- mi[[row]]$library
      libs <- na.omit(libs[libs != ""]) # remove blank libraries
      if (length(libs) > 0) {
        present <- vector(mode = "logical", length = length(libs))
        suppressWarnings({
          for (lib in 1:length(libs)) {
            present[lib] <- require(package = libs[lib], warn.conflicts = FALSE, character.only = TRUE, quietly = TRUE)
          }
        })
        check <- ifelse(present, "", as.character(icon(name = "ban")))
        Package[row] <- paste(collapse = "<br/>", paste(mi[[row]]$library, check))
      }
      d <- mi[[row]]$parameters
      Hyperparams[row] <- paste(collapse = "<br/>", paste0(d$parameter, " - ", d$label, " [", d$class,"]"))
      Regression[row] <- ifelse("Regression" %in% mi[[row]]$type, as.character(icon("check-square", class = "fa-3x")), "")
      Classification[row] <- ifelse("Classification" %in% mi[[row]]$type , as.character(icon("check-square", class = "fa-3x")),"")
      Tags[row] <- paste(collapse = "<br/>", mi[[row]]$tags)
      ClassProbs[row] <- ifelse(is.function(mi[[row]]$prob), as.character(icon("check-square", class = "fa-3x")), "")
    }
    data.frame(Model = names(mi), Label, Package, Regression, Classification, Tags, Hyperparams, ClassProbs, stringsAsFactors = FALSE)
  })
  
  # output Available ----
  output$Available <- DT::renderDataTable({
     m <- getMethods()
     m <- m[m$Regression != "", !colnames(m) %in% c("Regression", "Classification", "ClassProbs")]  # hide columns because we are looking at regression methods only
     DT::datatable(m, escape = FALSE, options = list(pageLength = 5, lengthMenu = c(5,10,15)), rownames = FALSE, selection = "none")
  })
  
  # reactive getTrainData ----
  getTrainData <- reactive({
    getData()[getSplit(),]
  })
  
  # reactive getTestData ----
  getTestData <- reactive({
    getData()[-getSplit(),]
  })
  
  # reactive getTrControl ----
  getTrControl <- reactive({
    # shared bootstrap specification i.e. 25 x bootstrap
    y <- getTrainData()[,"Response"]
    n <- 25
    set.seed(673)
    seeds <- vector(mode = "list", length = n + 1)
    for (i in 1:n) {
      seeds[[i]] <- as.integer(c(runif(n = 500, min = 1000, max = 5000)))
    }
    seeds[[n + 1]] <- as.integer(runif(n = 1, min = 1000, max = 5000))
    trainControl(method = "boot", 
                 number = n, 
                 repeats = NA, 
                 allowParallel = TRUE,
                 search = "grid", 
                 index = caret::createResample(y = y, times = n),
                 savePredictions = "final", 
                 seeds = seeds, 
                 trim = TRUE)
    
 
  })
  
  # output SplitSummary ----
  output$SplitSummary <- renderPrint({
    cat(paste("Training observations:", nrow(getTrainData()), "\n", "Testing observations:", nrow(getTestData())))
  })
  
  # reactive getResamples ----
  getResamples <- reactive({
    models2 <- reactiveValuesToList(models) %>% 
      rlist::list.clean( fun = is.null, recursive = FALSE)
    req(length(models2) > 1)
    results <- caret::resamples(models2)
    
    #scale metrics using null model. Tough code to follow -sorry
    NullModel <- "null"
    if (input$NullNormalise & NullModel %in% results$models) {
      actualNames <- colnames(results$values)
      # Normalise the various hyper-metrics except R2 (as this is already normalised)
      for (metric in c("RMSE", "MAE")) {
        col <- paste(sep = "~", NullModel, metric)
        if (col %in% actualNames) {
          nullMetric <- mean(results$values[, col], na.rm = TRUE)
          if (!is.na(nullMetric) & nullMetric != 0) {
            for (model in results$models) {
              mcol <- paste(sep = "~", model, metric)
              if (mcol %in% actualNames) {
                results$values[, mcol] <- results$values[, mcol] / nullMetric
              }
            }
          }
        }
      }
    }
    
    # hide results worse than null model
    subset <- rep(TRUE, length(models2))
    if (input$HideWorse & NullModel %in% names(models2)) {
      actualNames <- colnames(results$values)
      col <- paste(sep = "~", "null","RMSE" )
      if (col %in% actualNames) {
        nullMetric <- mean(results$values[, col], na.rm = TRUE)
        if (!is.na(nullMetric)) {
          m <- 0
          for (model3 in results$models) {
            m <- m + 1
            mcol <- paste(sep = "~", model3, "RMSE")
            if (mcol %in% actualNames) {
              subset[m] <- mean(results$values[, mcol], na.rm = TRUE) <= nullMetric
            }
          }
        }
      }
      results$models <- results$models[subset]
    }
    
    best_model <- ""
    
    actualNames <- colnames(results$values)
    
    rmse_means <- sapply(results$models, function(model) {
      mcol <- paste(sep = "~", model, "RMSE")
      if (mcol %in% actualNames) {
        mean(results$values[, mcol], na.rm = TRUE)
      } else {
        Inf
      }
    })
    
    if (length(rmse_means) > 0 && any(is.finite(rmse_means))) {
      best_model <- names(which.min(rmse_means))
    }
    
    updateRadioButtons(session = session,
                       inputId = "Choice",
                       choices = results$models,
                       selected = best_model)  
    results
  })
  
  # output SelectionBoxPlot (plot) ----
  output$SelectionBoxPlot <- renderPlot({
    mod <- getResamples()
    bwplot(mod, notch = input$Notch)
  })
  
  # output Title (UI) ----
  output$Title <- renderUI({
    tags$h3(paste("Unseen data results for chosen model:", input$Choice))
  })
  
  # reactive getTestResults ----
  getTestResults <- reactive({
    dat <- getTestData()
    req(input$Choice)
    mod <- models[[input$Choice]]
    predictions <- predict(mod, newdata = dat)
    d <- data.frame(dat$Response, predictions, row.names = rownames(dat))
    colnames(d) <- c("obs", "pred")
    d
  })
  
  # reactive getTrainResults ----
  getTrainResults <- reactive({
    dat <- getTrainData()
    req(input$Choice)
    mod <- models[[input$Choice]]
    predictions <- predict(mod, newdata = dat)
    d <- data.frame(dat$Response, predictions, row.names = rownames(dat))
    colnames(d) <- c("obs", "pred")
    d
  })
  
  # Range for charts
  getResidualRange <- reactive({
    d1 <- getTrainResults()
    d1$residuals <- d1$obs - d1$pred
    d2 <- getTestResults()
    d2$residuals <- d2$obs - d2$pred
    d <- c(d1$residuals, d2$residuals)
    range(d, na.rm = TRUE)
  })
  
  # output TestSummary (print)
  output$TestSummary <- renderPrint({
    if (is.null(input$Choice) || input$Choice == "") {
      cat("No model chosen")
    } else {
      caret::defaultSummary(getTestResults())
    }
  })
  
  # output TestPlot (plot) ----
  output$TestPlot <- renderPlot({
    d <- getTestResults()
    req(nrow(d) > 0)
    par(pty = "s")
    range <- range(c(d$obs, d$pred), na.rm = TRUE)
    plot(d, xlim = range, ylim = range, main = "Predicted versus Observed for test data")
    abline(a = 0, b = 1, col = c("blue"), lty = c(2), lwd = c(3))
  })
  
  # output TestResiduals (plot) ----
  output$TestResiduals <- renderPlot({
    d <- getTestResults()
    req(nrow(d) > 0)
    d$residuals <- d$obs - d$pred
    coef <- input$IqrM
    limits <- boxplot.stats(x = d$residuals, coef = coef)$stats
    label <- ifelse(d$residuals < limits[1] | d$residuals > limits[5], rownames(d), NA)
    ggplot(d, mapping = aes(y = residuals, x = 0)) +
      ylim(getResidualRange()[1], getResidualRange()[2]) +
      geom_boxplot(coef = coef, orientation = "vertical", ) +
      ggrepel::geom_text_repel(aes(label = label)) +
      labs(title = "Test-Residual Boxplot",  subtitle = paste(coef, "IQR Multiplier")) +
      theme(axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank())
  })
  
  # output TrainResiduals (plot) ----
  output$TrainResiduals <- renderPlot({
    d <- getTrainResults()
    req(nrow(d) > 0)
    d$residuals <- d$obs - d$pred
    coef <- input$IqrM
    limits <- boxplot.stats(x = d$residuals, coef = coef)$stats
    label <- ifelse(d$residuals < limits[1] | d$residuals > limits[5], rownames(d), NA)
    ggplot(d, mapping = aes(y = residuals, x = 0, label = label)) +
      ylim(getResidualRange()[1], getResidualRange()[2]) +
      geom_boxplot(coef = coef, orientation = "vertical") +
      ggrepel::geom_text_repel() +
      labs(title = "Train-Residual Boxplot",  subtitle = paste(coef, "IQR Multiplier")) +
      theme(axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank())
  })
  
  
  # METHOD * null ---------------------------------------------------------------------------------------------------------------------------
  
  # reactive getNullRecipe ----
  getNullRecipe <- reactive({
    form <- formula(Response ~ .)
    recipes::recipe(form, data = getTrainData())
  })
  
  # observeEvent null_Go ----
  observeEvent(
    input$null_Go,
    {
      method <- "null"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      obj <- startMode(input$Parallel)
      tryCatch({
        model <- caret::train(getNullRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl())
        deleteRds(method)
        saveToRds(model, method)
        models[[method]] <- model
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(obj)
      })
    }
  )

  observeEvent(
    input$null_Load,
    {
      method  <- "null"
      model <- loadRds(method, session)
      if (!is.null(model)) {
        models[[method]] <- model
      }
    }
  )
  
  observeEvent(
    input$null_Delete,
    {
      method <- "null"
      models[[method]] <- NULL
      gc()
    }
  )
  
  # observeEvent null_Metrics ----
  output$null_Metrics <- renderTable({
    method <- "null"
    mod <- models[[method]]
    req(mod)
    mod$results[ which.min(mod$results[, "RMSE"]), ]
  })
  
  # output null_Recipe (table) ----
  output$null_Recipe <- renderTable({
    method <- "null"
    mod <- models[[method]]
    req(mod)
    terms <- mod$recipe$term_info
    n <- dim(terms)[1]
    types <- vector(mode="character", length=n)
    for (row in 1:n) {
      types[row] <- paste(collapse = " ", unlist(terms$type[row]))
    }
    terms$type <- types
    terms
  })  


  
  
  # METHOD * glmnet ---------------------------------------------------------------------------------------------------------------------------
  library(glmnet)   #  <------ Declare any modelling packages that are needed (see Method List tab)
  # reactive getGlmnetRecipe ----
  getGlmnetRecipe <- reactive({
    form <- formula(Response ~ .)
    recipes::recipe(form, data = getTrainData()) %>%
      dynamicSteps(input$glmnet_Preprocess) %>%           # use <method>_Preprocess 
      step_rm(has_type("date"))   # remove original date variables
  })
  
  # observe GO event ----
  observeEvent(
    input$glmnet_Go,
    {
      method <- "glmnet"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      obj <- startMode(input$Parallel)
      tryCatch({
        model <- caret::train(getGlmnetRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 5, na.action = na.pass)
        deleteRds(method)
        saveToRds(model, method)
        models[[method]] <- model
      },
      finally = {
        removeNotification(id = method)
        stopMode(obj)
      })
     }
  )
  
  observeEvent(
    input$glmnet_Load,
    {
      method  <- "glmnet"
      model <- loadRds(method, session)
      if (!is.null(model)) {
        models[[method]] <- model
      }
    }
  )
  
  observeEvent(
    input$glmnet_Delete,
    {
      method <- "glmnet"
      models[[method]] <- NULL
      gc()
    }
  )
  
  # output method summary text ----
  output$glmnet_MethodSummary <- renderText({
    method <- "glmnet"
    description(method)
  })
  
  # output resampling metrics table ----
  output$glmnet_Metrics <- renderTable({
    method <- "glmnet"
    mod <- models[[method]]
    req(mod)
    mod$results[ which.min(mod$results[, "RMSE"]), ]
  })
  
  # output hyperparameter tuning chart ----
  output$glmnet_ModelTune <- renderPlot({
    method <- "glmnet"
    mod <- models[[method]]
    req(mod)
    plot(mod)
  })

  # output an html formatted recipe "print" ----
  output$glmnet_RecipePrint <- renderUI({
    method <- "glmnet"
    mod <- models[[method]]
    req(mod)
    html <- mod$recipe %>%
      print() %>%
      cli::cli_fmt() %>%
      cli::ansi_collapse(sep="<br>", last = "<br>") %>%
      cli::ansi_html(escape_reserved = FALSE) %>%
      gsub(pattern = "──────", replacement = "─",  x = ., fixed = TRUE)
    css <- paste(format(ansi_html_style()), collapse= "\n")
    tagList(
      tags$head(tags$style(css)),
      tags$pre(HTML(html))
    )
  })
    
  
  # output Recipe-output table ----
  output$glmnet_RecipeOutput <- renderTable({
    method <- "glmnet"
    mod <- models[[method]]
    req(mod)
    terms <- as.data.frame(mod$recipe$term_info)
    n <- dim(terms)[1]
    types <- vector(mode="character", length=n)
    for (row in 1:n) {
      types[row] <- paste(collapse = " ", unlist(terms$type[row]))
    }
    terms$type <- types
    terms |>
      dplyr::filter(role == "predictor") |>
      dplyr::select(type, source) |>
      dplyr::group_by(type, source) |>
      dplyr::summarise(count = n())
  })  
  
  # output training summary print ----
  output$glmnet_TrainSummary <- renderPrint({
    method <- "glmnet"
    mod <- models[[method]]
    req(mod)
    print(mod)
  })

  # output coefficient print ----
  output$glmnet_Coef <- renderTable({
    req(models$glmnet)
    co <- as.matrix(coef(models$glmnet$finalModel, s  = models$glmnet$bestTune$lambda))  # special for glmnet
    df <- as.data.frame(co, row.names = rownames(co))
    df[df$s1 != 0.000, ,drop=FALSE]
  }, rownames = TRUE, colnames = FALSE)
  
  
  
  # METHOD * pls ---------------------------------------------------------------------------------------------------------------------------
  library(pls)  #  <------ Declare any modelling packages that are needed (see Method List tab)
  
  # reactive getPlsRecipe ----
  getPlsRecipe <- reactive({
    form <- formula(Response ~ .)
    recipes::recipe(form, data = getTrainData()) %>%
      dynamicSteps(input$pls_Preprocess) %>%   # use <method>_Preprocess
      step_rm(has_type("date"))   # remove original date variables
  })
  
  # observe GO event ----
  observeEvent(
    input$pls_Go,
    {
      method <- "pls"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      obj <- startMode(input$Parallel)
      tryCatch({
        model <- caret::train(getPlsRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), 
                              tuneLength = 25, na.action = na.pass)
        deleteRds(method)
        saveToRds(model, method)
        models[[method]] <- model
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(obj)
      })
    }
  )
  
  observeEvent(
    input$pls_Load,
    {
      method  <- "pls"
      model <- loadRds(method, session)
      if (!is.null(model)) {
        models[[method]] <- model
      }
    }
  )
  
  observeEvent(
    input$pls_Delete,
    {
      method <- "pls"
      models[[method]] <- NULL
      gc()
    }
  )
  
  # output method summary text ----
  output$pls_MethodSummary <- renderText({
    method <- "pls"
    description(method)
  })

  # output resampling metrics table ----
  output$pls_Metrics <- renderTable({
    method <- "pls"
    mod <- models[[method]]
    req(mod)
    mod$results[ which.min(mod$results[, "RMSE"]), ]
  })
  
  # output hyperparameter tuning chart ----
  output$pls_ModelTune <- renderPlot({
    method <- "pls"
    mod <- models[[method]]
    req(mod)
    plot(mod)
  })     
  
  # output an html formatted recipe "print" ----
  output$pls_RecipePrint <- renderUI({
    method <- "pls"
    mod <- models[[method]]
    req(mod)
    html <- mod$recipe %>%
      print() %>%
      cli::cli_fmt() %>%
      cli::ansi_collapse(sep="<br>", last = "<br>") %>%
      cli::ansi_html(escape_reserved = FALSE) %>%
      gsub(pattern = "──────", replacement = "─",  x = ., fixed = TRUE)
    css <- paste(format(ansi_html_style()), collapse= "\n")
    tagList(
      tags$head(tags$style(css)),
      tags$pre(HTML(html))
    )
  })

  # output the recipe-output table ----
  output$pls_RecipeOutput <- renderTable({
    method <- "pls"
    mod <- models[[method]]
    req(mod)
    terms <- as.data.frame(mod$recipe$term_info)
    n <- dim(terms)[1]
    types <- vector(mode="character", length=n)
    for (row in 1:n) {
      types[row] <- paste(collapse = " ", unlist(terms$type[row]))
    }
    terms$type <- types
    terms |>
      dplyr::filter(role == "predictor") |>
      dplyr::select(type, source) |>
      dplyr::group_by(type, source) |>
      dplyr::summarise(count = n())
  })  

  # output the training summary print ----
  output$pls_TrainSummary <- renderPrint({
    method <- "pls"
    mod <- models[[method]]
    req(mod)
    print(mod)
  })
  
  # output coefficients table ----
  output$pls_Coef <- renderTable({
    req(models$pls)
    co <- coef(models$pls$finalModel)
    as.data.frame(co, row.names = rownames(co))
  }, rownames = TRUE, colnames = FALSE)
  
  
  # METHOD * rpart ---------------------------------------------------------------------------------------------------------------------------
  library(rpart)  #  <------ Declare any modelling packages that are needed (see Method List tab)
  library(rpart.plot)
  
  # reactive getRpartRecipe ----
  getRpartRecipe <- reactive({
    form <- formula(Response ~ .)
    recipes::recipe(form, data = getTrainData()) %>%
      dynamicSteps(input$rpart_Preprocess) %>%   # use <method>_Preprocess
      step_rm(has_type("date"))
  })
  
  # observe the GO event -----
  observeEvent(
    input$rpart_Go,
    {
      method <- "rpart"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      obj <- startMode(input$Parallel)
      tryCatch({
        model <- caret::train(getRpartRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(),
                              tuneLength = 5, na.action = na.rpart)  #<- note the rpart-specific value for na.action (not needed for other methods)
        deleteRds(method)
        saveToRds(model, method)
        models[[method]] <- model
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(obj)
      })
    }
  )

  observeEvent(
    input$rpart_Load,
    {
      method  <- "rpart"
      model <- loadRds(method, session)
      if (!is.null(model)) {
        models[[method]] <- model
      }
    }
  )
  
  observeEvent(
    input$rpart_Delete,
    {
      method <- "rpart"
      models[[method]] <- NULL
      gc()
    }
  )
  
  # output the method summary text ----
  output$rpart_MethodSummary <- renderText({
    method <- "rpart"
    description(method)
  })
  
  # output the resampling metrics table ----
  output$rpart_Metrics <- renderTable({
    method <- "rpart"
    mod <- models[[method]]
    req(mod)
    mod$results[ which.min(mod$results[, "RMSE"]), ]
  })
  
  # output recipe-outputs table ----
  output$rpart_RecipeOutput <- renderTable({
    method <- "rpart"
    mod <- models[[method]]
    req(mod)
    terms <- as.data.frame(mod$recipe$term_info)
    n <- dim(terms)[1]
    types <- vector(mode="character", length=n)
    for (row in 1:n) {
      types[row] <- paste(collapse = " ", unlist(terms$type[row]))
    }
    terms$type <- types
    terms |>
      dplyr::filter(role == "predictor") |>
      dplyr::select(type, source) |>
      dplyr::group_by(type, source) |>
      dplyr::summarise(count = n())
  })  

  # output hyperparameter tuning chart ----
  output$rpart_ModelTune <- renderPlot({
    method <- "rpart"
    mod <- models[[method]]
    req(mod)
    plot(mod)
  })
  
  # output a model tree-chart ----
  output$rpart_ModelTree <- renderPlot({
    method <- "rpart"
    mod <- models[[method]]
    req(mod)
    rpart.plot::rpart.plot(mod$finalModel, roundint = FALSE)
  })     
  
  # output an html formatted recipe print ----
  output$rpart_RecipePrint <- renderUI({
    method <- "rpart"
    mod <- models[[method]]
    req(mod)
    html <- mod$recipe %>%
      print() %>%
      cli::cli_fmt() %>%
      cli::ansi_collapse(sep="<br>", last = "<br>") %>%
      cli::ansi_html(escape_reserved = FALSE) %>%
      gsub(pattern = "──────", replacement = "─",  x = ., fixed = TRUE)
    css <- paste(format(ansi_html_style()), collapse= "\n")
    tagList(
      tags$head(tags$style(css)),
      tags$pre(HTML(html))
    )
    
  })

  # output a training summary print ----
  output$rpart_TrainSummary <- renderPrint({
    method <- "rpart"
    mod <- models[[method]]
    req(mod)
    print(mod)
  })
  
  # maintenance point ---------------------------------------------------------------------------------------------------------------------------
  # Add further methods here.  You have the pls, glmnet and rpart templates to paste here - each has different layout 
  # and plotting characteristics so choose a good one and/or change the code more substantially
  # METHOD * lmStepAIC ---------------------------------------------------------------------------------------------------------------------------
  library(MASS)  #  <------ Declare any modelling packages that are needed (see Method List tab)
  
  # reactive getPlsRecipe ----
  getstepwiseRecipe <- reactive({
    form <- formula(Response ~ .)
    recipes::recipe(form, data = getTrainData()) %>%
      dynamicSteps(input$stepwise_Preprocess) %>%   # use <method>_Preprocess
      step_rm(has_type("date"))   # remove original date variables
  })
  
  # observe GO event ----
  observeEvent(
    input$stepwise_Go,
    {
      method <- "lmStepAIC"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      obj <- startMode(input$Parallel)
      tryCatch({
        model <- caret::train(getstepwiseRecipe(), 
                              data = getTrainData(), 
                              method = method, 
                              metric = "RMSE", 
                              trControl = getTrControl(), 
                              tuneLength = 25,
                              na.action = na.pass)
        deleteRds(method)
        saveToRds(model, method)
        models[[method]] <- model
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(obj)
      })
    }
  )
  
  observeEvent(
    input$stepwise_Load,
    {
      method  <- "lmStepAIC"
      model <- loadRds(method, session)
      if (!is.null(model)) {
        models[[method]] <- model
      }
    }
  )
  
  observeEvent(
    input$stepwise_Delete,
    {
      method <- "lmStepAIC"
      models[[method]] <- NULL
      gc()
    }
  )
  
  # output method summary text ----
  output$stepwise_MethodSummary <- renderText({
    method <- "lmStepAIC"
    description(method)
  })
  
  # output resampling metrics table ----
  output$stepwise_Metrics <- renderTable({
    method <- "lmStepAIC"
    mod <- models[[method]]
    req(mod)
    mod$results[ which.min(mod$results[, "RMSE"]), ]
  })
  
 
  
  # output an html formatted recipe "print" ----
  output$stepwise_RecipePrint <- renderUI({
    method <- "lmStepAIC"
    mod <- models[[method]]
    req(mod)
    html <- mod$recipe %>%
      print() %>%
      cli::cli_fmt() %>%
      cli::ansi_collapse(sep="<br>", last = "<br>") %>%
      cli::ansi_html(escape_reserved = FALSE) %>%
      gsub(pattern = "──────", replacement = "─",  x = ., fixed = TRUE)
    css <- paste(format(ansi_html_style()), collapse= "\n")
    tagList(
      tags$head(tags$style(css)),
      tags$pre(HTML(html))
    )
  })
  
  # output the recipe-output table ----
  output$stepwise_RecipeOutput <- renderTable({
    method <- "lmStepAIC"
    mod <- models[[method]]
    req(mod)
    terms <- as.data.frame(mod$recipe$term_info)
    n <- dim(terms)[1]
    types <- vector(mode="character", length=n)
    for (row in 1:n) {
      types[row] <- paste(collapse = " ", unlist(terms$type[row]))
    }
    terms$type <- types
    terms |>
      dplyr::filter(role == "predictor") |>
      dplyr::select(type, source) |>
      dplyr::group_by(type, source) |>
      dplyr::summarise(count = n())
  })  
  
  # output the training summary print ----
  output$stepwise_TrainSummary <- renderPrint({
    method <- "lmStepAIC"
    mod <- models[[method]]
    req(mod)
    print(mod)
    
    cat("=== Caret Training Summary ===\n")
    print(mod)
    
    cat("\n\n=== Final Stepwise Model Summary ===\n")
    print(summary(mod$finalModel))
  })
  
  # output coefficients table ----
  output$stepwise_Coef <- renderTable({
    req(models$lmStepAIC)
    co <- coef(models$lmStepAIC$finalModel)
    as.data.frame(co, row.names = rownames(co))
  }, rownames = TRUE, colnames = FALSE)
  
  # Principal Component Regression ---------------------------------------------------------------------------------------------------------------------------
  
  library(pls)  
  
  # reactive getPlsRecipe ----
  getpcrRecipe <- reactive({
    form <- formula(Response ~ .)
    recipes::recipe(form, data = getTrainData()) %>%
      dynamicSteps(input$pcr_Preprocess) %>%   # use <method>_Preprocess
      step_rm(has_type("date"))   # remove original date variables
  })
  
  # observe GO event ----
  observeEvent(
    input$pcr_Go,
    {
      method <- "pcr"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      obj <- startMode(input$Parallel)
      tryCatch({
        
        rec <- getpcrRecipe()
        
        prep_rec <- recipes::prep(rec, training = getTrainData(), retain = TRUE)
        baked <- recipes::juice(prep_rec)
        
        max_ncomp <- min(
          25,
          ncol(baked) - 1,   
          nrow(baked) - 1
        )
        model <- caret::train(getpcrRecipe(), 
                              data = getTrainData(), 
                              method = method, 
                              metric = "RMSE",
                              trControl = getTrControl(), 
                              tuneGrid = expand.grid(ncomp = 1:max_ncomp),
                              na.action = na.pass)
        deleteRds(method)
        saveToRds(model, method)
        models[[method]] <- model
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(obj)
      })
    }
  )
  
  observeEvent(
    input$pcr_Load,
    {
      method  <- "pcr"
      model <- loadRds(method, session)
      if (!is.null(model)) {
        models[[method]] <- model
      }
    }
  )
  
  observeEvent(
    input$pcr_Delete,
    {
      method <- "pcr"
      models[[method]] <- NULL
      gc()
    }
  )
  
  # output method summary text ----
  output$pcr_MethodSummary <- renderText({
    method <- "pcr"
    description(method)
  })
  
  # output resampling metrics table ----
  output$pcr_Metrics <- renderTable({
    method <- "pcr"
    mod <- models[[method]]
    req(mod)
    mod$results[ which.min(mod$results[, "RMSE"]), ]
  })
  
  # output hyperparameter tuning chart ----
  output$pcr_ModelTune <- renderPlot({
    method <- "pcr"
    mod <- models[[method]]
    req(mod)
    plot(mod)
  })     
  
  # output an html formatted recipe "print" ----
  output$pcr_RecipePrint <- renderUI({
    method <- "pcr"
    mod <- models[[method]]
    req(mod)
    html <- mod$recipe %>%
      print() %>%
      cli::cli_fmt() %>%
      cli::ansi_collapse(sep="<br>", last = "<br>") %>%
      cli::ansi_html(escape_reserved = FALSE) %>%
      gsub(pattern = "──────", replacement = "─",  x = ., fixed = TRUE)
    css <- paste(format(ansi_html_style()), collapse= "\n")
    tagList(
      tags$head(tags$style(css)),
      tags$pre(HTML(html))
    )
  })
  
  # output the recipe-output table ----
  output$pcr_RecipeOutput <- renderTable({
    method <- "pcr"
    mod <- models[[method]]
    req(mod)
    terms <- as.data.frame(mod$recipe$term_info)
    n <- dim(terms)[1]
    types <- vector(mode="character", length=n)
    for (row in 1:n) {
      types[row] <- paste(collapse = " ", unlist(terms$type[row]))
    }
    terms$type <- types
    terms |>
      dplyr::filter(role == "predictor") |>
      dplyr::select(type, source) |>
      dplyr::group_by(type, source) |>
      dplyr::summarise(count = n())
  })  
  
  # output the training summary print ----
  output$pcr_TrainSummary <- renderPrint({
    method <- "pcr"
    mod <- models[[method]]
    req(mod)
    print(mod)
  })
  
  # output coefficients table ----
  output$pcr_Coef <- renderTable({
    req(models$pcr)
    co <- coef(models$pcr$finalModel)
    as.data.frame(co, row.names = rownames(co))
  }, rownames = TRUE, colnames = FALSE)
  
  # METHOD * ranger (random forest)  ---------------------------------------------------------------------------------------------------------------------------
  library(ranger)
  
  # reactive getRangerRecipe ----
  getRangerRecipe <- reactive({
    form <- formula(Response ~ .)
    recipes::recipe(form, data = getTrainData()) %>%
      dynamicSteps(input$ranger_Preprocess) %>%
      step_rm(has_type("date"))
  })
  
  # observe the GO event -----
  observeEvent(
    input$ranger_Go,
    {
      method <- "ranger"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      obj <- startMode(input$Parallel)
      tryCatch({
        model <- caret::train(
          getRangerRecipe(),
          data = getTrainData(),
          method = method,
          metric = "RMSE",
          trControl = getTrControl(),
          tuneLength = 5,
          importance = "impurity"
        )
        deleteRds(method)
        saveToRds(model, method)
        models[[method]] <- model
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(obj)
      })
    }
  )
  
  observeEvent(
    input$ranger_Load,
    {
      method  <- "ranger"
      model <- loadRds(method, session)
      if (!is.null(model)) {
        models[[method]] <- model
      }
    }
  )
  
  observeEvent(
    input$ranger_Delete,
    {
      method <- "ranger"
      models[[method]] <- NULL
      gc()
    }
  )
  
  # output the method summary text ----
  output$ranger_MethodSummary <- renderText({
    method <- "ranger"
    description(method)
  })
  
  # output the resampling metrics table ----
  output$ranger_Metrics <- renderTable({
    method <- "ranger"
    mod <- models[[method]]
    req(mod)
    mod$results[which.min(mod$results[, "RMSE"]), ]
  })
  
  # output recipe-outputs table ----
  output$ranger_RecipeOutput <- renderTable({
    method <- "ranger"
    mod <- models[[method]]
    req(mod)
    terms <- as.data.frame(mod$recipe$term_info)
    n <- dim(terms)[1]
    types <- vector(mode="character", length=n)
    for (row in 1:n) {
      types[row] <- paste(collapse = " ", unlist(terms$type[row]))
    }
    terms$type <- types
    terms |>
      dplyr::filter(role == "predictor") |>
      dplyr::select(type, source) |>
      dplyr::group_by(type, source) |>
      dplyr::summarise(count = n())
  })
  
  # output hyperparameter tuning chart ----
  output$ranger_ModelTune <- renderPlot({
    method <- "ranger"
    mod <- models[[method]]
    req(mod)
    plot(mod)
  })
  

  # output variable importance plot ----
  output$ranger_VarImp <- renderPlot({
    method <- "ranger"
    mod <- models[[method]]
    req(mod)
    varImp <- caret::varImp(mod)
    plot(varImp)
  })
  
  # output an html formatted recipe print ----
  output$ranger_RecipePrint <- renderUI({
    method <- "ranger"
    mod <- models[[method]]
    req(mod)
    html <- mod$recipe %>%
      print() %>%
      cli::cli_fmt() %>%
      cli::ansi_collapse(sep="<br>", last = "<br>") %>%
      cli::ansi_html(escape_reserved = FALSE) %>%
      gsub(pattern = "──────", replacement = "─", x = ., fixed = TRUE)
    css <- paste(format(ansi_html_style()), collapse= "\n")
    tagList(
      tags$head(tags$style(css)),
      tags$pre(HTML(html))
    )
  })
  
  # output a training summary print ----
  output$ranger_TrainSummary <- renderPrint({
    method <- "ranger"
    mod <- models[[method]]
    req(mod)
    print(mod)
  })
  
  # METHOD * gbm (gradient boosted trees)  ---------------------------------------------------------------------------------------------------------------------------
  library(gbm)
  library(plyr)
  
  # reactive getGbmRecipe ----
  getGbmRecipe <- reactive({
    form <- formula(Response ~ .)
    recipes::recipe(form, data = getTrainData()) %>%
      dynamicSteps(input$gbm_Preprocess) %>%
      step_rm(has_type("date"))
  })
  
  # observe the GO event -----
  observeEvent(
    input$gbm_Go,
    {
      method <- "gbm"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      obj <- startMode(input$Parallel)
      tryCatch({
        
        gbm_grid <- expand.grid(
          n.trees = c(100, 250, 500, 1000, 1500, 3000),
          interaction.depth = c(2, 4, 6),
          shrinkage = c(0.03, 0.1),
          n.minobsinnode = 10
        )
        
        model <- caret::train(
          getGbmRecipe(),
          data = getTrainData(),
          method = method,
          metric = "RMSE",
          trControl = getTrControl(),
          tuneGrid = gbm_grid,
          verbose = FALSE
        )
        
        deleteRds(method)
        saveToRds(model, method)
        models[[method]] <- model
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(obj)
      })
    }
  )
  
  observeEvent(
    input$gbm_Load,
    {
      method  <- "gbm"
      model <- loadRds(method, session)
      if (!is.null(model)) {
        models[[method]] <- model
      }
    }
  )
  
  observeEvent(
    input$gbm_Delete,
    {
      method <- "gbm"
      models[[method]] <- NULL
      gc()
    }
  )
  
  output$gbm_MethodSummary <- renderText({
    method <- "gbm"
    description(method)
  })
  
  output$gbm_Metrics <- renderTable({
    method <- "gbm"
    mod <- models[[method]]
    req(mod)
    mod$results[which.min(mod$results[, "RMSE"]), ]
  })
  
  output$gbm_RecipeOutput <- renderTable({
    method <- "gbm"
    mod <- models[[method]]
    req(mod)
    terms <- as.data.frame(mod$recipe$term_info)
    n <- dim(terms)[1]
    types <- vector(mode = "character", length = n)
    for (row in 1:n) {
      types[row] <- paste(collapse = " ", unlist(terms$type[row]))
    }
    terms$type <- types
    terms |>
      dplyr::filter(role == "predictor") |>
      dplyr::select(type, source) |>
      dplyr::group_by(type, source) |>
      dplyr::summarise(count = n())
  })
  
  output$gbm_ModelTune <- renderPlot({
    method <- "gbm"
    mod <- models[[method]]
    req(mod)
    
    res <- mod$results
    
    ggplot(res, aes(
      x = n.trees,
      y = RMSE,
      colour = factor(interaction.depth),
      group = interaction.depth
    )) +
      geom_line() +
      geom_point(size = 2) +
      facet_wrap(~ shrinkage, labeller = label_both) +
      labs(
        x = "Number of trees",
        y = "RMSE",
        colour = "Interaction depth"
      ) +
      theme_minimal()
  })
  
  output$gbm_VarImp <- renderPlot({
    method <- "gbm"
    mod <- models[[method]]
    req(mod)
    varImp <- caret::varImp(mod)
    plot(varImp)
  })
  
  output$gbm_RecipePrint <- renderUI({
    method <- "gbm"
    mod <- models[[method]]
    req(mod)
    html <- mod$recipe %>%
      print() %>%
      cli::cli_fmt() %>%
      cli::ansi_collapse(sep = "<br>", last = "<br>") %>%
      cli::ansi_html(escape_reserved = FALSE) %>%
      gsub(pattern = "──────", replacement = "─", x = ., fixed = TRUE)
    css <- paste(format(ansi_html_style()), collapse = "\n")
    tagList(
      tags$head(tags$style(css)),
      tags$pre(HTML(html))
    )
  })
  
  output$gbm_TrainSummary <- renderPrint({
    method <- "gbm"
    mod <- models[[method]]
    req(mod)
    print(mod)
  })
  # k-Nearest Neighbours Regression ---------------------------------------------------------------------------------------------------------------------------
  
  library(kknn)
  
  # reactive getKknnRecipe ----
  getkknnRecipe <- reactive({
    form <- formula(Response ~ .)
    recipes::recipe(form, data = getTrainData()) %>%
      dynamicSteps(input$kknn_Preprocess) %>%
      step_rm(has_type("date"))
  })
  
  # observe GO event ----
  observeEvent(
    input$kknn_Go,
    {
      method <- "kknn"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      obj <- startMode(input$Parallel)
      tryCatch({
        
        #tuning grid for knn hyperparams 
        knn_grid <-  expand.grid(
          kmax = seq(5, 50, by = 5),
          distance = c(1, 1.5, 2),
          kernel = c(
            "rectangular",
            "triangular",
            "epanechnikov",
            "biweight",
            "triweight",
            "cos",
            "inv",
            "gaussian",
            "optimal" )
          )
        
        #training knn model
        model <- caret::train(
          getkknnRecipe(),
          data = getTrainData(),
          method = method,
          metric = "RMSE",
          trControl = getTrControl(),
          tuneGrid = knn_grid,
          na.action = na.pass
        )
        deleteRds(method)
        saveToRds(model, method)
        models[[method]] <- model
      },
      finally = {
        removeNotification(id = method)
        stopMode(obj)
      })
    }
  )
  
  observeEvent(
    input$kknn_Load,
    {
      method <- "kknn"
      model <- loadRds(method, session)
      if (!is.null(model)) {
        models[[method]] <- model
      }
    }
  )
  
  observeEvent(
    input$kknn_Delete,
    {
      method <- "kknn"
      models[[method]] <- NULL
      gc()
    }
  )
  
  output$kknn_MethodSummary <- renderText({
    method <- "kknn"
    description(method)
  })
  
  output$kknn_Metrics <- renderTable({
    method <- "kknn"
    mod <- models[[method]]
    req(mod)
    mod$results[which.min(mod$results[, "RMSE"]), ]
  })
  
  output$kknn_ModelTune <- renderPlot({
    method <- "kknn"
    mod <- models[[method]]
    req(mod)
    plot(mod)
  })
  
  output$kknn_RecipePrint <- renderUI({
    method <- "kknn"
    mod <- models[[method]]
    req(mod)
    html <- mod$recipe %>%
      print() %>%
      cli::cli_fmt() %>%
      cli::ansi_collapse(sep = "<br>", last = "<br>") %>%
      cli::ansi_html(escape_reserved = FALSE) %>%
      gsub(pattern = "──────", replacement = "─", x = ., fixed = TRUE)
    css <- paste(format(ansi_html_style()), collapse = "\n")
    tagList(
      tags$head(tags$style(css)),
      tags$pre(HTML(html))
    )
  })
  
  output$kknn_RecipeOutput <- renderTable({
    method <- "kknn"
    mod <- models[[method]]
    req(mod)
    terms <- as.data.frame(mod$recipe$term_info)
    n <- dim(terms)[1]
    types <- vector(mode = "character", length = n)
    for (row in 1:n) {
      types[row] <- paste(collapse = " ", unlist(terms$type[row]))
    }
    terms$type <- types
    terms |>
      dplyr::filter(role == "predictor") |>
      dplyr::select(type, source) |>
      dplyr::group_by(type, source) |>
      dplyr::summarise(count = n())
  })
  
  output$kknn_TrainSummary <- renderPrint({
    method <- "kknn"
    mod <- models[[method]]
    req(mod)
    print(mod)
  })
  
  # output tuning/best parameters table ----
  output$kknn_Coef <- renderTable({
    req(models$kknn)
    models$kknn$bestTune
  }, rownames = FALSE)
  
  # Selected methods plot ---------------------------------------------------------------------------------------------------------------------------
  
  #Plot of selected models using code from lab files
  output$selected_methods <- renderPlot({
    modelInfo <- caret::getModelInfo()
    tags <- vector(mode = "list", length = length(modelInfo))
    Classification <- Regression <- ClassProbs <- rep(NA, length = length(modelInfo))
    for (i in seq(along = modelInfo)){
      tags[[i]] <- modelInfo[[i]]$tags
      Classification[i] <- ifelse("Classification" %in% modelInfo[[i]]$type, 1, 0)
      Regression[i] <- ifelse("Regression" %in% modelInfo[[i]]$type, 1, 0)
      ClassProbs[i] <- ifelse(is.null(modelInfo[[i]]$prob), 0, 1)
    }
    tabs <- table(unlist(tags))
    tabs <- tabs[order(tolower(names(tabs)))]
    terms <- names(tabs)
    terms <- terms[terms != ""]
    dat <- matrix(0, ncol = length(terms), nrow = length(tags))
    colnames(dat) <- terms
    hasTag <- lapply(tags, function(x, y) which(y %in% x), y = terms)
    for (i in seq(along = hasTag)) {
      dat[i, hasTag[[i]]] <- 1
    }
    dat <- cbind(Classification, Regression, dat)
    wide <- as.data.frame(dat, row.names = names(modelInfo))
    
    dd <- wide[wide$Regression == 1, ] %>%
      stats::dist(method = "euclidean") %>%
      stats::cmdscale(k = 2) %>%
      data.frame()
    
    dd$model <- rownames(dd)
    
    dd$selected <- dd$model %in% methods_used
    
    p_all <- ggplot(dd, aes(x = X1, y = X2)) +
      ggtitle("Methods Available in App") +
      xlab("Coordinate 1") +
      ylab("Coordinate 2") +
      
      # Draw grey (non-selected) first
      geom_point(
        data = subset(dd, !selected),
        color = "grey70",
        size = 3
      ) +
      
      # Draw selected on top
      geom_point(
        data = subset(dd, selected),
        color = "red",
        size = 3
      ) +
      
      # Labels only for selected
      ggrepel::geom_text_repel(
        data = subset(dd, selected),
        aes(label = model),
        color = "red",
        size = 5,
        max.overlaps = 50,
        na.rm = TRUE
      ) +
      
      theme(
        plot.title = element_text(lineheight = 1, face = "bold", hjust = 0.5),
        legend.position = "none"
      )

    p_all
  })
  
  # Neural Networks ---------------------------------------------------------------------------------------------------------------------------
  # METHOD * avNNet ---------------------------------------------------------------------------------------------------------------------------
  
  # reactive getAvNNetRecipe ----
  getAvNNetRecipe <- reactive({
    form <- formula(Response ~ .)
    recipes::recipe(form, data = getTrainData()) %>%
      dynamicSteps(input$avNNet_Preprocess) %>%
      step_rm(has_type("date"))
  })
  
  # observe GO event ----
  observeEvent(
    input$avNNet_Go,
    {
      method <- "avNNet"
      models[[method]] <- NULL
      
      showNotification(
        id = method,
        paste("Processing", method, "model using resampling"),
        session = session,
        duration = NULL
      )
      
      obj <- startMode(input$Parallel)
      
      tryCatch({
        
        #Neural net hyperparam tuning grid
        avNNetGrid <- expand.grid(
          size = c(1, 3, 5, 7),
          decay = c(0, 0.001, 0.01, 0.1),
          bag = c(FALSE, TRUE)
        )
        
        model <- caret::train(
          getAvNNetRecipe(),
          data = getTrainData(),
          method = method,
          metric = "RMSE",
          trControl = getTrControl(),
          tuneGrid = avNNetGrid,
          linout = TRUE,
          trace = FALSE,
          na.action = na.pass
        )
        
        deleteRds(method)
        saveToRds(model, method)
        models[[method]] <- model
        
      },
      finally = {
        removeNotification(id = method)
        stopMode(obj)
      })
    }
  )
  
  observeEvent(
    input$avNNet_Load,
    {
      method <- "avNNet"
      model <- loadRds(method, session)
      if (!is.null(model)) {
        models[[method]] <- model
      }
    }
  )
  
  observeEvent(
    input$avNNet_Delete,
    {
      method <- "avNNet"
      models[[method]] <- NULL
      gc()
    }
  )
  
  output$avNNet_MethodSummary <- renderText({
    method <- "avNNet"
    description(method)
  })
  
  output$avNNet_Metrics <- renderTable({
    method <- "avNNet"
    mod <- models[[method]]
    req(mod)
    mod$results[which.min(mod$results[, "RMSE"]), ]
  })
  
  output$avNNet_ModelTune <- renderPlot({
    method <- "avNNet"
    mod <- models[[method]]
    req(mod)
    plot(mod)
  })
  
  output$avNNet_RecipePrint <- renderUI({
    method <- "avNNet"
    mod <- models[[method]]
    req(mod)
    
    html <- mod$recipe %>%
      print() %>%
      cli::cli_fmt() %>%
      cli::ansi_collapse(sep = "<br>", last = "<br>") %>%
      cli::ansi_html(escape_reserved = FALSE) %>%
      gsub(pattern = "──────", replacement = "─", x = ., fixed = TRUE)
    
    css <- paste(format(ansi_html_style()), collapse = "\n")
    
    tagList(
      tags$head(tags$style(css)),
      tags$pre(HTML(html))
    )
  })
  
  output$avNNet_RecipeOutput <- renderTable({
    method <- "avNNet"
    mod <- models[[method]]
    req(mod)
    
    terms <- as.data.frame(mod$recipe$term_info)
    n <- dim(terms)[1]
    types <- vector(mode = "character", length = n)
    
    for (row in 1:n) {
      types[row] <- paste(collapse = " ", unlist(terms$type[row]))
    }
    
    terms$type <- types
    
    terms |>
      dplyr::filter(role == "predictor") |>
      dplyr::select(type, source) |>
      dplyr::group_by(type, source) |>
      dplyr::summarise(count = n(), .groups = "drop")
  })
  
  output$avNNet_TrainSummary <- renderPrint({
    method <- "avNNet"
    mod <- models[[method]]
    req(mod)
    print(mod)
  })
  
  # output tuning/best parameters table ----
  output$avNNet_Coef <- renderTable({
    req(models$avNNet)
    models$avNNet$bestTune
  }, rownames = FALSE)
  
  # METHOD * rbf ---------------------------------------------------------------------------------------------------------------------------
  # reactive getRbfRecipe ----
  getRbfRecipe <- reactive({
    form <- formula(Response ~ .)
    recipes::recipe(form, data = getTrainData()) %>%
      dynamicSteps(input$rbf_Preprocess) %>%
      step_rm(has_type("date"))
  })
  
  # observe GO event ----
  observeEvent(
    input$rbf_Go,
    {
      method <- "rbf"
      models[[method]] <- NULL
      
      showNotification(
        id = method,
        paste("Processing", method, "model using resampling"),
        session = session,
        duration = NULL
      )
      
      obj <- startMode(input$Parallel)
      
      tryCatch({
        
        # smaller grid for caret::rbf
        rbfGrid <- expand.grid(
          size = c(3)
        )
        
        model <- caret::train(
          getRbfRecipe(),
          data = getTrainData(),
          method = method,
          metric = "RMSE",
          trControl = getTrControl(),
          tuneGrid = rbfGrid,
          na.action = na.pass
        )
        
        deleteRds(method)
        saveToRds(model, method)
        models[[method]] <- model
        
      },
      finally = {
        removeNotification(id = method)
        stopMode(obj)
      })
    }
  )
  
  observeEvent(
    input$rbf_Load,
    {
      method <- "rbf"
      model <- loadRds(method, session)
      if (!is.null(model)) {
        models[[method]] <- model
      }
    }
  )
  
  observeEvent(
    input$rbf_Delete,
    {
      method <- "rbf"
      models[[method]] <- NULL
      gc()
    }
  )
  
  output$rbf_MethodSummary <- renderText({
    method <- "rbf"
    description(method)
  })
  
  output$rbf_Metrics <- renderTable({
    method <- "rbf"
    mod <- models[[method]]
    req(mod)
    mod$results[which.min(mod$results[, "RMSE"]), ]
  })
  
  output$rbf_ModelTune <- renderPlot({
    method <- "rbf"
    mod <- models[[method]]
    req(mod)
    plot(mod)
  })
  
  output$rbf_RecipePrint <- renderUI({
    method <- "rbf"
    mod <- models[[method]]
    req(mod)
    
    html <- mod$recipe %>%
      print() %>%
      cli::cli_fmt() %>%
      cli::ansi_collapse(sep = "<br>", last = "<br>") %>%
      cli::ansi_html(escape_reserved = FALSE) %>%
      gsub(pattern = "──────", replacement = "─", x = ., fixed = TRUE)
    
    css <- paste(format(ansi_html_style()), collapse = "\n")
    
    tagList(
      tags$head(tags$style(css)),
      tags$pre(HTML(html))
    )
  })
  
  output$rbf_RecipeOutput <- renderTable({
    method <- "rbf"
    mod <- models[[method]]
    req(mod)
    
    terms <- as.data.frame(mod$recipe$term_info)
    n <- dim(terms)[1]
    types <- vector(mode = "character", length = n)
    
    for (row in 1:n) {
      types[row] <- paste(collapse = " ", unlist(terms$type[row]))
    }
    
    terms$type <- types
    
    terms |>
      dplyr::filter(role == "predictor") |>
      dplyr::select(type, source) |>
      dplyr::group_by(type, source) |>
      dplyr::summarise(count = n(), .groups = "drop")
  })
  
  output$rbf_TrainSummary <- renderPrint({
    method <- "rbf"
    mod <- models[[method]]
    req(mod)
    print(mod)
  })
  
  # output tuning/best parameters table ----
  output$rbf_Coef <- renderTable({
    req(models[["rbf"]])
    models[["rbf"]]$bestTune
  }, rownames = FALSE)
  
  # METHOD * elm ---------------------------------------------------------------------------------------------------------------------------
  # library(elmNN)
  # reactive getElmRecipe ----
  getElmRecipe <- reactive({
    form <- formula(Response ~ .)
    recipes::recipe(form, data = getTrainData()) %>%
      dynamicSteps(input$elm_Preprocess) %>%
      step_rm(has_type("date"))
  })
  
  # observe GO event ----
  observeEvent(
    input$elm_Go,
    {
      method <- "elm"
      models[[method]] <- NULL
      
      showNotification(
        id = method,
        paste("Processing", method, "model using resampling"),
        session = session,
        duration = NULL
      )
      
      obj <- startMode(input$Parallel)
      
      tryCatch({
        
        # ELM hyperparameter tuning grid
        elmGrid <- expand.grid(
          nhid = c(1, 3, 5, 7, 9),
          actfun = c("purelin", "sig", "radbas")
        )
        
        model <- caret::train(
          getElmRecipe(),
          data = getTrainData(),
          method = method,
          metric = "RMSE",
          trControl = getTrControl(),
          tuneGrid = elmGrid,
          na.action = na.pass
        )
        
        deleteRds(method)
        saveToRds(model, method)
        models[[method]] <- model
        
      },
      finally = {
        removeNotification(id = method)
        stopMode(obj)
      })
    }
  )
  
  observeEvent(
    input$elm_Load,
    {
      method <- "elm"
      model <- loadRds(method, session)
      if (!is.null(model)) {
        models[[method]] <- model
      }
    }
  )
  
  observeEvent(
    input$elm_Delete,
    {
      method <- "elm"
      models[[method]] <- NULL
      gc()
    }
  )
  
  output$elm_MethodSummary <- renderText({
    method <- "elm"
    description(method)
  })
  
  output$elm_Metrics <- renderTable({
    method <- "elm"
    mod <- models[[method]]
    req(mod)
    mod$results[which.min(mod$results[, "RMSE"]), ]
  })
  
  output$elm_ModelTune <- renderPlot({
    method <- "elm"
    mod <- models[[method]]
    req(mod)
    plot(mod)
  })
  
  output$elm_RecipePrint <- renderUI({
    method <- "elm"
    mod <- models[[method]]
    req(mod)
    
    html <- mod$recipe %>%
      print() %>%
      cli::cli_fmt() %>%
      cli::ansi_collapse(sep = "<br>", last = "<br>") %>%
      cli::ansi_html(escape_reserved = FALSE) %>%
      gsub(pattern = "──────", replacement = "─", x = ., fixed = TRUE)
    
    css <- paste(format(ansi_html_style()), collapse = "\n")
    
    tagList(
      tags$head(tags$style(css)),
      tags$pre(HTML(html))
    )
  })
  
  output$elm_RecipeOutput <- renderTable({
    method <- "elm"
    mod <- models[[method]]
    req(mod)
    
    terms <- as.data.frame(mod$recipe$term_info)
    n <- dim(terms)[1]
    types <- vector(mode = "character", length = n)
    
    for (row in 1:n) {
      types[row] <- paste(collapse = " ", unlist(terms$type[row]))
    }
    
    terms$type <- types
    
    terms |>
      dplyr::filter(role == "predictor") |>
      dplyr::select(type, source) |>
      dplyr::group_by(type, source) |>
      dplyr::summarise(count = n(), .groups = "drop")
  })
  
  output$elm_TrainSummary <- renderPrint({
    method <- "elm"
    mod <- models[[method]]
    req(mod)
    print(mod)
  })
  
  # output tuning/best parameters table ----
  output$elm_Coef <- renderTable({
    req(models[["elm"]])
    models[["elm"]]$bestTune
  }, rownames = FALSE)
  
  # METHOD * brnn ---------------------------------------------------------------------------------------------------------------------------
  
  library(brnn)
  
  # reactive getBrnnRecipe ----
  getBrnnRecipe <- reactive({
    form <- formula(Response ~ .)
    recipes::recipe(form, data = getTrainData()) %>%
      dynamicSteps(input$brnn_Preprocess) %>%
      step_rm(has_type("date"))
  })
  
  # observe GO event ----
  observeEvent(
    input$brnn_Go,
    {
      method <- "brnn"
      models[[method]] <- NULL
      
      showNotification(
        id = method,
        paste("Processing", method, "model using resampling"),
        session = session,
        duration = NULL
      )
      
      obj <- startMode(input$Parallel)
      
      tryCatch({
        
        # BRNN hyperparameter tuning grid
        brnnGrid <- expand.grid(
          neurons = c(1, 3, 5, 7, 9)
        )
        
        model <- caret::train(
          getBrnnRecipe(),
          data = getTrainData(),
          method = method,
          metric = "RMSE",
          trControl = getTrControl(),
          tuneGrid = brnnGrid,
          na.action = na.pass
        )
        
        deleteRds(method)
        saveToRds(model, method)
        models[[method]] <- model
        
      },
      finally = {
        removeNotification(id = method)
        stopMode(obj)
      })
    }
  )
  
  observeEvent(
    input$brnn_Load,
    {
      method <- "brnn"
      model <- loadRds(method, session)
      if (!is.null(model)) {
        models[[method]] <- model
      }
    }
  )
  
  observeEvent(
    input$brnn_Delete,
    {
      method <- "brnn"
      models[[method]] <- NULL
      gc()
    }
  )
  
  output$brnn_MethodSummary <- renderText({
    method <- "brnn"
    description(method)
  })
  
  output$brnn_Metrics <- renderTable({
    method <- "brnn"
    mod <- models[[method]]
    req(mod)
    mod$results[which.min(mod$results[, "RMSE"]), ]
  })
  
  output$brnn_ModelTune <- renderPlot({
    method <- "brnn"
    mod <- models[[method]]
    req(mod)
    plot(mod)
  })
  
  output$brnn_RecipePrint <- renderUI({
    method <- "brnn"
    mod <- models[[method]]
    req(mod)
    
    html <- mod$recipe %>%
      print() %>%
      cli::cli_fmt() %>%
      cli::ansi_collapse(sep = "<br>", last = "<br>") %>%
      cli::ansi_html(escape_reserved = FALSE) %>%
      gsub(pattern = "──────", replacement = "─", x = ., fixed = TRUE)
    
    css <- paste(format(ansi_html_style()), collapse = "\n")
    
    tagList(
      tags$head(tags$style(css)),
      tags$pre(HTML(html))
    )
  })
  
  output$brnn_RecipeOutput <- renderTable({
    method <- "brnn"
    mod <- models[[method]]
    req(mod)
    
    terms <- as.data.frame(mod$recipe$term_info)
    n <- dim(terms)[1]
    types <- vector(mode = "character", length = n)
    
    for (row in 1:n) {
      types[row] <- paste(collapse = " ", unlist(terms$type[row]))
    }
    
    terms$type <- types
    
    terms |>
      dplyr::filter(role == "predictor") |>
      dplyr::select(type, source) |>
      dplyr::group_by(type, source) |>
      dplyr::summarise(count = n(), .groups = "drop")
  })
  
  output$brnn_TrainSummary <- renderPrint({
    method <- "brnn"
    mod <- models[[method]]
    req(mod)
    print(mod)
  })
  
  # output tuning/best parameters table ----
  output$brnn_Coef <- renderTable({
    req(models[["brnn"]])
    models[["brnn"]]$bestTune
  }, rownames = FALSE)
  
  # METHOD * dnn ---------------------------------------------------------------------------------------------------------------------------
  
  library(dnn)
  library(deepnet)
  
  # reactive getDnnRecipe ----
  getDnnRecipe <- reactive({
    form <- formula(Response ~ .)
    recipes::recipe(form, data = getTrainData()) %>%
      dynamicSteps(input$dnn_Preprocess) %>%
      step_rm(has_type("date"))
  })
  
  # observe GO event ----
  observeEvent(
    input$dnn_Go,
    {
      method <- "dnn"
      models[[method]] <- NULL
      
      showNotification(
        id = method,
        paste("Processing", method, "model using resampling"),
        session = session,
        duration = NULL
      )
      
      obj <- startMode(input$Parallel)
      
      tryCatch({
        
        # DNN hyperparameter tuning grid
        dnnGrid <- expand.grid(
          layer1 = c(32, 64, 128),
          layer2 = c(16, 32, 64),
          layer3 = c(0, 16, 32),
          hidden_dropout = c(0.0, 0.2, 0.4),
          visible_dropout = c(0.0, 0.2)
        )
        
        # dnnGrid <- expand.grid(
        #   layer1 = c(32, 64),
        #   layer2 = c(16, 32),
        #   layer3 = c(0, 16),          # 0 = no third layer
        #   hidden_dropout = c(0.0, 0.2),
        #   visible_dropout = c(0.0, 0.2)
        # )
        
        model <- caret::train(
          getDnnRecipe(),
          data = getTrainData(),
          method = method,
          metric = "RMSE",
          trControl = getTrControl(),
          tuneGrid = dnnGrid
        )
        
        deleteRds(method)
        saveToRds(model, method)
        models[[method]] <- model
        
      },
      finally = {
        removeNotification(id = method)
        stopMode(obj)
      })
    }
  )
  
  observeEvent(
    input$dnn_Load,
    {
      method <- "dnn"
      model <- loadRds(method, session)
      if (!is.null(model)) {
        models[[method]] <- model
      }
    }
  )
  
  observeEvent(
    input$dnn_Delete,
    {
      method <- "dnn"
      models[[method]] <- NULL
      gc()
    }
  )
  
  output$dnn_MethodSummary <- renderText({
    method <- "dnn"
    description(method)
  })
  
  output$dnn_Metrics <- renderTable({
    method <- "dnn"
    mod <- models[[method]]
    req(mod)
    mod$results[which.min(mod$results[, "RMSE"]), ]
  })
  
  output$dnn_ModelTune <- renderPlot({
    method <- "dnn"
    mod <- models[[method]]
    req(mod)
    plot(mod)
  })
  
  output$dnn_RecipePrint <- renderUI({
    method <- "dnn"
    mod <- models[[method]]
    req(mod)
    
    html <- mod$recipe %>%
      print() %>%
      cli::cli_fmt() %>%
      cli::ansi_collapse(sep = "<br>", last = "<br>") %>%
      cli::ansi_html(escape_reserved = FALSE) %>%
      gsub(pattern = "──────", replacement = "─", x = ., fixed = TRUE)
    
    css <- paste(format(ansi_html_style()), collapse = "\n")
    
    tagList(
      tags$head(tags$style(css)),
      tags$pre(HTML(html))
    )
  })
  
  output$dnn_RecipeOutput <- renderTable({
    method <- "dnn"
    mod <- models[[method]]
    req(mod)
    
    terms <- as.data.frame(mod$recipe$term_info)
    n <- dim(terms)[1]
    types <- vector(mode = "character", length = n)
    
    for (row in 1:n) {
      types[row] <- paste(collapse = " ", unlist(terms$type[row]))
    }
    
    terms$type <- types
    
    terms |>
      dplyr::filter(role == "predictor") |>
      dplyr::select(type, source) |>
      dplyr::group_by(type, source) |>
      dplyr::summarise(count = n(), .groups = "drop")
  })
  
  output$dnn_TrainSummary <- renderPrint({
    method <- "dnn"
    mod <- models[[method]]
    req(mod)
    print(mod)
  })
  
  # output tuning/best parameters table ----
  output$dnn_Coef <- renderTable({
    req(models[["dnn"]])
    models[["dnn"]]$bestTune
  }, rownames = FALSE)
  
  # METHOD * lssvmLinear ---------------------------------------------------------------------------------------------------------------------------
  
  library(kernlab)
  
  # reactive getLssvmLinearRecipe ----
  getLssvmLinearRecipe <- reactive({
    form <- formula(Response ~ .)
    recipes::recipe(form, data = getTrainData()) %>%
      dynamicSteps(input$lssvmLinear_Preprocess) %>%
      step_rm(has_type("date"))
  })
  
  # observe GO event ----
  observeEvent(
    input$lssvmLinear_Go,
    {
      method <- "lssvmLinear"
      models[[method]] <- NULL
      
      showNotification(
        id = method,
        paste("Processing", method, "model using resampling"),
        session = session,
        duration = NULL
      )
      
      obj <- startMode(input$Parallel)
      
      tryCatch({
        
        # lssvmLinear tuning grid: tau only
        lssvmLinearGrid <- expand.grid(
          tau = c(0.001, 0.003, 0.01, 0.03, 0.1, 0.3, 1)
        )
        
        model <- caret::train(
          getLssvmLinearRecipe(),
          data = getTrainData(),
          method = method,
          metric = "RMSE",
          trControl = getTrControl(),
          tuneGrid = lssvmLinearGrid,
          na.action = na.pass
        )
        
        deleteRds(method)
        saveToRds(model, method)
        models[[method]] <- model
        
      },
      finally = {
        removeNotification(id = method)
        stopMode(obj)
      })
    }
  )
  
  observeEvent(
    input$lssvmLinear_Load,
    {
      method <- "lssvmLinear"
      model <- loadRds(method, session)
      if (!is.null(model)) {
        models[[method]] <- model
      }
    }
  )
  
  observeEvent(
    input$lssvmLinear_Delete,
    {
      method <- "lssvmLinear"
      models[[method]] <- NULL
      gc()
    }
  )
  
  output$lssvmLinear_MethodSummary <- renderText({
    method <- "lssvmLinear"
    description(method)
  })
  
  output$lssvmLinear_Metrics <- renderTable({
    method <- "lssvmLinear"
    mod <- models[[method]]
    req(mod)
    mod$results[which.min(mod$results[, "RMSE"]), ]
  })
  
  output$lssvmLinear_ModelTune <- renderPlot({
    method <- "lssvmLinear"
    mod <- models[[method]]
    req(mod)
    plot(mod)
  })
  
  output$lssvmLinear_RecipePrint <- renderUI({
    method <- "lssvmLinear"
    mod <- models[[method]]
    req(mod)
    
    html <- mod$recipe %>%
      print() %>%
      cli::cli_fmt() %>%
      cli::ansi_collapse(sep = "<br>", last = "<br>") %>%
      cli::ansi_html(escape_reserved = FALSE) %>%
      gsub(pattern = "──────", replacement = "─", x = ., fixed = TRUE)
    
    css <- paste(format(ansi_html_style()), collapse = "\n")
    
    tagList(
      tags$head(tags$style(css)),
      tags$pre(HTML(html))
    )
  })
  
  output$lssvmLinear_RecipeOutput <- renderTable({
    method <- "lssvmLinear"
    mod <- models[[method]]
    req(mod)
    
    terms <- as.data.frame(mod$recipe$term_info)
    n <- dim(terms)[1]
    types <- vector(mode = "character", length = n)
    
    for (row in 1:n) {
      types[row] <- paste(collapse = " ", unlist(terms$type[row]))
    }
    
    terms$type <- types
    
    terms |>
      dplyr::filter(role == "predictor") |>
      dplyr::select(type, source) |>
      dplyr::group_by(type, source) |>
      dplyr::summarise(count = n(), .groups = "drop")
  })
  
  output$lssvmLinear_TrainSummary <- renderPrint({
    method <- "lssvmLinear"
    mod <- models[[method]]
    req(mod)
    print(mod)
  })
  
  # output tuning/best parameters table ----
  output$lssvmLinear_Coef <- renderTable({
    req(models[["lssvmLinear"]])
    models[["lssvmLinear"]]$bestTune
  }, rownames = FALSE)
  
  # METHOD * svmLinear ---------------------------------------------------------------------------------------------------------------------------
  
  library(kernlab)
  
  # reactive getSvmLinearRecipe ----
  getSvmLinearRecipe <- reactive({
    form <- formula(Response ~ .)
    recipes::recipe(form, data = getTrainData()) %>%
      dynamicSteps(input$svmLinear_Preprocess) %>%
      step_rm(has_type("date"))
  })
  
  # observe GO event ----
  observeEvent(
    input$svmLinear_Go,
    {
      method <- "svmLinear"
      models[[method]] <- NULL
      
      showNotification(
        id = method,
        paste("Processing", method, "model using resampling"),
        session = session,
        duration = NULL
      )
      
      obj <- startMode(input$Parallel)
      
      tryCatch({
        
        # svmLinear tuning grid: C only
        svmLinearGrid <- expand.grid(
          C = c(0.001, 0.01, 0.1, 1, 10)
        )
        
        model <- caret::train(
          getSvmLinearRecipe(),
          data = getTrainData(),
          method = method,
          metric = "RMSE",
          trControl = getTrControl(),
          tuneGrid = svmLinearGrid,
          na.action = na.pass
        )
        
        deleteRds(method)
        saveToRds(model, method)
        models[[method]] <- model
        
      },
      finally = {
        removeNotification(id = method)
        stopMode(obj)
      })
    }
  )
  
  observeEvent(
    input$svmLinear_Load,
    {
      method <- "svmLinear"
      model <- loadRds(method, session)
      if (!is.null(model)) {
        models[[method]] <- model
      }
    }
  )
  
  observeEvent(
    input$svmLinear_Delete,
    {
      method <- "svmLinear"
      models[[method]] <- NULL
      gc()
    }
  )
  
  output$svmLinear_MethodSummary <- renderText({
    method <- "svmLinear"
    description(method)
  })
  
  output$svmLinear_Metrics <- renderTable({
    method <- "svmLinear"
    mod <- models[[method]]
    req(mod)
    mod$results[which.min(mod$results[, "RMSE"]), ]
  })
  
  output$svmLinear_ModelTune <- renderPlot({
    method <- "svmLinear"
    mod <- models[[method]]
    req(mod)
    plot(mod)
  })
  
  output$svmLinear_RecipePrint <- renderUI({
    method <- "svmLinear"
    mod <- models[[method]]
    req(mod)
    
    html <- mod$recipe %>%
      print() %>%
      cli::cli_fmt() %>%
      cli::ansi_collapse(sep = "<br>", last = "<br>") %>%
      cli::ansi_html(escape_reserved = FALSE) %>%
      gsub(pattern = "──────", replacement = "─", x = ., fixed = TRUE)
    
    css <- paste(format(ansi_html_style()), collapse = "\n")
    
    tagList(
      tags$head(tags$style(css)),
      tags$pre(HTML(html))
    )
  })
  
  output$svmLinear_RecipeOutput <- renderTable({
    method <- "svmLinear"
    mod <- models[[method]]
    req(mod)
    
    terms <- as.data.frame(mod$recipe$term_info)
    n <- dim(terms)[1]
    types <- vector(mode = "character", length = n)
    
    for (row in 1:n) {
      types[row] <- paste(collapse = " ", unlist(terms$type[row]))
    }
    
    terms$type <- types
    
    terms |>
      dplyr::filter(role == "predictor") |>
      dplyr::select(type, source) |>
      dplyr::group_by(type, source) |>
      dplyr::summarise(count = n(), .groups = "drop")
  })
  
  output$svmLinear_TrainSummary <- renderPrint({
    method <- "svmLinear"
    mod <- models[[method]]
    req(mod)
    print(mod)
  })
  
  # output tuning/best parameters table ----
  output$svmLinear_Coef <- renderTable({
    req(models[["svmLinear"]])
    models[["svmLinear"]]$bestTune
  }, rownames = FALSE)
  
  library(kernlab)
  
  # METHOD * svmLinear ---------------------------------------------------------------------------------------------------------------------------
  
  # reactive getSvmLinearRecipe ----
  getSvmLinearRecipe <- reactive({
    form <- formula(Response ~ .)
    recipes::recipe(form, data = getTrainData()) %>%
      dynamicSteps(input$svmLinear_Preprocess) %>%
      step_rm(has_type("date"))
  })
  
  # observe GO event ----
  observeEvent(
    input$svmLinear_Go,
    {
      method <- "svmLinear"
      models[[method]] <- NULL
      
      showNotification(
        id = method,
        paste("Processing", method, "model using resampling"),
        session = session,
        duration = NULL
      )
      
      obj <- startMode(input$Parallel)
      
      tryCatch({
        
        # svmLinear tuning grid: C only
        svmLinearGrid <- expand.grid(
          C = c(0.001, 0.01, 0.1, 1, 10)
        )
        
        model <- caret::train(
          getSvmLinearRecipe(),
          data = getTrainData(),
          method = method,
          metric = "RMSE",
          trControl = getTrControl(),
          tuneGrid = svmLinearGrid,
          na.action = na.pass
        )
        
        deleteRds(method)
        saveToRds(model, method)
        models[[method]] <- model
        
      },
      finally = {
        removeNotification(id = method)
        stopMode(obj)
      })
    }
  )
  
  observeEvent(
    input$svmLinear_Load,
    {
      method <- "svmLinear"
      model <- loadRds(method, session)
      if (!is.null(model)) {
        models[[method]] <- model
      }
    }
  )
  
  observeEvent(
    input$svmLinear_Delete,
    {
      method <- "svmLinear"
      models[[method]] <- NULL
      gc()
    }
  )
  
  output$svmLinear_MethodSummary <- renderText({
    method <- "svmLinear"
    description(method)
  })
  
  output$svmLinear_Metrics <- renderTable({
    method <- "svmLinear"
    mod <- models[[method]]
    req(mod)
    mod$results[which.min(mod$results[, "RMSE"]), ]
  })
  
  output$svmLinear_ModelTune <- renderPlot({
    method <- "svmLinear"
    mod <- models[[method]]
    req(mod)
    plot(mod)
  })
  
  output$svmLinear_RecipePrint <- renderUI({
    method <- "svmLinear"
    mod <- models[[method]]
    req(mod)
    
    html <- mod$recipe %>%
      print() %>%
      cli::cli_fmt() %>%
      cli::ansi_collapse(sep = "<br>", last = "<br>") %>%
      cli::ansi_html(escape_reserved = FALSE) %>%
      gsub(pattern = "──────", replacement = "─", x = ., fixed = TRUE)
    
    css <- paste(format(ansi_html_style()), collapse = "\n")
    
    tagList(
      tags$head(tags$style(css)),
      tags$pre(HTML(html))
    )
  })
  
  output$svmLinear_RecipeOutput <- renderTable({
    method <- "svmLinear"
    mod <- models[[method]]
    req(mod)
    
    terms <- as.data.frame(mod$recipe$term_info)
    n <- dim(terms)[1]
    types <- vector(mode = "character", length = n)
    
    for (row in 1:n) {
      types[row] <- paste(collapse = " ", unlist(terms$type[row]))
    }
    
    terms$type <- types
    
    terms |>
      dplyr::filter(role == "predictor") |>
      dplyr::select(type, source) |>
      dplyr::group_by(type, source) |>
      dplyr::summarise(count = n(), .groups = "drop")
  })
  
  output$svmLinear_TrainSummary <- renderPrint({
    method <- "svmLinear"
    mod <- models[[method]]
    req(mod)
    print(mod)
  })
  
  # output tuning/best parameters table ----
  output$svmLinear_Coef <- renderTable({
    req(models[["svmLinear"]])
    models[["svmLinear"]]$bestTune
  }, rownames = FALSE)
  
  
  # METHOD * svmPoly ---------------------------------------------------------------------------------------------------------------------------
  
  # reactive getSvmPolyRecipe ----
  getSvmPolyRecipe <- reactive({
    form <- formula(Response ~ .)
    recipes::recipe(form, data = getTrainData()) %>%
      dynamicSteps(input$svmPoly_Preprocess) %>%
      step_rm(has_type("date"))
  })
  
  # observe GO event ----
  observeEvent(
    input$svmPoly_Go,
    {
      method <- "svmPoly"
      models[[method]] <- NULL
      
      showNotification(
        id = method,
        paste("Processing", method, "model using resampling"),
        session = session,
        duration = NULL
      )
      
      obj <- startMode(input$Parallel)
      
      tryCatch({
        
        # svmPoly tuning grid: degree, scale, C
        svmPolyGrid <- expand.grid(
          degree = c(2, 3, 4),
          scale = c(0.001, 0.01, 0.1),
          C = c(0.1, 1, 10)
        )
        
        model <- caret::train(
          getSvmPolyRecipe(),
          data = getTrainData(),
          method = method,
          metric = "RMSE",
          trControl = getTrControl(),
          tuneGrid = svmPolyGrid,
          na.action = na.pass
        )
        
        deleteRds(method)
        saveToRds(model, method)
        models[[method]] <- model
        
      },
      finally = {
        removeNotification(id = method)
        stopMode(obj)
      })
    }
  )
  
  observeEvent(
    input$svmPoly_Load,
    {
      method <- "svmPoly"
      model <- loadRds(method, session)
      if (!is.null(model)) {
        models[[method]] <- model
      }
    }
  )
  
  observeEvent(
    input$svmPoly_Delete,
    {
      method <- "svmPoly"
      models[[method]] <- NULL
      gc()
    }
  )
  
  output$svmPoly_MethodSummary <- renderText({
    method <- "svmPoly"
    description(method)
  })
  
  output$svmPoly_Metrics <- renderTable({
    method <- "svmPoly"
    mod <- models[[method]]
    req(mod)
    mod$results[which.min(mod$results[, "RMSE"]), ]
  })
  
  output$svmPoly_ModelTune <- renderPlot({
    method <- "svmPoly"
    mod <- models[[method]]
    req(mod)
    plot(mod)
  })
  
  output$svmPoly_RecipePrint <- renderUI({
    method <- "svmPoly"
    mod <- models[[method]]
    req(mod)
    
    html <- mod$recipe %>%
      print() %>%
      cli::cli_fmt() %>%
      cli::ansi_collapse(sep = "<br>", last = "<br>") %>%
      cli::ansi_html(escape_reserved = FALSE) %>%
      gsub(pattern = "──────", replacement = "─", x = ., fixed = TRUE)
    
    css <- paste(format(ansi_html_style()), collapse = "\n")
    
    tagList(
      tags$head(tags$style(css)),
      tags$pre(HTML(html))
    )
  })
  
  output$svmPoly_RecipeOutput <- renderTable({
    method <- "svmPoly"
    mod <- models[[method]]
    req(mod)
    
    terms <- as.data.frame(mod$recipe$term_info)
    n <- dim(terms)[1]
    types <- vector(mode = "character", length = n)
    
    for (row in 1:n) {
      types[row] <- paste(collapse = " ", unlist(terms$type[row]))
    }
    
    terms$type <- types
    
    terms |>
      dplyr::filter(role == "predictor") |>
      dplyr::select(type, source) |>
      dplyr::group_by(type, source) |>
      dplyr::summarise(count = n(), .groups = "drop")
  })
  
  output$svmPoly_TrainSummary <- renderPrint({
    method <- "svmPoly"
    mod <- models[[method]]
    req(mod)
    print(mod)
  })
  
  # output tuning/best parameters table ----
  output$svmPoly_Coef <- renderTable({
    req(models[["svmPoly"]])
    models[["svmPoly"]]$bestTune
  }, rownames = FALSE)
  
  
  # METHOD * svmExpoString ---------------------------------------------------------------------------------------------------------------------------
  
  # reactive getSvmExpoStringRecipe ----
  getSvmExpoStringRecipe <- reactive({
    form <- formula(Response ~ .)
    recipes::recipe(form, data = getTrainData()) %>%
      dynamicSteps(input$svmExpoString_Preprocess) %>%
      step_rm(has_type("date"))
  })
  
  # observe GO event ----
  observeEvent(
    input$svmExpoString_Go,
    {
      method <- "svmExpoString"
      models[[method]] <- NULL
      
      showNotification(
        id = method,
        paste("Processing", method, "model using resampling"),
        session = session,
        duration = NULL
      )
      
      obj <- startMode(input$Parallel)
      
      tryCatch({
        
        # svmExpoString tuning grid: lambda, C
        svmExpoStringGrid <- expand.grid(
          lambda = c(0.001, 0.01, 0.1, 1),
          C = c(0.1, 1, 10)
        )
        
        model <- caret::train(
          getSvmExpoStringRecipe(),
          data = getTrainData(),
          method = method,
          metric = "RMSE",
          trControl = getTrControl(),
          tuneGrid = svmExpoStringGrid,
          na.action = na.pass
        )
        
        deleteRds(method)
        saveToRds(model, method)
        models[[method]] <- model
        
      },
      finally = {
        removeNotification(id = method)
        stopMode(obj)
      })
    }
  )
  
  observeEvent(
    input$svmExpoString_Load,
    {
      method <- "svmExpoString"
      model <- loadRds(method, session)
      if (!is.null(model)) {
        models[[method]] <- model
      }
    }
  )
  
  observeEvent(
    input$svmExpoString_Delete,
    {
      method <- "svmExpoString"
      models[[method]] <- NULL
      gc()
    }
  )
  
  output$svmExpoString_MethodSummary <- renderText({
    method <- "svmExpoString"
    description(method)
  })
  
  output$svmExpoString_Metrics <- renderTable({
    method <- "svmExpoString"
    mod <- models[[method]]
    req(mod)
    mod$results[which.min(mod$results[, "RMSE"]), ]
  })
  
  output$svmExpoString_ModelTune <- renderPlot({
    method <- "svmExpoString"
    mod <- models[[method]]
    req(mod)
    plot(mod)
  })
  
  output$svmExpoString_RecipePrint <- renderUI({
    method <- "svmExpoString"
    mod <- models[[method]]
    req(mod)
    
    html <- mod$recipe %>%
      print() %>%
      cli::cli_fmt() %>%
      cli::ansi_collapse(sep = "<br>", last = "<br>") %>%
      cli::ansi_html(escape_reserved = FALSE) %>%
      gsub(pattern = "──────", replacement = "─", x = ., fixed = TRUE)
    
    css <- paste(format(ansi_html_style()), collapse = "\n")
    
    tagList(
      tags$head(tags$style(css)),
      tags$pre(HTML(html))
    )
  })
  
  output$svmExpoString_RecipeOutput <- renderTable({
    method <- "svmExpoString"
    mod <- models[[method]]
    req(mod)
    
    terms <- as.data.frame(mod$recipe$term_info)
    n <- dim(terms)[1]
    types <- vector(mode = "character", length = n)
    
    for (row in 1:n) {
      types[row] <- paste(collapse = " ", unlist(terms$type[row]))
    }
    
    terms$type <- types
    
    terms |>
      dplyr::filter(role == "predictor") |>
      dplyr::select(type, source) |>
      dplyr::group_by(type, source) |>
      dplyr::summarise(count = n(), .groups = "drop")
  })
  
  output$svmExpoString_TrainSummary <- renderPrint({
    method <- "svmExpoString"
    mod <- models[[method]]
    req(mod)
    print(mod)
  })
  
  # output tuning/best parameters table ----
  output$svmExpoString_Coef <- renderTable({
    req(models[["svmExpoString"]])
    models[["svmExpoString"]]$bestTune
  }, rownames = FALSE)
  
  
  # METHOD * svmRadial ---------------------------------------------------------------------------------------------------------------------------
  
  # reactive getSvmRadialRecipe ----
  getSvmRadialRecipe <- reactive({
    form <- formula(Response ~ .)
    recipes::recipe(form, data = getTrainData()) %>%
      dynamicSteps(input$svmRadial_Preprocess) %>%
      step_rm(has_type("date"))
  })
  
  # observe GO event ----
  observeEvent(
    input$svmRadial_Go,
    {
      method <- "svmRadial"
      models[[method]] <- NULL
      
      showNotification(
        id = method,
        paste("Processing", method, "model using resampling"),
        session = session,
        duration = NULL
      )
      
      obj <- startMode(input$Parallel)
      
      tryCatch({
        
        # svmRadial tuning grid: sigma, C
        svmRadialGrid <- expand.grid(
          sigma = c(0.001, 0.01, 0.1),
          C = c(0.1, 1, 10)
        )
        
        model <- caret::train(
          getSvmRadialRecipe(),
          data = getTrainData(),
          method = method,
          metric = "RMSE",
          trControl = getTrControl(),
          tuneGrid = svmRadialGrid,
          na.action = na.pass
        )
        
        deleteRds(method)
        saveToRds(model, method)
        models[[method]] <- model
        
      },
      finally = {
        removeNotification(id = method)
        stopMode(obj)
      })
    }
  )
  
  observeEvent(
    input$svmRadial_Load,
    {
      method <- "svmRadial"
      model <- loadRds(method, session)
      if (!is.null(model)) {
        models[[method]] <- model
      }
    }
  )
  
  observeEvent(
    input$svmRadial_Delete,
    {
      method <- "svmRadial"
      models[[method]] <- NULL
      gc()
    }
  )
  
  output$svmRadial_MethodSummary <- renderText({
    method <- "svmRadial"
    description(method)
  })
  
  output$svmRadial_Metrics <- renderTable({
    method <- "svmRadial"
    mod <- models[[method]]
    req(mod)
    mod$results[which.min(mod$results[, "RMSE"]), ]
  })
  
  output$svmRadial_ModelTune <- renderPlot({
    method <- "svmRadial"
    mod <- models[[method]]
    req(mod)
    plot(mod)
  })
  
  output$svmRadial_RecipePrint <- renderUI({
    method <- "svmRadial"
    mod <- models[[method]]
    req(mod)
    
    html <- mod$recipe %>%
      print() %>%
      cli::cli_fmt() %>%
      cli::ansi_collapse(sep = "<br>", last = "<br>") %>%
      cli::ansi_html(escape_reserved = FALSE) %>%
      gsub(pattern = "──────", replacement = "─", x = ., fixed = TRUE)
    
    css <- paste(format(ansi_html_style()), collapse = "\n")
    
    tagList(
      tags$head(tags$style(css)),
      tags$pre(HTML(html))
    )
  })
  
  output$svmRadial_RecipeOutput <- renderTable({
    method <- "svmRadial"
    mod <- models[[method]]
    req(mod)
    
    terms <- as.data.frame(mod$recipe$term_info)
    n <- dim(terms)[1]
    types <- vector(mode = "character", length = n)
    
    for (row in 1:n) {
      types[row] <- paste(collapse = " ", unlist(terms$type[row]))
    }
    
    terms$type <- types
    
    terms |>
      dplyr::filter(role == "predictor") |>
      dplyr::select(type, source) |>
      dplyr::group_by(type, source) |>
      dplyr::summarise(count = n(), .groups = "drop")
  })
  
  output$svmRadial_TrainSummary <- renderPrint({
    method <- "svmRadial"
    mod <- models[[method]]
    req(mod)
    print(mod)
  })
  
  # output tuning/best parameters table ----
  output$svmRadial_Coef <- renderTable({
    req(models[["svmRadial"]])
    models[["svmRadial"]]$bestTune
  }, rownames = FALSE)
  
  
  # METHOD * svmSpectrumString ---------------------------------------------------------------------------------------------------------------------------
  library(earth)
  # reactive getSvmSpectrumStringRecipe ----
  getSvmSpectrumStringRecipe <- reactive({
    form <- formula(Response ~ .)
    recipes::recipe(form, data = getTrainData()) %>%
      dynamicSteps(input$svmSpectrumString_Preprocess) %>%
      step_rm(has_type("date"))
  })
  
  # observe GO event ----
  observeEvent(
    input$svmSpectrumString_Go,
    {
      method <- "svmSpectrumString"
      models[[method]] <- NULL
      
      showNotification(
        id = method,
        paste("Processing", method, "model using resampling"),
        session = session,
        duration = NULL
      )
      
      obj <- startMode(input$Parallel)
      
      tryCatch({
        
        # svmSpectrumString tuning grid: length, C
        svmSpectrumStringGrid <- expand.grid(
          length = c(3, 4, 5, 6),
          C = c(0.1, 1, 10)
        )
        
        model <- caret::train(
          getSvmSpectrumStringRecipe(),
          data = getTrainData(),
          method = method,
          metric = "RMSE",
          trControl = getTrControl(),
          tuneGrid = svmSpectrumStringGrid,
          na.action = na.pass
        )
        
        deleteRds(method)
        saveToRds(model, method)
        models[[method]] <- model
        
      },
      finally = {
        removeNotification(id = method)
        stopMode(obj)
      })
    }
  )
  
  observeEvent(
    input$svmSpectrumString_Load,
    {
      method <- "svmSpectrumString"
      model <- loadRds(method, session)
      if (!is.null(model)) {
        models[[method]] <- model
      }
    }
  )
  
  observeEvent(
    input$svmSpectrumString_Delete,
    {
      method <- "svmSpectrumString"
      models[[method]] <- NULL
      gc()
    }
  )
  
  output$svmSpectrumString_MethodSummary <- renderText({
    method <- "svmSpectrumString"
    description(method)
  })
  
  output$svmSpectrumString_Metrics <- renderTable({
    method <- "svmSpectrumString"
    mod <- models[[method]]
    req(mod)
    mod$results[which.min(mod$results[, "RMSE"]), ]
  })
  
  output$svmSpectrumString_ModelTune <- renderPlot({
    method <- "svmSpectrumString"
    mod <- models[[method]]
    req(mod)
    plot(mod)
  })
  
  output$svmSpectrumString_RecipePrint <- renderUI({
    method <- "svmSpectrumString"
    mod <- models[[method]]
    req(mod)
    
    html <- mod$recipe %>%
      print() %>%
      cli::cli_fmt() %>%
      cli::ansi_collapse(sep = "<br>", last = "<br>") %>%
      cli::ansi_html(escape_reserved = FALSE) %>%
      gsub(pattern = "──────", replacement = "─", x = ., fixed = TRUE)
    
    css <- paste(format(ansi_html_style()), collapse = "\n")
    
    tagList(
      tags$head(tags$style(css)),
      tags$pre(HTML(html))
    )
  })
  
  output$svmSpectrumString_RecipeOutput <- renderTable({
    method <- "svmSpectrumString"
    mod <- models[[method]]
    req(mod)
    
    terms <- as.data.frame(mod$recipe$term_info)
    n <- dim(terms)[1]
    types <- vector(mode = "character", length = n)
    
    for (row in 1:n) {
      types[row] <- paste(collapse = " ", unlist(terms$type[row]))
    }
    
    terms$type <- types
    
    terms |>
      dplyr::filter(role == "predictor") |>
      dplyr::select(type, source) |>
      dplyr::group_by(type, source) |>
      dplyr::summarise(count = n(), .groups = "drop")
  })
  
  output$svmSpectrumString_TrainSummary <- renderPrint({
    method <- "svmSpectrumString"
    mod <- models[[method]]
    req(mod)
    print(mod)
  })
  
  # output tuning/best parameters table ----
  output$svmSpectrumString_Coef <- renderTable({
    req(models[["svmSpectrumString"]])
    models[["svmSpectrumString"]]$bestTune
  }, rownames = FALSE)
  
  
  # METHOD * gaussprLinear ---------------------------------------------------------------------------------------------------------------------------
  library(kernlab)
  # reactive getGaussprLinearRecipe ----
  getGaussprLinearRecipe <- reactive({
    form <- formula(Response ~ .)
    recipes::recipe(form, data = getTrainData()) %>%
      dynamicSteps(input$gaussprLinear_Preprocess) %>%
      step_rm(has_type("date"))
  })
  
  # observe GO event ----
  observeEvent(
    input$gaussprLinear_Go,
    {
      method <- "gaussprLinear"
      models[[method]] <- NULL
      
      showNotification(
        id = method,
        paste("Processing", method, "model using resampling"),
        session = session,
        duration = NULL
      )
      
      obj <- startMode(input$Parallel)
      
      tryCatch({
        
        # gaussprLinear has no tuneGrid
        
        model <- caret::train(
          getGaussprLinearRecipe(),
          data = getTrainData(),
          method = method,
          metric = "RMSE",
          trControl = getTrControl(),
          na.action = na.pass
        )
        
        deleteRds(method)
        saveToRds(model, method)
        models[[method]] <- model
        
      },
      finally = {
        removeNotification(id = method)
        stopMode(obj)
      })
    }
  )
  
  observeEvent(
    input$gaussprLinear_Load,
    {
      method <- "gaussprLinear"
      model <- loadRds(method, session)
      if (!is.null(model)) {
        models[[method]] <- model
      }
    }
  )
  
  observeEvent(
    input$gaussprLinear_Delete,
    {
      method <- "gaussprLinear"
      models[[method]] <- NULL
      gc()
    }
  )
  
  output$gaussprLinear_MethodSummary <- renderText({
    method <- "gaussprLinear"
    description(method)
  })
  
  output$gaussprLinear_Metrics <- renderTable({
    method <- "gaussprLinear"
    mod <- models[[method]]
    req(mod)
    mod$results[which.min(mod$results[, "RMSE"]), ]
  })
  
  output$gaussprLinear_ModelTune <- renderPlot({
    method <- "gaussprLinear"
    mod <- models[[method]]
    req(mod)
    plot(mod)
  })
  
  output$gaussprLinear_RecipePrint <- renderUI({
    method <- "gaussprLinear"
    mod <- models[[method]]
    req(mod)
    
    html <- mod$recipe %>%
      print() %>%
      cli::cli_fmt() %>%
      cli::ansi_collapse(sep = "<br>", last = "<br>") %>%
      cli::ansi_html(escape_reserved = FALSE) %>%
      gsub(pattern = "──────", replacement = "─", x = ., fixed = TRUE)
    
    css <- paste(format(ansi_html_style()), collapse = "\n")
    
    tagList(
      tags$head(tags$style(css)),
      tags$pre(HTML(html))
    )
  })
  
  output$gaussprLinear_RecipeOutput <- renderTable({
    method <- "gaussprLinear"
    mod <- models[[method]]
    req(mod)
    
    terms <- as.data.frame(mod$recipe$term_info)
    n <- dim(terms)[1]
    types <- vector(mode = "character", length = n)
    
    for (row in 1:n) {
      types[row] <- paste(collapse = " ", unlist(terms$type[row]))
    }
    
    terms$type <- types
    
    terms |>
      dplyr::filter(role == "predictor") |>
      dplyr::select(type, source) |>
      dplyr::group_by(type, source) |>
      dplyr::summarise(count = n(), .groups = "drop")
  })
  
  output$gaussprLinear_TrainSummary <- renderPrint({
    method <- "gaussprLinear"
    mod <- models[[method]]
    req(mod)
    print(mod)
  })
  
  # output tuning/best parameters table ----
  output$gaussprLinear_Coef <- renderTable({
    req(models[["gaussprLinear"]])
    models[["gaussprLinear"]]$bestTune
  }, rownames = FALSE)
  
  
  # METHOD * svmLinear3 ---------------------------------------------------------------------------------------------------------------------------
  library(LiblineaR)
  # reactive getSvmLinear3Recipe ----
  getSvmLinear3Recipe <- reactive({
    form <- formula(Response ~ .)
    recipes::recipe(form, data = getTrainData()) %>%
      dynamicSteps(input$svmLinear3_Preprocess) %>%
      step_rm(has_type("date"))
  })
  
  # observe GO event ----
  observeEvent(
    input$svmLinear3_Go,
    {
      method <- "svmLinear3"
      models[[method]] <- NULL
      
      showNotification(
        id = method,
        paste("Processing", method, "model using resampling"),
        session = session,
        duration = NULL
      )
      
      obj <- startMode(input$Parallel)
      
      tryCatch({
        
        # svmLinear3 tuning grid 
        svmLinear3Grid <- expand.grid(
          cost = c(0.001, 0.01, 0.1, 1, 10),
          Loss = c("L2", "L1")   
        )
        
        model <- caret::train(
          getSvmLinear3Recipe(),
          data = getTrainData(),
          method = method,
          metric = "RMSE",
          trControl = getTrControl(),
          tuneGrid = svmLinear3Grid,
          na.action = na.pass
        )
        
        deleteRds(method)
        saveToRds(model, method)
        models[[method]] <- model
        
      },
      finally = {
        removeNotification(id = method)
        stopMode(obj)
      })
    }
  )
  
  observeEvent(
    input$svmLinear3_Load,
    {
      method <- "svmLinear3"
      model <- loadRds(method, session)
      if (!is.null(model)) {
        models[[method]] <- model
      }
    }
  )
  
  observeEvent(
    input$svmLinear3_Delete,
    {
      method <- "svmLinear3"
      models[[method]] <- NULL
      gc()
    }
  )
  
  output$svmLinear3_MethodSummary <- renderText({
    method <- "svmLinear3"
    description(method)
  })
  
  output$svmLinear3_Metrics <- renderTable({
    method <- "svmLinear3"
    mod <- models[[method]]
    req(mod)
    mod$results[which.min(mod$results[, "RMSE"]), ]
  })
  
  output$svmLinear3_ModelTune <- renderPlot({
    method <- "svmLinear3"
    mod <- models[[method]]
    req(mod)
    plot(mod)
  })
  
  output$svmLinear3_RecipePrint <- renderUI({
    method <- "svmLinear3"
    mod <- models[[method]]
    req(mod)
    
    html <- mod$recipe %>%
      print() %>%
      cli::cli_fmt() %>%
      cli::ansi_collapse(sep = "<br>", last = "<br>") %>%
      cli::ansi_html(escape_reserved = FALSE) %>%
      gsub(pattern = "──────", replacement = "─", x = ., fixed = TRUE)
    
    css <- paste(format(ansi_html_style()), collapse = "\n")
    
    tagList(
      tags$head(tags$style(css)),
      tags$pre(HTML(html))
    )
  })
  
  output$svmLinear3_RecipeOutput <- renderTable({
    method <- "svmLinear3"
    mod <- models[[method]]
    req(mod)
    
    terms <- as.data.frame(mod$recipe$term_info)
    n <- dim(terms)[1]
    types <- vector(mode = "character", length = n)
    
    for (row in 1:n) {
      types[row] <- paste(collapse = " ", unlist(terms$type[row]))
    }
    
    terms$type <- types
    
    terms |>
      dplyr::filter(role == "predictor") |>
      dplyr::select(type, source) |>
      dplyr::group_by(type, source) |>
      dplyr::summarise(count = n(), .groups = "drop")
  })
  
  output$svmLinear3_TrainSummary <- renderPrint({
    method <- "svmLinear3"
    mod <- models[[method]]
    req(mod)
    print(mod)
  })
  
  output$svmLinear3_Coef <- renderTable({
    req(models[["svmLinear3"]])
    models[["svmLinear3"]]$bestTune
  }, rownames = FALSE)
  
  # METHOD * gcvEarth ---------------------------------------------------------------------------------------------------------------------------
  library(import)
  library(earth)
  # reactive getGcvEarthRecipe ----
  getGcvEarthRecipe <- reactive({
    form <- formula(Response ~ .)
    recipes::recipe(form, data = getTrainData()) %>%
      dynamicSteps(input$gcvEarth_Preprocess) %>%
      step_rm(has_type("date"))
  })
  
  # observe GO event ----
  observeEvent(
    input$gcvEarth_Go,
    {
      method <- "gcvEarth"
      models[[method]] <- NULL
      
      showNotification(
        id = method,
        paste("Processing", method, "model using resampling"),
        session = session,
        duration = NULL
      )
      
      obj <- startMode(input$Parallel)
      
      tryCatch({
        
        # gcvEarth tuning grid: degree
        gcvEarthGrid <- expand.grid(
          degree = c(1, 2, 3)
        )
        
        model <- caret::train(
          getGcvEarthRecipe(),
          data = getTrainData(),
          method = method,
          metric = "RMSE",
          trControl = getTrControl(),
          tuneGrid = gcvEarthGrid
        )
        
        deleteRds(method)
        saveToRds(model, method)
        models[[method]] <- model
        
      },
      finally = {
        removeNotification(id = method)
        stopMode(obj)
      })
    }
  )
  
  observeEvent(
    input$gcvEarth_Load,
    {
      method <- "gcvEarth"
      model <- loadRds(method, session)
      if (!is.null(model)) {
        models[[method]] <- model
      }
    }
  )
  
  observeEvent(
    input$gcvEarth_Delete,
    {
      method <- "gcvEarth"
      models[[method]] <- NULL
      gc()
    }
  )
  
  output$gcvEarth_MethodSummary <- renderText({
    method <- "gcvEarth"
    description(method)
  })
  
  output$gcvEarth_Metrics <- renderTable({
    method <- "gcvEarth"
    mod <- models[[method]]
    req(mod)
    mod$results[which.min(mod$results[, "RMSE"]), ]
  })
  
  output$gcvEarth_ModelTune <- renderPlot({
    method <- "gcvEarth"
    mod <- models[[method]]
    req(mod)
    plot(mod)
  })
  
  output$gcvEarth_RecipePrint <- renderUI({
    method <- "gcvEarth"
    mod <- models[[method]]
    req(mod)
    
    html <- mod$recipe %>%
      print() %>%
      cli::cli_fmt() %>%
      cli::ansi_collapse(sep = "<br>", last = "<br>") %>%
      cli::ansi_html(escape_reserved = FALSE) %>%
      gsub(pattern = "──────", replacement = "─", x = ., fixed = TRUE)
    
    css <- paste(format(ansi_html_style()), collapse = "\n")
    
    tagList(
      tags$head(tags$style(css)),
      tags$pre(HTML(html))
    )
  })
  
  output$gcvEarth_RecipeOutput <- renderTable({
    method <- "gcvEarth"
    mod <- models[[method]]
    req(mod)
    
    terms <- as.data.frame(mod$recipe$term_info)
    n <- dim(terms)[1]
    types <- vector(mode = "character", length = n)
    
    for (row in 1:n) {
      types[row] <- paste(collapse = " ", unlist(terms$type[row]))
    }
    
    terms$type <- types
    
    terms |>
      dplyr::filter(role == "predictor") |>
      dplyr::select(type, source) |>
      dplyr::group_by(type, source) |>
      dplyr::summarise(count = n(), .groups = "drop")
  })
  
  output$gcvEarth_TrainSummary <- renderPrint({
    method <- "gcvEarth"
    mod <- models[[method]]
    req(mod)
    print(mod)
  })
  
  # output tuning/best parameters table ----
  output$gcvEarth_Coef <- renderTable({
    req(models[["gcvEarth"]])
    models[["gcvEarth"]]$bestTune
  }, rownames = FALSE)
  
  
  # METHOD * gamboost ---------------------------------------------------------------------------------------------------------------------------
  library(mboost)
  
  # reactive getGamboostRecipe ----
  getGamboostRecipe <- reactive({
    form <- formula(Response ~ .)
    recipes::recipe(form, data = getTrainData()) %>%
      dynamicSteps(input$gamboost_Preprocess) %>%
      step_rm(has_type("date"))
  })
  
  # observe GO event ----
  observeEvent(
    input$gamboost_Go,
    {
      method <- "gamboost"
      models[[method]] <- NULL
      
      showNotification(
        id = method,
        paste("Processing", method, "model using resampling"),
        session = session,
        duration = NULL
      )
      
      obj <- startMode(input$Parallel)
      
      tryCatch({
        
        # gamboost tuning grid: mstop, prune
        gamboostGrid <- expand.grid(
          mstop = c(50, 100, 150, 200),
          prune = c("no", "yes")
        )
        
        model <- caret::train(
          getGamboostRecipe(),
          data = getTrainData(),
          method = method,
          metric = "RMSE",
          trControl = getTrControl(),
          tuneGrid = gamboostGrid
        )
        
        deleteRds(method)
        saveToRds(model, method)
        models[[method]] <- model
        
      },
      finally = {
        removeNotification(id = method)
        stopMode(obj)
      })
    }
  )
  
  observeEvent(
    input$gamboost_Load,
    {
      method <- "gamboost"
      model <- loadRds(method, session)
      if (!is.null(model)) {
        models[[method]] <- model
      }
    }
  )
  
  observeEvent(
    input$gamboost_Delete,
    {
      method <- "gamboost"
      models[[method]] <- NULL
      gc()
    }
  )
  
  output$gamboost_MethodSummary <- renderText({
    method <- "gamboost"
    description(method)
  })
  
  output$gamboost_Metrics <- renderTable({
    method <- "gamboost"
    mod <- models[[method]]
    req(mod)
    mod$results[which.min(mod$results[, "RMSE"]), ]
  })
  
  output$gamboost_ModelTune <- renderPlot({
    method <- "gamboost"
    mod <- models[[method]]
    req(mod)
    plot(mod)
  })
  
  output$gamboost_RecipePrint <- renderUI({
    method <- "gamboost"
    mod <- models[[method]]
    req(mod)
    
    html <- mod$recipe %>%
      print() %>%
      cli::cli_fmt() %>%
      cli::ansi_collapse(sep = "<br>", last = "<br>") %>%
      cli::ansi_html(escape_reserved = FALSE) %>%
      gsub(pattern = "──────", replacement = "─", x = ., fixed = TRUE)
    
    css <- paste(format(ansi_html_style()), collapse = "\n")
    
    tagList(
      tags$head(tags$style(css)),
      tags$pre(HTML(html))
    )
  })
  
  output$gamboost_RecipeOutput <- renderTable({
    method <- "gamboost"
    mod <- models[[method]]
    req(mod)
    
    terms <- as.data.frame(mod$recipe$term_info)
    n <- dim(terms)[1]
    types <- vector(mode = "character", length = n)
    
    for (row in 1:n) {
      types[row] <- paste(collapse = " ", unlist(terms$type[row]))
    }
    
    terms$type <- types
    
    terms |>
      dplyr::filter(role == "predictor") |>
      dplyr::select(type, source) |>
      dplyr::group_by(type, source) |>
      dplyr::summarise(count = n(), .groups = "drop")
  })
  
  output$gamboost_TrainSummary <- renderPrint({
    method <- "gamboost"
    mod <- models[[method]]
    req(mod)
    print(mod)
  })
  
  # output tuning/best parameters table ----
  output$gamboost_Coef <- renderTable({
    req(models[["gamboost"]])
    models[["gamboost"]]$bestTune
  }, rownames = FALSE)
  
  
  # # METHOD * bartMachine ---------------------------------------------------------------------------------------------------------------------------
  # library(rJava)
  # library(bartMachine)
  # # reactive getBartMachineRecipe ----
  # getBartMachineRecipe <- reactive({
  #   form <- formula(Response ~ .)
  #   recipes::recipe(form, data = getTrainData()) %>%
  #     dynamicSteps(input$bartMachine_Preprocess) %>%
  #     step_rm(has_type("date"))
  # })
  # 
  # # observe GO event ----
  # observeEvent(
  #   input$bartMachine_Go,
  #   {
  #     method <- "bartMachine"
  #     models[[method]] <- NULL
  #     
  #     showNotification(
  #       id = method,
  #       paste("Processing", method, "model using resampling"),
  #       session = session,
  #       duration = NULL
  #     )
  #     
  #     obj <- startMode(input$Parallel)
  #     
  #     tryCatch({
  #       
  #       # bartMachine tuning grid: num_trees, k, alpha, beta, nu
  #       bartMachineGrid <- expand.grid(
  #         num_trees = c(50, 100, 200),
  #         k = c(2, 3, 5),
  #         alpha = c(0.95),
  #         beta = c(2),
  #         nu = c(3, 5, 10)
  #       )
  #       
  #       model <- caret::train(
  #         getBartMachineRecipe(),
  #         data = getTrainData(),
  #         method = method,
  #         metric = "RMSE",
  #         trControl = getTrControl(),
  #         tuneGrid = bartMachineGrid,
  #         na.action = na.pass
  #       )
  #       
  #       deleteRds(method)
  #       saveToRds(model, method)
  #       models[[method]] <- model
  #       
  #     },
  #     finally = {
  #       removeNotification(id = method)
  #       stopMode(obj)
  #     })
  #   }
  # )
  # 
  # observeEvent(
  #   input$bartMachine_Load,
  #   {
  #     method <- "bartMachine"
  #     model <- loadRds(method, session)
  #     if (!is.null(model)) {
  #       models[[method]] <- model
  #     }
  #   }
  # )
  # 
  # observeEvent(
  #   input$bartMachine_Delete,
  #   {
  #     method <- "bartMachine"
  #     models[[method]] <- NULL
  #     gc()
  #   }
  # )
  # 
  # output$bartMachine_MethodSummary <- renderText({
  #   method <- "bartMachine"
  #   description(method)
  # })
  # 
  # output$bartMachine_Metrics <- renderTable({
  #   method <- "bartMachine"
  #   mod <- models[[method]]
  #   req(mod)
  #   mod$results[which.min(mod$results[, "RMSE"]), ]
  # })
  # 
  # output$bartMachine_ModelTune <- renderPlot({
  #   method <- "bartMachine"
  #   mod <- models[[method]]
  #   req(mod)
  #   plot(mod)
  # })
  # 
  # output$bartMachine_RecipePrint <- renderUI({
  #   method <- "bartMachine"
  #   mod <- models[[method]]
  #   req(mod)
  #   
  #   html <- mod$recipe %>%
  #     print() %>%
  #     cli::cli_fmt() %>%
  #     cli::ansi_collapse(sep = "<br>", last = "<br>") %>%
  #     cli::ansi_html(escape_reserved = FALSE) %>%
  #     gsub(pattern = "──────", replacement = "─", x = ., fixed = TRUE)
  #   
  #   css <- paste(format(ansi_html_style()), collapse = "\n")
  #   
  #   tagList(
  #     tags$head(tags$style(css)),
  #     tags$pre(HTML(html))
  #   )
  # })
  # 
  # output$bartMachine_RecipeOutput <- renderTable({
  #   method <- "bartMachine"
  #   mod <- models[[method]]
  #   req(mod)
  #   
  #   terms <- as.data.frame(mod$recipe$term_info)
  #   n <- dim(terms)[1]
  #   types <- vector(mode = "character", length = n)
  #   
  #   for (row in 1:n) {
  #     types[row] <- paste(collapse = " ", unlist(terms$type[row]))
  #   }
  #   
  #   terms$type <- types
  #   
  #   terms |>
  #     dplyr::filter(role == "predictor") |>
  #     dplyr::select(type, source) |>
  #     dplyr::group_by(type, source) |>
  #     dplyr::summarise(count = n(), .groups = "drop")
  # })
  # 
  # output$bartMachine_TrainSummary <- renderPrint({
  #   method <- "bartMachine"
  #   mod <- models[[method]]
  #   req(mod)
  #   print(mod)
  # })
  # 
  # # output tuning/best parameters table ----
  # output$bartMachine_Coef <- renderTable({
  #   req(models[["bartMachine"]])
  #   models[["bartMachine"]]$bestTune
  # }, rownames = FALSE)
  # 
  
  # METHOD * cubist ---------------------------------------------------------------------------------------------------------------------------
  library(Cubist)
  # reactive getCubistRecipe ----
  getCubistRecipe <- reactive({
    form <- formula(Response ~ .)
    recipes::recipe(form, data = getTrainData()) %>%
      dynamicSteps(input$cubist_Preprocess) %>%
      step_rm(has_type("date"))
  })
  
  # observe GO event ----
  observeEvent(
    input$cubist_Go,
    {
      method <- "cubist"
      models[[method]] <- NULL
      
      showNotification(
        id = method,
        paste("Processing", method, "model using resampling"),
        session = session,
        duration = NULL
      )
      
      obj <- startMode(input$Parallel)
      
      tryCatch({
        
        # cubist tuning grid: committees, neighbors
        cubistGrid <- expand.grid(
          committees = c(1, 10, 25, 50),
          neighbors = c(0, 1, 5, 9)
        )
        
        model <- caret::train(
          getCubistRecipe(),
          data = getTrainData(),
          method = method,
          metric = "RMSE",
          trControl = getTrControl(),
          tuneGrid = cubistGrid
        )
        
        deleteRds(method)
        saveToRds(model, method)
        models[[method]] <- model
        
      },
      finally = {
        removeNotification(id = method)
        stopMode(obj)
      })
    }
  )
  
  observeEvent(
    input$cubist_Load,
    {
      method <- "cubist"
      model <- loadRds(method, session)
      if (!is.null(model)) {
        models[[method]] <- model
      }
    }
  )
  
  observeEvent(
    input$cubist_Delete,
    {
      method <- "cubist"
      models[[method]] <- NULL
      gc()
    }
  )
  
  output$cubist_MethodSummary <- renderText({
    method <- "cubist"
    description(method)
  })
  
  output$cubist_Metrics <- renderTable({
    method <- "cubist"
    mod <- models[[method]]
    req(mod)
    mod$results[which.min(mod$results[, "RMSE"]), ]
  })
  
  output$cubist_ModelTune <- renderPlot({
    method <- "cubist"
    mod <- models[[method]]
    req(mod)
    plot(mod)
  })
  
  output$cubist_RecipePrint <- renderUI({
    method <- "cubist"
    mod <- models[[method]]
    req(mod)
    
    html <- mod$recipe %>%
      print() %>%
      cli::cli_fmt() %>%
      cli::ansi_collapse(sep = "<br>", last = "<br>") %>%
      cli::ansi_html(escape_reserved = FALSE) %>%
      gsub(pattern = "──────", replacement = "─", x = ., fixed = TRUE)
    
    css <- paste(format(ansi_html_style()), collapse = "\n")
    
    tagList(
      tags$head(tags$style(css)),
      tags$pre(HTML(html))
    )
  })
  
  output$cubist_RecipeOutput <- renderTable({
    method <- "cubist"
    mod <- models[[method]]
    req(mod)
    
    terms <- as.data.frame(mod$recipe$term_info)
    n <- dim(terms)[1]
    types <- vector(mode = "character", length = n)
    
    for (row in 1:n) {
      types[row] <- paste(collapse = " ", unlist(terms$type[row]))
    }
    
    terms$type <- types
    
    terms |>
      dplyr::filter(role == "predictor") |>
      dplyr::select(type, source) |>
      dplyr::group_by(type, source) |>
      dplyr::summarise(count = n(), .groups = "drop")
  })
  
  output$cubist_TrainSummary <- renderPrint({
    method <- "cubist"
    mod <- models[[method]]
    req(mod)
    print(mod)
  })
  
  # output tuning/best parameters table ----
  output$cubist_Coef <- renderTable({
    req(models[["cubist"]])
    models[["cubist"]]$bestTune
  }, rownames = FALSE)
  
  
  # METHOD * logicBag ---------------------------------------------------------------------------------------------------------------------------
  library(BiocManager)
  library(logicFS)
  # reactive getLogicBagRecipe ----
  getLogicBagRecipe <- reactive({
    form <- formula(Response ~ .)
    recipes::recipe(form, data = getTrainData()) %>%
      dynamicSteps(input$logicBag_Preprocess) %>%
      step_rm(has_type("date"))
  })
  
  # observe GO event ----
  observeEvent(
    input$logicBag_Go,
    {
      method <- "logicBag"
      models[[method]] <- NULL
      
      showNotification(
        id = method,
        paste("Processing", method, "model using resampling"),
        session = session,
        duration = NULL
      )
      
      obj <- startMode(input$Parallel)
      
      tryCatch({
        
        # logicBag tuning grid: nleaves, ntrees
        logicBagGrid <- expand.grid(
          nleaves = c(4, 6, 8, 10),
          ntrees = c(25, 50, 100)
        )
        
        model <- caret::train(
          getLogicBagRecipe(),
          data = getTrainData(),
          method = method,
          metric = "RMSE",
          trControl = getTrControl(),
          tuneGrid = logicBagGrid
        )
        
        deleteRds(method)
        saveToRds(model, method)
        models[[method]] <- model
        
      },
      finally = {
        removeNotification(id = method)
        stopMode(obj)
      })
    }
  )
  
  observeEvent(
    input$logicBag_Load,
    {
      method <- "logicBag"
      model <- loadRds(method, session)
      if (!is.null(model)) {
        models[[method]] <- model
      }
    }
  )
  
  observeEvent(
    input$logicBag_Delete,
    {
      method <- "logicBag"
      models[[method]] <- NULL
      gc()
    }
  )
  
  output$logicBag_MethodSummary <- renderText({
    method <- "logicBag"
    description(method)
  })
  
  output$logicBag_Metrics <- renderTable({
    method <- "logicBag"
    mod <- models[[method]]
    req(mod)
    mod$results[which.min(mod$results[, "RMSE"]), ]
  })
  
  output$logicBag_ModelTune <- renderPlot({
    method <- "logicBag"
    mod <- models[[method]]
    req(mod)
    plot(mod)
  })
  
  output$logicBag_RecipePrint <- renderUI({
    method <- "logicBag"
    mod <- models[[method]]
    req(mod)
    
    html <- mod$recipe %>%
      print() %>%
      cli::cli_fmt() %>%
      cli::ansi_collapse(sep = "<br>", last = "<br>") %>%
      cli::ansi_html(escape_reserved = FALSE) %>%
      gsub(pattern = "──────", replacement = "─", x = ., fixed = TRUE)
    
    css <- paste(format(ansi_html_style()), collapse = "\n")
    
    tagList(
      tags$head(tags$style(css)),
      tags$pre(HTML(html))
    )
  })
  
  output$logicBag_RecipeOutput <- renderTable({
    method <- "logicBag"
    mod <- models[[method]]
    req(mod)
    
    terms <- as.data.frame(mod$recipe$term_info)
    n <- dim(terms)[1]
    types <- vector(mode = "character", length = n)
    
    for (row in 1:n) {
      types[row] <- paste(collapse = " ", unlist(terms$type[row]))
    }
    
    terms$type <- types
    
    terms |>
      dplyr::filter(role == "predictor") |>
      dplyr::select(type, source) |>
      dplyr::group_by(type, source) |>
      dplyr::summarise(count = n(), .groups = "drop")
  })
  
  output$logicBag_TrainSummary <- renderPrint({
    method <- "logicBag"
    mod <- models[[method]]
    req(mod)
    print(mod)
  })
  
  # output tuning/best parameters table ----
  output$logicBag_Coef <- renderTable({
    req(models[["logicBag"]])
    models[["logicBag"]]$bestTune
  }, rownames = FALSE)
  
  # end of maintenance point ---------------------------------------------------------------------------------------------------------------------------

  
  
})
