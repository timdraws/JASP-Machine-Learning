#
# Copyright (C) 2019 University of Amsterdam
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#

MLRegressionRegularized <- function(jaspResults, dataset, options, ...) {
  
  # Read dataset
  dataset <- .regRegReadData(dataset, options)
  
  # Check if results can be computed
  ready <- (options$target != "" && length(.v(options$predictors)) > 0)
  
  # Error checking
  if (ready) errors <- .regRegErrorHandling(dataset, options)
  
  # Compute (a list of) results from which tables and plots can be created
  if (ready) regRegResults <- .regRegComputeResults(jaspResults, dataset, options)
  
  # Output tables
  .regRegTable(     jaspResults, options, regRegResults, ready)
  .regRegCoefTable( jaspResults, options, regRegResults, ready)
  .regRegApplyTable(jaspResults, options, regRegResults, ready)
  
  # Output plots
  if (ready) .regRegLarsPlot(    jaspResults, options, regRegResults)
  if (ready) .regRegCVLambdaPlot(jaspResults, options, regRegResults)
  if (ready) .regRegPredPerfPlot(jaspResults, options, regRegResults)
  
  return()
}

# Read dataset
.regRegReadData <- function(dataset, options) {
  
  if (options$target == "")    options$target    <- NULL
  if (options$indicator == "") options$indicator <- NULL
  if (options$weights == "")   options$weights   <- NULL
  
  data <- .readDataSetToEnd(columns.as.numeric = c(options$target, options$weights), columns = options$predictors,
                            columns.as.factor = options$indicator)
  
  return(data)
}

# Error checking
.regRegErrorHandling <- function(dataset, options) {
  
  # Error Check 1: Provide a test set
  if (options$dataTrain == 1) {
    JASP:::.quitAnalysis("Please provide a test set.")
  }
  
  # Error Check 2: Provide at least 10 training observations
  if ((nrow(dataset) * options$dataTrain) < 10) {
    JASP:::.quitAnalysis("Please provide at least 10 training observations.")
  }
  
  # Error Check 3: There should be least 2 predictors, otherwise randomForest() complains
  if (length(.v(options$predictors)) < 2L) {
    JASP:::.quitAnalysis("Please provide at least 2 predictors.")
  }
  
}

# Compute results
.regRegComputeResults <- function(jaspResults, dataset, options) {
  
  if (!is.null(jaspResults[["stateRegRegResults"]])) return (jaspResults[["stateRegRegResults"]]$object)
  
  # Create results object and add options
  results <- list()
  results[["spec"]] <- .regRegCalcSpecs(dataset, options)
  
  # Prepare data
  preds <- which(colnames(dataset) %in% .v(options$predictors)) # predictors
  target <- which(colnames(dataset) == .v(options$target)) # target
  if(options$indicator != "") indicator <- which(colnames(dataset) == .v(options$indicator))
  
  # Deal with NAs: omit all rows that contain at least one missing value
  if (options$applyModel == "applyImpute") { # save target NA observations to make predictions later
      
    idxApply <- which(is.na(dataset[, target]))
    predImpute <- cbind(dataset[idxApply, target, drop = FALSE], na.omit(dataset[idxApply, preds, drop = FALSE]))
    
    print(predImpute) # this turns out as it should; so what's wrong here? NA column target not ok?? how should data be predicted?
    
  } else {
    
    dataset <- na.omit(dataset) 
    
  }
  
  # Splitting the data into training set, test set, and application set in case indicator is provided
  if (options$applyModel == "applyIndicator" && options$indicator != "") {
    
    idxApply <- which(dataset[, indicator] == 1)
    idxModel <- which(dataset[, indicator] == 0)
    
    applyData <- dataset[idxApply, , drop = FALSE]
    modelData <- dataset[idxModel, , drop = FALSE]
    
  } else if (options$applyModel == "applyImpute") {
    
    applyData <- predImpute
    modelData <- dataset[-idxApply, , drop = FALSE]
    
  } else {
    
    modelData <- dataset
    
  }
  
  # Set seed	
  if (options$seedBox) set.seed(options$seed)
  
  # Compile training and test data
  idxTrain <- sample(1:nrow(modelData), floor(options$dataTrain * nrow(modelData)))
  idxTest <- (1:nrow(modelData))[-idxTrain]
  
  formula <- as.formula(paste(.v(options$target), "~", paste(.v(options$predictors), collapse = " + ")))
  
  trainPreds <- model.matrix(formula, modelData[idxTrain, c(preds, target), drop = FALSE])[, -1]
  trainTarget <- as.numeric(modelData[idxTrain, target])
  
  testPreds <- model.matrix(formula, modelData[idxTest, c(preds, target), drop = FALSE])[, -1]
  testTarget <- as.numeric(modelData[idxTest, target])
  
  if (exists("applyData")) applyData <- model.matrix(formula, applyData[, , drop = FALSE])[, -1]
  
  if (options$weights != "" && options$applyModel != "applyImpute") {
    weights <- modelData[idxTrain, .v(options$weights)]
  } else if (options$weights != "" && options$applyModel == "applyImpute") {
    weights <- modelWeights[idxTrain]
  } else {
    weights <- rep(1, nrow(trainPreds))
  }
  
  # Run regularized regression
  results[["res"]] <- glmnet::cv.glmnet(x = trainPreds, y = trainTarget, nfolds = 10, type.measure = "deviance",
                                        family = "gaussian", weights = weights, offset = NULL,
                                        alpha = results$spec$alpha, standardize = options$standardize,
                                        intercept = options$intercept, thresh = options$thresh)
  
  if (options$shrinkage == "manual") {
    results[["lambda"]] <- results$spec$lambda
  } else if (options$shrinkage == "optMin") {
    results[["lambda"]] <- results$res$lambda.min
  } else {
    results[["lambda"]] <- results$res$lambda.1se
  }
  
  results[["cvMSE"]]       <- results$res$cvm[results$res$lambda == results$lambda]
  results[["cvMSELambda"]] <- data.frame(lambda = results$res$lambda, MSE = results$res$cvm, sd = results$res$cvsd)
  
  results[["data"]] <- list(trainPreds = trainPreds, trainTarget = trainTarget, 
                            testPreds = testPreds, testTarget = testTarget)
  
  # Derive test set predictions and calculate test error rate
  modPred <- predict(results$res, newx = testPreds, s = results$lambda, type = "link", exact = TRUE,
                     x = trainPreds, y = trainTarget, weights = rep(1, nrow(trainPreds)), offset = NULL,
                     alpha = results$spec$alpha, standardize = options$standardize,
                     intercept = options$intercept, thresh = options$thresh)
  
  # Predictive performance
  results[["predPerf"]] <- data.frame(pred = as.numeric(modPred), obs = as.numeric(testTarget))
  results[["testMSE"]]  <- mean((modPred - testTarget)^2)
  results[["testR2"]]   <- round(cor(modPred, testTarget)^2, 2)
  
  # Coefficients table
  results[["coefs"]] <- coef(results$res, s = results$lambda)
  
  # Apply model to new data if requested
  if((options$applyModel == "applyIndicator" && options$indicator != "") || options$applyModel == "applyImpute") {
    
    applyPred <- predict(results$res, newx = applyData, s = results$lambda, type = "link", exact = TRUE,
                         x = trainPreds, y = trainTarget, weights = rep(1, nrow(trainPreds)), offset = NULL,
                         alpha = results$spec$alpha, standardize = options$standardize, intercept = options$intercept,
                         thresh = options$thresh)
    
    results[["apply"]] <- data.frame(case = idxApply, pred = as.numeric(applyPred))
    
  }
  
  # Save results to state
  jaspResults[["stateRegRegResults"]] <- createJaspState(results)
  jaspResults[["stateRegRegResults"]]$dependOn(options = c("target", "predictors", "indicator", "weights", "penalty",
                                                           "applyModel", "alpha", "thresh", "dataTrain", "standardize",
                                                           "intercept", "shrinkage", "lambda", "seedBox", "seed"))
  
  return(results)
}

.regRegCalcSpecs <- function(dataset, options) {
  
  specs <- list()
  
  # Setting the shrinkage parameter lambda
  if (options$shrinkage == "manual") specs$lambda <- options$lambda
  
  # Choosing the regularization method
  if (options$penalty == "ridge") {
    specs$alpha <- 0
    specs$penalty <- "L2 (Ridge)"
  } else if (options$penalty == "lasso") {
    specs$alpha <- 1
    specs$penalty <- "L1 (Lasso)"
  } else {
    specs$alpha <- options$alpha
    specs$penalty <- "Elastic Net"
  }
  
  return(specs)
}

# Output functions
.regRegTable <- function(jaspResults, options, regRegResults, ready) {
  if (!is.null(jaspResults[["regRegTable"]])) return()
  
  # Create table and bind to jaspResults
  regRegTable                           <- createJaspTable(title = "Regularized Linear Regression Model Summary")
  jaspResults[["regRegTable"]]          <- regRegTable
  jaspResults[["regRegTable"]]$position <- 1
  jaspResults[["regRegTable"]]$dependOn(options = c("target", "predictors", "indicator", "weights", "penalty",
                                                    "applyModel", "alpha", "thresh", "dataTrain", "standardize",
                                                    "intercept", "shrinkage", "lambda", "seedBox", "seed"))
  
  # Add column info
  if (options$dataTrain < 1) {
    regRegTable$addColumnInfo(name = "testMSE",  title = "Test Set MSE", type = "number", format = "sf:4")
  }
  
  if (options$dataTrain < 1) {
    regRegTable$addColumnInfo(name = "testR2",  title = "Test Set R\u00B2", type = "number", format = "sf:4")
  }
  
  if (options$shrinkage == "auto") {
    regRegTable$addColumnInfo(name = "cvMSE",  title = "CV MSE", type = "number", format = "sf:4") 
  }
  
  regRegTable$addColumnInfo(name = "penalty"  ,  title = "Penalty"  , type = "string")
  
  if (options$penalty == "elasticNet") {
    regRegTable$addColumnInfo(name = "alpha",  title = "α", type = "number", format = "sf:4") 
  }
  
  regRegTable$addColumnInfo(name = "lambda",  title = "\u03BB", type = "number", format = "sf:4")
  regRegTable$addColumnInfo(name = "nTrain",  title = "n(Train)", type = "integer")
  regRegTable$addColumnInfo(name = "nTest",  title = "n(Test)", type = "integer")
  
  if (options$lambda == 0) {
    regRegTable$addFootnote("With \u03BB equal to 0, linear regression is performed.", symbol="<i>Note.</i>") 
  }
  
  # Add data per column
  if (options$dataTrain < 1){
    regRegTable[["testMSE"]]  <- if (ready) regRegResults$testMSE else "."
  }
  
  if (options$dataTrain < 1){
    regRegTable[["testR2"]]  <- if (ready) regRegResults$testR2 else "."
  }
  
  if (options$shrinkage == "auto") {
    regRegTable[["cvMSE"]]   <- if (ready) regRegResults$cvMSE else "." 
  }
  
  regRegTable[["penalty"]] <- if (ready) regRegResults$spec$penalty else "."
  
  if (options$penalty == "elasticNet") regRegTable[["alpha"]] <- if (ready) options$alpha else "."
  
  if (options$shrinkage == "manual") {
    regRegTable[["lambda"]]  <- if (ready) regRegResults$spec$lambda    else "." 
  } else if (options$shrinkage == "optMin") {
    regRegTable[["lambda"]]  <- if (ready) regRegResults$res$lambda.min else "." 
  } else {
    regRegTable[["lambda"]]  <- if (ready) regRegResults$res$lambda.1se else "." 
  }
  
  regRegTable[["nTrain"]] <- if (ready) nrow(regRegResults$data$trainPreds) else "."
  regRegTable[["nTest"]]  <- if (ready) nrow(regRegResults$data$testPreds)  else "."

}

.regRegCoefTable <- function(jaspResults, options, regRegResults, ready) {
  if (!is.null(jaspResults[["regRegCoefTable"]]) || !options$regRegCoefTable) return()
  
  # Create table
  regRegCoefTable                           <- createJaspTable(title = "Regression Coefficients")
  jaspResults[["regRegCoefTable"]]          <- regRegCoefTable
  jaspResults[["regRegCoefTable"]]$position <- 2
  jaspResults[["regRegCoefTable"]]$dependOn(options = c("target", "predictors", "indicator", "weights", "penalty",
                                                        "applyModel", "alpha", "thresh", "dataTrain", "standardize",
                                                        "intercept", "shrinkage", "lambda", "seedBox", "seed",
                                                        "regRegCoefTable"))
  
  # Add column info
  regRegCoefTable$addColumnInfo(name = "var",  title = " ", type = "string")
  regRegCoefTable$addColumnInfo(name = "coefs",  title = "Coefficient", type = "number", format = "sf:4")
  
  # Disentangle variable names
  predictors_unv <- if(ready) .unv(.v(options$predictors))
  coefs_unv      <- if(ready) .unv(rownames(regRegResults$coefs))
  predictors     <- if(ready) .v(options$predictors)
  coefs          <- if(ready) rownames(regRegResults$coefs)
  if(ready) for (i in 1:length(coefs)) {
    coefs[i] <- paste(predictors_unv[which(startsWith(coefs_unv[i], predictors_unv))][1], 
                      paste("(",stringr::str_remove(coefs[i], predictors[which(startsWith(coefs[i], predictors))][1]),
                      ")", sep = ""))
    if (startsWith(coefs[i], "NA")) coefs[i] <- "Intercept"
    if (endsWith(  coefs[i], "()")) coefs[i] <- gsub("\\(|\\)", "", coefs[i])
  }
  
  # Add data per column
  regRegCoefTable[["var"]]   <- if(ready) coefs else "."
  regRegCoefTable[["coefs"]] <- if(ready) as.numeric(regRegResults$coefs) else "."

}

.regRegApplyTable <- function(jaspResults, options, regRegResults, ready) {
  if (!is.null(jaspResults[["applyModel"]]) || options$applyModel == "noApp") return()
  
  # Create table and bind to jaspResults
  regRegApplyTable                  <- createJaspTable(title = "Regularized Regression Model Predictions")
  jaspResults[["regRegApplyTable"]] <- regRegApplyTable
  jaspResults[["regRegApplyTable"]] <- 3
  jaspResults[["regRegApplyTable"]]$dependOn(options = c("target", "predictors", "indicator", "weights", "penalty",
                                                         "applyModel", "alpha", "thresh", "dataTrain", "standardize",
                                                         "intercept", "shrinkage", "lambda", "seedBox", "seed",
                                                         "applyModel"))
  
  # Add column info
  regRegApplyTable$addColumnInfo(name = "case",  title = "Case", type = "integer")
  regRegApplyTable$addColumnInfo(name = "pred",  title = "Prediction", type = "number", format = "sf:4")
  
  # Add data per column
  regRegApplyTable[["case"]]  <- if (ready) as.integer(regRegResults$apply$case)   else "."
  regRegApplyTable[["pred"]]  <- if (ready) as.numeric(regRegResults$apply$pred) else "."
  
}

.regRegLarsPlot <- function(jaspResults, options, regRegResults, ready) {
  if (!options$plotLars) return()
  
  if (options$legendLars) legPos <- "right" else legPos <- "none"
  object       <- regRegResults$res$glmnet.fit
  coefs        <- as.matrix(regRegResults$res$glmnet.fit$beta)
  d            <- stack(as.data.frame(coefs))
  d$ind        <- rep(.unv(rownames(coefs)), (nrow(d) / nrow(coefs)))
  d$lambda     <- rep(object$lambda, each = nrow(coefs))
  limitsLambda <- c(min(pretty(d$lambda)), max(pretty(d$lambda)))
  limitsCoefs  <- c(min(pretty(d$values)), max(pretty(d$values)))

  # regRegLarsPlot <- plot(regRegResults$res$glmnet.fit, "norm", label = TRUE)
  regRegLarsPlot <- JASPgraphs::themeJasp(
    ggplot2::ggplot(data = d, mapping = ggplot2::aes(x = lambda, y = values, colour = ind), show.legend = TRUE) +
      ggplot2::geom_line() +
      ggplot2::scale_x_continuous("\u03BB", limits = limitsLambda, breaks = pretty(limitsLambda)) +
      ggplot2::scale_y_continuous("Coefficients", limits = limitsCoefs, breaks = pretty(limitsCoefs))
    , legend.position = legPos, legend.title = ""
  )
  
  # Create plot and bind to jaspResults
  regRegLarsPlot <- createJaspPlot(plot = regRegLarsPlot, title = "Variable Trace Plot", width = 400, height = 400)

  jaspResults[["regRegLarsPlot"]] <- regRegLarsPlot
  jaspResults[["regRegLarsPlot"]]$position <- 4
  jaspResults[["regRegLarsPlot"]]$dependOn(options = c("target", "predictors", "indicator", "weights", "penalty",
                                                       "applyModel", "alpha", "thresh", "dataTrain", "standardize",
                                                       "intercept", "shrinkage", "lambda", "seedBox", "seed",
                                                       "plotLars"))
}

.regRegCVLambdaPlot <- function(jaspResults, options, regRegResults, ready) {
  if (!options$plotCVLambda) return()
  
  if (options$legendCVLambda) legPos <- "top" else legPos <- "none"
  
  limitsLambda <- c(min(pretty(regRegResults$cvMSELambda$lambda)), max(pretty(regRegResults$cvMSELambda$lambda)))
  limitsMSE    <- c(min(pretty(regRegResults$cvMSELambda$MSE - regRegResults$cvMSELambda$sd)),
                    max(pretty(regRegResults$cvMSELambda$MSE + regRegResults$cvMSELambda$sd)))
  
  regRegCVLambdaPlot <- JASPgraphs::themeJasp(
    ggplot2::ggplot(data = regRegResults$cvMSELambda, mapping = ggplot2::aes(x = lambda, y = MSE)) +
      ggplot2::geom_ribbon(data = regRegResults$cvMSELambda, mapping = ggplot2::aes(ymin = MSE - sd, ymax = MSE + sd),
                           fill = "grey90") +
      ggplot2::geom_line(size = 1, colour = "black") +
      ggplot2::scale_x_continuous("\u03BB", limits = limitsLambda, breaks = pretty(limitsLambda)) +
      ggplot2::scale_y_continuous("CV Mean Squared Error", limits = limitsMSE, breaks = pretty(limitsMSE)) +
      ggplot2::geom_vline(
        ggplot2::aes(xintercept = regRegResults$res$lambda.min, color = "lambdaMin"), linetype = "dashed") +
      ggplot2::geom_vline(
        ggplot2::aes(xintercept = regRegResults$res$lambda.1se, color = "lambda1se"), linetype = "dashed") +
      ggplot2::scale_color_manual(name = "", values = c(lambdaMin = "#14a1e3", lambda1se = "#99c454"),
                                  labels = c(lambdaMin = "Min. CV MSE", lambda1se = "\u03BB 1 SE"))
    ,
    legend.position = legPos
    )
  
  # Create plot and bind to jaspResults
  regRegCVLambdaPlot <- createJaspPlot(plot = regRegCVLambdaPlot, title = "Lambda Evaluation",
                                       width = 400, height = 400)
  
  jaspResults[["regRegCVLambdaPlot"]] <- regRegCVLambdaPlot
  jaspResults[["regRegCVLambdaPlot"]]$position <- 5
  jaspResults[["regRegCVLambdaPlot"]]$dependOn(options = c("target", "predictors", "indicator", "weights", "penalty",
                                                           "applyModel", "alpha", "thresh", "dataTrain", "standardize",
                                                           "intercept", "shrinkage", "lambda", "seedBox", "seed",
                                                           "plotCVLambda"))
}

.regRegPredPerfPlot <- function(jaspResults, options, regRegResults, ready) {
  if (!options$plotPredPerf) return()
  
  limits <- c(round(min(c(min(floor(regRegResults$predPerf$pred))  , min(floor(regRegResults$predPerf$obs))))),
              round(max(c(max(ceiling(regRegResults$predPerf$pred)), max(ceiling(regRegResults$predPerf$obs))))))
  
  regRegPredPerfPlot <- JASPgraphs::themeJasp(
    ggplot2::ggplot(data = regRegResults$predPerf, mapping = ggplot2::aes(x = obs, y = pred)) +
      JASPgraphs::geom_point() +
      ggplot2::geom_line(data = data.frame(x = limits, y = limits), mapping = ggplot2::aes(x = x, y = y),
                         col = "darkred", size = 1) +
      ggplot2::scale_x_continuous("Observed" , limits = limits, breaks = pretty(limits)) +
      ggplot2::scale_y_continuous("Predicted", limits = limits, breaks = pretty(limits))
  )
  
  if (options$dataTrain < 1) {
    title <- "Predictive Performance on Test Set"
  } else {
    title <- "Predictive Performance on Training Set"
  }

  regRegPredPerfPlot <- createJaspPlot(plot = regRegPredPerfPlot, title = title, width = 400, height = 400)
  
  jaspResults[["regRegPredPerfPlot"]] <- regRegPredPerfPlot
  jaspResults[["regRegPredPerfPlot"]]$position <- 6
  jaspResults[["regRegPredPerfPlot"]]$dependOn(options = c("target", "predictors", "indicator", "weights", "penalty",
                                                           "applyModel", "alpha", "thresh", "dataTrain", "standardize",
                                                           "intercept", "shrinkage", "lambda", "seedBox", "seed",
                                                           "plotPredPerf"))
}
