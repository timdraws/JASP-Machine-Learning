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

MLRegressionBoosting <- function(jaspResults, dataset, options, ...) {
  
  # Read dataset
  dataset <- .regBoostReadData(dataset, options)
  
  # Check if results can be computed
  ready <- (options$target != "" && length(.v(options$predictors)) > 0)
  
  # Error checking
  if (ready) errors <- .regBoostErrorHandling(dataset, options)
  
  # Compute (a list of) results from which tables and plots can be created
  if (ready) regBoostResults <- .regBoostComputeResults(jaspResults, dataset, options)
  
  # Output tables
  .regBoostTable(      jaspResults, options, regBoostResults, ready)
  .regBoostRelInfTable(jaspResults, options, regBoostResults, ready)
  .regBoostApplyTable( jaspResults, options, regBoostResults, ready)
  
  # Output plots
  if (ready) .regBoostRelInfPlot(          jaspResults, options, regBoostResults)
  if (ready) .regBoostPlotDeviance(        jaspResults, options, regBoostResults)
  if (ready) .regBoostPlotOOBChangeDev(    jaspResults, options, regBoostResults)
  if (ready) .regBoostPlotPredPerformance( jaspResults, options, regBoostResults)
  
  return()
}

# Read dataset
.regBoostReadData <- function(dataset, options) {
  
  if (options$target == "")    options$target <- NULL
  if (options$indicator == "") options$indicator <- NULL
  
  data <- .readDataSetToEnd(columns.as.numeric = options$target, columns = options$predictors,
                            columns.as.factor = options$indicator)
  
  return(data)
}

# Error checking
.regBoostErrorHandling <- function(dataset, options) {
  
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
.regBoostComputeResults <- function(jaspResults, dataset, options, analysisOptions) {
  
  if (!is.null(jaspResults[["stateClassBoostResults"]])) return (jaspResults[["stateClassBoostResults"]]$object)
  
  # Create results object and add options
  results <- list()
  results[["spec"]] <- .regBoostCalcSpecs(dataset, options)
  
  # Prepare data
  preds  <- which(colnames(dataset) %in% .v(options$predictors)) # predictors
  target <- which(colnames(dataset) == .v(options$target)) # target
  if(options$indicator != "") indicator <- which(colnames(dataset) == .v(options$indicator))
  
  # Deal with NAs: apply roughfix or omit NA rows
  if (sum(is.na(dataset)) > 0) {
    
    if (options$applyModel == "applyImpute") {
      
      idxApply <- which(is.na(dataset[, target]))
      
      if (options$NAs == "roughfix") {
        predImpute <- randomForest::na.roughfix(dataset[idxApply, preds])
      } else {
        predImpute <- na.omit(dataset[idxApply, preds])
      }
      
    }
    
    if (options$NAs == "roughfix") dataset <- randomForest::na.roughfix(dataset) else dataset <- na.omit(dataset)
    
  }
  
  # Splitting the data into training set, test set, and application set
  if (options$applyModel == "applyIndicator" && options$indicator != "") {
    
    idxApply <- which(dataset[, indicator] == 1)
    idxModel <- which(dataset[, indicator] == 0)
    
    applyData <- dataset[idxApply, preds, drop = FALSE]
    modelData <- dataset[idxModel, ]
    
  } else {
    
    modelData <- dataset
    
  }
  
  # Set seed	
  if (options$seedBox) set.seed(options$seed)
  
  # Compile training and test data
  idxTrain <- sample(1:nrow(modelData), floor(options$dataTrain * nrow(modelData)))
  idxTest  <- (1:nrow(modelData))[-idxTrain]
  
  trainData  <- modelData[idxTrain, c(preds, target), drop = FALSE]
  testData   <- modelData[idxTest, preds, drop = FALSE]
  testTarget <- as.numeric(modelData[idxTest, target])
  
  # Prepare Boosting
  formula <- as.formula(paste(.v(options$target), "~", paste(.v(options$predictors), collapse = " + ")))
  
  # Run Boosting
  results[["res"]] <- gbm::gbm(formula = formula, data = trainData, n.trees = options$noOfTrees,
                               shrinkage = options$shrinkage, interaction.depth = options$int.depth,
                               cv.folds = results$spec$modelOptimization, bag.fraction = options$bag.fraction,
                               n.minobsinnode = options$nNode, distribution = results$spec$dist)
  
  results[["data"]] <- list(trainData = trainData, testData = testData, testTarget = testTarget)
  results[["relInf"]] <- summary(results$res, plot = FALSE)
  
  if(options$modelOptimization == "cv") results[["method"]] <- "cv" else results[["method"]] <- "OOB"
  
  if (options$modelOptimization != "noOpt") {
    results[["optTrees"]] <- gbm::gbm.perf(results$res, plot.it = FALSE, method = results$method)[1]
  } else {
    results[["optTrees"]] <- options$noOfTrees
  }
  
  # Derive test set predictions
  modPred <- gbm::predict.gbm(results$res, testData, n.trees = results$optTrees, type = "response")
  
  # Predictive performance
  results[["predPerf"]] <- data.frame(pred = as.numeric(modPred), obs = as.numeric(testTarget))
  results[["testMSE"]]  <- mean((modPred - testTarget)^2)
  results[["testR2"]]   <- round(cor(modPred, testTarget)^2, 2)
  
  # Apply model to new data if requested
  if(options$applyModel == "applyIndicator" && options$indicator != "") {
    
    applyPred <- gbm::predict.gbm(results$res, newdata = applyData, n.trees = results$optTrees, type = "response")
    results[["apply"]] <- round(data.frame(case = idxApply, pred = applyPred), 2)
    
  } else if (options$applyModel == "applyImpute") {
    
    applyPred <- gbm::predict.gbm(results$res, newdata = predImpute, n.trees = results$optTrees, type = "response")
    results[["apply"]] <- round(data.frame(case = idxApply, pred = applyPred), 2)
    
  }
  
  # Save results to state
  jaspResults[["stateClassBoostResults"]] <- createJaspState(results)
  jaspResults[["stateClassBoostResults"]]$dependOn(options = c("target", "predictors", "indicator", "applyModel",
                                                               "noOfTrees", "shrinkage", "int.depth",
                                                               "modelOptimization", "cvFolds", "nNode", "dataTrain",
                                                               "dataTrain", "bag.fraction", "dist", "seedBox", "seed"))
  
  return(results)
}

.regBoostCalcSpecs <- function(modelData, options) {
  specs <- list()
  
  # Should cross-validation be performed?
  if (options$modelOptimization == "cv") specs$modelOptimization <- options$cvFolds else specs$modelOptimization <- 0
  
  # Which distribution should be used?
  if (options$dist == "tdist") {
    
    specs$dist <- "tdist"
    specs$distribution <- "t"
    
  } else if (options$dist == "laplace") {
    
    specs$dist <- "laplace"
    specs$distribution <- "Laplace"
    
  } else {
    
    specs$dist <- "gaussian"
    specs$distribution <- "Gaussian"
    
  }
  
  return(specs)
}

# Output functions
.regBoostTable <- function(jaspResults, options, regBoostResults, ready, analysisOptions) {
  if (!is.null(jaspResults[["regBoostTable"]])) return()
  
  # Create table and bind to jaspResults
  regBoostTable <- createJaspTable(title = "Boosting Regression Model Summary")
  jaspResults[["regBoostTable"]] <- regBoostTable
  jaspResults[["regBoostTable"]]$dependOn(options = c("target", "predictors", "indicator", "applyModel",
                                                      "noOfTrees", "shrinkage", "int.depth", "modelOptimization",
                                                      "cvFolds", "nNode", "dataTrain", "dataTrain", "bag.fraction",
                                                      "dist", "seedBox", "seed"))
  
  # Add column info
  if(options$dataTrain < 1){
    regBoostTable$addColumnInfo(name = "testError" ,  title = "Test Set MSE"  , type = "number", format = "sf:4")
  }
  if (options$dataTrain < 1) {
    regBoostTable$addColumnInfo(name = "testR2",  title = "Test Set R\u00B2", type = "number", format = "sf:4")
  }
  regBoostTable$addColumnInfo(name = "ntrees"      ,  title = "Trees"         , type = "integer"                )
  regBoostTable$addColumnInfo(name = "shrinkage"   ,  title = "Shrinkage"     , type = "number", format = "sf:4")
  regBoostTable$addColumnInfo(name = "intDepth"    ,  title = "Int. Depth"    , type = "integer"                )
  regBoostTable$addColumnInfo(name = "minObsInNode",  title = "Min. Obs. Node", type = "integer"                )
  regBoostTable$addColumnInfo(name = "ntrain"      ,  title = "n(Train)"     , type = "integer"                 )
  regBoostTable$addColumnInfo(name = "ntest"       ,  title = "n(Test)"      , type = "integer"                 )
  
  # Add data per column
  if (options$dataTrain < 1){ regBoostTable[["testError"]] <- if (ready) regBoostResults$testError else "." }
  if (options$dataTrain < 1){ regBoostTable[["testR2"]]    <- if (ready) regBoostResults$testR2    else "." }
  regBoostTable[["ntrees"]]       <- if (ready) regBoostResults$optTrees                else "."
  regBoostTable[["shrinkage"]]    <- if (ready) regBoostResults$res$shrinkage           else "."
  regBoostTable[["intDepth"]]     <- if (ready) regBoostResults$res$interaction.depth   else "."
  regBoostTable[["minObsInNode"]] <- if (ready) options$nNode                           else "."
  regBoostTable[["ntrain"]]       <- if (ready) regBoostResults$res$nTrain              else "."
  regBoostTable[["ntest"]]        <- if (ready) length(regBoostResults$data$testTarget) else "."
  
}

.regBoostRelInfTable <- function(jaspResults, options, regBoostResults, ready, analysisOptions) {
  if (!options$regBoostRelInfTable || !is.null(jaspResults[["regBoostRelInfTable"]])) return()
  
  # Create table
  regBoostRelInfTable <- createJaspTable(title = "Relative Influence")
  jaspResults[["regBoostRelInfTable"]] <- regBoostRelInfTable
  jaspResults[["regBoostRelInfTable"]]$dependOn(options = c("target", "predictors", "indicator", "applyModel",
                                                            "noOfTrees", "shrinkage", "int.depth",
                                                            "modelOptimization", "cvFolds", "nNode", "dataTrain",
                                                            "dataTrain", "bag.fraction", "dist", "seedBox", "seed",
                                                            "regBoostRelInfTable"))
  
  # Add column info
  regBoostRelInfTable$addColumnInfo(name = "predictor",  title = " ", type = "string")
  regBoostRelInfTable$addColumnInfo(name = "relIn",  title = "Relative Influence", type = "number", format = "sf:4")
  
  # Add data per column
  regBoostRelInfTable[["predictor"]] <- if(ready) .unv(regBoostResults$relInf$var) else "."
  regBoostRelInfTable[["relIn"]]     <- if(ready) regBoostResults$relInf$rel.inf   else "."
  
}

.regBoostApplyTable <- function(jaspResults, options, regBoostResults, ready, analysisOptions) {
  if (options$applyModel == "noApp" || !is.null(jaspResults[["applyModel"]])) return()
  
  # Create table and bind to jaspResults
  regBoostApplyTable <- createJaspTable(title = "Boosting Model Predictions")
  jaspResults[["regBoostApplyTable"]] <- regBoostApplyTable
  jaspResults[["regBoostApplyTable"]]$dependOn(options = c("target", "predictors", "indicator", "applyModel",
                                                           "noOfTrees", "shrinkage", "int.depth",
                                                           "modelOptimization", "cvFolds", "nNode", "dataTrain",
                                                           "dataTrain", "bag.fraction", "dist", "seedBox", "seed",
                                                           "applyModel"))
  
  # Add column info
  regBoostApplyTable$addColumnInfo(name = "case",  title = "Case"      , type = "integer")
  regBoostApplyTable$addColumnInfo(name = "pred",  title = "Prediction", type = "string")
  
  # Add data per column
  regBoostApplyTable[["case"]]  <- if (ready) as.integer(regBoostResults$apply$case)   else "."
  regBoostApplyTable[["pred"]]  <- if (ready) as.character(regBoostResults$apply$pred) else "."
  
}

.regBoostRelInfPlot <- function(jaspResults, options, regBoostResults, ready, analysisOptions) {
  if (!options$plotRelInf || !is.null(jaspResults[["regBoostRelInfPlot"]])) return()
  
  relInfPlot <- JASPgraphs::themeJasp(
    ggplot2::ggplot(regBoostResults$relInf, ggplot2::aes(x = reorder(.unv(as.factor(var)), rel.inf), y = rel.inf)) +
      ggplot2::geom_bar(stat = "identity", fill = "grey", col = "black", size = .3) +
      ggplot2::labs(x = "", y = "Relative Influence"),
    horizontal = TRUE
  )
  
  # Create plot and bind to jaspResults
  regBoostRelInfPlot <- createJaspPlot(plot = relInfPlot, title = "Relative Influence Plot",
                                         width = 500, height = 20 * nrow(regBoostResults$relInf) + 60)
  jaspResults[["regBoostRelInfPlot"]] <- regBoostRelInfPlot
  jaspResults[["regBoostRelInfPlot"]]$dependOn(options = c("target", "predictors", "indicator", "applyModel",
                                                           "noOfTrees", "shrinkage", "int.depth", "modelOptimization",
                                                           "cvFolds", "nNode", "dataTrain", "dataTrain", 
                                                           "bag.fraction", "dist", "seedBox", "seed", "plotRelInf"))
}

.regBoostPlotDeviance <- function(jaspResults, options, regBoostResults, ready, analysisOptions) {
  if (!options$plotDeviance || !is.null(jaspResults[["plotDeviance"]])) return()

  deviance <- data.frame(
    trees = 1:regBoostResults$res$n.trees,
    trainError = c(regBoostResults$res$train.error, regBoostResults$res$cv.error),
    what = rep(c("OOB", "CV"), c(length(regBoostResults$res$train.error), length(regBoostResults$res$cv.error)))
    )
  
  plotDeviance <- JASPgraphs::themeJasp(
    ggplot2::ggplot(data = deviance, mapping = ggplot2::aes(x = trees, y = trainError, group = what, color = what)) +
      ggplot2::geom_line(size = 1, show.legend = regBoostResults$method != "OOB") +
      ggplot2::scale_x_continuous(name = "Trees", labels = scales::comma) +
      ggplot2::ylab("Gaussian Deviance") +
      ggplot2::scale_color_manual(name = "", values = c("OOB" = "gray20", "CV" = "#99c454")) +
      ggplot2::geom_vline(xintercept = regBoostResults$optTrees, color = "lightgray", linetype = "dashed"),
    legend.position = "right"
  )

  # Create plot and bind to jaspResults
  plotDeviance <- createJaspPlot(plot = plotDeviance, title = "Deviance Plot", width = 500, height = 400)
  jaspResults[["plotDeviance"]] <- plotDeviance
  jaspResults[["plotDeviance"]]$dependOn(options = c("target", "predictors", "indicator", "applyModel",
                                                     "noOfTrees", "shrinkage", "int.depth", "modelOptimization",
                                                     "cvFolds", "nNode", "dataTrain", "dataTrain", 
                                                     "bag.fraction", "dist", "seedBox", "seed", "plotDeviance"))
  
}

.regBoostPlotOOBChangeDev <- function(jaspResults, options, regBoostResults, ready, analysisOptions) {
  if (!options$plotOOBChangeDev || !is.null(jaspResults[["regBoostPlotOOBChangeDev"]])) return()
  
  oobDev <- data.frame(trees = 1:regBoostResults$res$n.trees, oobImprove = regBoostResults$res$oobag.improve)
  
  plotOOBChangeDev <- JASPgraphs::themeJasp(
    ggplot2::ggplot(data = oobDev, mapping = ggplot2::aes(x = trees, y = oobImprove)) +
      ggplot2::geom_line(size = 1) +
      ggplot2::geom_smooth(size = 1, colour = "darkred", se = FALSE) +
      ggplot2::scale_x_continuous(name = "Trees", labels = scales::comma) +
      ggplot2::ylab(paste("OOB Change in ", regBoostResults$spec$distribution, " Deviance")) +
      ggplot2::geom_vline(xintercept = regBoostResults$optTrees, color = "gray20", linetype = "dashed")
  )
  
  # Create plot and bind to jaspResults
  regBoostPlotOOBChangeDev <- createJaspPlot(plot = plotOOBChangeDev,title = "OOB Improvement Plot",
                                               width = 400, height = 400)
  jaspResults[["regBoostPlotOOBChangeDev"]] <- regBoostPlotOOBChangeDev
  jaspResults[["regBoostPlotOOBChangeDev"]]$dependOn(options = c("target", "predictors", "indicator", "applyModel",
                                                                 "noOfTrees", "shrinkage", "int.depth",
                                                                 "modelOptimization", "cvFolds", "nNode", "dataTrain",
                                                                 "dataTrain", "bag.fraction", "dist", "seedBox", 
                                                                 "seed", "plotOOBChangeDev"))
}

.regBoostPlotPredPerformance <- function(jaspResults, options, regBoostResults, ready) {
  if (!options$plotPredPerf) return()
  
  limits <- c(round(min(c(min(floor(regBoostResults$predPerf$pred))  , min(floor(regBoostResults$predPerf$obs))))),
              round(max(c(max(ceiling(regBoostResults$predPerf$pred)), max(ceiling(regBoostResults$predPerf$obs))))))
  
  regBoostPredPerfPlot <- JASPgraphs::themeJasp(
    ggplot2::ggplot(data = regBoostResults$predPerf, mapping = ggplot2::aes(x = obs, y = pred)) +
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
  
  regBoostPredPerfPlot <- createJaspPlot(plot = regBoostPredPerfPlot, title = title, width = 400, height = 400)
  
  jaspResults[["plotPredPerformance"]] <- regBoostPredPerfPlot
  jaspResults[["plotPredPerformance"]]$position <- 7
  jaspResults[["plotPredPerformance"]]$dependOn(options = c("target", "predictors", "indicator", "applyModel",
                                                            "noOfTrees", "shrinkage", "int.depth",
                                                            "modelOptimization", "cvFolds", "nNode", "dataTrain",
                                                            "dataTrain", "bag.fraction", "dist", "seedBox", 
                                                            "seed", "plotPredPerformance"))
}
