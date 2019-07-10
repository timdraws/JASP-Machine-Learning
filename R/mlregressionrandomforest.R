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

MLRegressionRandomForest <- function(jaspResults, dataset, options, ...) {
  
  # Read dataset
  dataset <- .regRanForReadData(dataset, options)
  
  # Check if results can be computed
  ready <- (!is.null(options$target) && length(.v(options$predictors)) > 0)
  
  # Error checking
  if (ready) errors <- .regRanForErrorHandling(dataset, options)
  
  # Compute (a list of) results from which tables and plots can be created
  if (ready) regRanForResults <- .regRanForComputeResults(jaspResults, dataset, options)
  
  # Output tables
  .regRanForTable(      jaspResults, options, regRanForResults, ready)
  .regRanForVarImpTable(jaspResults, options, regRanForResults, ready)
  .regRanForApplyTable( jaspResults, options, regRanForResults, ready)

  # Output plots
  if (ready) .regRanForPlotVarImpAcc(        jaspResults, options, regRanForResults)
  if (ready) .regRanForPlotVarImpPur(        jaspResults, options, regRanForResults)
  if (ready) .regRanForPlotTreesVsModelError(jaspResults, options, regRanForResults)
  if (ready) .regRanForPlotPredPerformance(  jaspResults, options, regRanForResults)
  
  return()
}

# Read dataset
.regRanForReadData <- function(dataset, options) {
  
  if (options$target == "")    options$target <- NULL
  if (options$indicator == "") options$indicator <- NULL
  
  data <- .readDataSetToEnd(columns.as.numeric = options$target, columns = options$predictors,
                            columns.as.factor = options$indicator)
  
  return(data)
}

# Check for errors
.regRanForErrorHandling <- function(dataset, options) {
  
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
.regRanForComputeResults <- function(jaspResults, dataset, options, analysisOptions) {
  
  if (!is.null(jaspResults[["stateRegRanForResults"]])) return (jaspResults[["stateRegRanForResults"]]$object)
  
  results <- list()
  results[["spec"]] <- .regRanForCalcSpecs(dataset, options)
  
  # Set seed	
  if (options$seedBox) set.seed(options$seed)
  
  # Prepare data
  preds <- which(colnames(dataset) %in% .v(options$predictors)) # predictors
  target <- which(colnames(dataset) == .v(options$target)) # target
  if(options$indicator != "") indicator <- which(colnames(dataset) == .v(options$indicator))
  
  # Deal with NAs: omit all rows that contain at least one missing value
  if (options$applyModel == "applyImpute") { # save target NA observations to make predictions later
    
    idxApply <- which(is.na(dataset[, target]))
    predImpute <- cbind(dataset[idxApply, target, drop = FALSE], na.omit(dataset[idxApply, preds, drop = FALSE]))
    
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
  
  idtrainPreds <- sample(1:nrow(modelData), floor(options$dataTrain * nrow(modelData)))
  idtestPreds <- (1:nrow(modelData))[-idtrainPreds]
  
  trainPreds <- modelData[idtrainPreds, preds, drop = FALSE]
  trainTarget <- modelData[idtrainPreds, target]
  testPreds  <- modelData[idtestPreds, preds, drop = FALSE]
  testTarget  <- modelData[idtestPreds, target]
  
  # Run Random Forest
  results[["res"]] <- randomForest::randomForest(x = trainPreds, y = trainTarget, xtest = testPreds, ytest = testTarget,
                                                 ntree = options$noOfTrees, mtry = results$spec$noOfPredictors,
                                                 sampsize = ceiling(options$bagFrac * nrow(dataset)),
                                                 importance = TRUE, keep.forest = TRUE)
  
  results[["data"]]       <- list(trainPreds = trainPreds, trainTarget = trainTarget,
                                  testPreds = testPreds, testTarget = testTarget)
  
  results[["importance"]] <- results$res$importance
  modPred                 <- results$res$test$predicted
  
  # Making a variable importance table
  results[["varImp"]] <- plyr::arrange(data.frame(
    Variable = .unv(as.factor(names(results$res$importance[,1]))),
    MeanIncrMSE = results$res$importance[, 1],
    TotalDecrNodeImp = results$res$importance[, 2]
  ), -MeanIncrMSE)
  
  # Predictive performance
  results[["predPerf"]] <- data.frame(pred = as.numeric(modPred), obs = as.numeric(testTarget))
  results[["testMSE"]]  <- mean((modPred - testTarget)^2)
  results[["testR2"]]   <- round(cor(modPred, testTarget)^2, 2)
  
  # Applying the model
  if(options$indicator != "") {
    results[["apply"]] <- randomForest::predict.randomForest(results$res, applyData, type = "response")
  } else {
    results[["apply"]] <- NULL
  }
  
  # Save results to state
  jaspResults[["stateregRanForResults"]] <- createJaspState(results)
  jaspResults[["stateregRanForResults"]]$dependOn(options = c("target", "predictors", "indicator", "applyModel",
                                                              "noOfTrees", "noOfPredictors", "numberOfPredictors",
                                                              "dataTrain", "bagFrac", "seedBox", "seed"))
  
  return(results)
}

.regRanForCalcSpecs <- function(dataset, options) {
  specs <- list()
  
  # Setting the number of variables considered at each split
  if (options$noOfPredictors == "manual") {
    specs$noOfPredictors <- as.integer(options$numberOfPredictors)
  } else {
    specs$noOfPredictors <- if (!is.null(options$target) && !is.factor(options$target)) 
      max(floor(length(.v(options$predictors))/3), 1) else floor(sqrt(length(.v(options$predictors))))
  }
  
  return(specs)
}

.regRanForTable <- function(jaspResults, options, regRanForResults, ready, analysisOptions) {
  if (!is.null(jaspResults[["regRanForTable"]])) return()
  
  # Create table and bind to jaspResults
  regRanForTable <- createJaspTable(title = "Random Forest Regression Model Summary")
  jaspResults[["regRanForTable"]] <- regRanForTable
  jaspResults[["regRanForTable"]]$position <- 1
  jaspResults[["regRanForTable"]]$dependOn(options = c("target", "predictors", "indicator", "applyModel",
                                                       "noOfTrees", "noOfPredictors", "numberOfPredictors",
                                                       "dataTrain", "bagFrac", "seedBox", "seed"))
  
  # Add column info
  if(options$dataTrain < 1){
    regRanForTable$addColumnInfo(name = "testMSE",  title = "Test Set MSE", type = "number", format = "sf:4")
  }
  if (options$dataTrain < 1) {
    regRanForTable$addColumnInfo(name = "testR2",  title = "Test Set R\u00B2", type = "number", format = "sf:4")
  }
  regRanForTable$addColumnInfo(name = "oobMSE",  title = "OOB MSE"             , type = "number", format = "sf:4")
  regRanForTable$addColumnInfo(name = "ntrees",  title = "Trees"               , type = "integer")
  regRanForTable$addColumnInfo(name = "mtry"  ,  title = "Predictors per split", type = "integer")
  regRanForTable$addColumnInfo(name = "nTrain",  title = "n(Train)"            , type = "integer")
  regRanForTable$addColumnInfo(name = "nTest" ,  title = "n(Test)"             , type = "integer")
  
  # Add data per column
  if (options$dataTrain < 1){ regRanForTable[["testMSE"]] <- if (ready) regRanForResults$testMSE       else "." }
  if (options$dataTrain < 1){ regRanForTable[["testR2"]]    <- if (ready) regRanForResults$testR2      else "." }
  regRanForTable[["oobMSE"]]  <- if (ready) regRanForResults$res$mse[length(regRanForResults$res$mse)] else "."
  regRanForTable[["ntrees"]]  <- if (ready) regRanForResults$res$ntree else "."
  regRanForTable[["mtry"]]    <- if (ready) regRanForResults$res$mtry else "."
  regRanForTable[["nTrain"]]  <- if (ready) nrow(regRanForResults$data$trainPreds) else "."
  regRanForTable[["nTest"]]   <- if (ready) nrow(regRanForResults$data$testPreds) else "."
  
}

.regRanForVarImpTable <- function(jaspResults, options, regRanForResults, ready, analysisOptions) {
  if (!options$regRanForVarImpTable || !is.null(jaspResults[["regRanForVarImpTable"]])) return()
  
  # Create table
  regRanForVarImpTable <- createJaspTable(title = "Variable Importance")
  jaspResults[["regRanForVarImpTable"]] <- regRanForVarImpTable
  jaspResults[["regRanForVarImpTable"]]$position <- 2
  jaspResults[["regRanForVarImpTable"]]$dependOn(options = c("target", "predictors", "indicator", "applyModel",
                                                             "noOfTrees", "noOfPredictors", "numberOfPredictors",
                                                             "dataTrain", "bagFrac", "seedBox", "seed",
                                                             "regRanForVarImpTable"))
  
  # Add column info
  regRanForVarImpTable$addColumnInfo(name = "predictor",  title = " ", type = "string")
  regRanForVarImpTable$addColumnInfo(name = "acc",  title = "Mean decr. in accuracy", type = "number", format = "sf:4")
  regRanForVarImpTable$addColumnInfo(name = "pur",  title = "Total incr. in node purity", type = "number",
                                     format = "sf:4")
  
  # Add data per column
  regRanForVarImpTable[["predictor"]] <- if(ready) as.character(regRanForResults$varImp$Variable) else "."
  regRanForVarImpTable[["acc"]]       <- if(ready) regRanForResults$varImp$MeanIncrMSE            else "."
  regRanForVarImpTable[["pur"]]       <- if(ready) regRanForResults$varImp$TotalDecrNodeImp       else "."
  
}

.regRanForApplyTable <- function(jaspResults, options, regRanForResults, ready, analysisOptions) {
  if (!is.null(jaspResults[["regRanForApplyTable"]])) return()
  if (options$indicator == "") return()
  
  # Create table and bind to jaspResults
  regRanForApplyTable <- createJaspTable(title = "Random Forest Model Predictions")
  jaspResults[["regRanForApplyTable"]] <- regRanForApplyTable
  jaspResults[["regRanForApplyTable"]]$position <- 3
  jaspResults[["regRanForApplyTable"]]$dependOn(options = c("target", "predictors", "indicator", "applyModel",
                                                            "noOfTrees", "noOfPredictors", "numberOfPredictors",
                                                            "dataTrain", "bagFrac", "seedBox", "seed",
                                                            "applyModel"))
  
  # Add column info
  regRanForApplyTable$addColumnInfo(name = "row",  title = "Row", type = "integer")
  regRanForApplyTable$addColumnInfo(name = "pred",  title = "Prediction", type = "number", format = "sf:4")
  
  # Add data per column
  regRanForApplyTable[["row"]]  <- if (ready) as.numeric(rownames(as.data.frame(regRanForResults$apply))) else "."
  regRanForApplyTable[["pred"]]  <- if (ready) as.numeric(regRanForResults$apply) else "."
  
}

.regRanForPlotVarImpAcc <- function(jaspResults, options, regRanForResults, ready) {
  if (!options$plotVarImpAcc) return()
  
  regRanForPlotVarImpAcc <- JASPgraphs::themeJasp(
    ggplot2::ggplot(regRanForResults$varImp, ggplot2::aes(x = reorder(Variable, MeanIncrMSE), y = MeanIncrMSE)) +
      ggplot2::geom_bar(stat = "identity", fill = "grey", col = "black", size = .3) +
      ggplot2::labs(x = "", y = "Mean Decrease in Accuracy"),
    horizontal = TRUE
  )
  
  regRanForPlotVarImpAcc <- createJaspPlot(plot = regRanForPlotVarImpAcc, 
                                             title = "Mean Decrease in Accuracy per Variable",
                                             width = 400, height = 20 * nrow(regRanForResults$varImp) + 60)
  
  jaspResults[["regRanForPlotVarImpAcc"]] <- regRanForPlotVarImpAcc
  jaspResults[["regRanForPlotVarImpAcc"]]$position <- 4
  jaspResults[["regRanForPlotVarImpAcc"]]$dependOn(options = c("target", "predictors", "indicator", "applyModel",
                                                               "noOfTrees", "noOfPredictors", "numberOfPredictors",
                                                               "dataTrain", "bagFrac", "seedBox", "seed",
                                                               "plotVarImpAcc"))
}

.regRanForPlotVarImpPur <- function(jaspResults, options, regRanForResults, ready) {
  if (!options$plotVarImpPur) return()
  
  regRanForPlotVarImpPur <- JASPgraphs::themeJasp(
    ggplot2::ggplot(regRanForResults$varImp,
                    ggplot2::aes(x = reorder(Variable, TotalDecrNodeImp), y = TotalDecrNodeImp)) +
      ggplot2::geom_bar(stat = "identity", fill = "grey", col = "black", size = .3) +
      ggplot2::labs(x = "", y = "Total Increase in Node Purity"),
    horizontal = TRUE
  )
  
  regRanForPlotVarImpPur <- createJaspPlot(plot = regRanForPlotVarImpPur,
                                           title = "Total Increase in Node Purity per Variable",
                                           width = 400, height = 20 * nrow(regRanForResults$varImp) + 60)
  
  jaspResults[["regRanForPlotVarImpPur"]] <- regRanForPlotVarImpPur
  jaspResults[["regRanForPlotVarImpPur"]]$position <- 5
  jaspResults[["regRanForPlotVarImpPur"]]$dependOn(options = c("target", "predictors", "indicator", "applyModel",
                                                               "noOfTrees", "noOfPredictors", "numberOfPredictors",
                                                               "dataTrain", "bagFrac", "seedBox", "seed",
                                                               "plotVarImpPur"))
}

.regRanForPlotTreesVsModelError <- function(jaspResults, options, regRanForResults, ready) {
  if (!options$plotTreesVsModelError) return()
  
  treesMSE <- dplyr::tibble(
    trees = 1:length(regRanForResults$res$mse),
    MSE = regRanForResults$res$mse
  )
  
  plotTreesVsModelError <- JASPgraphs::themeJasp(
    ggplot2::ggplot(data = treesMSE, mapping = ggplot2::aes(x = trees, y = MSE)) +
      ggplot2::geom_line(size = 1) +
      ggplot2::xlab("Trees") +
      ggplot2::ylab("OOB Mean Squared Error")
  )
  
  plotTreesVsModelError <- createJaspPlot(plot = plotTreesVsModelError, title = "Trees vs. Model Error",
                                          width = 400, height = 400)
  
  jaspResults[["plotTreesVsModelError"]] <- plotTreesVsModelError
  jaspResults[["plotTreesVsModelError"]]$position <- 6
  jaspResults[["plotTreesVsModelError"]]$dependOn(options = c("target", "predictors", "indicator", "applyModel",
                                                              "noOfTrees", "noOfPredictors", "numberOfPredictors",
                                                              "dataTrain", "bagFrac", "seedBox", "seed",
                                                              "plotTreesVsModelError"))
  
}

.regRanForPlotPredPerformance <- function(jaspResults, options, regRanForResults, ready) {
  if (!options$plotPredPerf) return()
  
  limits <- c(round(min(c(min(floor(regRanForResults$predPerf$pred))  , min(floor(regRanForResults$predPerf$obs))))),
              round(max(c(max(ceiling(regRanForResults$predPerf$pred)), max(ceiling(regRanForResults$predPerf$obs))))))
  
  regRanForPredPerfPlot <- JASPgraphs::themeJasp(
    ggplot2::ggplot(data = regRanForResults$predPerf, mapping = ggplot2::aes(x = obs, y = pred)) +
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
  
  regRanForPredPerfPlot <- createJaspPlot(plot = regRanForPredPerfPlot, title = title, width = 400, height = 400)
  
  jaspResults[["plotPredPerformance"]] <- regRanForPredPerfPlot
  jaspResults[["plotPredPerformance"]]$position <- 7
  jaspResults[["plotPredPerformance"]]$dependOn(options = c("target", "predictors", "indicator", "applyModel",
                                                            "noOfTrees", "noOfPredictors", "numberOfPredictors",
                                                            "dataTrain", "bagFrac", "seedBox", "seed",
                                                            "plotPredPerformance"))
}
