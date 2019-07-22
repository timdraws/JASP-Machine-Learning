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
  
	# Preparatory work
	dataset <- .readDataRegressionAnalyses(dataset, options)
	.errorHandlingRegressionAnalyses(dataset, options)
	
	# Check if analysis is ready to run
	ready <- .regressionAnalysesReady(options, type = "randomForest")
  
  # Compute (a list of) results from which tables and plots can be created
  .regressionMachineLearning(dataset, options, jaspResults, ready, type = "randomForest")

  # create the results table
	.regressionMachineLearningTable(options, jaspResults, ready, type = "randomForest")

  # Create the evaluation metrics table
	.regressionEvaluationMetrics(dataset, options, jaspResults, ready)

  # Create the variable importance table
  .randomForestVariableImportance(options, jaspResults, ready, purpose = "regression")

  # Create the predicted performance plot
	.regressionPredictedPerformancePlot(options, jaspResults, ready, position = 5)

  # Create the trees vs model error plot
  .randomForestTreesErrorPlot(options, jaspResults, ready, position = 6, purpose = "regression")

  # Create the mean decrease in accuracy plot
  .randomForestPlotDecreaseAccuracy(options, jaspResults, ready, position = 7, purpose = "regression")

  # Create the total increase in node purity plot
  .randomForestPlotIncreasePurity(options, jaspResults, ready, position = 8, purpose = "regression")

}

.randomForestRegression <- function(dataset, options, jaspResults){
  
  dataset                 <- na.omit(dataset)
  train.index             <- sample(c(TRUE,FALSE),nrow(dataset), replace = TRUE, prob = c(options[['trainingDataManual']], 1-options[['trainingDataManual']]))
  train                   <- dataset[train.index, ]
  test                    <- dataset[!train.index, ]

  predictors <- train[, .v(options[["predictors"]])]
  target <- train[, .v(options[["target"]])]
  test_predictors <- test[, .v(options[["predictors"]])]
  test_target <- test[, .v(options[["target"]])]

  if (options$noOfPredictors == "manual") {
    noOfPredictors <- options[["numberOfPredictors"]]
  } else {
    noOfPredictors <- floor(sqrt(length(options[["numberOfPredictors"]])))
  }

  if(options[["modelOpt"]] == "optimizationManual"){
      rfit <- randomForest::randomForest(x = predictors, y = target, xtest = test_predictors, ytest = test_target,
                                              ntree = options[["noOfTrees"]], mtry = noOfPredictors,
                                              sampsize = ceiling(options[["bagFrac"]]*nrow(dataset)),
                                              importance = TRUE, keep.forest = TRUE)
      noOfTrees <- options[["noOfTrees"]]
  } else if(options[["modelOpt"]] == "optimizationError"){
    rfit <- randomForest::randomForest(x = predictors, y = target, xtest = test_predictors, ytest = test_target,
                                        ntree = options[["maxTrees"]], mtry = noOfPredictors,
                                        sampsize = ceiling(options[["bagFrac"]]*nrow(dataset)),
                                        importance = TRUE, keep.forest = TRUE)
    oobError <- rfit$mse
    optimTrees <- which.min(oobError)[length(which.min(oobError))]

    rfit <- randomForest::randomForest(x = predictors, y = target, xtest = test_predictors, ytest = test_target,
                                            ntree = optimTrees, mtry = noOfPredictors,
                                            sampsize = ceiling(options[["bagFrac"]]*nrow(dataset)),
                                            importance = TRUE, keep.forest = TRUE)

    noOfTrees <- optimTrees
  }

  trainingFit <- randomForest::randomForest(x = predictors, y = target, xtest = predictors, ytest = target,
                                      ntree = noOfTrees, mtry = noOfPredictors,
                                      sampsize = ceiling(options[["bagFrac"]]*nrow(dataset)),
                                      importance = TRUE, keep.forest = TRUE)

  regressionResult <- list()
  regressionResult[["rfit"]]          <- rfit
  regressionResult[["trainingFit"]]   <- trainingFit
  regressionResult[["train"]]         <- train
  regressionResult[["test"]]          <- test
  regressionResult[["noOfTrees"]]     <- noOfTrees
  regressionResult[["predPerSplit"]]  <- noOfPredictors
  regressionResult[["bagFrac"]]       <- ceiling(options[["bagFrac"]]*nrow(dataset))
  regressionResult[["y"]]             <- rfit$test[["predicted"]]
  regressionResult[["x"]]             <- test[,.v(options[["target"]])]
  regressionResult[["mse"]]           <- mean((rfit$test[["predicted"]] - test[,.v(options[["target"]])])^2)
  regressionResult[["ntrain"]]        <- nrow(train)
  regressionResult[["ntest"]]         <- nrow(test)
  regressionResult[["oobError"]]      <- rfit$mse[length(rfit$mse)]
  regressionResult[["varImp"]] <- plyr::arrange(data.frame(
    Variable         = .unv(as.factor(names(rfit$importance[,1]))),
    MeanIncrMSE      = rfit$importance[, 1],
    TotalDecrNodeImp = rfit$importance[, 2]
  ), -TotalDecrNodeImp)
   
  return(regressionResult)
}

.randomForestVariableImportance <- function(options, jaspResults, ready, purpose){

  if(!is.null(jaspResults[["tableVariableImportance"]]) || !options[["tableVariableImportance"]]) return()
  
  tableVariableImportance <- createJaspTable(title = "Variable Importance")
  tableVariableImportance$position <- 4
  tableVariableImportance$dependOn(options = c("tableVariableImportance", "scaleEqualSD", "target", "predictors", "modelOpt", "maxTrees",
                                                "noOfTrees", "bagFrac", "noOfPredictors", "numberOfPredictors", "seed", "seedBox"))

  tableVariableImportance$addColumnInfo(name = "predictor",  title = " ", type = "string")
  tableVariableImportance$addColumnInfo(name = "MDiA",  title = "Mean decrease in accuracy", type = "number")
  tableVariableImportance$addColumnInfo(name = "MDiNI",  title = "Total increase in node purity", type = "number")
  
  jaspResults[["tableVariableImportance"]] <- tableVariableImportance

  if(!ready)  return()

  result <- base::switch(purpose,
                          "classification" = jaspResults[["classificationResult"]]$object,
                          "regression" = jaspResults[["regressionResult"]]$object)

  varImpOrder <- sort(result[["rfit"]]$importance[,1], decr = TRUE, index.return = TRUE)$ix
  
  tableVariableImportance[["predictor"]] <- .unv(.v(result[["varImp"]]$Variable))
  tableVariableImportance[["MDiA"]]      <- result[["varImp"]]$MeanIncrMSE    
  tableVariableImportance[["MDiNI"]]     <- result[["varImp"]]$TotalDecrNodeImp
  
}

.randomForestTreesErrorPlot <- function(options, jaspResults, ready, position, purpose){

  if(!is.null(jaspResults[["plotTreesVsModelError"]]) || !options[["plotTreesVsModelError"]]) return()

  plotTreesVsModelError <- createJaspPlot(plot = NULL, title = "Out-of-bag Error Plot", width = 500, height = 300)
  plotTreesVsModelError$position <- position
  plotTreesVsModelError$dependOn(options = c("plotTreesVsModelError", "trainingDataManual", "scaleEqualSD", "modelOpt", "maxTrees",
                                            "target", "predictors", "seed", "seedBox", "noOfTrees", "bagFrac", "noOfPredictors", "numberOfPredictors"))
  jaspResults[["plotTreesVsModelError"]] <- plotTreesVsModelError

  if(!ready) return()

  result <- base::switch(purpose,
                  "classification" = jaspResults[["classificationResult"]]$object,
                  "regression" = jaspResults[["regressionResult"]]$object)
  xTitle <- base::switch(purpose,
                          "classification" = "Out-of-bag \nClassification Error",
                          "regression" = "Out-of-bag \nMean Squared Error")

  values <- base::switch(purpose,
                        "classification" = result[["rfit"]]$err.rate[,1],
                        "regression" = result[["rfit"]]$mse)

  values2 <- base::switch(purpose,
                          "classification" = result[["trainingFit"]]$err.rate[,1],
                          "regression" = result[["trainingFit"]]$mse)
  values <- c(values, values2)

  treesMSE <- data.frame(
    trees = rep(1:length(values2), 2),
    error = values, 
    type = rep(c("Test set", "Training set"), each = length(values2))
  )

  xBreaks <- JASPgraphs::getPrettyAxisBreaks(treesMSE[["trees"]], min.n = 4)
  yBreaks <- JASPgraphs::getPrettyAxisBreaks(treesMSE[["error"]], min.n = 4)
  
  p <- ggplot2::ggplot(data = treesMSE, mapping = ggplot2::aes(x = trees, y = error, linetype = type)) +
        JASPgraphs::geom_line()
  if(max(treesMSE[["trees"]]) <= 25)
    p <- p + JASPgraphs::geom_point()

  p <- p + ggplot2::scale_x_continuous(name = "Number of Trees", labels = xBreaks, breaks = xBreaks) +
            ggplot2::scale_y_continuous(name = xTitle, labels = yBreaks, breaks = yBreaks) +
            ggplot2::labs(linetype = "")
  p <- JASPgraphs::themeJasp(p, legend.position = "top")

  plotTreesVsModelError$plotObject <- p
}

.randomForestPlotDecreaseAccuracy <- function(options, jaspResults, ready, position, purpose){

  if(!is.null(jaspResults[["plotDecreaseAccuracy"]]) || !options[["plotDecreaseAccuracy"]]) return()

  plotDecreaseAccuracy <- createJaspPlot(plot = NULL, title = "Mean Decrease in Accuracy", width = 500, height = 300)
  plotDecreaseAccuracy$position <- position
  plotDecreaseAccuracy$dependOn(options = c("plotDecreaseAccuracy", "trainingDataManual", "scaleEqualSD", "modelOpt", "maxTrees",
                                            "target", "predictors", "seed", "seedBox", "noOfTrees", "bagFrac", "noOfPredictors", "numberOfPredictors"))
  jaspResults[["plotDecreaseAccuracy"]] <- plotDecreaseAccuracy

  if(!ready) return()

  result <- base::switch(purpose,
                        "classification" = jaspResults[["classificationResult"]]$object,
                        "regression" = jaspResults[["regressionResult"]]$object)
  
  p <- ggplot2::ggplot(result[["varImp"]], ggplot2::aes(x = reorder(Variable, MeanIncrMSE), y = MeanIncrMSE)) +
      ggplot2::geom_bar(stat = "identity", fill = "grey", col = "black", size = .3) +
      ggplot2::labs(x = "", y = "Mean Decrease in Accuracy")
  p <-JASPgraphs::themeJasp(p, horizontal = TRUE)
  
  plotDecreaseAccuracy$plotObject <- p
}

.randomForestPlotIncreasePurity <- function(options, jaspResults, ready, position, purpose){

  if(!is.null(jaspResults[["plotIncreasePurity"]]) || !options[["plotIncreasePurity"]]) return()

  plotIncreasePurity <- createJaspPlot(plot = NULL, title = "Total Increase in Node Purity", width = 500, height = 300)
  plotIncreasePurity$position <- position
  plotIncreasePurity$dependOn(options = c("plotIncreasePurity", "trainingDataManual", "scaleEqualSD", "modelOpt", "maxTrees",
                                            "target", "predictors", "seed", "seedBox", "noOfTrees", "bagFrac", "noOfPredictors", "numberOfPredictors"))
  jaspResults[["plotIncreasePurity"]] <- plotIncreasePurity

  if(!ready) return()

  result <- base::switch(purpose,
                      "classification" = jaspResults[["classificationResult"]]$object,
                      "regression" = jaspResults[["regressionResult"]]$object)
  
  p <- ggplot2::ggplot(result[["varImp"]], ggplot2::aes(x = reorder(Variable, TotalDecrNodeImp), y = TotalDecrNodeImp)) +
        ggplot2::geom_bar(stat = "identity", fill = "grey", col = "black", size = .3) +
        ggplot2::labs(x = "", y = "Total Increase in Node Purity")
  p <- JASPgraphs::themeJasp(p, horizontal = TRUE)

  plotIncreasePurity$plotObject <- p
}
