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

MLClassificationRandomForest <- function(jaspResults, dataset, options, ...) {
  
  # Preparatory work
  dataset <- .readDataClassificationAnalyses(dataset, options)
  .errorHandlingClassificationAnalyses(dataset, options)
  
  # Check if analysis is ready to run
  ready <- .classificationAnalysesReady(options, type = "randomForest")

  # Run the analysis
  .classification(dataset, options, jaspResults, ready, type = "randomForest")

  # create the results table
  .classificationTable(options, jaspResults, ready, type = "randomForest")

  # Create the confusion table
  .classificationConfusionTable(dataset, options, jaspResults, ready)

  # Create the variable importance table
  .randomForestClassificationVariableImportance(options, jaspResults, ready)

  # Create the trees vs model error plot
  .randomForestClassificationTreesError(options, jaspResults, ready, position = 4)

  # Create the ROC curve
  .rocCurve(options, jaspResults, ready, position = 5)

  # Create the mean decrease in accuracy plot
  .randomForestPlotDecreaseAccuracy(options, jaspResults, ready, position = 6)

  # Create the total increase in node purity plot
  .randomForestPlotIncreasePurity(options, jaspResults, ready, position = 7)

  # Decision boundaries
  .classificationDecisionBoundaries(dataset, options, jaspResults, ready, position = 8, type = "randomForest")
}

.randomForestClassification <- function(dataset, options, jaspResults){
  
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
    oobError <- rfit$err.rate[, 1]
    optimTrees <- which.min(oobError)[length(which.min(oobError))]

    rfit <- randomForest::randomForest(x = predictors, y = target, xtest = test_predictors, ytest = test_target,
                                            ntree = optimTrees, mtry = noOfPredictors,
                                            sampsize = ceiling(options[["bagFrac"]]*nrow(dataset)),
                                            importance = TRUE, keep.forest = TRUE)

    noOfTrees <- optimTrees
  }


  classificationResult <- list()
  classificationResult[["rfit"]]          <- rfit
  classificationResult[["train"]]         <- train
  classificationResult[["test"]]          <- test
  classificationResult[["noOfTrees"]]     <- noOfTrees
  classificationResult[["predPerSplit"]]  <- noOfPredictors
  classificationResult[["bagFrac"]]       <- ceiling(options[["bagFrac"]]*nrow(dataset))
  classificationResult[["y"]]             <- rfit$test[["predicted"]]
  classificationResult[["x"]]             <- test[,.v(options[["target"]])]
  classificationResult[["confTable"]]     <- table('Pred' = classificationResult[["y"]], 'Real' = test[,.v(options[["target"]])])
  classificationResult[["mse"]]           <- 1 - sum(diag(prop.table(classificationResult[['confTable']])))
  classificationResult[["ntrain"]]        <- nrow(train)
  classificationResult[["ntest"]]         <- nrow(test)
  classificationResult[["oobError"]]      <- rfit$err.rate[length(rfit$err.rate)]
  classificationResult[["varImp"]] <- plyr::arrange(data.frame(
    Variable         = .unv(as.factor(names(rfit$importance[,1]))),
    MeanIncrMSE      = rfit$importance[, 1],
    TotalDecrNodeImp = rfit$importance[, 2]
  ), -TotalDecrNodeImp)

  return(classificationResult)
}

.randomForestClassificationVariableImportance <- function(options, jaspResults, ready){

  if(!is.null(jaspResults[["tableVariableImportance"]]) || !options[["tableVariableImportance"]]) return()
  
  tableVariableImportance <- createJaspTable(title = "Variable Importance")
  tableVariableImportance$position <- 3
  tableVariableImportance$dependOn(options = c("tableVariableImportance", "scaleEqualSD", "target", "predictors", "modelOpt", "maxTrees",
                                                "noOfTrees", "bagFrac", "noOfPredictors", "numberOfPredictors", "seed", "seedBox"))

  tableVariableImportance$addColumnInfo(name = "predictor",  title = " ", type = "string")
  tableVariableImportance$addColumnInfo(name = "MDiA",  title = "Mean decrease in accuracy", type = "number")
  tableVariableImportance$addColumnInfo(name = "MDiNI",  title = "Total increase in node purity", type = "number")
  
  jaspResults[["tableVariableImportance"]] <- tableVariableImportance

  if(!ready)  return()

  classificationResult <- jaspResults[["classificationResult"]]$object

  varImpOrder <- sort(classificationResult[["rfit"]]$importance[,1], decr = TRUE, index.return = TRUE)$ix
  
  tableVariableImportance[["predictor"]] <- .unv(.v(classificationResult[["varImp"]]$Variable))
  tableVariableImportance[["MDiA"]]      <- classificationResult[["varImp"]]$MeanIncrMSE    
  tableVariableImportance[["MDiNI"]]     <- classificationResult[["varImp"]]$TotalDecrNodeImp
  
}

.randomForestPlotDecreaseAccuracy <- function(options, jaspResults, ready, position){

  if(!is.null(jaspResults[["plotDecreaseAccuracy"]]) || !options[["plotDecreaseAccuracy"]]) return()

  plotDecreaseAccuracy <- createJaspPlot(plot = NULL, title = "Mean Decrease in Accuracy", width = 500, height = 300)
  plotDecreaseAccuracy$position <- position
  plotDecreaseAccuracy$dependOn(options = c("plotDecreaseAccuracy", "trainingDataManual", "scaleEqualSD", "modelOpt", "maxTrees",
                                            "target", "predictors", "seed", "seedBox", "noOfTrees", "bagFrac", "noOfPredictors", "numberOfPredictors"))
  jaspResults[["plotDecreaseAccuracy"]] <- plotDecreaseAccuracy

  if(!ready) return()

  classificationResult <- jaspResults[["classificationResult"]]$object
  
  p <- ggplot2::ggplot(classificationResult[["varImp"]], ggplot2::aes(x = reorder(Variable, MeanIncrMSE), y = MeanIncrMSE)) +
      ggplot2::geom_bar(stat = "identity", fill = "grey", col = "black", size = .3) +
      ggplot2::labs(x = "", y = "Mean Decrease in Accuracy")
  p <-JASPgraphs::themeJasp(p, horizontal = TRUE)
  
  plotDecreaseAccuracy$plotObject <- p
}

.randomForestPlotIncreasePurity <- function(options, jaspResults, ready, position){

  if(!is.null(jaspResults[["plotIncreasePurity"]]) || !options[["plotIncreasePurity"]]) return()

  plotIncreasePurity <- createJaspPlot(plot = NULL, title = "Total Increase in Node Purity", width = 500, height = 300)
  plotIncreasePurity$position <- position
  plotIncreasePurity$dependOn(options = c("plotIncreasePurity", "trainingDataManual", "scaleEqualSD", "modelOpt", "maxTrees",
                                            "target", "predictors", "seed", "seedBox", "noOfTrees", "bagFrac", "noOfPredictors", "numberOfPredictors"))
  jaspResults[["plotIncreasePurity"]] <- plotIncreasePurity

  if(!ready) return()

  classificationResult <- jaspResults[["classificationResult"]]$object
  
  p <- ggplot2::ggplot(classificationResult[["varImp"]], ggplot2::aes(x = reorder(Variable, TotalDecrNodeImp), y = TotalDecrNodeImp)) +
        ggplot2::geom_bar(stat = "identity", fill = "grey", col = "black", size = .3) +
        ggplot2::labs(x = "", y = "Total Increase in Node Purity")
  p <- JASPgraphs::themeJasp(p, horizontal = TRUE)

  plotIncreasePurity$plotObject <- p
}

.randomForestClassificationTreesError <- function(options, jaspResults, ready, position){

  if(!is.null(jaspResults[["plotTreesVsModelError"]]) || !options[["plotTreesVsModelError"]]) return()

  plotTreesVsModelError <- createJaspPlot(plot = NULL, title = "Trees vs. Out-of-bag Error", width = 500, height = 300)
  plotTreesVsModelError$position <- position
  plotTreesVsModelError$dependOn(options = c("plotTreesVsModelError", "trainingDataManual", "scaleEqualSD", "modelOpt", "maxTrees",
                                            "target", "predictors", "seed", "seedBox", "noOfTrees", "bagFrac", "noOfPredictors", "numberOfPredictors"))
  jaspResults[["plotTreesVsModelError"]] <- plotTreesVsModelError

  if(!ready) return()

  classificationResult <- jaspResults[["classificationResult"]]$object

  treesMSE <- dplyr::tibble(
    trees = 1:length(classificationResult[["rfit"]]$err.rate[,1]),
    error = classificationResult[["rfit"]]$err.rate[,1]
  )

  xBreaks <- JASPgraphs::getPrettyAxisBreaks(treesMSE[["trees"]], min.n = 4)
  yBreaks <- JASPgraphs::getPrettyAxisBreaks(treesMSE[["error"]], min.n = 4)
  
  p <- ggplot2::ggplot(data = treesMSE, mapping = ggplot2::aes(x = trees, y = error)) +
        ggplot2::geom_line() +
        ggplot2::scale_x_continuous(name = "Number of Trees", labels = xBreaks, breaks = xBreaks) +
        ggplot2::scale_y_continuous(name = "OOB Classification Error", labels = yBreaks, breaks = yBreaks)
  p <- JASPgraphs::themeJasp(p)
  plotTreesVsModelError$plotObject <- p
}
