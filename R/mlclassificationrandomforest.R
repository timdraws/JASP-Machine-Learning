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

  # Create the validation measures table
  .classificationEvaluationMetrics(dataset, options, jaspResults, ready)

  # Create the variable importance table
  .randomForestVariableImportance(options, jaspResults, ready, purpose = "classification")

  # Create the trees vs model error plot
  .randomForestTreesErrorPlot(options, jaspResults, ready, position = 5, purpose = "classification")

  # Create the ROC curve
  .rocCurve(dataset, options, jaspResults, ready, position = 6, type = "randomForest")

  # Create the mean decrease in accuracy plot
  .randomForestPlotDecreaseAccuracy(options, jaspResults, ready, position = 7, purpose = "classification")

  # Create the total increase in node purity plot
  .randomForestPlotIncreasePurity(options, jaspResults, ready, position = 8, purpose = "classification")

  # Decision boundaries
  .classificationDecisionBoundaries(dataset, options, jaspResults, ready, position = 9, type = "randomForest")
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
