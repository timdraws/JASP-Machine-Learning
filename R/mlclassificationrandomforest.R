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
  
  # # Output plots
  # if (ready) .classRanForPlotVarImp1(          jaspResults, options, classRanForResults, ready)
  # if (ready) .classRanForPlotVarImp2(          jaspResults, options, classRanForResults, ready)
  # if (ready) .classRanForPlotTreesVsModelError(jaspResults, options, classRanForResults, ready)

  # Decision boundaries
  .classificationDecisionBoundaries(dataset, options, jaspResults, ready, position = 10, type = "randomForest")
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

  rfit <- randomForest::randomForest(x = predictors, y = target, xtest = test_predictors, ytest = test_target,
                                                 ntree = options[["noOfTrees"]], mtry = noOfPredictors,
                                                 sampsize = ceiling(options[["bagFrac"]]*nrow(dataset)),
                                                 importance = TRUE, keep.forest = TRUE)

  classificationResult <- list()
  classificationResult[["rfit"]]          <- rfit
  classificationResult[["train"]]         <- train
  classificationResult[["test"]]          <- test
  classificationResult[["noOfTrees"]]     <- options[["noOfTrees"]]
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
  tableVariableImportance$dependOn(options = c("tableVariableImportance", "scaleEqualSD", "target", "predictors",
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

.classRanForPlotVarImp1 <- function(jaspResults, options, classRanForResults, ready) {
  if (!options$plotVarImp1) return()
  
  varImpPlot1 <- JASPgraphs::themeJasp(
    ggplot2::ggplot(classRanForResults$varImp, ggplot2::aes(x = reorder(Variable, MeanIncrMSE), y = MeanIncrMSE)) +
      ggplot2::geom_bar(stat = "identity", fill = "grey", col = "black", size = .3) +
      ggplot2::labs(x = "", y = "Mean Decrease in Accuracy"),
    horizontal = TRUE
  )
  
  jaspResults[['varImpPlot1']] <- createJaspPlot(plot = varImpPlot1, title = "Mean Decrease in Accuracy per Variable",
                                                 width = 400, height = 20 * nrow(classRanForResults$varImp) + 60)
  
  jaspResults[["varImpPlot1"]]$position <- 5
  jaspResults[["varImpPlot1"]]$dependOn(optionsFromObject =jaspResults[["classRanForTable"]])
  jaspResults[["varImpPlot1"]]$dependOn(options ="plotVarImp1")
}

.classRanForPlotVarImp2 <- function(jaspResults, options, classRanForResults, ready) {
  if (!options$plotVarImp2) return()
  
  varImpPlot2 <- JASPgraphs::themeJasp(
    ggplot2::ggplot(classRanForResults$varImp, ggplot2::aes(x = reorder(Variable, TotalDecrNodeImp), 
                                                            y = TotalDecrNodeImp)) +
      ggplot2::geom_bar(stat = "identity", fill = "grey", col = "black", size = .3) +
      ggplot2::labs(x = "", y = "Total Increase in Node Purity"),
    horizontal = TRUE
  )
  
  jaspResults[['varImpPlot2']] <- createJaspPlot(plot = varImpPlot2, 
                                                 title = "Total Increase in Node Purity per Variable",
                                                 width = 400, height = 20 * nrow(classRanForResults$varImp) + 60)
  
  jaspResults[["varImpPlot2"]]$position <- 6
  jaspResults[["varImpPlot2"]]$dependOn(optionsFromObject =jaspResults[["classRanForTable"]])
  jaspResults[["varImpPlot2"]]$dependOn(options ="plotVarImp2")
}

.randomForestClassificationTreesError <- function(options, jaspResults, ready, position){

  if(!is.null(jaspResults[["plotTreesVsModelError"]]) || !options[["plotTreesVsModelError"]]) return()

  plotTreesVsModelError <- createJaspPlot(plot = NULL, title = "Trees vs. Model Error", width = 500, height = 300)
  plotTreesVsModelError$position <- position
  plotTreesVsModelError$dependOn(options = c("plotTreesVsModelError", "trainingDataManual", "scaleEqualSD",
                                            "target", "predictors", "seed", "seedBox", "noOfTrees", "bagFrac", "noOfPredictors", "numberOfPredictors"))
  jaspResults[["plotTreesVsModelError"]] <- plotTreesVsModelError

  if(!ready) return()

  classificationResult <- jaspResults[["classificationResult"]]$object

  treesMSE <- dplyr::tibble(
    trees = 1:length(classificationResult[["rfit"]]$err.rate[,1]),
    error = classificationResult[["rfit"]]$err.rate[,1]
  )
  
  p <- ggplot2::ggplot(data = treesMSE, mapping = ggplot2::aes(x = trees, y = error)) +
        ggplot2::geom_line() +
        ggplot2::scale_x_continuous(name = "Trees", labels = scales::comma) +
        ggplot2::scale_y_continuous(name = "OOB Classification Error")
  p <- JASPgraphs::themeJasp(p)
  plotTreesVsModelError$plotObject <- p
}
