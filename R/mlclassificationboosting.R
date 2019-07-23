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

MLClassificationBoosting <- function(jaspResults, dataset, options, ...) {
  
  # Preparatory work
  dataset <- .readDataClassificationAnalyses(dataset, options)
  .errorHandlingClassificationAnalyses(dataset, options)
  
  # Check if analysis is ready to run
  ready <- .classificationAnalysesReady(options, type = "boosting")

  # Compute results and create the model summary table
  .classificationTable(dataset, options, jaspResults, ready, type = "boosting")

  # Create the confusion table
  .classificationConfusionTable(dataset, options, jaspResults, ready)

  # Create the evaluation metrics table
  .classificationEvaluationMetrics(dataset, options, jaspResults, ready)

  # Create the relative influence table
  .boostingRelativeInfluenceTable(options, jaspResults, ready, purpose = "classification")

  # Create the OOB improvement plot
  .boostingOOBimprovementPlot(options, jaspResults, ready, position = 5, purpose = "classification")

  # Create the ROC curve
  .rocCurve(dataset, options, jaspResults, ready, position = 6, type = "boosting")

  # Create the Andrews curves
  .classificationAndrewsCurves(dataset, options, jaspResults, ready, position = 7)

  # Create the deviance plot
  .boostingDeviancePlot(options, jaspResults, ready, position = 8, purpose = "classification")

  # Create the relative influence plot
  .boostingRelativeInfluencePlot(options, jaspResults, ready, position = 9, purpose = "classification")

  # Decision boundaries
  .classificationDecisionBoundaries(dataset, options, jaspResults, ready, position = 10, type = "boosting")
}

.boostingClassification <- function(dataset, options, jaspResults) {
  
  dataset                 <- na.omit(dataset)
  train.index             <- sample(c(TRUE,FALSE),nrow(dataset), replace = TRUE, prob = c(options[['trainingDataManual']], 1-options[['trainingDataManual']]))
  train                   <- dataset[train.index, ]
  test                    <- dataset[!train.index, ]

  formula <- jaspResults[["formula"]]$object

  if(options[["modelValid"]] == "validationManual"){
    noOfFolds <- 0
  } else if(options[["modelValid"]] == "validationKFold"){
    noOfFolds <- options[["noOfFolds"]]
  }

  bfit <- gbm::gbm(formula = formula, data = train, n.trees = options[["noOfTrees"]],
                          shrinkage = options[["shrinkage"]], interaction.depth = options[["intDepth"]],
                          cv.folds = noOfFolds, bag.fraction = options[["bagFrac"]], n.minobsinnode = options[["nNode"]],
                          distribution = "multinomial", n.cores=1) #multiple cores breaks modules in JASP, see: INTERNAL-jasp#372

  if(options[["modelOpt"]] == "optimizationManual"){
    
    noOfTrees <- options[["noOfTrees"]]

  } else if(options[["modelOpt"]] == "optimizationError"){

    noOfTrees <- gbm::gbm.perf(bfit, plot.it = FALSE, method = "OOB")[1]
    bfit <- gbm::gbm(formula = formula, data = train, n.trees = noOfTrees,
                        shrinkage = options[["shrinkage"]], interaction.depth = options[["intDepth"]],
                        cv.folds = noOfFolds, bag.fraction = options[["bagFrac"]], n.minobsinnode = options[["nNode"]],
                        distribution = "multinomial", n.cores=1) #multiple cores breaks modules in JASP, see: INTERNAL-jasp#372

  }

  probabilities <- gbm::predict.gbm(bfit, newdata = test, n.trees = noOfTrees, type = "response")
  fitted.values <- colnames(probabilities)[apply(probabilities, 1, which.max)]

  classificationResult <- list()
  classificationResult[["model"]]       <- bfit
  classificationResult[["formula"]]     <- formula
  classificationResult[['confTable']]   <- table('Pred' = fitted.values, 'Real' = test[,.v(options[["target"]])])
  classificationResult[['mse']]         <- 1 - sum(diag(prop.table(classificationResult[['confTable']])))
  classificationResult[["relInf"]]      <- summary(bfit, plot = FALSE)
  classificationResult[["noOfFolds"]]   <- noOfFolds
  classificationResult[["noOfTrees"]]   <- noOfTrees
  classificationResult[["ntrain"]]      <- nrow(train)
  classificationResult[["ntest"]]       <- nrow(test)
  classificationResult[["y"]]           <- fitted.values
  classificationResult[["x"]]           <- test[,.v(options[["target"]])]
  classificationResult[["train"]]       <- train
  classificationResult[["test"]]        <- test
  classificationResult[["method"]]      <- ifelse(options[["modelValid"]] == "validationManual", yes = "OOB", no = "")

  return(classificationResult)
}
