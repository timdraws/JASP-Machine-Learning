#
# Copyright (C) 2017 University of Amsterdam
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

MLClassificationKNN <- function(jaspResults, dataset, options, ...) {
  
    # Preparatory work
    dataset <- .readDataClassificationAnalyses(dataset, options)
    .errorHandlingClassificationAnalyses(dataset, options)
    
    # Check if analysis is ready to run
    ready <- .classificationAnalysesReady(options, type = "knn")
    
    # Run the analysis
    .classification(dataset, options, jaspResults, ready, type = "knn")
    
    # create the results table
    .classificationTable(options, jaspResults, ready, type = "knn")
    
    # Create the confusion table
    .classificationConfusionTable(dataset, options, jaspResults, ready)

    # Create the validation measures table
    .classificationEvaluationMetrics(dataset, options, jaspResults, ready)
    
    # Create the classification error plot
    .knnErrorPlot(dataset, options, jaspResults, ready, position = 4, purpose = "classification")

    # Create the ROC curve
    .rocCurve(dataset, options, jaspResults, ready, position = 5, type = "knn")

    # Decision boundaries
    .classificationDecisionBoundaries(dataset, options, jaspResults, ready, position = 6, type = "knn")
}

.knnClassification <- function(dataset, options, jaspResults){

  formula <- jaspResults[["formula"]]$object

  if(options[["modelValid"]] == "validationManual"){

    dataset                 <- na.omit(dataset)
    train.index             <- sample(c(TRUE,FALSE),nrow(dataset), replace = TRUE, prob = c(options[['trainingDataManual']], 1-options[['trainingDataManual']]))
    train                   <- dataset[train.index, ]
    test                    <- dataset[!train.index, ]

    if(options[["modelOpt"]] == "optimizationManual"){
        kfit <- kknn::kknn(formula = formula, train = train, test = test, k = options[['noOfNearestNeighbours']], 
                    distance = options[['distanceParameterManual']], kernel = options[['weights']], scale = FALSE)
        nn <- options[['noOfNearestNeighbours']]
    } else { 
      nnRange <- 1:options[["maxK"]]
      errorStore <- numeric(length(nnRange))
      trainErrorStore <- numeric(length(nnRange))
      startProgressbar(length(nnRange))
      for(i in nnRange){
          kfit_tmp <- kknn::kknn(formula = formula, train = train, test = test, k = i, 
              distance = options[['distanceParameterManual']], kernel = options[['weights']], scale = FALSE)
          errorStore[i] <- 1 - sum(diag(prop.table(table(kfit_tmp$fitted.values, test[,.v(options[["target"]])]))))
          kfit_tmp2 <- kknn::kknn(formula = formula, train = train, test = train, k = i, 
				      distance = options[['distanceParameterManual']], kernel = options[['weights']], scale = FALSE)
			    trainErrorStore[i] <- 1 - sum(diag(prop.table(table(kfit_tmp2$fitted.values, train[,.v(options[["target"]])]))))
          progressbarTick()
      }
      nn <- base::switch(options[["modelOpt"]],
                          "optimizationError" = nnRange[which.min(errorStore)])
      kfit <- kknn::kknn(formula = formula, train = train, test = test, k = nn, 
                distance = options[['distanceParameterManual']], kernel = options[['weights']], scale = FALSE)
    }
    weights <- options[["weights"]]
    distance <- options[["distanceParameterManual"]]

  } else if(options[["modelValid"]] == "validationLeaveOneOut"){

    if(options[["modelOpt"]] == "optimizationManual"){
      optimkfit <- kknn::train.kknn(formula = formula, data = dataset, ks = options[['noOfNearestNeighbours']], scale = FALSE, distance = options[['distanceParameterManual']], kernel = options[['weights']])
      nn <- options[['noOfNearestNeighbours']]
    } else {
      optimkfit <- kknn::train.kknn(formula = formula, data = dataset, ks = 1:options[["maxK"]], scale = FALSE, distance = options[['distanceParameterManual']], kernel = options[['weights']])  
      errorStore <- as.numeric(optimkfit$MISCLASS)
      nn <- base::switch(options[["modelOpt"]],
                    "optimizationError" = optimkfit$best.parameters$k)
    }

    weights <- options[["weights"]]
    distance <- options[["distanceParameterManual"]]

		kfit <- list(fitted.values = as.numeric(optimkfit[["fitted.values"]][[1]]))
		train <- dataset
		test <- dataset

  } else if(options[["modelValid"]] == "validationKFold"){

    if(options[["modelOpt"]] == "optimizationManual"){
      optimkfit <- kknn::cv.kknn(formula = formula, data = dataset, distance = options[['distanceParameterManual']], kernel = options[['weights']],
                            kcv = options[['noOfFolds']], k = options[['noOfNearestNeighbours']])
      nn <- options[['noOfNearestNeighbours']]
    } else {

      nnRange <- 1:options[["maxK"]]
      errorStore <- numeric(length(nnRange))
      startProgressbar(length(nnRange))
      for(i in nnRange){
          kfit_tmp <- kknn::cv.kknn(formula = formula, data = dataset, distance = options[['distanceParameterManual']], kernel = options[['weights']],
                            kcv = options[['noOfFolds']], k = i)
          errorStore[i] <- 1 - sum(diag(prop.table(table(kfit_tmp[[1]][,1], kfit_tmp[[1]][,2]))))
          progressbarTick()
      }
      nn <- base::switch(options[["modelOpt"]],
                        "optimizationError" = nnRange[which.min(errorStore)])

      optimkfit <- kknn::cv.kknn(formula = formula, data = dataset, distance = options[['distanceParameterManual']], kernel = options[['weights']],
                            kcv = options[['noOfFolds']], k = nn)
    }
      
    kfit <- list(fitted.values = as.numeric(optimkfit[[1]][, 2]))

    weights <- options[["weights"]]
    distance <- options[["distanceParameterManual"]]
    train <- dataset
    test <- dataset

  }

  classificationResult <- list()
  classificationResult[["formula"]]     <- formula
  classificationResult[["model"]]       <- kfit
  classificationResult[['confTable']]   <- table('Pred' = kfit$fitted.values, 'Real' = test[,.v(options[["target"]])])
  classificationResult[['mse']]         <- 1 - sum(diag(prop.table(classificationResult[['confTable']])))
  classificationResult[["nn"]]          <- nn
  classificationResult[["weights"]]     <- weights
  classificationResult[["distance"]]    <- distance
  classificationResult[["ntrain"]]      <- nrow(train)
  classificationResult[["ntest"]]       <- nrow(test)
  classificationResult[["x"]]           <- test[,.v(options[["target"]])]
  classificationResult[["y"]]           <- kfit$fitted.values

  if(options[["modelOpt"]] == "optimizationError")
    classificationResult[["errorStore"]] <- errorStore
  if(options[["modelOpt"]] == "optimizationError" && options[["modelValid"]] == "validationManual")
		classificationResult[["trainErrorStore"]] <- trainErrorStore

  return(classificationResult)
}