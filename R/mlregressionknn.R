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

MLRegressionKNN <- function(jaspResults, dataset, options, state=NULL) {

	# Preparatory work
	dataset <- .readDataRegressionAnalyses(dataset, options)
	.errorHandlingRegressionAnalyses(dataset, options)
	
	# Check if analysis is ready to run
	ready <- .regressionAnalysesReady(options, type = "knn")
		
	# Run the analysis
	.regressionMachineLearning(dataset, options, jaspResults, ready, type = "knn")		

	# create the results table
	.regressionMachineLearningTable(options, jaspResults, ready, type = "knn")

	# Create the predicted performance plot
	.regressionPredictedPerformancePlot(options, jaspResults, ready, position = 3)
		
	# Create the mean squared error plot
	.regressionErrorPlot(dataset, options, jaspResults, ready, position = 4)
}

.knnRegression <- function(dataset, options, jaspResults, ready){
	
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
		startProgressbar(length(nnRange))
		for(i in nnRange){
			kfit_tmp <- kknn::kknn(formula = formula, train = train, test = test, k = i, 
				distance = options[['distanceParameterManual']], kernel = options[['weights']], scale = FALSE)
			errorStore[i] <- mean( (kfit_tmp$fitted.values -  test[,.v(options[["target"]])])^2 )
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
			errorStore <- as.numeric(optimkfit$MEAN.SQU)
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
			errorStore[i] <- mean( (kfit_tmp[[1]][,1] -  kfit_tmp[[1]][,2])^2 )
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

	regressionResult <- list()
	regressionResult[["formula"]]     <- formula
	regressionResult[["model"]]       <- kfit
	regressionResult[['mse']]         <- mean( (kfit$fitted.values -  test[,.v(options[["target"]])])^2 )
	regressionResult[["nn"]]          <- nn
	regressionResult[["weights"]]     <- weights
	regressionResult[["distance"]]    <- distance
	regressionResult[["ntrain"]]      <- nrow(train)
	regressionResult[["ntest"]]       <- nrow(test)
	regressionResult[["x"]]			  <- test[,.v(options[["target"]])]
	regressionResult[["y"]]			  <- kfit$fitted.values

	if(options[["modelOpt"]] == "optimizationError")
		regressionResult[["errorStore"]] <- errorStore

	return(regressionResult)
}
