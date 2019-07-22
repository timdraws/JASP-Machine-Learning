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

.readDataRegressionAnalyses <- function(dataset, options){
  target                    <- NULL
  if(options[["target"]] != "")
    target                  <- options[["target"]]
  predictors                <- unlist(options['predictors'])
  predictors                <- predictors[predictors != ""]
  variables.to.read         <- c(target, predictors)
  if (is.null(dataset)){
    dataset <- .readDataSetToEnd(columns.as.numeric = variables.to.read, exclude.na.listwise = variables.to.read)
  }
  if(options[["scaleEqualSD"]])
    dataset <- as.data.frame(scale(dataset))
  return(dataset)
}

.errorHandlingRegressionAnalyses <- function(dataset, options){
  predictors                <- unlist(options['predictors'])
  predictors                <- predictors[predictors != ""]
  target                    <- NULL
  if(options[["target"]] != "")
    target                  <- options[["target"]]
  variables.to.read         <- c(predictors, target)
  errors <- .hasErrors(dataset, perform, type = c('infinity', 'observations'),
                       all.target = variables.to.read,
                       observations.amount = "< 2",
                       exitAnalysisIfErrors = TRUE)
}

.regressionAnalysesReady <- function(options, type){
  if(type == "randomForest" || type == "boosting" || type == "regularized"){
    ready <- length(options[["predictors"]][options[["predictors"]] != ""]) >= 2 && options[["target"]] != ""
  } else if(type == "knn"){
    ready <- length(options[["predictors"]][options[["predictors"]] != ""]) >= 1 && options[["target"]] != ""
  }
  return(ready)
}

.regressionFormula <- function(options, jaspResults){
  predictors <- .v(options[["predictors"]])
  target <- .v(options[["target"]])
  formula <- formula(paste(target, "~", paste(predictors, collapse=" + ")))
  jaspResults[["formula"]] <- createJaspState(formula)
  jaspResults[["formula"]]$dependOn(options = c("predictors", "target"))
}

.regressionMachineLearning <- function(dataset, options, jaspResults, ready, type){

  if(!is.null(jaspResults[["regressionResult"]])) return()

  # set the seed so that every time the same set is chosen (to prevent random results) ##
  if(options[["seedBox"]]) set.seed(options[["seed"]])

  if(ready){
    .regressionFormula(options, jaspResults)
    
    if(type == "knn"){
      regressionResult <- .knnRegression(dataset, options, jaspResults)
    } else if(type == "regularized"){
      regressionResult <- .regularizedRegression(dataset, options, jaspResults)
    } else if(type == "randomForest"){
      regressionResult <- .randomForestRegression(dataset, options, jaspResults)
    } else if(type == "boosting"){
      regressionResult <- .boostingRegression(dataset, options, jaspResults)
    }
    jaspResults[["regressionResult"]] <- createJaspState(regressionResult)
    jaspResults[["regressionResult"]]$dependOn(options = c("noOfNearestNeighbours", "trainingDataManual", "distanceParameterManual", "weights", "scaleEqualSD", "modelOpt",
                                                              "target", "predictors", "seed", "seedBox", "validationLeaveOneOut", "confusionProportions", "maxK", "noOfFolds", "modelValid",
                                                              "penalty", "alpha", "thresh", "intercept", "shrinkage", "lambda", "noOfTrees", "noOfPredictors", "numberOfPredictors", "bagFrac",
                                                              "intDepth", "nNode", "distance"))
  }
}

.regressionMachineLearningTable <- function(options, jaspResults, ready, type){

  if(!is.null(jaspResults[["regressionTable"]])) return() #The options for this table didn't change so we don't need to rebuild it

  title <- base::switch(type,
                      "knn" = "K-Nearest Neighbors Regression",
                      "regularized" = "Regularized Linear Regression",
                      "randomForest" = "Random Forest Regression",
                      "boosting" = "Boosting Regression")

  regressionTable <- createJaspTable(title)
  regressionTable$position <- 1
  regressionTable$dependOn(options =c("noOfNearestNeighbours", "trainingDataManual", "distanceParameterManual", "weights", "scaleEqualSD", "modelOpt",
                                          "target", "predictors", "seed", "seedBox", "validationLeaveOneOut", "maxK", "noOfFolds", "modelValid",
                                          "penalty", "alpha", "thresh", "intercept", "shrinkage", "lambda", 
                                          "noOfTrees", "noOfPredictors", "numberOfPredictors", "bagFrac", "intDepth", "nNode", "distance"))

  if(type == "knn"){

    regressionTable$addColumnInfo(name = 'nn', title = 'Nearest neighbors', type = 'integer')
    regressionTable$addColumnInfo(name = 'weights', title = 'Weights', type = 'string')
    regressionTable$addColumnInfo(name = 'distance', title = 'Distance', type = 'string')

  } else if(type == "regularized"){

    regressionTable$addColumnInfo(name = 'penalty', title = 'Penalty', type = 'string')
    if(options[["penalty"]] == "elasticNet")
      regressionTable$addColumnInfo(name = 'alpha', title = '\u03B1', type = 'number')
    regressionTable$addColumnInfo(name = 'lambda', title = '\u03BB', type = 'number')

  } else if(type == "randomForest"){

    regressionTable$addColumnInfo(name = 'trees', title = 'No. of Trees', type = 'integer')
    regressionTable$addColumnInfo(name = 'preds', title = 'Predictors per split', type = 'integer')
  
  } else if(type == "boosting"){

    regressionTable$addColumnInfo(name = 'trees', title = 'No. of Trees', type = 'integer')
    regressionTable$addColumnInfo(name = 'depth', title = 'Interaction depth', type = 'integer')
    regressionTable$addColumnInfo(name = 'shrinkage', title = 'Shrinkage', type = 'number')
    regressionTable$addColumnInfo(name = "minObs",  title = "Min. Obs. Node", type = "integer")
    regressionTable$addColumnInfo(name = 'distribution', title = 'Loss function', type = 'integer')

  }

  regressionTable$addColumnInfo(name = 'ntrain', title = 'n(Train)', type = 'integer')
  regressionTable$addColumnInfo(name = 'ntest', title = 'n(Test)', type = 'integer')
  regressionTable$addColumnInfo(name = 'mse', title = 'Test set MSE', type = 'number', format = 'dp:3')

  if(type == "randomForest"){
    regressionTable$addColumnInfo(name = 'oob', title = 'OOB Error', type = 'number')
  }

  requiredVars <- ifelse(type == "knn", yes = 1, no = 2)
  if(!ready)
    regressionTable$addFootnote(message = paste0("Please provide a target variable and at least ", requiredVars, " predictor variable(s)."), symbol = "<i>Note.</i>")

  jaspResults[["regressionTable"]] <- regressionTable
  
  if(!ready)  return()

  regressionResult <- jaspResults[["regressionResult"]]$object
  
  if(type == "knn"){

    if(regressionResult[["nn"]] == options[["maxK"]] && options[["modelOpt"]] != "validationManual"){
      regressionTable$addFootnote(message="The optimum number of nearest neighbors is the maximum number. You might want to adjust the range op optimization.", symbol="<i>Note.</i>")
    }

    distance  <- ifelse(regressionResult[["distance"]] == 1, yes = "Manhattan", no = "Euclidian")    
    row <- data.frame(nn = regressionResult[["nn"]], weights = regressionResult[["weights"]], distance = distance, ntrain = regressionResult[["ntrain"]], ntest = regressionResult[["ntest"]], mse = regressionResult[["mse"]])
    regressionTable$addRows(row)

  } else if(type == "regularized"){

    if (regressionResult[["lambda"]] == 0)
      regressionTable$addFootnote("When \u03BB is set to 0 linear regression is performed.", symbol="<i>Note.</i>") 

    row <- data.frame(penalty = regressionResult[["penalty"]], lambda = regressionResult[["lambda"]], ntrain = regressionResult[["ntrain"]], ntest = regressionResult[["ntest"]], mse = regressionResult[["mse"]])
    if(options[["penalty"]] == "elasticNet")
      row <- cbind(row, alpha = regressionResult[["alpha"]])
    regressionTable$addRows(row)

  } else if(type == "randomForest"){

    row <- data.frame(trees = regressionResult[["noOfTrees"]], preds = regressionResult[["predPerSplit"]], ntrain = regressionResult[["ntrain"]], ntest = regressionResult[["ntest"]], mse = regressionResult[["mse"]], oob = regressionResult[["oobError"]])
    regressionTable$addRows(row)

  } else if(type == "boosting"){

    distribution <- base::switch(options[["distance"]], "tdist" = "t", "gaussian" = "Gaussian", "laplace" = "Laplace")
    row <- data.frame(trees = regressionResult[["noOfTrees"]], depth = options[["intDepth"]], shrinkage = options[["shrinkage"]], minObs = options[["nNode"]], distribution = distribution, ntrain = regressionResult[["ntrain"]], ntest = regressionResult[["ntest"]], mse = regressionResult[["mse"]])
    regressionTable$addRows(row)

  }
}

.regressionEvaluationMetrics <- function(dataset, options, jaspResults, ready){

  if(!is.null(jaspResults[["validationMeasures"]]) || !options[["validationMeasures"]]) return()
  
  validationMeasures <- createJaspTable(title = "Evaluation Metrics")
  validationMeasures$position <- 2
  validationMeasures$dependOn(options = c("validationMeasures", "noOfNearestNeighbours", "trainingDataManual", "distanceParameterManual", "weights", "scaleEqualSD", "modelOpt",
                                                              "target", "predictors", "seed", "seedBox", "validationLeaveOneOut", "confusionProportions", "maxK", "noOfFolds", "modelValid",
                                                              "penalty", "alpha", "thresh", "intercept", "shrinkage", "lambda", "noOfTrees", "noOfPredictors", "numberOfPredictors", "bagFrac",
                                                              "intDepth", "nNode", "distance"))

  validationMeasures$addColumnInfo(name = "measures", title = "Metric", type = "string")
  validationMeasures$addColumnInfo(name = "values", title = "", type = "string")

  measures <- c("MSE", "RMSE", "MAE", "MAPE", "R\u00B2")
  validationMeasures[["measures"]] <- measures
  
  jaspResults[["validationMeasures"]] <- validationMeasures

  if(!ready)  return()

  regressionResult <- jaspResults[["regressionResult"]]$object

  obs <- regressionResult[["x"]]
  pred <- regressionResult[["y"]]

  mse <- round(regressionResult[["mse"]], 3)
  rmse <- round(sqrt(mse), 3)
  mae <- round(mean(abs(obs - pred)), 3)
  mape <- paste0(round(mean( abs((obs - pred) / obs) ) * 100, 2), "%")
  r_squared <- round(cor(obs, pred)^2, 3)

  values <- c(mse, rmse, mae, mape, r_squared)

  validationMeasures[["values"]] <- values
  
}

.regressionPredictedPerformancePlot <- function(options, jaspResults, ready, position){

  if(!is.null(jaspResults[["predictedPerformancePlot"]]) || !options[["predictedPerformancePlot"]]) return()

  predictedPerformancePlot <- createJaspPlot(plot = NULL, title = "Predicted Performance Plot", width = 400, height = 300)
  predictedPerformancePlot$position <- position
  predictedPerformancePlot$dependOn(options = c("noOfNearestNeighbours", "trainingDataManual", "distanceParameterManual", "weights", "scaleEqualSD", "modelOpt",
                                                            "target", "predictors", "seed", "seedBox", "modelValid", "maxK", "noOfFolds", "modelValid", "predictedPerformancePlot",
                                                            "penalty", "alpha", "thresh", "intercept", "shrinkage", "lambda", "noOfTrees", "noOfPredictors", "numberOfPredictors", "bagFrac",
                                                            "intDepth", "nNode", "distance"))
  jaspResults[["predictedPerformancePlot"]] <- predictedPerformancePlot

  if(!ready) return()

  regressionResult <- jaspResults[["regressionResult"]]$object
  
  predPerformance <- data.frame(true = regressionResult[["x"]], predicted = regressionResult[["y"]])
  limits <- c(round(min(c(floor(predPerformance$true), floor(predPerformance$predicted)))),
              round(max(c(ceiling(predPerformance$true), ceiling(predPerformance$predicted)))))

  p <- ggplot2::ggplot(data = predPerformance, mapping = ggplot2::aes(x = true, y = predicted)) +
      ggplot2::geom_line(data = data.frame(x = limits, y = limits), mapping = ggplot2::aes(x = x, y = y), col = "darkred", size = 1) +
      JASPgraphs::geom_point() +
      ggplot2::scale_x_continuous("Observed values", limits = limits, labels = scales::comma,
                                  breaks = seq(min(limits), max(limits), length.out = 6)) +
      ggplot2::scale_y_continuous("Predicted values", limits = limits, labels = scales::comma,
                                  breaks = seq(min(limits), max(limits), length.out = 6))
  p <- JASPgraphs::themeJasp(p)

  predictedPerformancePlot$plotObject <- p
}
