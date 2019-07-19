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

MLRegressionRegularized <- function(jaspResults, dataset, options, ...) {
  
	# Preparatory work
	dataset <- .readDataRegularizedRegression(dataset, options)
	.errorHandlingRegressionAnalyses(dataset, options)
	
	# Check if analysis is ready to run
	ready <- .regressionAnalysesReady(options, type = "regularized")
  
  	# Run the analysis
	.regressionMachineLearning(dataset, options, jaspResults, ready, type = "regularized")		

	# create the results table
	.regressionMachineLearningTable(options, jaspResults, ready, type = "regularized")

  # Create the regression coefficients table
  .regressionRegularizedCoefTable(options, jaspResults, ready)

  # Create the predicted performance plot
	.regressionPredictedPerformancePlot(options, jaspResults, ready, position = 4)

  # Create the variable trace plot
  .regressionRegularizedVariableTracePlot(options, jaspResults, ready, position = 5)

  # Create the lambda evaluation plot
  .regressionRegularizedLambdaEvaluation(options, jaspResults, ready, position = 6)
}

# Read dataset
.readDataRegularizedRegression <- function(dataset, options){
  
  target                    <- NULL
  if(options[["target"]] != "")
    target                  <- options[["target"]]
  weights <- NULL
  if(options[["weights"]] != "")
    weights                 <- options[["weights"]]
  predictors                <- unlist(options['predictors'])
  predictors                <- predictors[predictors != ""]
  variables.to.read         <- c(target, predictors, weights)
  if (is.null(dataset)){
    dataset <- .readDataSetToEnd(columns.as.numeric = variables.to.read, exclude.na.listwise = variables.to.read)
  }
  if(options[["scaleEqualSD"]])
    dataset <- as.data.frame(scale(dataset))
  return(dataset)
}

.regularizedRegression <- function(dataset, options, jaspResults){

  formula <- jaspResults[["formula"]]$object
  
  dataset                 <- na.omit(dataset)
  train.index             <- sample(c(TRUE,FALSE),nrow(dataset), replace = TRUE, prob = c(options[['trainingDataManual']], 1-options[['trainingDataManual']]))
  train                   <- dataset[train.index, ]
  test                    <- dataset[!train.index, ]
  
  # Choosing the regularization method
  if(options[["penalty"]] == "ridge") {
    alpha <- 0
    penalty <- "L2 (Ridge)"
  } else if(options[["penalty"]] == "lasso") {
    alpha <- 1
    penalty <- "L1 (Lasso)"
  } else {
    alpha <- options[["alpha"]]
    penalty <- "Elastic Net"
  }

  if(options[["weights"]] != ""){
    weights <- train[, .v(options[["weights"]])]
  } else {
    weights <- rep(1, nrow(train))
  }

  train_pred <- as.matrix(train[,.v(options[["predictors"]])])
  train_target <- train[, .v(options[["target"]])]
  test_pred <- as.matrix(test[,.v(options[["predictors"]])])
  test_target <- test[, .v(options[["target"]])]
  
  # Run regularized regression
  regfit <- glmnet::cv.glmnet(x = train_pred, y = train_target, nfolds = 10, type.measure = "deviance",
                                  family = "gaussian", weights = weights, offset = NULL,
                                  alpha = alpha, standardize = FALSE,
                                  intercept = options[["intercept"]], thresh = options[["thresh"]])
  
  lambda <- base::switch(options[["shrinkage"]],
                          "manual" = options[["lambda"]],
                          "optMin" = regfit[["lambda.min"]],
                          "opt1SE" = regfit[["lambda.1se"]])

  # Derive test set predictions and calculate test error rate
  modPred <- predict(regfit, newx = test_pred, s = lambda, type = "link", exact = TRUE,
                     x = train_pred, y = train_target, weights = rep(1, nrow(train)), offset = NULL,
                     alpha = alpha, standardize = FALSE, intercept = options[["intercept"]], thresh = options[["thresh"]])
  
  regressionResult <- list()
  regressionResult[["model"]]       <- regfit
  regressionResult[["lambda"]]      <- lambda
  regressionResult[["penalty"]]     <- penalty
  regressionResult[["alpha"]]       <- alpha
  regressionResult[["ntrain"]]      <- nrow(train)
	regressionResult[["ntest"]]       <- nrow(test)
  regressionResult[["mse"]]         <- mean( (as.numeric(modPred) -  test[,.v(options[["target"]])])^2 )
  regressionResult[["coefTable"]]   <- coef(regfit, s = lambda)
  regressionResult[["x"]]           <- test[,.v(options[["target"]])]
  regressionResult[["y"]]           <- as.numeric(modPred)
  regressionResult[["cvMSE"]]       <- regfit[["cvm"]][regfit[["lambda"]] == lambda]
  regressionResult[["cvMSELambda"]] <- data.frame(lambda = regfit[["lambda"]], MSE = regfit[["cvm"]], sd = regfit[["cvsd"]])
  # results[["testR2"]]   <- round(cor(modPred, testTarget)^2, 2)
  
  return(regressionResult)
}

.regressionRegularizedCoefTable <- function(options, jaspResults, ready){

  if(!is.null(jaspResults[["coefTable"]]) || !options[["coefTable"]]) return() #The options for this table didn't change so we don't need to rebuild it

  coefTable <- createJaspTable("Regression Coefficients")
  coefTable$position <- 2
  coefTable$dependOn(options =c("coefTable","trainingDataManual", "weights", "scaleEqualSD", "modelOpt",
                                          "target", "predictors", "seed", "seedBox", "modelValid",
                                          "penalty", "alpha", "thresh", "intercept", "shrinkage", "lambda"))
  
  coefTable$addColumnInfo(name = "var",  title = "", type = "string")
  coefTable$addColumnInfo(name = "coefs",  title = "Coefficient (\u03B2)", type = "number")

  jaspResults[["coefTable"]] <- coefTable

  if(!ready && options[["target"]] == "" && length(unlist(options[["predictors"]])) > 0){
    varStrings <- options[["predictors"]]
    if(options[["intercept"]])
      varStrings <- c("(Intercept)", varStrings)
    coefTable[["var"]]   <- varStrings
  }
  
  if(!ready)  return()

  regressionResult <- jaspResults[["regressionResult"]]$object

  coefTab <- regressionResult[["coefTable"]]

  if(!options[["intercept"]]){
    labs <- .unv(rownames(coefTab))[-1]
    values <- as.numeric(coefTab)[-1]
  } else {
    labs <- c("(Intercept)", .unv(rownames(coefTab)[-1]))
    values <- as.numeric(coefTab)
  }
  
  coefTable[["var"]]   <- labs
  coefTable[["coefs"]] <- values
}

.regressionRegularizedVariableTracePlot <- function(options, jaspResults, ready, position){

  if(!is.null(jaspResults[["variableTrace"]]) || !options[["variableTrace"]]) return()

  variableTrace <- createJaspPlot(plot = NULL, title = "Variable Trace Plot", width = 500, height = 300)
  variableTrace$position <- position
  variableTrace$dependOn(options = c("variableTrace", "variableTraceLegend" ,"trainingDataManual", "weights", "scaleEqualSD", "modelOpt",
                                          "target", "predictors", "seed", "seedBox", "modelValid",
                                          "penalty", "alpha", "thresh", "intercept", "shrinkage", "lambda"))
  jaspResults[["variableTrace"]] <- variableTrace

  if(!ready) return()

  regressionResult <- jaspResults[["regressionResult"]]$object  

  model         <- regressionResult[["model"]]$glmnet.fit
  coefs         <- as.matrix(regressionResult[["model"]]$glmnet.fit$beta)
  d             <- stack(as.data.frame(coefs))
  d$ind         <- rep(.unv(rownames(coefs)), (nrow(d) / nrow(coefs)))
  d$lambda      <- rep(model$lambda, each = nrow(coefs))

  xBreaks <- JASPgraphs::getPrettyAxisBreaks(d$lambda, min.n = 4)
  yBreaks <- JASPgraphs::getPrettyAxisBreaks(d$values, min.n = 4)

  p <- ggplot2::ggplot(data = d, mapping = ggplot2::aes(x = lambda, y = values, colour = ind), show.legend = TRUE) +
        JASPgraphs::geom_line() +
        ggplot2::scale_x_continuous("\u03BB", breaks = xBreaks, labels = xBreaks) +
        ggplot2::scale_y_continuous("Coefficients", breaks = yBreaks, labels = yBreaks) + 
        ggplot2::scale_color_manual(values = colorspace::qualitative_hcl(n = length(options[["predictors"]]))) +
        ggplot2::labs(color = "Predictor")

  if(options[["variableTraceLegend"]]){
    p <- JASPgraphs::themeJasp(p, legend.position = "right")
  } else {
    p <- JASPgraphs::themeJasp(p)
  }
  
  variableTrace$plotObject <- p
}

.regressionRegularizedLambdaEvaluation <- function(options, jaspResults, ready, position){

  if(!is.null(jaspResults[["lambdaEvaluation"]]) || !options[["lambdaEvaluation"]]) return()

  lambdaEvaluation <- createJaspPlot(plot = NULL, title = "Lambda Evaluation Plot", width = 500, height = 300)
  lambdaEvaluation$position <- position
  lambdaEvaluation$dependOn(options = c("lambdaEvaluation", "lambdaEvaluationLegend" ,"trainingDataManual", "weights", "scaleEqualSD", "modelOpt",
                                          "target", "predictors", "seed", "seedBox", "modelValid",
                                          "penalty", "alpha", "thresh", "intercept", "shrinkage", "lambda"))
  jaspResults[["lambdaEvaluation"]] <- lambdaEvaluation

  if(!ready) return()

  regressionResult <- jaspResults[["regressionResult"]]$object 

  tempValues <- c(regressionResult[["cvMSELambda"]]$MSE - regressionResult[["cvMSELambda"]]$sd, regressionResult[["cvMSELambda"]]$MSE + regressionResult[["cvMSELambda"]]$sd)

  xBreaks <- JASPgraphs::getPrettyAxisBreaks(regressionResult[["cvMSELambda"]]$lambda, min.n = 4)
  yBreaks <- JASPgraphs::getPrettyAxisBreaks(tempValues, min.n = 4) 
  
  p <- ggplot2::ggplot(data = regressionResult[["cvMSELambda"]], mapping = ggplot2::aes(x = lambda, y = MSE)) +
        ggplot2::geom_ribbon(data = regressionResult[["cvMSELambda"]], mapping = ggplot2::aes(ymin = MSE - sd, ymax = MSE + sd), fill = "grey90") +
        JASPgraphs::geom_line() +
        ggplot2::scale_x_continuous("\u03BB", breaks = xBreaks, labels = xBreaks) +
        ggplot2::scale_y_continuous("CV Mean Squared Error", breaks = yBreaks, labels = yBreaks) +
        ggplot2::geom_vline(ggplot2::aes(xintercept = regressionResult[["model"]]$lambda.min, color = "lambdaMin"), linetype = "dashed") +
        ggplot2::geom_vline(ggplot2::aes(xintercept = regressionResult[["model"]]$lambda.1se, color = "lambda1se"), linetype = "dashed") +
        ggplot2::scale_color_manual(name = "", values = c(lambdaMin = "#14a1e3", lambda1se = "#99c454"), labels = c(lambdaMin = "Min. CV MSE", lambda1se = "\u03BB 1 SE"))
  
  if(options[["lambdaEvaluationLegend"]]){
    p <- JASPgraphs::themeJasp(p, legend.position = "top")
  } else {
    p <- JASPgraphs::themeJasp(p)
  }
  
  lambdaEvaluation$plotObject <- p
}
