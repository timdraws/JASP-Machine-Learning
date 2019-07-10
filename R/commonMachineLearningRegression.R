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
  if(!(options[["target"]] == ""))
    target                  <- options[["target"]]
  predictors                <- unlist(options['predictors'])
  predictors                <- predictors[predictors != ""]
  variables.to.read         <- c(target, predictors)
  if (is.null(dataset)){
    dataset <- .readDataSetToEnd(columns.as.numeric=variables.to.read, exclude.na.listwise=variables.to.read)
  }
  return(dataset)
}

.errorHandlingRegressionAnalyses <- function(dataset, options){
  predictors                <- unlist(options['predictors'])
  predictors                <- predictors[predictors != ""]
  target                    <- NULL
  if(!(options[["target"]] == ""))
    target                  <- options[["target"]]
  variables.to.read         <- c(predictors, target)
  errors <- .hasErrors(dataset, perform, type = c('infinity', 'observations'),
                       all.target = variables.to.read,
                       observations.amount = "< 2",
                       exitAnalysisIfErrors = TRUE)
}

.regressionAnalysesReady <- function(options){
  ready <- length(options[["predictors"]][options[["predictors"]] != ""]) >= 1 && options[["target"]] != ""
  return(ready)
}

.regressionFormula <- function(options, jaspResults){
  predictors <- .v(options[["predictors"]])
  target <- .v(options[["target"]])
  formula <- paste(target, "~", paste(predictors, collapse=" + "))
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
    } 
    jaspResults[["regressionResult"]] <- createJaspState(regressionResult)
    jaspResults[["regressionResult"]]$dependOn(options = c("noOfNearestNeighbours", "trainingDataManual", "distanceParameterManual", "weights", "scaleEqualSD", "modelOpt",
                                                              "target", "predictors", "seed", "seedBox", "validationLeaveOneOut", "confusionProportions", "maxK", "noOfFolds", "modelValid"))
  }

}

.regressionMachineLearningTable <- function(options, jaspResults, ready, type){

  if(!is.null(jaspResults[["regressionTable"]])) return() #The options for this table didn't change so we don't need to rebuild it

  title <- base::switch(type,
                      "knn" = "K-Nearest Neighbors Regression")

  regressionTable <- createJaspTable(title)
  regressionTable$position <- 1
  regressionTable$dependOn(options =c("noOfNearestNeighbours", "trainingDataManual", "distanceParameterManual", "weights", "scaleEqualSD", "modelOpt",
                                          "target", "predictors", "seed", "seedBox", "validationLeaveOneOut", "maxK", "noOfFolds", "modelValid"))

  if(type == "knn"){
    regressionTable$addColumnInfo(name = 'nn', title = 'Nearest neighbors', type = 'integer')
    regressionTable$addColumnInfo(name = 'weights', title = 'Weights', type = 'string')
    regressionTable$addColumnInfo(name = 'distance', title = 'Distance', type = 'string')
  }
  regressionTable$addColumnInfo(name = 'ntrain', title = 'n(Train)', type = 'integer')
  regressionTable$addColumnInfo(name = 'ntest', title = 'n(Test)', type = 'integer')
  regressionTable$addColumnInfo(name = 'mse', title = 'Test set MSE', type = 'number', format = 'dp:3')

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
  }
}

.regressionErrorPlot <- function(dataset, options, jaspResults, ready, position){

  if(!is.null(jaspResults[["plotErrorVsK"]]) || !options[["plotErrorVsK"]] || options[["modelOpt"]] != "optimizationError") return()

  plotErrorVsK <- createJaspPlot(plot = NULL, title = "Mean Squared Error Plot", width = 500, height = 300)
  plotErrorVsK$position <- position
  plotErrorVsK$dependOn(options = c("plotErrorVsK","noOfNearestNeighbours", "trainingDataManual", "distanceParameterManual", "weights", "scaleEqualSD", "modelOpt",
                                                            "target", "predictors", "seed", "seedBox", "modelValid", "maxK", "noOfFolds", "modelValid"))
  jaspResults[["plotErrorVsK"]] <- plotErrorVsK

  if(!ready) return()

  regressionResult <- jaspResults[["regressionResult"]]$object  
   
  xvalues <- 1:options[["maxK"]]
  yvalues <- regressionResult[["errorStore"]]      
  d <- data.frame(x = xvalues, y = yvalues)
  xBreaks <- JASPgraphs::getPrettyAxisBreaks(d$x, min.n = 4)
  yBreaks <- JASPgraphs::getPrettyAxisBreaks(d$y, min.n = 4)
    
  p <- ggplot2::ggplot(data = d, ggplot2::aes(x = x, y = y)) +
          JASPgraphs::geom_point()
  p <- p + ggplot2::scale_x_continuous(name = "Nearest neighbors", breaks = xBreaks, limits = range(xBreaks)) + 
          ggplot2::scale_y_continuous(name = "Mean squared error", breaks = yBreaks, limits = range(yBreaks))
  p <- p + JASPgraphs::geom_point(ggplot2::aes(x = x, y = y), data = data.frame(x = regressionResult[["nn"]], y = yvalues[regressionResult[["nn"]]]), fill = "red")
  p <- JASPgraphs::themeJasp(p)

  plotErrorVsK$plotObject <- p
}

.regressionPredictedPerformancePlot <- function(options, jaspResults, ready, position){

  if(!is.null(jaspResults[["predictedPerformancePlot"]]) || !options[["predictedPerformancePlot"]]) return()

  predictedPerformancePlot <- createJaspPlot(plot = NULL, title = "Predicted Performance Plot", width = 400, height = 300)
  predictedPerformancePlot$position <- position
  predictedPerformancePlot$dependOn(options = c("noOfNearestNeighbours", "trainingDataManual", "distanceParameterManual", "weights", "scaleEqualSD", "modelOpt",
                                                            "target", "predictors", "seed", "seedBox", "modelValid", "maxK", "noOfFolds", "modelValid", "predictedPerformancePlot"))
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
