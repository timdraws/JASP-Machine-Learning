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

.readDataClassificationAnalyses <- function(dataset, options){
  target                    <- NULL
  if(!(options[["target"]] == ""))
    target                  <- options[["target"]]
  predictors                <- unlist(options['predictors'])
  predictors                <- predictors[predictors != ""]
  variables.to.read         <- c(target, predictors)
  if (is.null(dataset)) {
          dataset <- .readDataSetToEnd(columns.as.numeric=variables.to.read, exclude.na.listwise=variables.to.read)
  }
  if(options[["target"]] != "")
    dataset[, .v(options[["target"]])] <- factor(dataset[, .v(options[["target"]])])
  return(dataset)
}

.errorHandlingClassificationAnalyses <- function(dataset, options){
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

.classificationAnalysesReady <- function(options){
  ready <- length(options[["predictors"]][options[["predictors"]] != ""]) >= 1 && options[["target"]] != ""
  return(ready)
}

.classificationFormula <- function(options, jaspResults){
  predictors <- .v(options[["predictors"]])
  target <- .v(options[["target"]])
  formula <- paste(target, "~", paste(predictors, collapse=" + "))
  jaspResults[["formula"]] <- createJaspState(formula)
  jaspResults[["formula"]]$dependOn(options = c("predictors", "target"))
}

.classification <- function(dataset, options, jaspResults, ready, type){

  if(!is.null(jaspResults[["classificationResult"]])) return()

  # set the seed so that every time the same set is chosen (to prevent random results) ##
  if(options[["seedBox"]]) set.seed(options[["seed"]])

  if(ready){
    .classificationFormula(options, jaspResults)
    
    if(type == "knn"){
      classificationResult <- .knnClassification(dataset, options, jaspResults)
    } 
    jaspResults[["classificationResult"]] <- createJaspState(classificationResult)
    jaspResults[["classificationResult"]]$dependOn(options = c("noOfNearestNeighbours", "trainingDataManual", "distanceParameterManual", "weights", "scaleEqualSD", "modelOpt",
                                                              "target", "predictors", "seed", "seedBox", "validationLeaveOneOut", "confusionProportions", "maxK", "noOfFolds", "modelValid"))
  }

}

.classificationTable <- function(options, jaspResults, ready, type){

  if(!is.null(jaspResults[["classificationTable"]])) return() #The options for this table didn't change so we don't need to rebuild it

  title <- base::switch(type,
                      "knn" = "K-Nearest Neighbors Classification")

  classificationTable <- createJaspTable(title)
  classificationTable$position <- 1
  classificationTable$dependOn(options =c("noOfNearestNeighbours", "trainingDataManual", "distanceParameterManual", "weights", "scaleEqualSD", "modelOpt",
                                          "target", "predictors", "seed", "seedBox", "validationLeaveOneOut", "maxK", "noOfFolds", "modelValid"))

  if(type == "knn"){
    classificationTable$addColumnInfo(name = 'nn', title = 'Nearest neighbors', type = 'integer')
    classificationTable$addColumnInfo(name = 'weights', title = 'Weights', type = 'string')
    classificationTable$addColumnInfo(name = 'distance', title = 'Distance', type = 'string')
  }
  classificationTable$addColumnInfo(name = 'ntrain', title = 'n(Train)', type = 'integer')
  classificationTable$addColumnInfo(name = 'ntest', title = 'n(Test)', type = 'integer')
  classificationTable$addColumnInfo(name = 'mse', title = 'Test Set Error', type = 'number', format = 'dp:3')

  jaspResults[["classificationTable"]] <- classificationTable
  
  if(!ready)  return()

  classificationResult <- jaspResults[["classificationResult"]]$object
  
  if(type == "knn"){

    if(classificationResult[["nn"]] == options[["maxK"]] && options[["modelOpt"]] != "validationManual"){
      classificationTable$addFootnote(message="The optimum number of nearest neighbors is the maximum number. You might want to adjust the range op optimization.", symbol="<i>Note.</i>")
    }

    distance  <- ifelse(classificationResult[["distance"]] == 1, yes = "Manhattan", no = "Euclidian")    
    row <- data.frame(nn = classificationResult[["nn"]], weights = classificationResult[["weights"]], distance = distance, ntrain = classificationResult[["ntrain"]], ntest = classificationResult[["ntest"]], mse = classificationResult[["mse"]])
    classificationTable$addRows(row)

  }
}

.classificationConfusionTable <- function(dataset, options, jaspResults, ready){

  if (!is.null(jaspResults[["confusionTable"]]) || !options[["confusionTable"]]) return()
  
  confusionTable <- createJaspTable(title = "Confusion Matrix")
  confusionTable$position <- 2
  confusionTable$dependOn(options = c("noOfNearestNeighbours", "trainingDataManual", "distanceParameterManual", "weights", "scaleEqualSD", "modelOpt",
                                          "target", "predictors", "seed", "seedBox", "confusionTable", "confusionProportions", "maxK", "noOfFolds", "modelValid"))
  
  jaspResults[["confusionTable"]] <- confusionTable
  
  if(ready) {

    classificationResult <- jaspResults[["classificationResult"]]$object
    
    confusionTable$addColumnInfo(name = "pred_name", title = "", type = "string")
    confusionTable$addColumnInfo(name = "varname_pred", title = "", type = "string")

    confTable <- classificationResult[["confTable"]]
    if(options[["confusionProportions"]])
      confTable <- round(confTable / classificationResult[["ntest"]], 2)
    
    confusionTable[["pred_name"]] <- c("Predicted", rep("", nrow(confTable)-1))
    confusionTable[["varname_pred"]] <- colnames(confTable)
    
    for(i in 1:length(rownames(confTable))){
      name <- paste("varname_obs", i, sep = "")
      confusionTable$addColumnInfo(name = name, title = rownames(confTable)[i], type = "integer", overtitle = "Observed")
      confusionTable[[name]] <- confTable[, i]  
    }
    
  } else if(options[["target"]] != "" && !ready) {
    
    confusionTable$addColumnInfo(name = "pred_name", title = "", type = "string")
    confusionTable$addColumnInfo(name = "varname_pred", title = "", type = "string")

    factorLevels <- levels(dataset[, .v(options[["target"]])])
    
    confusionTable[["pred_name"]] <- c("Predicted", rep("", length(factorLevels) - 1))
    confusionTable[["varname_pred"]] <- factorLevels
    
    for(i in 1:length(factorLevels)){ 
      name <- paste("varname_obs", i, sep = "")
      confusionTable$addColumnInfo(name = name, title = factorLevels[i], type = "integer", overtitle = "Observed")
      confusionTable[[name]] <- rep(".", length(factorLevels)) 
    }
    
  } else {
    
    confusionTable$addColumnInfo(name = "pred_name"    , title = "" , type = "string")
    confusionTable$addColumnInfo(name = "varname_pred" , title = "" , type = "string")
    confusionTable$addColumnInfo(name = "varname_obs1", title = ".", type = "integer")
    confusionTable$addColumnInfo(name = "varname_obs2", title = ".", type = 'integer')
    
    confusionTable[["pred_name"]] <- c("Predicted", "")
    confusionTable[["varname_pred"]] <- rep(".", 2)
    confusionTable[["varname_obs1"]] <- rep("", 2)
    confusionTable[["varname_obs2"]] <- rep("", 2)
    
  }
  
}

.classificationErrorPlot <- function(dataset, options, jaspResults, ready, position){

  if(!is.null(jaspResults[["plotErrorVsK"]]) || !options[["plotErrorVsK"]] || options[["modelOpt"]] != "optimizationError") return()

  plotErrorVsK <- createJaspPlot(plot = NULL, title = "Classification Error Plot", width = 500, height = 300)
  plotErrorVsK$position <- position
  plotErrorVsK$dependOn(options = c("plotErrorVsK","noOfNearestNeighbours", "trainingDataManual", "distanceParameterManual", "weights", "scaleEqualSD", "modelOpt",
                                                            "target", "predictors", "seed", "seedBox", "modelValid", "maxK", "noOfFolds", "modelValid"))
  jaspResults[["plotErrorVsK"]] <- plotErrorVsK

  if(!ready) return()

  classificationResult <- jaspResults[["classificationResult"]]$object  
   
  xvalues <- 1:options[["maxK"]]
  yvalues <- classificationResult[["errorStore"]]      
  d <- data.frame(x = xvalues, y = yvalues)
  xBreaks <- JASPgraphs::getPrettyAxisBreaks(d$x, min.n = 4)
  yBreaks <- JASPgraphs::getPrettyAxisBreaks(d$y, min.n = 4)
    
  p <- ggplot2::ggplot(data = d, ggplot2::aes(x = x, y = y)) +
          JASPgraphs::geom_point()
  p <- p + ggplot2::scale_x_continuous(name = "Nearest neighbors", breaks = xBreaks, limits = range(xBreaks)) + 
          ggplot2::scale_y_continuous(name = "Classification error", breaks = yBreaks, limits = range(yBreaks))
  p <- p + JASPgraphs::geom_point(ggplot2::aes(x = x, y = y), data = data.frame(x = classificationResult[["nn"]], y = yvalues[classificationResult[["nn"]]]), fill = "red")
  p <- JASPgraphs::themeJasp(p)

  plotErrorVsK$plotObject <- p
}
