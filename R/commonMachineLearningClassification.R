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
  if(options[["target"]] != "")
    target                  <- options[["target"]]
  predictors                <- unlist(options['predictors'])
  predictors                <- predictors[predictors != ""]
  variables.to.read         <- c(target, predictors)
  if (is.null(dataset)){
    dataset <- .readDataSetToEnd(columns.as.numeric = variables.to.read, exclude.na.listwise = variables.to.read)
  }
  if(length(unlist(options[["predictors"]])) > 0 && options[["scaleEqualSD"]])
    dataset[,.v(options[["predictors"]])] <- scale(dataset[,.v(options[["predictors"]])])
  if(options[["target"]] != "")
    dataset[, .v(options[["target"]])] <- factor(dataset[, .v(options[["target"]])])
  return(dataset)
}

.errorHandlingClassificationAnalyses <- function(dataset, options){
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

.classificationAnalysesReady <- function(options, type){
  if(type == "lda" || type == "randomForest" || type == "boosting"){
    ready <- length(options[["predictors"]][options[["predictors"]] != ""]) >= 2 && options[["target"]] != ""
  } else if(type == "knn"){
    ready <- length(options[["predictors"]][options[["predictors"]] != ""]) >= 1 && options[["target"]] != ""
  }
  return(ready)
}

.classificationFormula <- function(options, jaspResults){
  predictors <- .v(options[["predictors"]])
  target <- .v(options[["target"]])
  formula <- formula(paste(target, "~", paste(predictors, collapse=" + ")))
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
    } else if(type == "lda"){
      classificationResult <- .ldaClassification(dataset, options, jaspResults)
    } else if(type == "randomForest"){
      classificationResult <- .randomForestClassification(dataset, options, jaspResults)
    } else if(type == "boosting"){
      classificationResult <- .boostingClassification(dataset, options, jaspResults)
    }
    jaspResults[["classificationResult"]] <- createJaspState(classificationResult)
    jaspResults[["classificationResult"]]$dependOn(options = c("noOfNearestNeighbours", "trainingDataManual", "distanceParameterManual", "weights", "scaleEqualSD", "modelOpt",
                                                              "target", "predictors", "seed", "seedBox", "validationLeaveOneOut", "confusionProportions", "maxK", "noOfFolds", "modelValid",
                                                              "estimationMethod", "noOfTrees", "bagFrac", "noOfPredictors", "numberOfPredictors", "shrinkage", "intDepth", "nNode"))
  }
}

.classificationTable <- function(dataset, options, jaspResults, ready, type){

  if(!is.null(jaspResults[["classificationTable"]])) return() #The options for this table didn't change so we don't need to rebuild it

  title <- base::switch(type,
                      "knn" = "K-Nearest Neighbors Classification",
                      "lda" = "Linear Discriminant Classification",
                      "randomForest" = "Random Forest Classification",
                      "boosting" = "Boosting Classification")

  classificationTable <- createJaspTable(title)
  classificationTable$position <- 1
  classificationTable$dependOn(options =c("noOfNearestNeighbours", "trainingDataManual", "distanceParameterManual", "weights", "scaleEqualSD", "modelOpt",
                                          "target", "predictors", "seed", "seedBox", "validationLeaveOneOut", "maxK", "noOfFolds", "modelValid",
                                          "estimationMethod", "noOfTrees", "bagFrac", "noOfPredictors", "numberOfPredictors", "shrinkage", "intDepth", "nNode"))

  if(type == "knn"){

    classificationTable$addColumnInfo(name = 'nn', title = 'Nearest neighbors', type = 'integer')
    classificationTable$addColumnInfo(name = 'weights', title = 'Weights', type = 'string')
    classificationTable$addColumnInfo(name = 'distance', title = 'Distance', type = 'string')
  
  } else if(type =="lda"){

    classificationTable$addColumnInfo(name = 'lda', title = 'Linear Discriminants', type = 'integer')
    classificationTable$addColumnInfo(name = 'method', title = 'Method', type = 'string')
  
  } else if(type == "randomForest"){
    classificationTable$addColumnInfo(name = 'trees', title = 'No. of Trees', type = 'integer')
    classificationTable$addColumnInfo(name = 'preds', title = 'Predictors per split', type = 'integer')
  
  } else if(type == "boosting"){

    classificationTable$addColumnInfo(name = 'trees', title = 'No. of Trees', type = 'integer')
    classificationTable$addColumnInfo(name = 'depth', title = 'Interaction depth', type = 'integer')
    classificationTable$addColumnInfo(name = 'shrinkage', title = 'Shrinkage', type = 'number')
    classificationTable$addColumnInfo(name = "minObs",  title = "Min. Obs. Node", type = "integer")
  
  }
  
  classificationTable$addColumnInfo(name = 'ntrain', title = 'n(Train)', type = 'integer')
  classificationTable$addColumnInfo(name = 'ntest', title = 'n(Test)', type = 'integer')
  classificationTable$addColumnInfo(name = 'mse', title = 'Test Set Error', type = 'number')
  
  if(type == "randomForest"){
    classificationTable$addColumnInfo(name = 'oob', title = 'OOB Error', type = 'number')
  }

  requiredVars <- ifelse(type == "knn", yes = 1, no = 2)
  if(!ready)
    classificationTable$addFootnote(message = paste0("Please provide a target variable and at least ", requiredVars, " predictor variable(s)."), symbol = "<i>Note.</i>")

  jaspResults[["classificationTable"]] <- classificationTable
  
  if(!ready)  return()

  # Run the analysis
  .classification(dataset, options, jaspResults, ready, type = type)

  classificationResult <- jaspResults[["classificationResult"]]$object
  
  if(type == "knn"){

    if(classificationResult[["nn"]] == options[["maxK"]] && options[["modelOpt"]] != "validationManual"){
      classificationTable$addFootnote(message="The optimum number of nearest neighbors is the maximum number. You might want to adjust the range op optimization.", symbol="<i>Note.</i>")
    }

    distance  <- ifelse(classificationResult[["distance"]] == 1, yes = "Manhattan", no = "Euclidian")    
    row <- data.frame(nn = classificationResult[["nn"]], weights = classificationResult[["weights"]], distance = distance, ntrain = classificationResult[["ntrain"]], ntest = classificationResult[["ntest"]], mse = classificationResult[["mse"]])
    classificationTable$addRows(row)

  } else if(type =="lda"){

    method <- base::switch(options[["estimationMethod"]], "moment" = "Moment", "mle" = "MLE", "covMve" = "MVE","t" = "t")
    row <- data.frame(lda = ncol(classificationResult[["scaling"]]), method = method, ntrain = classificationResult[["ntrain"]], ntest = classificationResult[["ntest"]], mse = classificationResult[["mse"]])
    classificationTable$addRows(row)

  } else if(type == "randomForest"){

    row <- data.frame(trees = classificationResult[["noOfTrees"]], preds = classificationResult[["predPerSplit"]], ntrain = classificationResult[["ntrain"]], ntest = classificationResult[["ntest"]], mse = classificationResult[["mse"]], oob = classificationResult[["oobError"]])
    classificationTable$addRows(row)

  } else if(type == "boosting"){
    row <- data.frame(trees = classificationResult[["noOfTrees"]], depth = options[["intDepth"]], shrinkage = options[["shrinkage"]], minObs = options[["nNode"]], ntrain = classificationResult[["ntrain"]], ntest = classificationResult[["ntest"]], mse = classificationResult[["mse"]])
    classificationTable$addRows(row)

  }
}

.classificationConfusionTable <- function(dataset, options, jaspResults, ready){

  if (!is.null(jaspResults[["confusionTable"]]) || !options[["confusionTable"]]) return()
  
  confusionTable <- createJaspTable(title = "Confusion Matrix")
  confusionTable$position <- 2
  confusionTable$dependOn(options = c("noOfNearestNeighbours", "trainingDataManual", "distanceParameterManual", "weights", "scaleEqualSD", "modelOpt",
                                          "target", "predictors", "seed", "seedBox", "confusionTable", "confusionProportions", "maxK", "noOfFolds", "modelValid", 
                                          "estimationMethod", "noOfTrees", "bagFrac", "noOfPredictors", "numberOfPredictors", "shrinkage", "intDepth", "nNode"))
  
  jaspResults[["confusionTable"]] <- confusionTable
  
  if(ready) {

    classificationResult <- jaspResults[["classificationResult"]]$object
    
    confusionTable$addColumnInfo(name = "obs_name", title = "", type = "string")
    confusionTable$addColumnInfo(name = "varname_obs", title = "", type = "string")

    confTable <- classificationResult[["confTable"]]
    if(options[["confusionProportions"]])
      confTable <- round(confTable / classificationResult[["ntest"]], 2)
    
    confusionTable[["obs_name"]] <- c("Observed", rep("", nrow(confTable)-1))
    confusionTable[["varname_obs"]] <- colnames(confTable)
    
    for(i in 1:length(rownames(confTable))){
      name <- paste("varname_pred", i, sep = "")
      confusionTable$addColumnInfo(name = name, title = rownames(confTable)[i], type = "integer", overtitle = "Predicted")
      confusionTable[[name]] <- confTable[i, ]  
    }
    
  } else if(options[["target"]] != "" && !ready) {
    
    confusionTable$addColumnInfo(name = "obs_name", title = "", type = "string")
    confusionTable$addColumnInfo(name = "varname_obs", title = "", type = "string")

    factorLevels <- levels(dataset[, .v(options[["target"]])])
    
    confusionTable[["obs_name"]] <- c("Observed", rep("", length(factorLevels) - 1))
    confusionTable[["varname_obs"]] <- factorLevels
    
    for(i in 1:length(factorLevels)){ 
      name <- paste("varname_pred", i, sep = "")
      confusionTable$addColumnInfo(name = name, title = factorLevels[i], type = "integer", overtitle = "Predicted")
      confusionTable[[name]] <- rep(".", length(factorLevels)) 
    }
    
  } else {
    
    confusionTable$addColumnInfo(name = "obs_name"    , title = "" , type = "string")
    confusionTable$addColumnInfo(name = "varname_obs" , title = "" , type = "string")
    confusionTable$addColumnInfo(name = "varname_pred1", title = ".", type = "integer")
    confusionTable$addColumnInfo(name = "varname_pred2", title = ".", type = 'integer')
    
    confusionTable[["obs_name"]] <- c("Observed", "")
    confusionTable[["varname_obs"]] <- rep(".", 2)
    confusionTable[["varname_pred1"]] <- rep("", 2)
    confusionTable[["varname_pred2"]] <- rep("", 2)
    
  }
  
}

.classificationDecisionBoundaries <- function(dataset, options, jaspResults, ready, position, type){

  if (!is.null(jaspResults[["decisionBoundary"]]) || !options[["decisionBoundary"]]) return()
  
  decisionBoundary <- createJaspPlot(title = "Decision Boundary Matrix", height = 400, width = 300)
  decisionBoundary$position <- position
  decisionBoundary$dependOn(options = c("decisionBoundary", "plotDensities", "plotStatistics", "trainingDataManual", "scaleEqualSD", "modelOpt",
                                          "target", "predictors", "seed", "seedBox", "modelValid", "estimationMethod", 
                                          "maxK", "noOfFolds", "modelValid", "noOfNearestNeighbors", "distanceParameterManual", "weights",
                                          "plotLegend", "plotPoints", "noOfTrees", "bagFrac", "noOfPredictors", "numberOfPredictors", 
                                          "shrinkage", "intDepth", "nNode"))
  jaspResults[["decisionBoundary"]] <- decisionBoundary 

  if(!ready || length(options[["predictors"]]) < 2)  return()
  
  .classificationFillDecisionBoundary(dataset, options, jaspResults, decisionBoundary, type)
}

.classificationFillDecisionBoundary <- function(dataset, options, jaspResults, decisionBoundary, type){

  classificationResult <- jaspResults[["classificationResult"]]$object

  variables <- options[["predictors"]]
  l <- length(variables)

  if (l <= 2) {
    width <- 580
    height <- 580
  } else {
    width <- 250 * l
    height <- 250 * l
  }

  decisionBoundary[["width"]]  <- width
  decisionBoundary[["height"]] <- height
  
  cexText <- 1.6
  
  plotMat <- matrix(list(), l - 1, l - 1)
  adjMargin <- ggplot2::theme(plot.margin = ggplot2::unit(c(.25, .40, .25, .25), "cm")) 
  oldFontSize <- JASPgraphs::getGraphOption("fontsize")
  JASPgraphs::setGraphOption("fontsize", .85 * oldFontSize)
  
  target <- dataset[, .v(options[["target"]])]
  startProgressbar(length(plotMat)+1)

  for (row in 2:l) {
    for (col in 1:(l-1)) {
      if (row == col) {     
          p <- JASPgraphs::drawAxis(xName = "", yName = "", force = TRUE) + adjMargin
          p <- p + ggplot2::xlab("")
          p <- p + ggplot2::ylab("")
          p <- JASPgraphs::themeJasp(p)
          
          plotMat[[row, col]] <- p
      }  
      if (col < row) {
          predictors <- dataset[, .v(options[["predictors"]])]
          predictors <- predictors[, c(col, row)]
          formula <- formula(paste(.v(options[["target"]]), "~", paste(colnames(predictors), collapse=" + ")))
          plotMat[[row-1, col]] <- .decisionBoundaryPlot(dataset, options, jaspResults, predictors, target, formula, l, type = type)
      } 
      if (col > row) {
          p <- JASPgraphs::drawAxis(xName = "", yName = "", force = TRUE) + adjMargin
          p <- p + ggplot2::xlab("")
          p <- p + ggplot2::ylab("")
          p <- JASPgraphs::themeJasp(p)
          
          plotMat[[row, col]] <- p
      }
      if(l > 2)
      if(options[["plotLegend"]])
        plotMat[[1, 2]] <- .legendPlot(dataset, options, col)
      progressbarTick()
    }
  }
  
  JASPgraphs::setGraphOption("fontsize", oldFontSize)
  # slightly adjust the positions of the labels left and above the plots.
  labelPos <- matrix(.5, 4, 2)
  labelPos[1, 1] <- .55
  labelPos[4, 2] <- .65
  p <- JASPgraphs::ggMatrixPlot(plotList = plotMat, leftLabels = variables[-1], topLabels = variables[-length(variables)],
                                scaleXYlabels = NULL, labelPos = labelPos)
  
  progressbarTick()
  decisionBoundary$plotObject <- p
}

.decisionBoundaryPlot <- function(dataset, options, jaspResults, predictors, target, formula, l, type){ 

    x_min <- min(predictors[,1])-0.5; x_max <- max(predictors[,1])+0.5
    y_min <- min(predictors[,2])-0.5; y_max <- max(predictors[,2])+0.5

    xBreaks <- JASPgraphs::getPrettyAxisBreaks(predictors[, 1], min.n = 4)
    yBreaks <- JASPgraphs::getPrettyAxisBreaks(predictors[, 2], min.n = 4)

    x_min <- xBreaks[1]; x_max <- xBreaks[length(xBreaks)]
    y_min <- yBreaks[1]; y_max <- yBreaks[length(yBreaks)]

    # Adjust the graining
    hs <- min(c(diff(range(xBreaks)), diff(range(yBreaks)))) / 50

    grid <- as.data.frame(expand.grid(seq(x_min, x_max, by = hs), seq(y_min, y_max, by =hs)))
    colnames(grid) <- colnames(predictors)

    classificationResult <- jaspResults[["classificationResult"]]$object

    if(type == "lda"){
      ldafit <- MASS::lda(formula, data = dataset)
      preds <- predict(ldafit, newdata = grid)$class
    } else if(type == "knn"){
      kfit <- kknn::train.kknn(formula = formula, data = dataset, ks = classificationResult[["nn"]], 
                  distance = classificationResult[['distance']], kernel = classificationResult[['weights']], scale = FALSE)
      preds <- predict(kfit, newdata = grid)
    } else if(type == "randomForest"){
      rfit <- randomForest::randomForest(x = predictors, y = target,
                                                 ntree = classificationResult[["noOfTrees"]], mtry = classificationResult[["predPerSplit"]],
                                                 sampsize = classificationResult[["bagFrac"]], importance = TRUE, keep.forest = TRUE)
      preds <- predict(rfit, newdata = grid)
    } else if(type == "boosting"){
      bfit <- gbm::gbm(formula = formula, data = dataset, n.trees = classificationResult[["noOfTrees"]],
                        shrinkage = options[["shrinkage"]], interaction.depth = options[["intDepth"]],
                        cv.folds = classificationResult[["noOfFolds"]], bag.fraction = options[["bagFrac"]], n.minobsinnode = options[["nNode"]],
                        distribution = "multinomial", n.cores=1) #multiple cores breaks modules in JASP, see: INTERNAL-jasp#372
      probabilities <- gbm::predict.gbm(bfit, newdata = grid, n.trees = classificationResult[["noOfTrees"]], type = "response")
      preds <- colnames(probabilities)[apply(probabilities, 1, which.max)]
    }

    gridData <- data.frame(x = grid[, 1], y = grid[, 2])
    pointData <- data.frame(x=predictors[, 1], y=predictors[, 2])

    p <- ggplot2::ggplot(data = gridData, mapping = ggplot2::aes(x = x, y = y)) +
          ggplot2::geom_tile(ggplot2::aes(fill = preds), alpha = 0.3, show.legend = FALSE) +
          ggplot2::labs(fill = options[["target"]]) + 
          ggplot2::scale_fill_manual(values = colorspace::qualitative_hcl(n = length(unique(target))))
    p <- p + ggplot2::scale_x_continuous(name = "", breaks = xBreaks, limits = range(xBreaks))
    p <- p + ggplot2::scale_y_continuous(name = "", breaks = yBreaks, limits = range(yBreaks))
    if(options[["plotPoints"]])
      p <- p + JASPgraphs::geom_point(data = pointData, ggplot2::aes(x = x, y = y, fill = target))
    if(l <= 2){
      p <- JASPgraphs::themeJasp(p, xAxis = TRUE, yAxis = TRUE, legend.position = "right")
    } else {
      p <- JASPgraphs::themeJasp(p, xAxis = TRUE, yAxis = TRUE)
    }
    return(p)
}

.legendPlot <- function(dataset, options, col){

  target <- dataset[, .v(options[["target"]])]
  predictors <- dataset[, .v(options[["predictors"]])]
  predictors <- predictors[, 1]
  lda.data <- data.frame(target = target, predictors = predictors)
  
  p <- ggplot2::ggplot(lda.data, ggplot2::aes(y = target, x = target, show.legend = TRUE)) +
        JASPgraphs::geom_point(ggplot2::aes(fill = target), alpha = 0) +
        ggplot2::xlab("") +
        ggplot2::ylab("") +
        ggplot2::theme(legend.key = ggplot2::element_blank()) +
        ggplot2::labs(fill = options[["target"]]) + 
        ggplot2::scale_fill_manual(values = colorspace::qualitative_hcl(n = length(unique(target))))
  p <- JASPgraphs::themeJasp(p, yAxis = FALSE, xAxis = FALSE, legend.position = "left")
  p <- p + ggplot2::theme(axis.ticks = ggplot2::element_blank(), axis.text.x = ggplot2::element_blank(), axis.text.y = ggplot2::element_blank())
  p <- p + ggplot2::guides(fill = ggplot2::guide_legend(override.aes = list(alpha = 1)))
  
  return(p)
}

.rocCurve <- function(dataset, options, jaspResults, ready, position, type){

  if(!is.null(jaspResults[["rocCurve"]]) || !options[["rocCurve"]]) return()

    rocCurve <- createJaspPlot(plot = NULL, title = "ROC Curves Plot", width = 500, height = 300)
    rocCurve$position <- position
    rocCurve$dependOn(options = c("rocCurve", "trainingDataManual", "scaleEqualSD", "modelOpt",
                                    "target", "predictors", "seed", "seedBox", "modelValid", "estimationMethod",
                                    "maxK", "noOfFolds", "modelValid", "noOfNearestNeighbors", "distanceParameterManual", "weights",
                                    "noOfTrees", "bagFrac", "noOfPredictors", "numberOfPredictors", "shrinkage", "intDepth", "nNode"))
    jaspResults[["rocCurve"]] <- rocCurve

    if(!ready) return()

    classificationResult <- jaspResults[["classificationResult"]]$object 

    lvls <- levels(factor(dataset[, .v(options[["target"]])]))

    predictors <- .v(options[["predictors"]])
    formula <- formula(paste("levelVar", "~", paste(predictors, collapse=" + ")))

    linedata <- data.frame(x = c(0,1), y = c(0,1))
    p <- ggplot2::ggplot(linedata, ggplot2::aes(x = x, y = y)) +
          JASPgraphs::geom_line(col = "black", linetype = 2) +
          ggplot2::xlab("False positive rate") +
          ggplot2::ylab("True positive rate")

    rocXstore <- NULL
    rocYstore <- NULL
    rocNamestore <- NULL

    for(i in 1:length(lvls)){

      levelVar <- dataset[,.v(options[["target"]])] == lvls[i]
      typeData <- cbind(dataset, levelVar = factor(levelVar))
      column <- which(colnames(typeData) == .v(options[["target"]]))
      typeData <- typeData[, -column]

      if(type == "knn"){

        kfit <- kknn::train.kknn(formula = formula, data = typeData, ks = classificationResult[["nn"]], 
                    distance = classificationResult[['distance']], kernel = classificationResult[['weights']], scale = FALSE)
        score <- predict(kfit, dataset, type = 'prob')[, 'TRUE']

      } else if(type == "lda"){

        ldafit <- MASS::lda(formula = formula, data = dataset, method = classificationResult[["method"]], CV = FALSE)
        score <- predict(ldafit, dataset, type = "prob")$posterior[, 'TRUE']

      } else if(type == "boosting"){

        levelVar <- as.character(levelVar)
        levelVar[levelVar == "TRUE"] <- 1
        levelVar[levelVar == "FALSE"] <- 0
        levelVar <- as.numeric(levelVar)
        column <- which(colnames(typeData) == "levelVar")
        typeData <- typeData[, -column]
        typeData <- cbind(typeData, levelVar = levelVar)

        bfit <- gbm::gbm(formula = formula, data = dataset, n.trees = classificationResult[["noOfTrees"]],
                    shrinkage = options[["shrinkage"]], interaction.depth = options[["intDepth"]],
                    cv.folds = classificationResult[["noOfFolds"]], bag.fraction = options[["bagFrac"]], n.minobsinnode = options[["nNode"]],
                    distribution = "bernoulli", n.cores=1) #multiple cores breaks modules in JASP, see: INTERNAL-jasp#372
        score <- predict(bfit, newdata = dataset, n.trees = classificationResult[["noOfTrees"]], type = "response")

      } else if(type == "randomForest"){
        
        column <- which(colnames(typeData) == "levelVar")
        typeData <- typeData[, -column]
        rfit <- randomForest::randomForest(x = typeData, y = factor(levelVar),
                   ntree = classificationResult[["noOfTrees"]], mtry = classificationResult[["predPerSplit"]],
                   sampsize = classificationResult[["bagFrac"]], importance = TRUE, keep.forest = TRUE)
        score <- predict(rfit, dataset, type = "prob")[, 'TRUE']

      }

      actual.class <- dataset[,.v(options[["target"]])] == lvls[i]
  
      pred <- ROCR::prediction(score, actual.class)
      nbperf <- ROCR::performance(pred, "tpr", "fpr")

      rocXstore <- c(rocXstore, unlist(nbperf@x.values))
      rocYstore <- c(rocYstore, unlist(nbperf@y.values))
      rocNamestore <- c(rocNamestore, rep(lvls[i], length(unlist(nbperf@y.values))))
    }

    rocData <- data.frame(x = rocXstore, y = rocYstore, name = rocNamestore)
    p <- p + JASPgraphs::geom_line(data = rocData, mapping = ggplot2::aes(x = x, y = y, col = name)) +
              ggplot2::scale_color_manual(values = colorspace::qualitative_hcl(n = length(lvls))) +
              ggplot2::labs(color = options[["target"]])
    p <- JASPgraphs::themeJasp(p, legend.position = "right")

    rocCurve$plotObject <- p
}

.classificationAndrewsCurves <- function(dataset, options, jaspResults, ready, position){

  if(!is.null(jaspResults[["andrewsCurve"]]) || !options[["andrewsCurve"]]) return()

  andrewsCurve <- createJaspPlot(plot = NULL, title = "Andrews Curves Plot", width = 500, height = 300)
  andrewsCurve$position <- position
  andrewsCurve$dependOn(options = c("andrewsCurve", "trainingDataManual", "scaleEqualSD", "modelOpt",
                                  "target", "predictors", "seed", "seedBox", "modelValid", "estimationMethod",
                                  "maxK", "noOfFolds", "modelValid", "noOfNearestNeighbors", "distanceParameterManual", "weights",
                                  "noOfTrees", "bagFrac", "noOfPredictors", "numberOfPredictors", "shrinkage", "intDepth", "nNode"))
  jaspResults[["andrewsCurve"]] <- andrewsCurve

  if(!ready) return()

  predictors <- dataset[, .v(options[["predictors"]])]
  target <- dataset[,.v(options[["target"]])]

  n <- nrow(predictors)
  m <- ncol(predictors)

  npts <- 100

  xpts <- seq(0 - pi, 0 + pi, length = npts)
  Y <- matrix(NA, nrow = n, ncol = npts)

  for (i in 1:n) {
    xs <- as.numeric(predictors[i, ])
    ypts <- c()
    for (p in xpts) {
      y <- xs[1]
      for (j in 2:m) {
        if (j%%2 == 1) {
          y <- y + xs[j] * sin((j%/%2) * p)
        }
        else {
          y <- y + xs[j] * cos((j%/%2) * p)
        }
      }
      ypts <- c(ypts, y)
    }
    Y[i, ] <- as.numeric(ypts)
  }

  Yvec <- NULL
  for(i in 1:nrow(Y)){
    Yvec <- c(Yvec, Y[i, ])
  }

  d <- data.frame(x = rep(xpts, n),
                  y = Yvec,
                  target = rep(target, each = length(xpts)),
                  observation = rep(1:n, each = length(xpts)))

  xBreaks <- JASPgraphs::getPrettyAxisBreaks(d$x, min.n = 4)
  yBreaks <- JASPgraphs::getPrettyAxisBreaks(d$y, min.n = 4)

  p <- ggplot2::ggplot(data = d, mapping = ggplot2::aes(x = x, y = y, color = target, group = observation)) +
        JASPgraphs::geom_line(size = 0.2) +
        ggplot2::scale_x_continuous(name = "", breaks = xBreaks, labels = xBreaks) + 
        ggplot2::scale_y_continuous(name = "", breaks = yBreaks, labels = yBreaks) +
        ggplot2::labs(color = options[["target"]]) +
        ggplot2::guides(color = ggplot2::guide_legend(override.aes = list(size = 1)))
  p <- JASPgraphs::themeJasp(p, legend.position = "right")

  andrewsCurve$plotObject <- p

}

.boxM <- function (data, grouping){
  # Taken from the R package "biotools", thanks!
    dname <- deparse(substitute(data))
    data <- as.matrix(data)
    grouping <- as.factor(as.character(grouping))
    p <- ncol(data)
    nlev <- nlevels(grouping)
    lev <- levels(grouping)
    dfs <- tapply(grouping, grouping, length) - 1
    mats <- aux <- list()
    for (i in 1:nlev) {
        mats[[i]] <- cov(data[grouping == lev[i], ])
        aux[[i]] <- mats[[i]] * dfs[i]
    }
    names(mats) <- lev
    pooled <- Reduce("+", aux)/sum(dfs)
    logdet <- log(unlist(lapply(mats, det)))
    minus2logM <- sum(dfs) * log(det(pooled)) - sum(logdet * 
        dfs)
    sum1 <- sum(1/dfs)
    Co <- (((2 * p^2) + (3 * p) - 1)/(6 * (p + 1) * (nlev - 1))) * 
        (sum1 - (1/sum(dfs)))
    X2 <- minus2logM * (1 - Co)
    dfchi <- (choose(p, 2) + p) * (nlev - 1)
    pval <- pchisq(X2, dfchi, lower.tail = FALSE)
    out <- structure(list(statistic = c(`Chi-Sq (approx.)` = X2), 
        parameter = c(df = dfchi), p.value = pval, cov = mats, 
        pooled = pooled, logDet = logdet, data.name = dname, 
        method = " Box's M-test for Homogeneity of Covariance Matrices"), 
        class = c("htest", "boxM"))
    return(out)
}

.classificationEvaluationMetrics <- function(dataset, options, jaspResults, ready){

  if(!is.null(jaspResults[["validationMeasures"]]) || !options[["validationMeasures"]]) return()
  
  validationMeasures <- createJaspTable(title = "Evaluation Metrics")
  validationMeasures$position <- 3
  validationMeasures$dependOn(options = c("validationMeasures", "noOfNearestNeighbours", "trainingDataManual", "distanceParameterManual", "weights", "scaleEqualSD", "modelOpt",
                                                            "target", "predictors", "seed", "seedBox", "modelValid", "maxK", "noOfFolds", "modelValid",
                                                            "estimationMethod", "shrinkage", "intDepth", "nNode"))

  validationMeasures$addColumnInfo(name = "group", title = "", type = "string")
  validationMeasures$addColumnInfo(name = "precision", title = "Precision", type = "number")
  validationMeasures$addColumnInfo(name = "recall", title = "Recall", type = "number")
  validationMeasures$addColumnInfo(name = "f1", title = "F1 score", type = "number")
  validationMeasures$addColumnInfo(name = "support", title = "Support", type = "integer")

  if(options[["target"]] != "")
    validationMeasures[["group"]] <- c(levels(factor(dataset[, .v(options[["target"]])])), "Average / Total")
  
  jaspResults[["validationMeasures"]] <- validationMeasures

  if(!ready)  return()

  classificationResult <- jaspResults[["classificationResult"]]$object

  pred <- factor(classificationResult[["y"]])
  real <- factor(classificationResult[["x"]])

  lvls <- levels(as.factor(real))

  precision <- numeric()
  recall <- numeric()
  f1 <- numeric()
  support <- numeric()

  for(i in 1:length(lvls)){

    TP                <- length(which(pred == lvls[i] & real == lvls[i]))
    FP                <- length(which(pred != lvls[i] & real == lvls[i]))
    FN                <- length(which(pred != lvls[i] & real != lvls[i]))

    precision_tmp     <- TP / (TP + FP)
    recall_tmp        <- TP / (TP + FN)
    f1_tmp            <- 2 * ( ( precision_tmp * recall_tmp ) / ( precision_tmp + recall_tmp ) )
    support_tmp       <- length(which(real == lvls[i]))

    precision[i]      <- precision_tmp
    recall[i]         <- recall_tmp 
    f1[i]             <- f1_tmp
    support[i]        <- support_tmp
  }

  precision[length(precision) + 1]    <- sum(precision * support) / sum(support)
  recall[length(recall) + 1]          <- sum(recall * support) / sum(support)
  f1[length(f1) + 1]                  <- sum(f1 * support) / sum(support)
  support[length(support) + 1]        <- sum(support)

  validationMeasures[["precision"]] <- precision
  validationMeasures[["recall"]] <- recall
  validationMeasures[["f1"]] <- f1
  validationMeasures[["support"]] <- support
}
