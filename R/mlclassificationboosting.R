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

  # Run the analysis
  .classification(dataset, options, jaspResults, ready, type = "boosting")

  # create the results table
  .classificationTable(options, jaspResults, ready, type = "boosting")

  # Create the confusion table
  .classificationConfusionTable(dataset, options, jaspResults, ready)

  # Create the relative influence table
  .classificationBoostingRelativeInfluenceTable(options, jaspResults, ready)

  # Create the OOB improvement plot
  .classificationBoostingOOBimprovement(options, jaspResults, ready, position = 4)

  # Create the deviance plot
  .classificationBoostingDeviancePlot(options, jaspResults, ready, position = 5)

  # Create the ROC curve
  .rocCurve(dataset, options, jaspResults, ready, position = 6, type = "boosting")

  # Create the relative influence plot
  .classificationBoostingRelativeInfluencePlot(options, jaspResults, ready, position = 7)

  # Decision boundaries
  .classificationDecisionBoundaries(dataset, options, jaspResults, ready, position = 8, type = "boosting")
}

# Compute results
.boostingClassification <- function(dataset, options, jaspResults) {
  
  dataset                 <- na.omit(dataset)
  train.index             <- sample(c(TRUE,FALSE),nrow(dataset), replace = TRUE, prob = c(options[['trainingDataManual']], 1-options[['trainingDataManual']]))
  train                   <- dataset[train.index, ]
  test                    <- dataset[!train.index, ]

  # Prepare Boosting
  formula <- jaspResults[["formula"]]$object

  if(options[["modelValid"]] == "validationManual"){
    noOfFolds <- 0
  } else if(options[["modelValid"]] == "validationKFold"){
    noOfFolds <- options[["noOfFolds"]]
  }

  bfit <- gbm::gbm(formula = formula, data = train, n.trees = options[["noOfTrees"]],
                          shrinkage = options[["shrinkage"]], interaction.depth = options[["intDepth"]],
                          cv.folds = noOfFolds, bag.fraction = options[["bagFrac"]], n.minobsinnode = options[["nNode"]],
                          distribution = "multinomial")

  if(options[["modelOpt"]] == "optimizationManual"){
    
    noOfTrees <- options[["noOfTrees"]]

  } else if(options[["modelOpt"]] == "optimizationError"){

    noOfTrees <- gbm::gbm.perf(bfit, plot.it = FALSE, method = "OOB")[1]
    bfit <- gbm::gbm(formula = formula, data = train, n.trees = noOfTrees,
                        shrinkage = options[["shrinkage"]], interaction.depth = options[["intDepth"]],
                        cv.folds = noOfFolds, bag.fraction = options[["bagFrac"]], n.minobsinnode = options[["nNode"]],
                        distribution = "multinomial")

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

.classificationBoostingRelativeInfluenceTable <- function(options, jaspResults, ready){

  if (!options[["classBoostRelInfTable"]] || !is.null(jaspResults[["classBoostRelInfTable"]])) return()
  
  classBoostRelInfTable <- createJaspTable(title = "Relative Influence")
  classBoostRelInfTable$position <- 3
  classBoostRelInfTable$dependOn(options = c("classBoostRelInfTable", "target", "predictors", "modelOpt", "maxTrees", "intDepth", "shrinkage",
                                                "noOfTrees", "bagFrac", "noOfPredictors", "numberOfPredictors", "seed", "seedBox", "modelValid", "nNode"))
  
  classBoostRelInfTable$addColumnInfo(name = "predictor",  title = "", type = "string")
  classBoostRelInfTable$addColumnInfo(name = "relIn",  title = "Relative Influence", type = "number")

  jaspResults[["classBoostRelInfTable"]] <- classBoostRelInfTable
  
  if(!ready)  return()

  classificationResult <- jaspResults[["classificationResult"]]$object

  classBoostRelInfTable[["predictor"]]  <- .unv(classificationResult[["relInf"]]$var)
  classBoostRelInfTable[["relIn"]]  <- classificationResult[["relInf"]]$rel.inf
}

.classificationBoostingOOBimprovement <- function(options, jaspResults, ready, position){

  if(!is.null(jaspResults[["plotOOBChangeDev"]]) || !options[["plotOOBChangeDev"]]) return()

  plotOOBChangeDev <- createJaspPlot(plot = NULL, title = "Out-of-bag Improvement Plot", width = 500, height = 300)
  plotOOBChangeDev$position <- position
  plotOOBChangeDev$dependOn(options = c("plotOOBChangeDev", "target", "predictors", "modelOpt", "maxTrees", "intDepth", "shrinkage",
                                "noOfTrees", "bagFrac", "noOfPredictors", "numberOfPredictors", "seed", "seedBox", "modelValid", "nNode"))
  jaspResults[["plotOOBChangeDev"]] <- plotOOBChangeDev

  if(!ready) return()

  classificationResult <- jaspResults[["classificationResult"]]$object 
  
  oobDev <- data.frame(trees = 1:classificationResult[["model"]]$n.trees, oobImprove = classificationResult[["model"]]$oobag.improve)
  
  if (nlevels(classificationResult[["test"]][,.v(options[["target"]])]) > 2L) {
    ylab <- "OOB Change in \n Multinomial Deviance"
  } else {
    ylab <- "OOB Change in \n Binomial Deviance"
  }

  xBreaks <- JASPgraphs::getPrettyAxisBreaks(oobDev[["trees"]], min.n = 4)
  yBreaks <- JASPgraphs::getPrettyAxisBreaks(oobDev[["oobImprove"]], min.n = 4)
  
  p <- ggplot2::ggplot(data = oobDev, mapping = ggplot2::aes(x = trees, y = oobImprove)) +
        JASPgraphs::geom_line() +
        ggplot2::geom_smooth(size = 1, colour = "darkred", se = FALSE) +
        ggplot2::scale_x_continuous(name = "Number of Trees", labels = xBreaks, breaks = xBreaks) +
        ggplot2::scale_y_continuous(name = ylab, labels = yBreaks, breaks = yBreaks)
  p <- JASPgraphs::themeJasp(p)

  plotOOBChangeDev$plotObject <- p
}

.classificationBoostingDeviancePlot <- function(options, jaspResults, ready, position){

  if(!is.null(jaspResults[["plotDeviance"]]) || !options[["plotDeviance"]]) return()

  plotDeviance <- createJaspPlot(plot = NULL, title = "Deviance Plot", width = 500, height = 300)
  plotDeviance$position <- position
  plotDeviance$dependOn(options = c("plotDeviance", "target", "predictors", "modelOpt", "maxTrees", "intDepth", "shrinkage",
                                "noOfTrees", "bagFrac", "noOfPredictors", "numberOfPredictors", "seed", "seedBox", "modelValid", "nNode"))
  jaspResults[["plotDeviance"]] <- plotDeviance

  if(!ready) return()

  classificationResult <- jaspResults[["classificationResult"]]$object  

  deviance <- data.frame(
    trees = 1:classificationResult[["model"]]$n.trees,
    trainError = c(classificationResult[["model"]]$train.error, classificationResult[["model"]]$cv.error),
    what = rep(c("OOB", "CV"), c(length(classificationResult[["model"]]$train.error), length(classificationResult[["model"]]$cv.error)))
  )

  xBreaks <- JASPgraphs::getPrettyAxisBreaks(deviance[["trees"]], min.n = 4)
  yBreaks <- JASPgraphs::getPrettyAxisBreaks(deviance[["trainError"]], min.n = 4)
  
  if (nlevels(classificationResult[["test"]][,.v(options[["target"]])]) > 2L) {
    ylab <- "Multinomial Deviance"
  } else {
    ylab <- "Binomial Deviance"
  }
  
  p <- ggplot2::ggplot(data = deviance, mapping = ggplot2::aes(x = trees, y = trainError, group = what, color = what)) +
        ggplot2::geom_line(size = 1, show.legend = classificationResult[["method"]] != "OOB") +
        ggplot2::scale_x_continuous(name = "Number of Trees", labels = xBreaks, breaks = xBreaks) +
        ggplot2::scale_y_continuous(name = ylab, labels = yBreaks, breaks = yBreaks) +
        ggplot2::scale_color_manual(name = "", values = c("OOB" = "gray20", "CV" = "#99c454"))
  p <- JASPgraphs::themeJasp(p, legend.position = "right")


   plotDeviance$plotObject <- p
}

.classificationBoostingRelativeInfluencePlot <- function(options, jaspResults, ready, position){

  if(!is.null(jaspResults[["plotRelInf"]]) || !options[["plotRelInf"]]) return()

  plotRelInf <- createJaspPlot(plot = NULL, title = "Relative Influence Plot", width = 500, height = 300)
  plotRelInf$position <- position
  plotRelInf$dependOn(options = c("plotRelInf", "target", "predictors", "modelOpt", "maxTrees", "intDepth", "shrinkage",
                                "noOfTrees", "bagFrac", "noOfPredictors", "numberOfPredictors", "seed", "seedBox", "modelValid", "nNode"))
  jaspResults[["plotRelInf"]] <- plotRelInf

  if(!ready) return()

  classificationResult <- jaspResults[["classificationResult"]]$object  

  p <- ggplot2::ggplot(classificationResult[["relInf"]], ggplot2::aes(x = reorder(.unv(as.factor(var)), rel.inf), y = rel.inf)) +
        ggplot2::geom_bar(stat = "identity", fill = "gray", col = "black", size = .3) +
        ggplot2::labs(x = "", y = "Relative Influence")
  p <- JASPgraphs::themeJasp(p, horizontal = TRUE)

  plotRelInf$plotObject <- p
}
