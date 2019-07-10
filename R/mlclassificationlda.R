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

MLClassificationLDA <- function(jaspResults, dataset, options, ...) {
  
  # Preparatory work
  dataset <- .readDataClassificationAnalyses(dataset, options)
  .errorHandlingClassificationAnalyses(dataset, options)
  
  # Check if analysis is ready to run
  ready <- .classificationAnalysesReady(options, type = "lda")

  # Run the analysis 
  .classification(dataset, options, jaspResults, ready, type = "lda")
  
  # create the results table
  .classificationTable(options, jaspResults, ready, type = "lda")

  # Create the confusion table
  .classificationConfusionTable(dataset, options, jaspResults, ready)

  # Create the coefficients table
  .ldaClassificationCoefficients(options, jaspResults, ready)

  # Create the prior and posterior table
  .ldaClassificationPriorPosterior(options, jaspResults, ready)

  # Create the group means table
  .ldaClassificationMeans(options, jaspResults, ready)

  # Create the test of equality of means table
  .ldaEqualityOfGroupMeans(dataset, options, jaspResults, ready)

  # Create the test of equality of movariance matrices table
  .ldaEqualityOfCovarianceMatrices(dataset, options, jaspResults, ready)

  # Create the multicollinearity table
  .ldaMulticollinearity(dataset, options, jaspResults, ready)

  # Create the ROC curve
  .rocCurve(options, jaspResults, ready, position = 8)

  # Create the LDA matrix plot 
  .ldaMatricesPlot(dataset, options, jaspResults, ready, position = 9)

  # Decision boundaries
  .classificationDecisionBoundaries(dataset, options, jaspResults, ready, position = 10, type = "lda")
}

# Error handling 
.classLdaErrorHandling <- function(dataset, options){
  # Error Check 1: There should be at least 5 observations in the target variable
  .hasErrors(dataset = dataset, perform = "run", type = c('observations', 'variance', 'infinity'),
             all.target = options$target, observations.amount = '< 5', exitAnalysisIfErrors = TRUE)
  
  # Error Check 2: The target variable should have at least 2 classes
  if (nlevels(dataset[, .v(options$target)]) < 2){
    JASP:::.quitAnalysis("The target variable should have at least 2 classes.")
  }
  
}

# Compute results 
.ldaClassification <- function(dataset, options, jaspResults){

  formula <- jaspResults[["formula"]]$object
  
  if(options[["modelValid"]] == "validationManual"){

    dataset                 <- na.omit(dataset)
    train.index             <- sample(c(TRUE,FALSE),nrow(dataset), replace = TRUE, prob = c(options[['trainingDataManual']], 1-options[['trainingDataManual']]))
    train                   <- dataset[train.index, ]
    test                    <- dataset[!train.index, ]

    method <- base::switch(options[["estimationMethod"]], 
                            "moment" = "moment",
                            "mle" = "mle",
                            "covMve" = "mve",
                            "t" = "t")
    ldafit <- MASS::lda(formula = formula, data = train, method = method, CV = FALSE)
    pred.values <- stats::predict(ldafit, newdata = test)

  }

  classificationResult <- list()
  classificationResult[["model"]] <- ldafit
  classificationResult[["relInf"]] <- summary(ldafit, plot = FALSE)
  classificationResult[["meanTable"]] <- ldafit[["means"]]
  classificationResult[["y"]] <- pred.values[["class"]]
  classificationResult[["x"]] <- test[,.v(options[["target"]])]
  classificationResult[['confTable']]   <- table('Pred' = pred.values[["class"]], 'Real' = test[,.v(options[["target"]])])
  classificationResult[["prior"]] <- ldafit[["prior"]]
  classificationResult[["postprob"]] <- colMeans(pred.values[["posterior"]])
  classificationResult[["ntrain"]] <- nrow(train)
  classificationResult[["ntest"]] <- nrow(test)
  classificationResult[["mse"]] <- 1 - sum(diag(prop.table(classificationResult[['confTable']])))
  classificationResult[["scaling"]] <- ldafit[["scaling"]]
  classificationResult[["train"]] <- train
  classificationResult[["test"]] <- test
  
  return(classificationResult)
}

.ldaClassificationCoefficients <- function(options, jaspResults, ready){

  if(!is.null(jaspResults[["coefficientsTable"]]) || !options[["coefficientsTable"]]) return()
  
  coefficientsTable <- createJaspTable(title = "Linear Discriminant Coefficients")
  coefficientsTable$position <- 3
  coefficientsTable$dependOn(options = c("coefficientsTable", "trainingDataManual", "scaleEqualSD", "modelOpt",
                                          "target", "predictors", "seed", "seedBox", "modelValid", "estimationMethod"))
  coefficientsTable$addColumnInfo(name = "pred_level", title = "", type = "string")
  
  jaspResults[["coefficientsTable"]] <- coefficientsTable

  if(!ready)  return()

  classificationResult <- jaspResults[["classificationResult"]]$object

  for (ldacoef in colnames(classificationResult[["scaling"]])){
    coefficientsTable$addColumnInfo(name = ldacoef, type = "number")
  }

  coefficients <- classificationResult[["scaling"]]
  row <- cbind(pred_level = .unv(rownames(coefficients)), as.data.frame(coefficients))
    
  coefficientsTable$addRows(row) 
}

.ldaClassificationPriorPosterior <- function(options, jaspResults, ready){

  if(!is.null(jaspResults[["priorTable"]]) || !options[["priorTable"]]) return()
  
  priorTable <- createJaspTable(title = "Prior and Posterior Group Probabilities")
  priorTable$position <- 4
  priorTable$dependOn(options = c("priorTable", "trainingDataManual", "scaleEqualSD", "modelOpt",
                                          "target", "predictors", "seed", "seedBox", "modelValid", "estimationMethod"))

  priorTable$addColumnInfo(name = "typeprob", title = "", type = "string")
  priorTable$addColumnInfo(name = "prior", title = "Prior", type = "number")
  priorTable$addColumnInfo(name = "posterior", title = "Posterior", type = "number")
  
  jaspResults[["priorTable"]] <- priorTable

  if(!ready)  return()

  classificationResult <- jaspResults[["classificationResult"]]$object
    
  levelPriors <- classificationResult[["prior"]]
  levelPost <- classificationResult[["postprob"]]

  row <- data.frame(typeprob = names(levelPriors), prior = levelPriors, posterior = levelPost)
  priorTable$addRows(row)
}

.ldaClassificationMeans <- function(options, jaspResults, ready){

  if(!is.null(jaspResults[["meanTable"]]) || !options[["meanTable"]]) return()
  
  meanTable <- createJaspTable(title = "Group Means in Training Data")
  meanTable$position <- 5
  meanTable$dependOn(options = c("meanTable", "trainingDataManual", "scaleEqualSD", "modelOpt",
                                          "target", "predictors", "seed", "seedBox", "modelValid", "estimationMethod"))

  meanTable$addColumnInfo(name = "target_level", title = "", type = "string")
  for (i in options[["predictors"]]){
    meanTable$addColumnInfo(name = i, type = "number", title = i)
  }
  
  jaspResults[["meanTable"]] <- meanTable

  if(!ready)  return()

  classificationResult <- jaspResults[["classificationResult"]]$object
  groupMeans <- classificationResult[["meanTable"]]
  colnames(groupMeans) <- .unv(colnames(groupMeans))
  
  row <- cbind(target_level = rownames(groupMeans), as.data.frame(groupMeans))  
  meanTable$addRows(row)
}

.ldaMatricesPlot <- function(dataset, options, jaspResults, ready, position){

  if (!is.null(jaspResults[["matrixplot"]]) || !options[["matrixplot"]]) return()
  
  matrixplot <- createJaspPlot(title = "Discriminant Matrix Plot", height = 400, width = 300)
  matrixplot$position <- position
  matrixplot$dependOn(options = c("matrixplot", "plotDensities", "plotStatistics", "trainingDataManual", "scaleEqualSD", "modelOpt",
                                          "target", "predictors", "seed", "seedBox", "modelValid", "estimationMethod"))
  jaspResults[["matrixplot"]] <- matrixplot 

  if(!ready)  return()

  classificationResult <- jaspResults[["classificationResult"]]$object
  
  .ldaFillMatrixPlot(dataset, options, jaspResults, classificationResult, matrixplot)
}

.ldaFillMatrixPlot <- function(dataset, options, jaspResults, classificationResult, matrixplot) {

  variables <- colnames(classificationResult[["scaling"]])
  l <- length(variables)

  if (l <= 2 && (options[["plotDensities"]] || options[["plotStatistics"]])) {
    width <- 580
    height <- 580
  } else if (l <= 2) {
    width <- 580
    height <- 580
  } else {
    width <- 250 * l
    height <- 250 * l
  }

  matrixplot[["width"]]  <- width
  matrixplot[["height"]] <- height
  
  cexText <- 1.6
  
  plotMat <- matrix(list(), l, l)
  adjMargin <- ggplot2::theme(plot.margin = ggplot2::unit(c(.25, .40, .25, .25), "cm")) 
  oldFontSize <- JASPgraphs::getGraphOption("fontsize")
  JASPgraphs::setGraphOption("fontsize", .85 * oldFontSize)
  
  jaspResults$startProgressbar(length(plotMat)+1)
  for (row in seq_len(l)) {
    for (col in seq_len(l)) {
      if (row == col) {
        if (options[["plotDensities"]]) {
            plotMat[[row, col]] <- .ldaDensityplot(classificationResult, options, col) + adjMargin # plot marginal (histogram with density estimator)
        } else {
          
          p <- JASPgraphs::drawAxis(xName = "", yName = "", force = TRUE) + adjMargin
          p <- p + ggplot2::xlab("")
          p <- p + ggplot2::ylab("")
          p <- JASPgraphs::themeJasp(p)
          
          plotMat[[row, col]] <- p
        }
      }
      
      if (col > row) {
        if (options[["plotStatistics"]]) {
            plotMat[[row, col]] <- .ldaScatterPlot(classificationResult, options, col) + adjMargin # plot scatterplot
          
        } else {
          
          p <- JASPgraphs::drawAxis(xName = "", yName = "", force = TRUE) + adjMargin
          p <- p + ggplot2::xlab("")
          p <- p + ggplot2::ylab("")
          p <- JASPgraphs::themeJasp(p)
          
          plotMat[[row, col]] <- p
        }
      }
      
      if (col < row) {
        if (l < 7) {
          if (options[["plotStatistics"]]) {
            p <- JASPgraphs::drawAxis(xName = "", yName = "", force = TRUE) + adjMargin
            p <- p + ggplot2::xlab("")
            p <- p + ggplot2::ylab("")
            p <- JASPgraphs::themeJasp(p)
            
            plotMat[[row, col]] <- p
          
          } else {
            
            p <- JASPgraphs::drawAxis(xName = "", yName = "", force = TRUE) + adjMargin
            p <- p + ggplot2::xlab("")
            p <- p + ggplot2::ylab("")
            p <- JASPgraphs::themeJasp(p)
            
            plotMat[[row, col]] <- p
          }
        }
        
        if (col == 1 && row == 2){
          plotMat[[2, 1]] <- .legendPlot(dataset, options, col) 
        }
        jaspResults$progressbarTick()
      }
    }
  }  
  JASPgraphs::setGraphOption("fontsize", oldFontSize)
  # slightly adjust the positions of the labels left and above the plots.
  labelPos <- matrix(.5, 4, 2)
  labelPos[1, 1] <- .55
  labelPos[4, 2] <- .65
  p <- JASPgraphs::ggMatrixPlot(plotList = plotMat, leftLabels = variables, topLabels = variables,
                                scaleXYlabels = NULL, labelPos = labelPos)
  
  jaspResults$progressbarTick()
  matrixplot$plotObject <- p
}
  
.ldaDensityplot <- function(classificationResult, options, col){

  target <- classificationResult[["train"]][, .v(options[["target"]])]
    
  if (length(colnames(classificationResult[["scaling"]])) == 1) {
      lda.fit.scaled <- cbind.data.frame(
                          scale(as.matrix(classificationResult[["train"]][,.v(options[["predictors"]])]), scale = FALSE) %*% classificationResult[["scaling"]], 
                          V2 = classificationResult[["train"]][,.v(options[["target"]])]
                        )
    
    p <- ggplot2::ggplot(data = lda.fit.scaled, ggplot2::aes(x = lda.fit.scaled[,paste("LD", col, sep = "")], group = as.factor(V2), color = as.factor(V2), show.legend = TRUE)) +
          JASPgraphs::geom_line(stat = "density") + 
          ggplot2::ylab("Density") + 
          ggplot2::xlab("") +
          ggplot2::labs(color = options[["target"]]) + 
          ggplot2::theme(legend.key = ggplot2::element_blank()) +
          ggplot2::scale_color_manual(values = colorspace::qualitative_hcl(n = length(unique(target))))
    p <- JASPgraphs::themeJasp(p, xAxis = TRUE, yAxis = TRUE, legend.position = "left")
    p <- p + ggplot2::theme(axis.ticks.y = ggplot2::element_blank(), axis.text.y = ggplot2::element_blank())
    p <- p + ggplot2::guides(color = ggplot2::guide_legend(override.aes = list(shape = 21)))
    
  } else {

    lda.fit.scaled <- cbind.data.frame(
                        scale(as.matrix(classificationResult[["train"]][,.v(options[["predictors"]])]), scale = FALSE) %*% classificationResult[["scaling"]], 
                        V2 = classificationResult[["train"]][,.v(options[["target"]])]
                        )
      
    p <- ggplot2::ggplot(data = lda.fit.scaled, ggplot2::aes(x = lda.fit.scaled[,paste("LD", col, sep = "")], group = as.factor(V2), color = as.factor(V2), show.legend = FALSE)) +
          JASPgraphs::geom_line(stat = "density") + 
          ggplot2::ylab("Density") + 
          ggplot2::xlab("") + 
          ggplot2::scale_color_manual(values = colorspace::qualitative_hcl(n = length(unique(target))))
    p <- JASPgraphs::themeJasp(p, xAxis = TRUE, yAxis = TRUE)
    p <- p + ggplot2::theme(axis.ticks.y = ggplot2::element_blank(), axis.text.y = ggplot2::element_blank())

  }
  return(p)
}

.ldaScatterPlot <- function(classificationResult, options, col){

  data <- classificationResult[["train"]] 
  target <- data[, .v(options[["target"]])]
  model <- classificationResult[["model"]]
  lda.data <- cbind(data, stats::predict(model, newdata = data)$x[,c(col - 1, col)])
  
  p <- ggplot2::ggplot(lda.data, ggplot2::aes(y = lda.data[,paste("LD", col - 1, sep = "")], x = lda.data[,paste("LD", col, sep = "")], show.legend = FALSE)) +
        JASPgraphs::geom_point(ggplot2::aes(fill = lda.data[,.v(options[["target"]])])) + 
        ggplot2::ylab("") + 
        ggplot2::xlab("") +
        ggplot2::labs(fill=options[["target"]]) + 
        ggplot2::scale_fill_manual(values = colorspace::qualitative_hcl(n = length(unique(target))))
  p <- JASPgraphs::themeJasp(p)    
  
  return(p)
}

.ldaEqualityOfGroupMeans <- function(dataset, options, jaspResults, ready){

  if(!is.null(jaspResults[["manovaTable"]]) || !options[["manovaTable"]]) return()
  
  manovaTable <- createJaspTable(title = "Tests of Equality of Group Means")
  manovaTable$position <- 6
  manovaTable$dependOn(options = c("manovaTable", "scaleEqualSD", "target", "predictors"))
  manovaTable$addColumnInfo(name = "model", title = "", type = "string")
  manovaTable$addColumnInfo(name = "f", title = "F", type = "number")
  manovaTable$addColumnInfo(name = "df1", title = "df1", type = "integer")
  manovaTable$addColumnInfo(name = "df2", title = "df2", type = "integer")
  manovaTable$addColumnInfo(name = "p", title = "p", type = "pvalue")

  manovaTable$addFootnote(message= "The null hypothesis specifies equal group means." , symbol="<i>Note.</i>")
  
  jaspResults[["manovaTable"]] <- manovaTable

  if(!ready)  return()

  target <- as.numeric(dataset[, .v(options[["target"]])])
  predictors <- as.matrix(dataset[, .v(options[["predictors"]])])

  manovaResult <- manova(predictors ~ target)
  manovaSummary <- summary(manovaResult, test="Wilks")

  # Individual models
  anovaSummary <- summary.aov(manovaResult)
  for(i in 1:length(anovaSummary)){
    sumTmp <- as.matrix(anovaSummary[[i]])
    F <- sumTmp[1, 4]
    df1 <- sumTmp[1, 1]
    df2 <- sumTmp[2, 1]
    p <- sumTmp[1, 5]
    row <- data.frame(model = options[["predictors"]][i], f = F, df1 = df1, df2 = df2, p = p)
    manovaTable$addRows(row)
  }

}

.ldaEqualityOfCovarianceMatrices <- function(dataset, options, jaspResults, ready){

  if(!is.null(jaspResults[["boxTest"]]) || !options[["boxTest"]]) return()
  
  boxTest <- createJaspTable(title = "Tests of Equality of Covariance Matrices")
  boxTest$position <- 7
  boxTest$dependOn(options = c("boxTest", "scaleEqualSD", "target", "predictors"))

  boxTest$addColumnInfo(name = "test", title = "Test", type = "string")
  boxTest$addColumnInfo(name = "x", title = "X\u00B2", type = "number")
  boxTest$addColumnInfo(name = "df", title = "df", type = "integer")
  boxTest$addColumnInfo(name = "p", title = "p", type = "pvalue")

  boxTest$addFootnote(message= "The null hypothesis specifies equal covariance matrices." , symbol="<i>Note.</i>")
  
  jaspResults[["boxTest"]] <- boxTest

  if(!ready)  return()

  target <- dataset[, .v(options[["target"]])]
  predictors <- dataset[, .v(options[["predictors"]])]

  testLabel <- "Box's M"
  boxSum <- .boxM(predictors, target)
  chi <- as.numeric(boxSum[["statistic"]])
  df <- as.numeric(boxSum[["parameter"]])
  p <- as.numeric(boxSum[["p.value"]])

  row <- data.frame(test = testLabel, x = chi, df = df, p = p)
  boxTest$addRows(row)
}

.ldaMulticollinearity <- function(dataset, options, jaspResults, ready){

  if(!is.null(jaspResults[["multicolTable"]]) || !options[["multicolTable"]]) return()
  
  multicolTable <- createJaspTable(title = "Pooled Within-Group Matrices Correlation")
  multicolTable$position <- 7
  multicolTable$dependOn(options = c("multicolTable", "scaleEqualSD", "target", "predictors"))
  
  jaspResults[["multicolTable"]] <- multicolTable

  if(!ready)  return()

  target <- dataset[, .v(options[["target"]])]
  predictors <- dataset[, .v(options[["predictors"]])]

  boxSum <- .boxM(predictors, target)
  corPooled <- cor(boxSum[["pooled"]])

  multicolTable$addColumnInfo(name = "empty", title = "", type = "string")

  for(i in 1:ncol(corPooled)){
    multicolTable$addColumnInfo(name = paste0("v", i), title = unlist(options[["predictors"]])[i], type = "number")
  }

  for(i in 1:nrow(corPooled)){
    row <- data.frame(empty = unlist(options[["predictors"]])[i])
    for(j in 1:ncol(corPooled)){
      if(j <= i)
        row[[paste0("v", j)]] <- corPooled[j, i]
    }
    multicolTable$addRows(row)
  }

}