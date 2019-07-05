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

MLFuzzyCMeans <- function(jaspResults, dataset, options, ...) {
  
  # read variables ##
  dataset             <- .cMeansClusteringReadData(dataset, options)
  
  # error handling & code variable names in base64
  .cMeansClusteringErrorHandling(dataset, options)
  ready               <- length(options[["predictors"]][options[["predictors"]] != ""] > 0)
  
  # Run the analysis and save the results
  res                 <- .cMeansClustering(dataset, options, jaspResults, ready)
  
  # create the evaluation table
  .cMeansClusteringSummaryTable(res, options, jaspResults, ready)
  
  # create the cluster information table
  .cMeansClusteringInformationTable(options, res, jaspResults, ready)
  
  # Create the cluster plot
  .cMeansClusterPlot(dataset, options, res, jaspResults, ready)
  
  # Create the within ss vs cluster plot
  .cMeansWithinSumOfSquaresPlot(options, res, jaspResults, ready)
}

.cMeansClusteringReadData <- function(dataset, options){
  predictors <- unlist(options[['predictors']])
  predictors <- predictors[predictors != ""]
  if (is.null(dataset)) {
    dataset <- .readDataSetToEnd(columns.as.numeric = predictors, exclude.na.listwise = predictors)
  }
  if(options[["scaleEqualSD"]]){
    dataset <- scale(dataset)
  }
  return(dataset)
}

.cMeansClusteringErrorHandling <- function(dataset, options){
  predictors <- unlist(options$predictors)
  if(length(predictors[predictors != '']) > 0){
    for(i in 1:length(predictors)){
      errors <- .hasErrors(dataset, perform, type = c('infinity', 'observations'),
                           all.target = predictors[i],
                           observations.amount = "< 2",
                           exitAnalysisIfErrors = TRUE)
    }
  }
}

.cMeansClustering <- function(dataset, options, jaspResults, ready){
  
  if(!is.null(jaspResults[["res"]]$object)) return(jaspResults[["res"]]$object)
  
  # set the seed so that every time the same set is chosen (to prevent random results) ##
  if(options[["seedBox"]])
    set.seed(options[["seed"]])
  
  if(ready){
    if(options[["modelOpt"]] == "validationManual"){
      res <- .cMeansClusteringManual(dataset, options)
    } else if(options[["modelOpt"]] == "validationSilh") {
      res <- .cMeansClusteringSilh(dataset, options)
    } else {
      res <- .cMeansClusteringOptimized(dataset, options)
    }
  } else {
    res <- list()
  }
  jaspResults[["res"]] <- createJaspState(res)
  jaspResults[["res"]]$dependOn(options =c("predictors", "noOfClusters", "noOfIterations", "modelOpt", "seed", 
                                         "maxClusters", "seedBox", "scaleEqualSD"))
  
  return(jaspResults[["res"]]$object)
}

.cMeansClusteringManual <- function(dataset, options){
  
  cfit <- e1071::cmeans(dataset[, .v(options[["predictors"]])],
                        centers = options[['noOfClusters']],
                        iter.max = options[['noOfIterations']],
                        m = options[["m"]])
  
  v <- cfit$centers
  clabels <- cfit$cluster
  
  csumsqrs <- .sumsqr(dataset[, .v(options[["predictors"]])], v, clabels)
  res <- list()
  res[['Predictions']] <- data.frame(
    'Observation' = 1:nrow(dataset),
    'Cluster' = cfit$cluster
  )
  res[["pred.values"]] <- cfit$cluster
  res[['clusters']] <- options[['noOfClusters']]
  res[["N"]] <- nrow(dataset)
  res[['size']] <- round(cfit$size, 0)
  res[['centroids']] <- cfit$centers
  res[['WSS']] <- csumsqrs$within.ss
  res[['TSS']] <- csumsqrs$tot.ss
  res[['BSS']] <- csumsqrs$between.ss
  res[['AICweights']] <- 1
  res[['BICweights']] <- 1
  m = ncol(cfit$centers)
  n = length(cfit$cluster)
  k = nrow(cfit$centers)
  D = csumsqrs$tot.within.ss
  res[['AIC']] <- round(D + 2*m*k, 2)
  res[['BIC']] <- round(D + log(n)*m*k, 2)
  res[['Silh_score']] <- round(summary(cluster::silhouette(cfit$cluster, dist(dataset[, .v(options[["predictors"]])])))[[4]], 2)
  res[['silh_scores']] <- round(summary(cluster::silhouette(cfit$cluster, dist(dataset[, .v(options[["predictors"]])])))[[2]], 2)
  return(res)
}

.cMeansClusteringSilh <- function(dataset, options) {
  
  avg_silh <- numeric(options[["maxClusters"]] - 1)
  res<-list()
  res[['clusterrange']] <- 1:options[["maxClusters"]]
  
  for (i in 2:options[['maxClusters']]) {
    cfit_tmp <- e1071::cmeans(dataset[, .v(options[["predictors"]])],
                       centers = i,
                       iter.max = options[['noOfIterations']],
                       m = options[["m"]])
    silh <- summary(cluster::silhouette(cfit_tmp$cluster, dist(dataset[, .v(options[["predictors"]])])))
    avg_silh[i] <- silh[4]

    v_tmp <- cfit_tmp$centers
    clabels_tmp <- cfit_tmp$cluster
    csumsqrs_tmp <- .sumsqr(dataset[, .v(options[["predictors"]])], v_tmp, clabels_tmp) 
    res[['WithinSumSquares_store']][i] <- csumsqrs_tmp$tot.within.ss
  }
  opt_n_clusters <- which.max(avg_silh)
  
  cfit <- e1071::cmeans(dataset[, .v(options[["predictors"]])],
                centers = opt_n_clusters,
                iter.max = options[['noOfIterations']],
                m = options[['m']])
  
  v <- cfit$centers
  clabels <- cfit$cluster
  
  csumsqrs <- .sumsqr(dataset[, .v(options[["predictors"]])], v, clabels)
  
  res[['Predictions']] <- data.frame(
    'Observation' = 1:nrow(dataset),
    'Cluster' = cfit$cluster
  )
  res[["pred.values"]] <- cfit$cluster
  res[['clusters']] <- opt_n_clusters
  res[["N"]] <- nrow(dataset)
  res[['size']] <- round(cfit$size, 0)
  res[['centroids']] <- cfit$centers
  res[['WSS']] <- csumsqrs$within.ss
  res[['TSS']] <- csumsqrs$tot.ss
  res[['BSS']] <- csumsqrs$between.ss
  res[['AICweights']] <- 1
  res[['BICweights']] <- 1
  m = ncol(cfit$centers)
  n = length(cfit$cluster)
  k = nrow(cfit$centers)
  D = csumsqrs$tot.within.ss
  res[['AIC']] <- round(D + 2*m*k, 2)
  res[['BIC']] <- round(D + log(n)*m*k, 2)
  res[['Silh_score']] <- round(summary(cluster::silhouette(cfit$cluster, dist(dataset[, .v(options[["predictors"]])])))[[4]], 2)
  res[['silh_scores']] <- round(summary(cluster::silhouette(cfit$cluster, dist(dataset[, .v(options[["predictors"]])])))[[2]], 2)
  return(res)
}

.cMeansClusteringOptimized <- function(dataset, options){
  
  WSS <- numeric(options[["maxClusters"]] - 1) # take as the max half of the N
  
  res<-list()
  res[['clusterrange']] <- 1:options[["maxClusters"]]
  
  for(i in 2:options[["maxClusters"]]){
    
    cfit_tmp <- e1071::cmeans(dataset[, .v(options[["predictors"]])],
                              centers = i,
                              iter.max = options[['noOfIterations']],
                              m = options[["m"]])
    
    v_tmp <- cfit_tmp$centers
    clabels_tmp <- cfit_tmp$cluster
    
    csumsqrs_tmp <- .sumsqr(dataset[, .v(options[["predictors"]])], v_tmp, clabels_tmp)
    
    res[['WithinSumSquares_store']][i] <- csumsqrs_tmp$tot.within.ss
    m = ncol(cfit_tmp$centers)
    n = length(cfit_tmp$cluster)
    k = nrow(cfit_tmp$centers)
    D = csumsqrs_tmp$tot.within.ss
    res[['AIC_store']][i] <- D + 2*m*k
    res[['BIC_store']][i] <- D + log(n)*m*k
    res[["N_store"]][i] <- nrow(dataset)
    res[['TSS_store']][i] <- csumsqrs_tmp$tot.ss
    res[['BSS_store']][i] <- csumsqrs_tmp$between.ss
    res[["rsquare_store"]][i] <- res[['BSS_store']][i]/res[['TSS_store']][i]
  }
  if(options[["modelOpt"]] == "validationAIC"){
    res[['clusters']] <- res[["clusterrange"]][[which.min(res[["AIC_store"]])]]
  } else if(options[["modelOpt"]] == "validationBIC"){
    res[['clusters']] <- res[["clusterrange"]][[which.min(res[["BIC_store"]])]]
  }
  # predictions for best model.
  cfit <- e1071::cmeans(dataset[, .v(options[["predictors"]])],
                 centers = res[['clusters']],
                 iter.max = options[['noOfIterations']],
                 m = options[['m']])
  res[["model"]] <- cfit
  res[['Predictions']] <- data.frame(
    'Observation' = 1:nrow(dataset),
    'Cluster' = cfit$cluster
  )
  res[['size']] <- round(cfit$size, 0)
  res[['centroids']] <- cfit$centers
  
  v <- cfit$centers
  clabels <- cfit$cluster
  
  csumsqrs <- .sumsqr(dataset[, .v(options[["predictors"]])], v, clabels)
  
  res[['WSS']] <- csumsqrs$within.ss
  res[['TSS']] <- csumsqrs$tot.ss
  res[['BSS']] <- csumsqrs$between.ss
  m = ncol(cfit$centers)
  n = length(cfit$cluster)
  k = nrow(cfit$centers)
  D = csumsqrs$tot.within.ss
  res[['AIC']] <- round(D + 2*m*k, 2)
  res[['BIC']] <- round(D + log(n)*m*k, 2)
  res[["N"]] <- nrow(dataset)
  dAIC <- res[["AIC_store"]][res[['clusters']]] - min(res[['AIC_store']])
  res[['AICweights']] <- exp((-.5)*dAIC)/sum(exp((-.5)*dAIC))
  dBIC <- res[['BIC_store']][res[['clusters']]] - min(res[['BIC_store']])
  res[["BICweights"]] <- exp((-.5)*dBIC)/sum(exp((-.5)*dBIC))
  res[['Silh_score']] <- round(summary(cluster::silhouette(cfit$cluster, dist(dataset[, .v(options[["predictors"]])])))[[4]], 2)
  res[['silh_scores']] <- round(summary(cluster::silhouette(cfit$cluster, dist(dataset[, .v(options[["predictors"]])])))[[2]], 2)
  return(res)
}

.cMeansClusteringSummaryTable <- function(res, options, jaspResults, ready){
  
  if(!is.null(jaspResults[["evaluationTable"]])) return() #The options for this table didn't change so we don't need to rebuild it
  
  evaluationTable                       <- createJaspTable("Fuzzy C-means Clustering")
  jaspResults[["evaluationTable"]]      <- evaluationTable
  jaspResults[["evaluationTable"]]$position <- 1
  evaluationTable$dependOn(options =c("predictors", "noOfClusters", "noOfIterations", "algorithm",
                                      "modelOpt", "seed", "maxClusters", "scaleEqualSD"))
  
  evaluationTable$addColumnInfo(name = 'clusters', title = 'Clusters', type = 'integer')
  evaluationTable$addColumnInfo(name = 'n', title = 'N', type = 'integer')
  evaluationTable$addColumnInfo(name = 'measure', title = 'R\u00B2', type = 'number', format = 'dp:2')
  evaluationTable$addColumnInfo(name = 'aic', title = 'AIC', type = 'number', format = 'dp:1')
  evaluationTable$addColumnInfo(name = 'bic', title = 'BIC', type = 'number', format = 'dp:1')
  evaluationTable$addColumnInfo(name = 'Silh', title = 'Silhouette', type = 'number', format = 'dp:1')
  
  if(!ready)
    return()
  
  if(res[["clusters"]] == options[["maxClusters"]] && options[["modelOpt"]] != "validationManual"){
    message <- "The optimum number of clusters is the maximum number of clusters. You might want to adjust the range op optimization."
    evaluationTable$addFootnote(message=message, symbol="<i>Note.</i>")
  }
  
  row <- data.frame(clusters = res[['clusters']], measure = res[['BSS']]/res[['TSS']], aic = res[['AIC']], bic = res[['BIC']], Silh = res[['Silh_score']], n = res[["N"]])
  evaluationTable$addRows(row)
}

.cMeansClusteringInformationTable <- function(options, res, jaspResults, ready){
  
  if(!is.null(jaspResults[["clusterInfoTable"]])) return() #The options for this table didn't change so we don't need to rebuild it
  
  if (options[['tableClusterInformation']]){
    
    clusterInfoTable                        <- createJaspTable("Cluster Information")
    jaspResults[["clusterInfoTable"]]       <- clusterInfoTable
    clusterInfoTable$dependOn(options =c("tableClusterInformation","predictors", "modelOpt",
                                       "noOfClusters", "tableClusterInfoSize",
                                       "tableClusterInfoSumSquares", "tableClusterInfoCentroids", "scaleEqualSD",
                                       "tableClusterInfoBetweenSumSquares", "tableClusterInfoTotalSumSquares", "maxClusters",
                                       "tableClusterInfoWSS", "tableClusterInfoSilhouette"))
    clusterInfoTable$position               <- 2
    clusterInfoTable$transpose              <- TRUE
    
    clusterInfoTable$addColumnInfo(name = 'cluster', title = 'Cluster', type = 'integer')
    clusterInfoTable$addColumnInfo(name = 'size', title = 'Size', type = 'integer')
    if(options[["tableClusterInfoWSS"]])
      clusterInfoTable$addColumnInfo(name = 'withinss', title = 'Within sum of squares', type = 'number', format = 'dp:2')
    if(options[["tableClusterInfoSilhouette"]])
      clusterInfoTable$addColumnInfo(name = 'silh_scores', title = 'Silhouette score', type = 'number', format = 'dp:2')
    
    if(!ready)
      return()
    
    if(options[['tableClusterInfoCentroids']]){
      for( i in 1:length(options[["predictors"]])){
        clusterInfoTable$addColumnInfo(name = paste0('centroid', i), title = paste0('Centroid ', options[["predictors"]][i]), type = 'number', format = 'dp:3')
      }
    }
    
    cluster <- 1:res[["clusters"]]
    size <- res[["size"]]
    withinss <- res[["WSS"]]
    silh_scores <- res[['silh_scores']]
    
    row <- data.frame(cluster = cluster, size = size)
    if(options[["tableClusterInfoWSS"]])
      row <- cbind(row, withinss = withinss)
    if(options[["tableClusterInfoSilhouette"]])
      row <- cbind(row, silh_scores = silh_scores)
    
    if(options[['tableClusterInfoCentroids']]){
      for( i in 1:length(options[["predictors"]])){
        row <- cbind(row, "tmp" = res[['centroids']][ ,i])
        colnames(row)[length(colnames(row))] <- paste0("centroid", i)
      }
    }
    
    if(options[['tableClusterInfoBetweenSumSquares']]){
      message <- paste0('The Between Sum of Squares of the ', res[["clusters"]], ' cluster model is ', round(res[['BSS']],2))
      clusterInfoTable$addFootnote(message=message, symbol="<i>Note.</i>")
    }
    
    if(options[['tableClusterInfoTotalSumSquares']]){
      message <- paste0('The Total Sum of Squares of the ', res[["clusters"]], ' cluster model is ', round(res[['TSS']],2))
      clusterInfoTable$addFootnote(message=message, symbol="<i>Note.</i>")
    }
    
    clusterInfoTable$addRows(row)
  }
}

.cMeansClusterPlot <- function(dataset, options, res, jaspResults, ready){
  if(!ready && options[['plot2dCluster']]){
    p <- createJaspPlot(plot = NULL, title= "Cluster Plot", width = 400, height = 300)
    p$setError("Plotting not possible: No analysis has been run.")
    return()
  } else if(ready && options[['plot2dCluster']]){
    if(is.null(jaspResults[["plot2dCluster"]])){
      
      if(options[["seedBox"]])
        set.seed(options[["seed"]])
      
      unique.rows <- which(!duplicated(dataset[, .v(options[["predictors"]])]))
      data <- dataset[unique.rows, .v(options[["predictors"]])]
      tsne_out <- Rtsne::Rtsne(as.matrix(data), perplexity = nrow(data)/4)
      
      cfit <- e1071::cmeans(data,
                            centers = res[["clusters"]],
                            iter.max = options[['noOfIterations']],
                            m = options[['m']])
      pred.values <- cfit$cluster
      
      tsne_plot <- data.frame(x = tsne_out$Y[,1], y = tsne_out$Y[,2], col = pred.values)
      p <- ggplot2::ggplot(tsne_plot) + ggplot2::geom_point(ggplot2::aes(x = x, y = y, fill = factor(col)), size = 4, stroke = 1, shape = 21, color = "black") + ggplot2::xlab(NULL) + ggplot2::ylab(NULL)
      p <- p + ggplot2::scale_fill_manual(values = colorspace::qualitative_hcl(n = res[["clusters"]]))
      p <- JASPgraphs::themeJasp(p)
      p <- p + ggplot2::theme(axis.ticks = ggplot2::element_blank(), axis.text.x = ggplot2::element_blank(), axis.text.y = ggplot2::element_blank())
      jaspResults[["plot2dCluster"]] 		<- createJaspPlot(plot = p, title= "T-sne Cluster Plot", width = 400, height = 300)
      jaspResults[["plot2dCluster"]]		$dependOn(options =c("predictors", "noOfClusters", "noOfIterations",
                                                         "aicweights", "modelOpt", "ready", "seed", "plot2dCluster",
                                                         "maxClusters", "scaleEqualSD", "seedBox"))
      jaspResults[["plot2dCluster"]] 		$position <- 3
    }
  }
}

.cMeansWithinSumOfSquaresPlot <- function(options, res, jaspResults, ready){
  if(!ready && options[['withinssPlot']] && options[["modelOpt"]] != "validationManual"){
    p <- createJaspPlot(plot = NULL, title= "Within Sum of Squares Plot", width = 400, height = 300)
    p$setError("Plotting not possible: No analysis has been run.")
    return()
  } else if(ready && options[['withinssPlot']] && options[["modelOpt"]] != "validationManual"){
    if(is.null(jaspResults[["optimPlot"]])){
      
      values <- res[['WithinSumSquares_store']][2:options[["maxClusters"]]]
      d <- data.frame(x = 2:options[["maxClusters"]], y = values)
      
      xBreaks <- JASPgraphs::getPrettyAxisBreaks(d$x)
      yBreaks <- JASPgraphs::getPrettyAxisBreaks(d$y)
      
      p <- ggplot2::ggplot(data = d, ggplot2::aes(x = x, y = y)) +
        JASPgraphs::geom_point()
      
      p <- p + ggplot2::scale_x_continuous(name = "Cluster", breaks = xBreaks, limits = range(xBreaks))
      p <- p + ggplot2::scale_y_continuous(name = "Within Sum of Squares", breaks = yBreaks, limits = range(yBreaks))
      p <- p + JASPgraphs::geom_point(data = data.frame(x = res[['clusters']], y = values[res[['clusters']]-1]), ggplot2::aes(x = x, y = y), fill = "red")
      
      p <- JASPgraphs::themeJasp(p)
      
      jaspResults[["optimPlot"]] 		 <- createJaspPlot(plot = p, title= "Within Sum of Squares Plot", height = 300, width = 400)
      jaspResults[["optimPlot"]]		   $dependOn(options =c("predictors", "noOfClusters","noOfRandomSets", "noOfIterations", "algorithm",
                                                          "aicweights", "modelOpt", "ready", "seed", "withinssPlot", "scaleEqualSD", "maxClusters"))
      jaspResults[["optimPlot"]] 		 $position <- 4
    }
  }
}

.sumsqr <- function(x, v, clusters){
  sumsqr <- function(x) sum(scale(x, scale = FALSE)^2)
  bwss <- sumsqr(v[clusters,])
  wss <- sapply(split(as.data.frame(x), clusters), sumsqr)
  twss <- sum(wss)
  tss <- bwss + twss
  ss <- list(bwss, wss, twss, tss)
  names(ss) <- c("between.ss", "within.ss", "tot.within.ss", "tot.ss")
  return(ss)
}