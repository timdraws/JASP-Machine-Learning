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

.readDataClusteringAnalyses <- function(dataset, options){
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

.errorHandlingClusteringAnalyses <- function(dataset, options){
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

.clusterAnalysesReady <- function(options){
  ready <- length(options[["predictors"]][options[["predictors"]] != ""]) >= 2
  return(ready)
}

.sumsqr <- function(x, v, clusters){
  sumsqr <- function(x) sum(scale(x, scale = FALSE)^2)
  bwss <- sumsqr(v[clusters,])
  wss <- sapply(split(as.data.frame(x), clusters), sumsqr)
  twss <- sum(wss)
  tss <- bwss + twss
  ss <- list(bwss, wss, twss, tss)
  names(ss) <- c("bss", "wss", "tot.within.ss", "tss")
  return(ss)
}

.clustering <- function(dataset, options, jaspResults, ready, type){
  
  if(!is.null(jaspResults[["clusterResult"]])) return()

  # set the seed so that every time the same set is chosen (to prevent random results) ##
  if(options[["seedBox"]]) set.seed(options[["seed"]])

  if(ready){
    if(type == "kmeans"){
      clusterResult <- .kMeansClustering(dataset, options, jaspResults)
    } else if(type == "cmeans"){
      clusterResult <- .cMeansClustering(dataset, options, jaspResults)
    }
    jaspResults[["clusterResult"]] <- createJaspState(clusterResult)
    jaspResults[["clusterResult"]]$dependOn(options =c("predictors", "noOfClusters","noOfRandomSets", "noOfIterations", "algorithm", "modelOpt", "seed", 
                                                      "maxClusters", "seedBox", "scaleEqualSD", "m"))
  }
}

.clusteringTable <- function(options, jaspResults, ready, type){

  if(!is.null(jaspResults[["clusteringTable"]])) return() #The options for this table didn't change so we don't need to rebuild it

  title <- base::switch(type,
                        "kmeans" = "K-Means Clustering",
                        "cmeans" = "Fuzzy C-Means Clustering")

  clusteringTable                       <- createJaspTable(title)
  clusteringTable$position <- 1
  clusteringTable$dependOn(options =c("predictors", "noOfClusters","noOfRandomSets", "noOfIterations", "algorithm",
                                      "aicweights", "modelOpt", "seed", "maxClusters", "scaleEqualSD", "m"))

  clusteringTable$addColumnInfo(name = 'clusters', title = 'Clusters', type = 'integer')
  clusteringTable$addColumnInfo(name = 'n', title = 'N', type = 'integer')
  clusteringTable$addColumnInfo(name = 'measure', title = 'R\u00B2', type = 'number', format = 'dp:2')
  clusteringTable$addColumnInfo(name = 'aic', title = 'AIC', type = 'number', format = 'dp:2')
  clusteringTable$addColumnInfo(name = 'bic', title = 'BIC', type = 'number', format = 'dp:2')
  clusteringTable$addColumnInfo(name = 'Silh', title = 'Silhouette', type = 'number', format = 'dp:2')

  clusteringTable$addCitation("Hartigan, J. A., & Wong, M. A. (1979). Algorithm AS 136: A k-means clustering algorithm. Journal of the Royal Statistical Society. Series C (Applied Statistics), 28(1), 100-108.")
  jaspResults[["clusteringTable"]]      <- clusteringTable

  if(!ready) return()

  clusterResult <- jaspResults[["clusterResult"]]$object
    
  if(clusterResult[["clusters"]] == options[["maxClusters"]] && options[["modelOpt"]] != "validationManual"){
    message <- "The optimum number of clusters is the maximum number of clusters. You might want to adjust the range of optimization."
    clusteringTable$addFootnote(message=message, symbol="<i>Note.</i>")
  }
  
  row <- data.frame(clusters = clusterResult[['clusters']], measure = clusterResult[['BSS']]/clusterResult[['TSS']], aic = round(clusterResult[['AIC']], 2),
                    bic = round(clusterResult[['BIC']], 2), Silh = round(clusterResult[['Silh_score']], 2), n = clusterResult[["N"]])
  clusteringTable$addRows(row)

}

.clusterInformationTable <- function(options, jaspResults, ready, type){

  if(!is.null(jaspResults[["clusterInfoTable"]]) || !options[["tableClusterInformation"]]) return()

    clusterInfoTable                        <- createJaspTable("Cluster Information")
    clusterInfoTable$dependOn(options =c("tableClusterInformation","predictors", "modelOpt", "noOfIterations",
                                        "noOfClusters","noOfRandomSets", "tableClusterInfoSize", "tableClusterInfoSilhouette",
                                        "tableClusterInfoSumSquares", "tableClusterInfoCentroids", "scaleEqualSD", "tableClusterInfoWSS",
                                        "tableClusterInfoBetweenSumSquares", "tableClusterInfoTotalSumSquares", "maxClusters", "m"))
  clusterInfoTable$position               <- 2
  clusterInfoTable$transpose              <- TRUE

  clusterInfoTable$addColumnInfo(name = 'cluster', title = 'Cluster', type = 'integer')
  clusterInfoTable$addColumnInfo(name = 'size', title = 'Size', type = 'integer')
  if(options[["tableClusterInfoWSS"]])
    clusterInfoTable$addColumnInfo(name = 'withinss', title = 'Within sum of squares', type = 'number', format = 'dp:2')
  if(options[["tableClusterInfoSilhouette"]])
    clusterInfoTable$addColumnInfo(name = 'silh_scores', title = 'Silhouette score', type = 'number', format = 'dp:2')

  jaspResults[["clusterInfoTable"]]       <- clusterInfoTable
    
  if(!ready) return()

  clusterResult <- jaspResults[["clusterResult"]]$object

  if(type == "kmeans" || type == "cmeans"){
    if(options[['tableClusterInfoCentroids']]){
      for( i in 1:length(options[["predictors"]])){
          clusterInfoTable$addColumnInfo(name = paste0('centroid', i), title = paste0('Centroid ', options[["predictors"]][i]), type = 'number', format = 'dp:3')
      }
    }
  }

  cluster <- 1:clusterResult[["clusters"]]
  size <- clusterResult[["size"]]
  withinss <- clusterResult[["WSS"]]
  silh_scores <- clusterResult[['silh_scores']]

  row <- data.frame(cluster = cluster, size = size)
  if(options[["tableClusterInfoWSS"]])
    row <- cbind(row, withinss = withinss)
  if(options[["tableClusterInfoSilhouette"]])
    row <- cbind(row, silh_scores = silh_scores)

  if(type == "kmeans" || type == "cmeans"){
    if(options[['tableClusterInfoCentroids']]){
        for( i in 1:length(options[["predictors"]])){
            row <- cbind(row, "tmp" = clusterResult[['centroids']][ ,i])
            colnames(row)[length(colnames(row))] <- paste0("centroid", i)
        }
    }
  }
  clusterInfoTable$addRows(row)

  if(options[['tableClusterInfoBetweenSumSquares']]){
      message <- paste0('The Between Sum of Squares of the ', clusterResult[["clusters"]], ' cluster model is ', round(clusterResult[['BSS']],2))
      clusterInfoTable$addFootnote(message=message, symbol="<i>Note.</i>")
  }
  if(options[['tableClusterInfoTotalSumSquares']]){
      message <- paste0('The Total Sum of Squares of the ', clusterResult[["clusters"]], ' cluster model is ', round(clusterResult[['TSS']],2))
      clusterInfoTable$addFootnote(message=message, symbol="<i>Note.</i>")
  }
}

.tsneClusterPlot <- function(dataset, options, type, jaspResults, ready, position){

  if(!is.null(jaspResults[["plot2dCluster"]]) || !options[["plot2dCluster"]]) return()

  clusterPlot <- createJaspPlot(plot = NULL, title = "T-sne Cluster Plot", width = 400, height = 300)
  clusterPlot$position <- position
  clusterPlot$dependOn(options = c("predictors", "noOfClusters","noOfRandomSets", "algorithm", "eps", "minPts", "distance",
                                          "noOfIterations", "modelOpt", "ready", "seed", "plot2dCluster", "maxClusters", "scaleEqualSD", "seedBox",
                                          "linkage", "m"))
  jaspResults[["plot2dCluster"]] <- clusterPlot

  if(!ready) return()

  clusterResult <- jaspResults[["clusterResult"]]$object
 
  if(options[["seedBox"]]) set.seed(options[["seed"]])
      
  unique.rows <- which(!duplicated(dataset[, .v(options[["predictors"]])]))
  data <- dataset[unique.rows, .v(options[["predictors"]])]
  tsne_out <- Rtsne::Rtsne(as.matrix(data), perplexity = nrow(data) / 4)

  if(type == "kmeans" || type == "cmeans"){
    fit <- base::switch(type,
                        "kmeans" = kmeans(data, centers = clusterResult[["clusters"]], iter.max = options[['noOfIterations']], nstart = options[['noOfRandomSets']], algorithm = options[['algorithm']]),
                        "cmeans" = e1071::cmeans(data, centers = clusterResult[["clusters"]], iter.max = options[['noOfIterations']], m = options[['m']]))  
    pred.values <- fit$cluster
    colSize <- clusterResult[["clusters"]]
  } else if(type == "hierarchical"){
      if (options[["distance"]] == "Pearson correlation") {
        hfit <- cutree(hclust(as.dist(1-cor(t(data), method="pearson")), method = options[["linkage"]]), k = clusterResult[['clusters']])
      } else {
        hfit <- cutree(hclust(dist(data), method = options[["linkage"]]), k = clusterResult[['clusters']])
      }
      pred.values <- hfit
      colSize <- clusterResult[["clusters"]]
  } else if (type == "densitybased"){
      if (options[["distance"]] == "Correlated densities") {
        fit <- dbscan::dbscan(as.dist(1-cor(t(data), method = "pearson")), eps = options[['eps']], minPts = options[['minPts']])
      } else {
        fit <- dbscan::dbscan(data, eps = options[['eps']], minPts = options[['minPts']])
      }
      pred.values <- fit$cluster
      colSize <- clusterResult[["clusters"]] + 1
  }
      
  tsne_plot <- data.frame(x = tsne_out$Y[,1], y = tsne_out$Y[,2], col = pred.values)
  p <- ggplot2::ggplot(tsne_plot) + 
        ggplot2::geom_point(ggplot2::aes(x = x, y = y, fill = factor(col)), size = 4, stroke = 1, shape = 21, color = "black") + 
        ggplot2::xlab(NULL) + 
        ggplot2::ylab(NULL) +
        ggplot2::scale_fill_manual(values = colorspace::qualitative_hcl(n = colSize))
  p <- JASPgraphs::themeJasp(p)
  p <- p + ggplot2::theme(axis.ticks = ggplot2::element_blank(), axis.text.x = ggplot2::element_blank(), axis.text.y = ggplot2::element_blank())
  clusterPlot$plotObject <- p
  
}

.clusterOptimizationPlot <- function(dataset, options, jaspResults, ready, position){

if(!is.null(jaspResults[["optimPlot"]]) || !options[["withinssPlot"]]) return()

  optimPlot <- createJaspPlot(plot = NULL, title = "Within Sum of Squares Plot", width = 400, height = 300)
  optimPlot$position <- position
  optimPlot$dependOn(options = c("predictors", "noOfClusters","noOfRandomSets", "algorithm", "eps", "minPts", "distance",
                                          "noOfIterations", "modelOpt", "ready", "seed", "plot2dCluster", "maxClusters", "scaleEqualSD", "seedBox",
                                          "linkage", "m", "withinssPlot"))
  jaspResults[["optimPlot"]] <- optimPlot

  if(!ready || options[["modelOpt"]] == "validationManual") return()

  clusterResult <- jaspResults[["clusterResult"]]$object  

  values <- clusterResult[['wssStore']]
  d <- data.frame(x = 2:options[["maxClusters"]], y = values)
       
  xBreaks <- JASPgraphs::getPrettyAxisBreaks(d$x, min.n = 4)
  yBreaks <- JASPgraphs::getPrettyAxisBreaks(d$y, min.n = 4)
   
  p <- ggplot2::ggplot(data = d, ggplot2::aes(x = x, y = y)) +
    JASPgraphs::geom_point()
   
  p <- p + ggplot2::scale_x_continuous(name = "Cluster", breaks = xBreaks, limits = range(xBreaks))
  p <- p + ggplot2::scale_y_continuous(name = "Within Sum of Squares", breaks = yBreaks, limits = range(yBreaks))
  p <- p + JASPgraphs::geom_point(data = data.frame(x = clusterResult[['clusters']], y = values[clusterResult[['clusters']] - 1]), ggplot2::aes(x = x, y = y), fill = "red")
  p <- JASPgraphs::themeJasp(p)

  optimPlot$plotObject <- p

}