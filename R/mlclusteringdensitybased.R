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

MLClusteringDensityBased <- function(jaspResults, dataset, options, ...) {
  
  .ss <- function(x) {
    sum(scale(x, scale = FALSE)^2)
  }
  
  .disttovar <- function(x) {
    mean(x**2)/2
  }
  
  .tss <- function(x) {
    n <- nrow(as.matrix(x))
    .disttovar(x)*(n-1)
  }
  

  # read variables ##
  dataset             <- .densityClusteringReadData(dataset, options)
  
  # error handling & code variable names in base64
  .densityClusteringErrorHandling(dataset, options)
  ready               <- length(options[["predictors"]][options[["predictors"]] != ""] > 0)
  
  # Run the analysis and save the results
  res                 <- .densityClustering(dataset, options, jaspResults, ready)
  
  # create the evaluation table
  .densityClusteringSummaryTable(dataset, res, options, jaspResults, ready)
  
  # create the cluster information table
  .densityClusteringInformationTable(dataset, options, res, jaspResults, ready)
  
  # Create the cluster plot
  .densityBasedClusterPlot(dataset, options, res, jaspResults, ready)
  
  # Create the k-distance plot
  .kdistPlot(dataset, options, res, jaspResults, ready)
}

.densityClusteringReadData <- function(dataset, options){
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

.densityClusteringErrorHandling <- function(dataset, options){
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

.densityClustering <- function(dataset, options, jaspResults, ready){
  
  .ss <- function(x) {
    sum(scale(x, scale = FALSE)^2)
  }
  
  .disttovar <- function(x) {
    mean(x**2)/2
  }
  
  .tss <- function(x) {
    n <- nrow(as.matrix(x))
    .disttovar(x)*(n-1)
  }
  
  if(!is.null(jaspResults[["res"]]$object)) return(jaspResults[["res"]]$object)
  
  # set the seed so that every time the same set is chosen (to prevent random results) ##
  if(options[["seedBox"]])
    set.seed(options[["seed"]])
  
  if(ready){
      res <- .densityClusteringManual(dataset, options)
    } else {
    res <- list()
  }
  jaspResults[["res"]] <- createJaspState(res)
  jaspResults[["res"]]$dependOn(c("predictors", "eps", "minPts", "modelOpt", "seed", 
                                         "seedBox", "scaleEqualSD", "distance",
                                  "Correlated densities"))
  
  return(jaspResults[["res"]]$object)
}

.densityClusteringManual <- function(dataset, options){
  
  .ss <- function(x) {
    sum(scale(x, scale = FALSE)^2)
  }
  
  .disttovar <- function(x) {
    mean(x**2)/2
  }
  
  .tss <- function(x) {
    n <- nrow(as.matrix(x))
    .disttovar(x)*(n-1)
  }
  
  densityfit <- 0
  if (options[["distance"]] == "Correlated densities") {
     densityfit <- dbscan::dbscan(as.dist(1-cor(t(as.data.frame(dataset[, .v(options[["predictors"]])])),
                                                              method = "pearson")),
                                 eps = options[['eps']],
                                 minPts = options[['minPts']])
  } else {
    densityfit <- dbscan::dbscan(as.data.frame(dataset[, .v(options[["predictors"]])]),
                                 eps = options[['eps']],
                                 minPts = options[['minPts']])
  }

  res <- list()
  res[['Predictions']] <- data.frame(
    'Observation' = 1:nrow(dataset),
    'Cluster' = densityfit$cluster
  )
  res[["pred.values"]] <- densityfit$cluster
  res[["noisePoints"]] <- length(densityfit$cluster[densityfit$cluster == 0])
  if (res[["noisePoints"]] > 0) {
    res[["clusters"]] <- length(table(densityfit$cluster))-1
  } else {
    res[["clusters"]] <- length(table(densityfit$cluster))
  }
  res[['eps']] <- densityfit$eps
  res[["N"]] <- nrow(dataset)
  res[['size']] <- as.data.frame(table(densityfit$cluster))[,2]
  
  res[["MinPts"]] <- options[["minPts"]]

  m = dim(as.data.frame(dataset[, .v(options[["predictors"]])]))[2]
  
  withinss <- 0
  for (i in 1:res[["clusters"]]) {
    if (m == 1) {
      withinss[i] <- .ss(dataset[, .v(options[["predictors"]])][densityfit$cluster == i])
    } else {
      withinss[i] <- .ss(dataset[, .v(options[["predictors"]])][densityfit$cluster == i,])
    }
  }
  
  res[['WSS_table']] <- withinss
  res[['WSS']] <- sum(res[['WSS_table']])
  
  tss <- 0
  if (res[["noisePoints"]] > 0) {
    tss <- .tss(dist(dataset[, .v(options[["predictors"]])][densityfit$cluster != 0]))
  } else {
    tss <- .tss(dist(dataset[, .v(options[["predictors"]])]))
  }
  
  res[['TSS']] <- tss
  res[['BSS']] <- res[['TSS']]-res[['WSS']]
  n = length(densityfit$cluster)
  k = res[["clusters"]]
  D = res[['WSS']]
  res[['AIC']] <- D + 2*m*k
  res[['BIC']] <- D + log(n)*m*k
  
  cluster_nul <- 0 
  for (i in 1:length(densityfit$cluster)) {
    if (densityfit$cluster[i] == 0) {
      cluster_nul <- cluster_nul + 1
    } 
  }
  
  res[["zero"]] = 0
  if (cluster_nul == length(densityfit$cluster)) {
    res[["zero"]] = 1
  }
  
  cluster_one <- 0
  for (i in 1:length(densityfit$cluster)) {
    if (densityfit$cluster[i] == 1) {
      cluster_one <- cluster_one + 1
    } 
  }
  
  res[["one"]] = 0
  if (cluster_one == length(densityfit$cluster)) {
    res[["one"]] = 1
  }
  
  if (res[["one"]] == 0 && res[["zero"]] == 0 && options[["distance"]] == "Normal densities") {
    res[['Silh_score']] <- summary(cluster::silhouette(densityfit$cluster, dist(dataset[, .v(options[["predictors"]])])))[[4]]
    res[['silh_scores']] <- summary(cluster::silhouette(densityfit$cluster, dist(dataset[, .v(options[["predictors"]])])))[[2]]
  } else if (res[["one"]] == 0 && res[["zero"]] == 0 && options[["distance"]] == "Correlated densities") {
    res[['Silh_score']] <- summary(cluster::silhouette(densityfit$cluster, as.dist(1-cor(t(dataset[, .v(options[["predictors"]])])))))[[4]]
    res[['silh_scores']] <- summary(cluster::silhouette(densityfit$cluster, as.dist(1-cor(t(dataset[, .v(options[["predictors"]])])))))[[2]]
  } else {
    res[['Silh_score']] <- 0
    res[['silh_scores']] <- 0
  }
  
  return(res)
}
  
.densityClusteringSummaryTable <- function(dataset, res, options, jaspResults, ready){
  
  if(!is.null(jaspResults[["evaluationTable"]])) return() #The options for this table didn't change so we don't need to rebuild it
  
  evaluationTable                       <- createJaspTable("Density based Clustering Model Summary")
  jaspResults[["evaluationTable"]]      <- evaluationTable
  jaspResults[["evaluationTable"]]$position <- 1
  evaluationTable$dependOn(c("predictors", "eps", "minPts", "modelOpt", "seed", "scaleEqualSD", 'k-distplot', "distance"))
  
  evaluationTable$addColumnInfo(name = 'clusters', title = 'Cluster(s)', type = 'integer')
  evaluationTable$addColumnInfo(name = 'eps', title = 'Eps', type = 'integer')
  evaluationTable$addColumnInfo(name = 'minPts', title = 'MinPts', type = 'integer')
  evaluationTable$addColumnInfo(name = 'measure', title = 'R\u00B2', type = 'number', format = 'dp:2')
  evaluationTable$addColumnInfo(name = 'aic', title = 'AIC', type = 'number', format = 'dp:2')
  evaluationTable$addColumnInfo(name = 'bic', title = 'BIC', type = 'number', format = 'dp:2')
  evaluationTable$addColumnInfo(name = 'Silh', title = 'Silhouette', type = 'number', format = 'dp:2')
  evaluationTable$addColumnInfo(name = 'n', title = 'N', type = 'integer')
  
  if(!ready)
    return()
  

  clusters <- res[["clusters"]]
  
  row <- data.frame(clusters = clusters, eps = round(res[["eps"]], 2), minPts = res[["MinPts"]],
                    measure = res[['BSS']]/res[['TSS']], aic = res[['AIC']],
                    bic = res[['BIC']], Silh = res[['Silh_score']], n = res[["N"]])
  
  if(res[["zero"]] == 1) {
    message <- "Your cluster model contains 0 clusters and only Noisepoints, we advise to change the Eps and MinPts parameters."
    evaluationTable$addFootnote(message=message, symbol="<i>Note.</i>")
  }
  
  if(res[["one"]] == 1) {
    message <- "Your cluster model contains 1 cluster and 0 Noisepoints. You could change the Eps and MinPts parameters."
    evaluationTable$addFootnote(message=message, symbol="<i>Note.</i>")
  }
  
  if (is.null(dim(dataset[, .v(options[["predictors"]])])) && options[['k-distplot']]) {
    message <- "Creating a k-distance plot is not possible, as there are no distances to be calculated, increase the dimensionality of your model"
    evaluationTable$addFootnote(message=message, symbol="<i>Note.</i>")
  }
  
  evaluationTable$addRows(row)
  }

.densityClusteringInformationTable <- function(dataset, options, res, jaspResults, ready){
  
  if(!is.null(jaspResults[["clusterInfoTable"]])) return() #The options for this table didn't change so we don't need to rebuild it
  
  if (options[['tableClusterInformation']]){
    
    clusterInfoTable                        <- createJaspTable("Cluster Information")
    jaspResults[["clusterInfoTable"]]       <- clusterInfoTable
    clusterInfoTable$dependOn(c("tableClusterInformation","predictors", "eps", "minPts", "modelOpt",
                                "scaleEqualSD", "tableClusterInfoBetweenSumSquares", "tableClusterInfoTotalSumSquares",
                                "tableClusterInfoWSS", "tableClusterInfoSilhouette", "distance"))
    clusterInfoTable$position               <- 2
    clusterInfoTable$transpose              <- TRUE
    
    clusterInfoTable$addColumnInfo(name = 'cluster', title = 'Cluster', type = 'integer')
    clusterInfoTable$addColumnInfo(name = 'size', title = 'Size', type = 'integer')
    if(options[["tableClusterInfoWSS"]])
      clusterInfoTable$addColumnInfo(name = 'withinss_table', title = 'Within sum of squares', type = 'number', format = 'dp:2')
    if(options[["tableClusterInfoSilhouette"]])
      clusterInfoTable$addColumnInfo(name = 'silh_scores', title = 'Silhouette score', type = 'number', format = 'dp:2')

    if(!ready)
      return()
    
    size <- res[["size"]]
    if (sum(size) == res[["noisePoints"]]) {
      cluster = 0
    } else if (res[["noisePoints"]] > 0) {
      cluster <- c("Noisepoints", 1:(res[["clusters"]]))
    } else {
      cluster <- 1:res[["clusters"]]
    }
    
    withinss_table <- 0
    if (sum(size) == res[["noisePoints"]]) {
      withinss_table <- 0
    } else if (res[["noisePoints"]] > 0) {
      withinss_table <- c(0, res[["WSS_table"]])
    } else {
      withinss_table <- res[["WSS_table"]]
    }
    
    silh_scores <- res[['silh_scores']]
    if (res[["noisePoints"]] > 0) {
      silh_scores[1] = 0
    }
    
    row <- data.frame(cluster = cluster, size = size)
    
    if(options[["tableClusterInfoWSS"]])
      row <- cbind(row, withinss_table = withinss_table)
    if(options[["tableClusterInfoSilhouette"]])
      row <- cbind(row, silh_scores = silh_scores)
    
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

.densityBasedClusterPlot <- function(dataset, options, res, jaspResults, ready){
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
      
      densityfit <- 0
      if (options[["distance"]] == "Correlated densities") {
        densityfit <- dbscan::dbscan(as.dist(1-cor(t(data),
                                                   method = "pearson")),
                                     eps = res[['eps']],
                                     minPts = res[['MinPts']])
      } else {
        densityfit <- dbscan::dbscan(data,
                                     eps = res[['eps']],
                                     minPts = res[['MinPts']])
      }
      
      pred.values <- densityfit$cluster
      
      tsne_plot <- data.frame(x = tsne_out$Y[,1], y = tsne_out$Y[,2], col = pred.values)
      p <- ggplot2::ggplot(tsne_plot) + ggplot2::geom_point(ggplot2::aes(x = x, y = y, fill = factor(col)), size = 4, stroke = 1, shape = 21, color = "black") + ggplot2::xlab(NULL) + ggplot2::ylab(NULL)
      p <- p + ggplot2::scale_fill_manual(values = colorspace::qualitative_hcl(n = res[["clusters"]] + 1))
      p <- JASPgraphs::themeJasp(p)
      p <- p + ggplot2::theme(axis.ticks = ggplot2::element_blank(), axis.text.x = ggplot2::element_blank(), axis.text.y = ggplot2::element_blank())
      jaspResults[["plot2dCluster"]] 		<- createJaspPlot(plot = p, title= "T-sne Cluster Plot", width = 400, height = 300)
      jaspResults[["plot2dCluster"]]		$dependOn(options =c("predictors", "eps", "minPts", "modelOpt", "seed",
                                                           "scaleEqualSD", "ready", "plot2dCluster", "seedBox", "distance"))
      jaspResults[["plot2dCluster"]] 		$position <- 3
    }
  }
}

.kdistPlot <- function(dataset, options, res, jaspResults, ready){
  if(!ready && options[['k-distplot']]){
    p <- createJaspPlot(plot = NULL, title= "K-distance plot", width = 400, height = 300)
    p$setError("Plotting not possible: No analysis has been run.")
    return()
  } else if(ready && options[['k-distplot']]){
    if(is.null(jaspResults[["optimPlot"]])){
      
      unique.rows <- which(!duplicated(dataset[, .v(options[["predictors"]])]))
      data <- dataset[unique.rows, .v(options[["predictors"]])]
      
      res_knn = 0
      dim_knn = 0
      x_knn <- 0
      if (options[["distance"]] == "Correlated densities") {
        res_knn = dbscan::kNNdist(as.dist(1-cor(t(as.data.frame(data)), method = "pearson")),
                          k = res[['MinPts']])
        # if (is.null(dim(dataset[, .v(options[["predictors"]])])))
        # p <- createJaspPlot(plot = NULL, title= "k-distance plot",
        #                     width = 400, height = 300)
        # p$setError("Plotting not possible, no distances can be calculated, increase dimensionality")
        
      } else {
        res_knn = dbscan::kNNdist(data , k = res[['MinPts']])
        
      }
      dim_knn = dim(res_knn)
      x_knn =  seq(1, dim_knn[1]*(dim_knn[2]-1))
      p <- ggplot2::ggplot() + 
        ggplot2::geom_line(ggplot2::aes(x = x_knn , y = sort(res_knn[,2:res[['MinPts']]]))) +
        ggplot2::xlab('Points sorted by distance') + 
        ggplot2::ylab('kNN distance')
      p <- JASPgraphs::themeJasp(p)
      
      jaspResults[["optimPlot"]] 		 <- createJaspPlot(plot = p,
                                                      title= "K-distance plot", height = 300, width = 400)
      jaspResults[["optimPlot"]]		   $dependOn(options =c("predictors", "eps", "minPts", "modelOpt", "seed",
                                                          "scaleEqualSD", "ready", "k-distplot", "distance"))
      jaspResults[["optimPlot"]] 		 $position <- 4
    }
  }
}





