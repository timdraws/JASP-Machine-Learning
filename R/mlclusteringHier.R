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

MLClusteringHier <- function(jaspResults, dataset, options, ...) {
  
  # Preparatory work
  dataset <- .readDataClusteringAnalyses(dataset, options)
  .errorHandlingClusteringAnalyses(dataset, options)
  
  # Check if analysis is ready to run
  ready  <- .clusterAnalysesReady(options)
  
  # Run the analysis and save the results
  clusterResult <- .hierClustering(dataset, options, jaspResults, ready)
  
  # create the evaluation table
  .hierClusteringSummaryTable(clusterResult, options, jaspResults, ready)
  
  # create the cluster information table
  .hierClusteringInformationTable(options, clusterResult, jaspResults, ready)
  
  # Create the cluster plot
  .tsneClusterPlot(dataset, options, clusterResult, type = "hierarchical", jaspResults, ready, position = 3)
  
  # Create dendrogram
  .hierdendrogram(dataset, options, clusterResult, jaspResults, ready)
}

.hierClustering <- function(dataset, options, jaspResults, ready){
  
  .disttovar <- function(x) {
    mean(x**2)/2
  }
  
  .tss <- function(x) {
    n <- nrow(as.matrix(x))
    .disttovar(x)*(n-1)
  }
  
  .ss <- function(x) {
    sum(scale(x, scale = FALSE)^2)
  }
  
  if(!is.null(jaspResults[["res"]]$object)) return(jaspResults[["res"]]$object)
  
  # set the seed so that every time the same set is chosen (to prevent random results) ##
  if(options[["seedBox"]])
    set.seed(options[["seed"]])
  
  if(ready){
    if(options[["modelOpt"]] == "validationManual"){
      res <- .hierClusteringManual(dataset, options)
    } else if (options[["modelOpt"]] == "silhouette"){
      res <- .hierClusteringOptimized(dataset, options)
    } else {
      res <- .hierClusteringWSS(dataset, options)
    }
  } else {
    res <- list()
  }
  jaspResults[["res"]] <- createJaspState(res)
  jaspResults[["res"]]$dependOn(options =c("predictors", "noOfClusters", "linkage", "modelOpt", "seed", 
                                           "maxClusters", "seedBox", "scaleEqualSD", "distance", "Pearson correlation"))
  
  return(jaspResults[["res"]]$object)
}

.hierClusteringManual <- function(dataset, options){
  
  .disttovar <- function(x) {
    mean(x**2)/2
  }
  
  .tss <- function(x) {
    n <- nrow(as.matrix(x))
    .disttovar(x)*(n-1)
  }
  
  .ss <- function(x) {
    sum(scale(x, scale = FALSE)^2)
  }
  
  hfit <- 0
  if (options[["distance"]] == "Pearson correlation") {
    hfit <- cutree(hclust(as.dist(1-cor(t(dataset[, .v(options[["predictors"]])]),
                                        method="pearson")),
                          method = options[["linkage"]]), k = options[['noOfClusters']])
  } else {
    hfit <- cutree(hclust(dist(dataset[, .v(options[["predictors"]])]),
                  method = options[["linkage"]]), k = options[['noOfClusters']])
  }
  
  res <- list()
  res[['Predictions']] <- data.frame(
    'Observation' = 1:nrow(dataset),
    'Cluster' = hfit
  )
  
  res[["pred.values"]] <- hfit
  res[['clusters']] <- options[['noOfClusters']]
  res[["N"]] <- nrow(dataset)
  res[["size"]] <- as.data.frame(table(hfit))[,2]
  res[["linkage"]] <- options[["linkage"]]
  
  m <- dim(as.data.frame(dataset[, .v(options[["predictors"]])]))[2]
  
  withinss <- 0
  for (i in 1:length(table(hfit))) {
    if (m == 1) {
      withinss[i] <- .ss(dataset[, .v(options[["predictors"]])][hfit == i])
    } else {
      withinss[i] <- .ss(dataset[, .v(options[["predictors"]])][hfit == i,])
    }
  }
  
  res[['WSS_table']] <- withinss
  res[['WSS']] <- sum(withinss)
  res[['TSS']] <- .tss(dist(dataset[, .v(options[["predictors"]])]))
  res[['BSS']] <- res[['TSS']]-res[['WSS']]
  n = length(hfit)
  k = length(table(hfit))
  D = res[['WSS']]
  res[['AIC']] <- D + 2*m*k
  res[['BIC']] <- D + log(n)*m*k
  
  if (options[["distance"]] == "Pearson correlation") {
    res[['silh_scores']] <- summary(cluster::silhouette(hfit, as.dist(1-cor(t(dataset[, .v(options[["predictors"]])])))))[[2]]
    res[["Silh_score"]] <- summary(cluster::silhouette(hfit, as.dist(1-cor(t(dataset[, .v(options[["predictors"]])])))))[[4]]
  } else {
    res[['silh_scores']] <- summary(cluster::silhouette(hfit, dist(dataset[, .v(options[["predictors"]])])))[[2]]
    res[["Silh_score"]] <- summary(cluster::silhouette(hfit, dist(dataset[, .v(options[["predictors"]])])))[[4]]
  }
  return(res)
}

  
.hierClusteringOptimized <- function(dataset, options){
  
  .disttovar <- function(x) {
    mean(x**2)/2
  }
  
  .tss <- function(x) {
    n <- nrow(as.matrix(x))
    .disttovar(x)*(n-1)
  }
  
  .ss <- function(x) {
    sum(scale(x, scale = FALSE)^2)
  }
  
  avg_silh <- numeric(options[["maxClusters"]] - 1)
  res<-list()
  res[['clusterrange']] <- 1:options[["maxClusters"]]
  
  for (i in 2:res[['clusterrange']]) {
    
    hfit_tmp <- cutree(hclust(dist(dataset[, .v(options[["predictors"]])]),
                            method = options[["linkage"]]), k = i)
    if (options[["distance"]] == "Pearson correlation") {
      silh <- summary(cluster::silhouette(hfit_tmp, as.dist(1-cor(t(dataset[, .v(options[["predictors"]])])))))
    } else {
      silh <- summary(cluster::silhouette(hfit_tmp, dist(dataset[, .v(options[["predictors"]])])))
    }
    avg_silh[i] <- silh[4]
  }
  opt_n_clusters <- which.max(avg_silh)
  
  hfit <- 0
  if (options[["distance"]] == "Pearson correlation") {
    hfit <- cutree(hclust(as.dist(1-cor(t(dataset[, .v(options[["predictors"]])]),
                                        method="pearson")),
                          method = options[["linkage"]]), k = opt_n_clusters)
  } else {
    hfit <- cutree(hclust(dist(dataset[, .v(options[["predictors"]])]),
                          method = options[["linkage"]]), k = opt_n_clusters)
  }
  
  res <- list()
  res[['Predictions']] <- data.frame(
    'Observation' = 1:nrow(dataset),
    'Cluster' = hfit
  )
  
  res[["pred.values"]] <- hfit
  res[['clusters']] <- opt_n_clusters
  res[["N"]] <- nrow(dataset)
  res[["size"]] <- as.data.frame(table(hfit))[,2]
  res[["linkage"]] <- options[["linkage"]]
  m <- dim(as.data.frame(dataset[, .v(options[["predictors"]])]))[2]
  
  withinss <- 0
  for (i in 1:length(table(hfit))) {
    if (m == 1) {
      withinss[i] <- .ss(dataset[, .v(options[["predictors"]])][hfit == i])
    } else {
      withinss[i] <- .ss(dataset[, .v(options[["predictors"]])][hfit == i,])
    }
  }

  res[['WSS_table']] <- withinss
  res[['WSS']] <- sum(withinss)
  res[['TSS']] <- .tss(dist(dataset[, .v(options[["predictors"]])]))
  res[['BSS']] <- res[['TSS']]-res[['WSS']]
  n = length(hfit)
  k = length(table(hfit))
  D = res[['WSS']]
  res[['AIC']] <- D + 2*m*k
  res[['BIC']] <- D + log(n)*m*k
  
  if (options[["distance"]] == "Pearson correlation") {
    res[['silh_scores']] <- summary(cluster::silhouette(hfit, as.dist(1-cor(t(dataset[, .v(options[["predictors"]])])))))[[2]]
    res[["Silh_score"]] <- summary(cluster::silhouette(hfit, as.dist(1-cor(t(dataset[, .v(options[["predictors"]])])))))[[4]]
  } else {
    res[['silh_scores']] <- summary(cluster::silhouette(hfit, dist(dataset[, .v(options[["predictors"]])])))[[2]]
    res[["Silh_score"]] <- summary(cluster::silhouette(hfit, dist(dataset[, .v(options[["predictors"]])])))[[4]]
  }
  
  return(res)
}

.hierClusteringWSS <- function(dataset, options) {
  
  .disttovar <- function(x) {
    mean(x**2)/2
  }
  
  .tss <- function(x) {
    n <- nrow(as.matrix(x))
    .disttovar(x)*(n-1)
  }
  
  .ss <- function(x) {
    sum(scale(x, scale = FALSE)^2)
  }
  
  WSS <- numeric(options[["maxClusters"]] - 1) # take as the max half of the N
  
  res<-list()
  res[['clusterrange']] <- 1:options[["maxClusters"]]
  
  for(i in 1:options[['maxClusters']]){
    
    hfit_tmp <- cutree(hclust(dist(dataset[, .v(options[["predictors"]])]),
                              method = options[["linkage"]]), k = i)
    
    m <- dim(as.data.frame(dataset[, .v(options[["predictors"]])]))[2]
    wss <- 0
    for (j in 1:length(table(hfit_tmp))) {
      if (m == 1) {
        wss[j] <- .ss(dataset[, .v(options[["predictors"]])][hfit_tmp == j])
      } else {
      wss[j] <- .ss(dataset[, .v(options[["predictors"]])][hfit_tmp == j,])
      }
    }
    
    res[['WithinSumSquares_store']][i] = sum(wss) # $within.cluster.ss
    n = length(hfit_tmp)
    k = length(table(hfit_tmp))
    D = sum(wss)
    res[['AIC_store']][i] <- D + 2*m*k
    res[['BIC_store']][i] <- D + log(n)*m*k
    res[["N_store"]][i] <- nrow(dataset)
    res[['TSS_store']][i] <- .tss(dist(dataset[, .v(options[["predictors"]])]))
    res[['BSS_store']][i] <- .tss(dist(dataset[, .v(options[["predictors"]])]))-sum(wss)
    res[["rsquare_store"]][i] <- res[['BSS_store']][i]/res[['TSS_store']][i]
  }
  if(options[["modelOpt"]] == "validationAIC"){
    res[['clusters']] <- res[["clusterrange"]][[which.min(res[["AIC_store"]])]]
  } else if(options[["modelOpt"]] == "validationBIC"){
    res[['clusters']] <- res[["clusterrange"]][[which.min(res[["BIC_store"]])]]
  } 
  
  # predictions for best model.
  hfit <- 0
  if (options[["distance"]] == "Pearson correlation") {
    hfit <- cutree(hclust(as.dist(1-cor(t(dataset[, .v(options[["predictors"]])]),
                                        method="pearson")),
                          method = options[["linkage"]]), k = res[['clusters']])
  } else {
    hfit <- cutree(hclust(dist(dataset[, .v(options[["predictors"]])]),
                          method = options[["linkage"]]), k = res[['clusters']])
  }
  
  res[['Predictions']] <- data.frame(
    'Observation' = 1:nrow(dataset),
    'Cluster' = hfit
  )
  res[["size"]] <- as.data.frame(table(hfit))[,2]
  res[["linkage"]] <- options[["linkage"]]
  
  
  withinss <- 0
  for (i in 1:length(table(hfit))) {
    if (m == 1) {
      withinss[i] <- .ss(dataset[, .v(options[["predictors"]])][hfit == i])
    } else {
      withinss[i] <- .ss(dataset[, .v(options[["predictors"]])][hfit == i,])
    }
  }
  res[['WSS_table']] <- withinss
  res[['WSS']] <- sum(withinss)
  res[['TSS']] <- .tss(dist(dataset[, .v(options[["predictors"]])]))
  res[['BSS']] <- res[['TSS']]-res[['WSS']]
  m = dim(as.data.frame(dataset[, .v(options[["predictors"]])]))[2]
  n = length(hfit)
  k = length(table(hfit))
  D = res[['WSS']]
  res[['AIC']] <- D + 2*m*k
  res[['BIC']] <- D + log(n)*m*k
  res[["N"]] <- nrow(dataset)
  dAIC <- res[["AIC_store"]][res[['clusters']]] - min(res[['AIC_store']])
  dBIC <- res[['BIC_store']][res[['clusters']]] - min(res[['BIC_store']])
  res[['silh_scores']] <- summary(cluster::silhouette(hfit, dist(dataset[, .v(options[["predictors"]])])))[[2]]
  res[["Silh_score"]] <- summary(cluster::silhouette(hfit, dist(dataset[, .v(options[["predictors"]])])))[[4]]
  
  return(res)
}

.hierClusteringSummaryTable <- function(res, options, jaspResults, ready){
  
  if(!is.null(jaspResults[["evaluationTable"]])) return() #The options for this table didn't change so we don't need to rebuild it
  
  evaluationTable                       <- createJaspTable("Hierarchical Clustering")
  jaspResults[["evaluationTable"]]      <- evaluationTable
  jaspResults[["evaluationTable"]]$position <- 1
  evaluationTable$dependOn(options =c("predictors", "noOfClusters", "maxClusters", "scaleEqualSD",
                                      "seed", "linkage", "modelOpt", "distance"))
  
  evaluationTable$addColumnInfo(name = 'clusters', title = 'Clusters', type = 'integer')
  evaluationTable$addColumnInfo(name = 'n', title = 'N', type = 'integer')
  evaluationTable$addColumnInfo(name = 'measure', title = 'R\u00B2', type = 'number', format = 'dp:2')
  evaluationTable$addColumnInfo(name = 'aic', title = 'AIC', type = 'number', format = 'dp:2')
  evaluationTable$addColumnInfo(name = 'bic', title = 'BIC', type = 'number', format = 'dp:2')
  evaluationTable$addColumnInfo(name = 'Silh', title = 'Silhouette', type = 'number', format = 'dp:2')
  
  if(!ready)
    return()
  
  if(res[["clusters"]] == options[["maxClusters"]] && options[["modelOpt"]] != "validationManual"){
    message <- "The optimum number of clusters is the maximum number of clusters. You might want to adjust the range op optimization."
    evaluationTable$addFootnote(message=message, symbol="<i>Note.</i>")
  }
  
  row <- data.frame(clusters = res[['clusters']],
                    measure = res[['BSS']]/res[['TSS']], aic = round(res[['AIC']], 2),
                    bic = round(res[['BIC']], 2), Silh = round(res[['Silh_score']], 2), n = res[["N"]])
  
  evaluationTable$addRows(row)
}

.hierClusteringInformationTable <- function(options, res, jaspResults, ready){
  
  if(!is.null(jaspResults[["clusterInfoTable"]]) || !options[['tableClusterInformation']]) return() #The options for this table didn't change so we don't need to rebuild it
    
    clusterInfoTable                        <- createJaspTable("Cluster Information")
    jaspResults[["clusterInfoTable"]]       <- clusterInfoTable
    clusterInfoTable$dependOn(options =c("tableClusterInformation","predictors", "modelOpt",
                                         "noOfClusters", "scaleEqualSD", "maxClusters",
                                         "linkage", "distance", "tableClusterInfoBetweenSumSquares",
                                         "tableClusterInfoTotalSumSquares", "tableClusterInfoSilhouette",
                                         "tableClusterInfoWSS"))
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
  
  cluster <- 1:res[["clusters"]]
  size <- res[["size"]]
  withinss_table <- res[["WSS_table"]]
  silh_scores <- res[['silh_scores']]
  
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

.hierdendrogram <- function(dataset, options, res, jaspResults, ready){
  if(!ready && options[['dendrogram']]){
    p <- createJaspPlot(plot = NULL, title= "Cluster Plot", width = 400, height = 300)
    p$setError("Plotting not possible: No analysis has been run.")
    return()
  } else if(ready && options[['dendrogram']]){
    if(is.null(jaspResults[["dendrogram"]])) {
      
      if(options[["seedBox"]])
        set.seed(options[["seed"]])
      unique.rows <- which(!duplicated(dataset[, .v(options[["predictors"]])]))
      data <- dataset[unique.rows, .v(options[["predictors"]])]

      hc <- 0
      if (options[["distance"]] == "Pearson correlation") {
        hc <- hclust(as.dist(1-cor(t(data),
                                            method="pearson")),
                              method = options[["linkage"]])
      } else {
        hc <- hclust(dist(data),
                              method = options[["linkage"]])
      }
      
      p <- ggdendro::ggdendrogram(hc)
      p <- JASPgraphs::themeJasp(p) + ggdendro::theme_dendro()
      
      jaspResults[["dendrogram"]] 		 <- createJaspPlot(plot = p, title= "Dendrogram", height = 300, width = 400)
      jaspResults[["dendrogram"]]		   $dependOn(options =c("predictors", "noOfClusters", "method", "linkage", "distance",
                                                           "modelOpt", "ready", "seed", "scaleEqualSD", "dendrogram"))
      jaspResults[["optimPlot"]] 		 $position <- 4
    }
  }
}

.disttovar <- function(x) {
  mean(x**2)/2
}

.tss <- function(x) {
  n <- nrow(as.matrix(x))
  .disttovar(x)*(n-1)
}

.ss <- function(x) {
  sum(scale(x, scale = FALSE)^2)
}

