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
  
  # read variables ##
  dataset             <- .hierClusteringReadData(dataset, options)
  
  # error handling & code variable names in base64
  .hierClusteringErrorHandling(dataset, options)
  ready               <- length(options[["predictors"]][options[["predictors"]] != ""] > 0)
  
  # Run the analysis and save the results
  res                 <- .hierClustering(dataset, options, jaspResults, ready)
  
  # create the evaluation table
  .hierClusteringSummaryTable(res, options, jaspResults, ready)
  
  # create the cluster information table
  .hierClusteringInformationTable(options, res, jaspResults, ready)
  
  # Create the cluster plot
  .hierClusterPlot(dataset, options, res, jaspResults, ready)
  
  # Create dendrogram
  .hierdendrogram(dataset, options, res, jaspResults, ready)
}

.hierClusteringReadData <- function(dataset, options){
  predictors <- unlist(options['predictors'])
  predictors <- predictors[predictors != ""]
  if (is.null(dataset)) {
    dataset <- .readDataSetToEnd(columns.as.numeric = predictors, exclude.na.listwise = predictors)
  }
  if(options[["scaleEqualSD"]]){
    dataset <- scale(dataset)
  }
  return(dataset)
}

.hierClusteringErrorHandling <- function(dataset, options){
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
                                           "maxClusters", "seedBox", "scaleEqualSD", "distance"))
  
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
  
  hfit <- cutree(hclust(dist(dataset[, .v(options[["predictors"]])]),
                        method = options[["linkage"]]), k = options[['noOfClusters']])
                 
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
  withinss <- 0
  for (i in 1:length(table(hfit))) {
    withinss[i] <- .ss(dataset[, .v(options[["predictors"]])][hfit == i])
  }
  res[['WSS_table']] <- withinss
  res[['WSS']] <- sum(withinss)
  res[['TSS']] <- .tss(dist(dataset[, .v(options[["predictors"]])]))
  res[['BSS']] <- res[['TSS']]-res[['WSS']]
  #m = ncol(kmeans(dataset[, .v(options[["predictors"]])], res[['clusters']])$centers)
  m = dim(dataset[, .v(options[["predictors"]])])[2]
  n = length(hfit)
  k = length(table(hfit))
  D = res[['WSS']]
  res[['AIC']] <- D + 2*m*k
  res[['BIC']] <- D + log(n)*m*k
  dSilh <- factoextra::eclust(dataset[, .v(options[["predictors"]])], "hclust",
                              k = options[['noOfClusters']])
  res[['silh_scores']] <- dSilh$silinfo$clus.avg.widths
  optimal_silh <- NbClust::NbClust(dataset[, .v(options[["predictors"]])],
                                   distance = "euclidean",
                                   min.nc = res[["clusters"]],
                                   max.nc = res[["clusters"]],
                                   method = options[["linkage"]],
                                   index = "silhouette")
  res[["Silh_score"]] <- optimal_silh$Best.nc[[2]]
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
  
  optimal <- NbClust::NbClust(dataset[, .v(options[["predictors"]])],
                              distance = "euclidean",
                              min.nc = 2, max.nc = options[["maxClusters"]],
                              method = options[["linkage"]],
                              index = "silhouette")
  
  hfit <- cutree(hclust(dist(dataset[, .v(options[["predictors"]])]),
                        method = options[["linkage"]]), k = optimal$Best.nc[[1]])
  
  res <- list()
  res[['Predictions']] <- data.frame(
    'Observation' = 1:nrow(dataset),
    'Cluster' = hfit
  )
  
  res[["pred.values"]] <- hfit
  res[['clusters']] <- optimal$Best.nc[[1]]
  res[["N"]] <- nrow(dataset)
  res[["size"]] <- as.data.frame(table(hfit))[,2]
  res[["linkage"]] <- options[["linkage"]]
  withinss <- 0
  for (i in 1:length(table(hfit))) {
    withinss[i] <- .ss(dataset[, .v(options[["predictors"]])][hfit == i])
  }
  res[['WSS_table']] <- withinss
  res[['WSS']] <- sum(withinss)
  res[['TSS']] <- .tss(dist(dataset[, .v(options[["predictors"]])]))
  res[['BSS']] <- res[['TSS']]-res[['WSS']]
  m = dim(dataset[, .v(options[["predictors"]])])[2]
  n = length(hfit)
  k = length(table(hfit))
  D = res[['WSS']]
  res[['AIC']] <- D + 2*m*k
  res[['BIC']] <- D + log(n)*m*k
  dSilh <- factoextra::eclust(dataset[, .v(options[["predictors"]])], "hclust",
                                      k = optimal$Best.nc[[1]])
  res[['silh_scores']] <- dSilh$silinfo$clus.avg.widths
  res[["Silh_score"]] <- dSilh$silinfo$avg.width
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
  
  for(i in 1:options[["maxClusters"]]){
    
    hfit_tmp <- cutree(hclust(dist(dataset[, .v(options[["predictors"]])]),
                  method = options[["linkage"]]), k = i)
    wss <- 0
    for (j in 1:length(table(hfit_tmp))) {
      wss[i] <- .ss(dataset[, .v(options[["predictors"]])][hfit_tmp == j])
    }
    
    #kfit_tmp <- kmeans(dataset[, .v(options[["predictors"]])],
                       #centers = i)
    
    #res[['WithinSumSquares_store']][i] <- kfit_tmp$tot.withinss
    #wss <- fpc::cluster.stats(dist(dataset[, .v(options[["predictors"]])]),
                              #hfit_tmp)
    #wss <- as.numeric(wss[[19]])
    
    res[['WithinSumSquares_store']][i] = sum(wss) # $within.cluster.ss
    m = dim(dataset[, .v(options[["predictors"]])])[2]
    n = length(hfit_tmp)
    k = length(table(hfit_tmp))
    D = res[['WithinSumSquares_store']][i]
    res[['AIC_store']][i] <- D + 2*m*k
    res[['BIC_store']][i] <- D + log(n)*m*k
    res[["N_store"]][i] <- nrow(dataset)
    res[['TSS_store']][i] <- .tss(dist(dataset[, .v(options[["predictors"]])]))
    res[['BSS_store']][i] <- res[['TSS_store']][i]-res[['WithinSumSquares_store']][i]
    res[["rsquare_store"]][i] <- res[['BSS_store']][i]/res[['TSS_store']][i]
  }
  if(options[["modelOpt"]] == "validationAIC"){
    res[['clusters']] <- res[["clusterrange"]][[which.min(res[["AIC_store"]])]]
  } else if(options[["modelOpt"]] == "validationBIC"){
    res[['clusters']] <- res[["clusterrange"]][[which.min(res[["BIC_store"]])]]
  } 
  
  # predictions for best model.
  hfit <- cutree(hclust(dist(dataset[, .v(options[["predictors"]])]),
                        method = options[["linkage"]]), k = res[['clusters']])
  dSilh <- factoextra::eclust(dataset[, .v(options[["predictors"]])], "hclust",
                              k = res[['clusters']])
  res[['Predictions']] <- data.frame(
    'Observation' = 1:nrow(dataset),
    'Cluster' = hfit
  )
  res[["size"]] <- as.data.frame(table(hfit))[,2]
  res[["linkage"]] <- options[["linkage"]]
  #res[['WSS']] <- kmeans(dataset[, .v(options[["predictors"]])],
                         #centers = i)$tot.withinss
  #wss2 <- fpc::cluster.stats(dist(dataset[, .v(options[["predictors"]])]),
                             #hfit)
  #wss2 <- as.numeric(wss2[[19]])
  #res[['WSS']] <- wss2
  
  #res[['WSS']] <- fpc::cluster.stats(dist(dataset[, .v(options[["predictors"]])]),
                                     #hfit)$within.cluster.ss
  withinss <- 0
  for (i in 1:length(table(hfit))) {
    withinss[i] <- .ss(dataset[, .v(options[["predictors"]])][hfit == i])
  }
  res[['WSS_table']] <- withinss
  res[['WSS']] <- sum(withinss)
  res[['TSS']] <- .tss(dist(dataset[, .v(options[["predictors"]])]))
  res[['BSS']] <- res[['TSS']]-res[['WSS']]
  res[['Silh_score']] <- dSilh$silinfo$avg.width
  m = dim(dataset[, .v(options[["predictors"]])])[2]
  n = length(hfit)
  k = length(table(hfit))
  D = res[['WSS']]
  res[['AIC']] <- D + 2*m*k
  res[['BIC']] <- D + log(n)*m*k
  res[["N"]] <- nrow(dataset)
  dAIC <- res[["AIC_store"]][res[['clusters']]] - min(res[['AIC_store']])
  dBIC <- res[['BIC_store']][res[['clusters']]] - min(res[['BIC_store']])
  res[['silh_scores']] <- dSilh$silinfo$clus.avg.widths
  return(res)
}

.hierClusteringSummaryTable <- function(res, options, jaspResults, ready){
  
  if(!is.null(jaspResults[["evaluationTable"]])) return() #The options for this table didn't change so we don't need to rebuild it
  
  evaluationTable                       <- createJaspTable("Hierarchical Clustering Model Summary")
  jaspResults[["evaluationTable"]]      <- evaluationTable
  jaspResults[["evaluationTable"]]$position <- 1
  evaluationTable$dependOn(options =c("predictors", "noOfClusters", "maxClusters", "scaleEqualSD",
                                      "seed", "linkage", "modelOpt", "distance", "silhouetteValue"))
  
  evaluationTable$addColumnInfo(name = 'clusters', title = 'Clusters', type = 'integer')
  evaluationTable$addColumnInfo(name = 'measure', title = 'R\u00B2', type = 'number', format = 'dp:2')
  evaluationTable$addColumnInfo(name = 'aic', title = 'AIC', type = 'number', format = 'dp:1')
  evaluationTable$addColumnInfo(name = 'bic', title = 'BIC', type = 'number', format = 'dp:1')
  evaluationTable$addColumnInfo(name = 'n', title = 'N', type = 'number', format = 'dp:1')
  
  if(!ready)
    return()
  
  if(res[["clusters"]] == options[["maxClusters"]] && options[["modelOpt"]] != "validationManual"){
    message <- "The optimum number of clusters is the maximum number of clusters. You might want to adjust the range op optimization."
    evaluationTable$addFootnote(message=message, symbol="<i>Note.</i>")
  }
  
  row <- data.frame(clusters = res[['clusters']],
                    measure = res[['BSS']]/res[['TSS']], aic = res[['AIC']],
                    bic = res[['BIC']], n = res[["N"]])
  
  if(options[['silhouetteValue']]){
    message <- paste0('The Silhouette value of the ', res[["clusters"]], ' cluster model is ', round(res[['Silh_score']],4))
    evaluationTable$addFootnote(message=message, symbol="<i>Note.</i>")
  }
  
  evaluationTable$addRows(row)
}

.hierClusteringInformationTable <- function(options, res, jaspResults, ready){
  
  if(!is.null(jaspResults[["clusterInfoTable"]])) return() #The options for this table didn't change so we don't need to rebuild it
  
  if (options[['tableClusterInformation']]){
    
    clusterInfoTable                        <- createJaspTable("Cluster Information")
    jaspResults[["clusterInfoTable"]]       <- clusterInfoTable
    clusterInfoTable$dependOn(options =c("tableClusterInformation","predictors", "modelOpt",
                                         "noOfClusters", "scaleEqualSD", "maxClusters",
                                         "linkage", "distance", "tableClusterInfoBetweenSumSquares",
                                         "tableClusterInfoTotalSumSquares"))
    clusterInfoTable$position               <- 2
    clusterInfoTable$transpose              <- TRUE
    
    clusterInfoTable$addColumnInfo(name = 'cluster', title = 'Cluster', type = 'integer')
    clusterInfoTable$addColumnInfo(name = 'size', title = 'Size', type = 'integer')
    clusterInfoTable$addColumnInfo(name = 'withinss_table', title = 'Within Sum of Squares', type = 'number', format = 'dp:2')
    clusterInfoTable$addColumnInfo(name = 'silh_scores', title = 'Silhouette scores', type = 'number', format = 'dp:2')
    
    
    if(!ready)
      return()
    
  }
  
  cluster <- 1:res[["clusters"]]
  size <- res[["size"]]
  withinss_table <- res[["WSS_table"]]
  silh_scores <- res[['silh_scores']]
  
  row <- data.frame(cluster = cluster, size = size, withinss_table = withinss_table, silh_scores = silh_scores)
  
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

.hierClusterPlot <- function(dataset, options, res, jaspResults, ready){
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
      
      #if(options[["distance"]] == "Euclidean") {
      #  data <- dist(dataset[, .v(options[["predictors"]])])
      #} else {
      #  data <- as.dist(1-cor(dataset[, .v(options[["predictors"]])],
      #                        method="pearson"))
      #}
      
      tsne_out <- Rtsne::Rtsne(as.matrix(data), perplexity = nrow(data)/4)
      
      hfit <- cutree(hclust(dist(data), method = options[["linkage"]]),
                            k = res[['clusters']])
                     pred.values <- hfit
                     
                     tsne_plot <- data.frame(x = tsne_out$Y[,1], y = tsne_out$Y[,2], col = pred.values)
                     p <- ggplot2::ggplot(tsne_plot) + ggplot2::geom_point(ggplot2::aes(x = x, y = y, fill = factor(col)), size = 4, stroke = 1, shape = 21, color = "black") + ggplot2::xlab(NULL) + ggplot2::ylab(NULL)
                     p <- p + ggplot2::scale_fill_manual(values = colorspace::qualitative_hcl(n = res[["clusters"]]))
                     p <- JASPgraphs::themeJasp(p)
                     p <- p + ggplot2::theme(axis.ticks = ggplot2::element_blank(), axis.text.x = ggplot2::element_blank(), axis.text.y = ggplot2::element_blank())
                     jaspResults[["plot2dCluster"]] 		<- createJaspPlot(plot = p, title= "T-sne Cluster Plot", width = 400, height = 300)
                     jaspResults[["plot2dCluster"]]		$dependOn(options =c("predictors", "noOfClusters", "seed", "plot2dCluster", "maxClusters", "scaleEqualSD",
                                                                          "distance", "linkage", "modelOpt", "seedBox"))
                     jaspResults[["plot2dCluster"]] 		$position <- 3
    }
  }
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
      hc <- hclust(dist(data), method = options[["linkage"]])
      p <- ggdendro::ggdendrogram(hc)
      p <- JASPgraphs::themeJasp(p) #+ 
        #ggplot2::theme(axis.title=element_blank(),
              #axis.text.x=element_blank(),
              #axis.ticks.x=element_blank())
      
      jaspResults[["dendrogram"]] 		 <- createJaspPlot(plot = p, title= "Dendrogram", height = 300, width = 400)
      jaspResults[["dendrogram"]]		   $dependOn(options =c("predictors", "noOfClusters", "method", "linkage", "distance",
                                                           "modelOpt", "ready", "seed", "scaleEqualSD", "dendrogram"))
      jaspResults[["optimPlot"]] 		 $position <- 4
    }
  }
}

