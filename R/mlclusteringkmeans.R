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

MLClusteringKMeans <- function(jaspResults, dataset, options, ...) {
    # read variables ##
    dataset <- .readDataClusteringAnalyses(dataset, options)
    
    # error handling & code variable names in base64
    .errorHandlingClusteringAnalyses(dataset, options)
    ready  <- length(options[["predictors"]][options[["predictors"]] != ""]) >= 2
    
    # Run the analysis and save the results
    clusterResult <- .kMeansClustering(dataset, options, jaspResults, ready)
    
    # create the evaluation table
    .kMeansClusteringSummaryTable(clusterResult, options, jaspResults, ready)
    
    # create the cluster information table
    .kMeansClusteringInformationTable(options, clusterResult, jaspResults, ready)
    
    # Create the cluster plot
    .tsneClusterPlot(dataset, options, clusterResult, type = "kmeans", jaspResults, ready, position = 3)
    
    # Create the within ss vs cluster plot
    .kMeansWithinSumOfSquaresPlot(options, clusterResult, jaspResults, ready)
}

.kMeansClustering <- function(dataset, options, jaspResults, ready){

  if(!is.null(jaspResults[["res"]]$object)) return(jaspResults[["res"]]$object)

  # set the seed so that every time the same set is chosen (to prevent random results) ##
  if(options[["seedBox"]])
    set.seed(options[["seed"]])

    if(ready){
        if(options[["modelOpt"]] == "validationManual"){
            res <- .kMeansClusteringManual(dataset, options)
        } else if(options[["modelOpt"]] == "validationSilh") {
          res <- .kMeansClusteringSilh(dataset, options)
        } else {
            res <- .kMeansClusteringOptimized(dataset, options, jaspResults)
        }
    } else {
        res <- list()
    }
    jaspResults[["res"]] <- createJaspState(res)
    jaspResults[["res"]]$dependOn(options =c("predictors", "noOfClusters","noOfRandomSets", "noOfIterations", "algorithm", "modelOpt", "seed", 
                                              "maxClusters", "seedBox", "scaleEqualSD"))

    return(jaspResults[["res"]]$object)
}

.kMeansClusteringManual <- function(dataset, options){
    
    kfit <- kmeans(dataset[, .v(options[["predictors"]])],
                   centers = options[['noOfClusters']],
                   iter.max = options[['noOfIterations']],
                   nstart = options[['noOfRandomSets']],
                   algorithm = options[['algorithm']])
    
    
    res <- list()
    res[['Predictions']] <- data.frame(
        'Observation' = 1:nrow(dataset),
        'Cluster' = kfit$cluster
    )
    res[["pred.values"]] <- kfit$cluster
    res[['clusters']] <- options[['noOfClusters']]
    res[["N"]] <- nrow(dataset)
    res[['size']] <- kfit$size
    res[['centroids']] <- kfit$centers
    res[['WSS']] <- kfit$withinss
    res[['TSS']] <- kfit$totss
    res[['BSS']] <- kfit$betweenss
    res[['AICweights']] <- 1
    res[['BICweights']] <- 1
    png(tempfile())

    m = ncol(kfit$centers)
    n = length(kfit$cluster)
    k = nrow(kfit$centers)
    D = kfit$tot.withinss
    res[['AIC']] <- D + 2*m*k
    res[['BIC']] <- D + log(n)*m*k
    res[['Silh_score']] <- summary(cluster::silhouette(kfit$cluster, dist(dataset[, .v(options[["predictors"]])])))[[4]]
    res[['silh_scores']] <- summary(cluster::silhouette(kfit$cluster, dist(dataset[, .v(options[["predictors"]])])))[[2]]
    return(res)
}

.kMeansClusteringSilh <- function(dataset, options) {
  
  avg_silh <- numeric(options[["maxClusters"]] - 1)
  res<-list()
  res[['clusterrange']] <- 1:options[["maxClusters"]]

  for (i in 2:res[['clusterrange']]) {
    kfit_tmp <- kmeans(dataset[, .v(options[["predictors"]])],
                   centers = i,
                   iter.max = options[['noOfIterations']],
                   nstart = options[['noOfRandomSets']],
                   algorithm = options[['algorithm']])
    silh <- summary(cluster::silhouette(kfit_tmp$cluster, dist(dataset[, .v(options[["predictors"]])])))
    avg_silh[i] <- silh[4]

    v_tmp <- kfit_tmp$centers
    clabels_tmp <- kfit_tmp$cluster
    csumsqrs_tmp <- .sumsqr(dataset[, .v(options[["predictors"]])], v_tmp, clabels_tmp) 
    res[['WithinSumSquares_store']][i] <- csumsqrs_tmp$tot.within.ss
  }
  opt_n_clusters <- which.max(avg_silh)
  
  kfit <- kmeans(dataset[, .v(options[["predictors"]])],
                 centers = opt_n_clusters,
                 iter.max = options[['noOfIterations']],
                 nstart = options[['noOfRandomSets']],
                 algorithm = options[['algorithm']])

  res[['Predictions']] <- data.frame(
    'Observation' = 1:nrow(dataset),
    'Cluster' = kfit$cluster
  )
  res[["pred.values"]] <- kfit$cluster
  res[['clusters']] <- opt_n_clusters
  res[["N"]] <- nrow(dataset)
  res[['size']] <- kfit$size
  res[['centroids']] <- kfit$centers
  res[['WSS']] <- kfit$withinss
  res[['TSS']] <- kfit$totss
  res[['BSS']] <- kfit$betweenss
  res[['AICweights']] <- 1
  res[['BICweights']] <- 1
  res[['Silh_score']] <- summary(cluster::silhouette(kfit$cluster, dist(dataset[, .v(options[["predictors"]])])))[[4]]
  m = ncol(kfit$centers)
  n = length(kfit$cluster)
  k = nrow(kfit$centers)
  D = kfit$tot.withinss
  res[['AIC']] <- D + 2*m*k
  res[['BIC']] <- D + log(n)*m*k
  res[['silh_scores']] <- summary(cluster::silhouette(kfit$cluster, dist(dataset[, .v(options[["predictors"]])])))[[2]]
  return(res)
}


.kMeansClusteringOptimized <- function(dataset, options, jaspResults){
    WSS <- numeric(options[["maxClusters"]] - 1) # take as the max half of the N

    res<-list()
    res[['clusterrange']] <- 1:options[["maxClusters"]]
    
    jaspResults$startProgressbar(options[["maxClusters"]])

    for(i in 1:options[["maxClusters"]]){

        kfit_tmp <- kmeans(dataset[, .v(options[["predictors"]])],
                           centers = i,
                           iter.max = options[['noOfIterations']],
                           nstart = options[['noOfRandomSets']],
                           algorithm = options[['algorithm']])

        res[['WithinSumSquares_store']][i] <- kfit_tmp$tot.withinss
        m = ncol(kfit_tmp$centers)
        n = length(kfit_tmp$cluster)
        k = nrow(kfit_tmp$centers)
        D = kfit_tmp$tot.withinss
        res[['AIC_store']][i] <- D + 2*m*k
        res[['BIC_store']][i] <- D + log(n)*m*k
        res[["N_store"]][i] <- nrow(dataset)
        res[['TSS_store']][i] <- kfit_tmp$totss
        res[['BSS_store']][i] <- kfit_tmp$betweenss
        res[["rsquare_store"]][i] <- res[['BSS_store']][i]/res[['TSS_store']][i]

        jaspResults$progressbarTick()
    }
    if(options[["modelOpt"]] == "validationAIC"){
      res[['clusters']] <- res[["clusterrange"]][[which.min(res[["AIC_store"]])]]
    } else if(options[["modelOpt"]] == "validationBIC"){
      res[['clusters']] <- res[["clusterrange"]][[which.min(res[["BIC_store"]])]]
    } 

    # predictions for best model.
    kfit <- kmeans(dataset[, .v(options[["predictors"]])],
                       centers = res[['clusters']],
                       iter.max = options[['noOfIterations']],
                       nstart = options[['noOfRandomSets']],
                       algorithm = options[['algorithm']])
    
    res[["model"]] <- kfit
    res[['Predictions']] <- data.frame(
        'Observation' = 1:nrow(dataset),
        'Cluster' = kfit$cluster
    )
    res[['size']] <- kfit$size
    res[['centroids']] <- kfit$centers
    res[['WSS']] <- kfit$withinss
    res[['TSS']] <- kfit$totss
    res[['BSS']] <- kfit$betweenss
    res[['Silh_score']] <- summary(cluster::silhouette(kfit$cluster, dist(dataset[, .v(options[["predictors"]])])))[[4]]
    m = ncol(kfit_tmp$centers)
    n = length(kfit_tmp$cluster)
    k = nrow(kfit_tmp$centers)
    D = kfit_tmp$tot.withinss
    res[['AIC']] <- D + 2*m*k
    res[['BIC']] <- D + log(n)*m*k
    res[["N"]] <- nrow(dataset)
    dAIC <- res[["AIC_store"]][res[['clusters']]] - min(res[['AIC_store']])
    res[['AICweights']] <- exp((-.5)*dAIC)/sum(exp((-.5)*dAIC))
    dBIC <- res[['BIC_store']][res[['clusters']]] - min(res[['BIC_store']])
    res[["BICweights"]] <- exp((-.5)*dBIC)/sum(exp((-.5)*dBIC))
    res[['silh_scores']] <- summary(cluster::silhouette(kfit$cluster, dist(dataset[, .v(options[["predictors"]])])))[[2]]
    return(res)
}

.kMeansClusteringSummaryTable <- function(res, options, jaspResults, ready){

  if(!is.null(jaspResults[["evaluationTable"]])) return() #The options for this table didn't change so we don't need to rebuild it

  evaluationTable                       <- createJaspTable("K-Means Clustering")
  jaspResults[["evaluationTable"]]      <- evaluationTable
  jaspResults[["evaluationTable"]]$position <- 1
  evaluationTable$dependOn(options =c("predictors", "noOfClusters","noOfRandomSets", "noOfIterations", "algorithm",
                                      "aicweights", "modelOpt", "seed", "maxClusters", "scaleEqualSD"))

  evaluationTable$addColumnInfo(name = 'clusters', title = 'Clusters', type = 'integer')
  evaluationTable$addColumnInfo(name = 'n', title = 'N', type = 'integer')
  evaluationTable$addColumnInfo(name = 'measure', title = 'R\u00B2', type = 'number', format = 'dp:2')
  evaluationTable$addColumnInfo(name = 'aic', title = 'AIC', type = 'number', format = 'dp:2')
  evaluationTable$addColumnInfo(name = 'bic', title = 'BIC', type = 'number', format = 'dp:2')
  evaluationTable$addColumnInfo(name = 'Silh', title = 'Silhouette', type = 'number', format = 'dp:2')

  evaluationTable$addCitation("Hartigan, J. A., & Wong, M. A. (1979). Algorithm AS 136: A k-means clustering algorithm. Journal of the Royal Statistical Society. Series C (Applied Statistics), 28(1), 100-108.")
  evaluationTable$addCitation("Wagenmakers, E. J., & Farrell, S. (2004). AIC model selection using Akaike weights. Psychonomic bulletin & review, 11(1), 192-196.")

  if(!ready)
    return()
    
  if(res[["clusters"]] == options[["maxClusters"]] && options[["modelOpt"]] != "validationManual"){
    message <- "The optimum number of clusters is the maximum number of clusters. You might want to adjust the range of optimization."
    evaluationTable$addFootnote(message=message, symbol="<i>Note.</i>")
  }

  row <- data.frame(clusters = res[['clusters']], measure = res[['BSS']]/res[['TSS']], aic = round(res[['AIC']], 2),
                    bic = round(res[['BIC']], 2), Silh = round(res[['Silh_score']], 2), n = round(res[["N"]], 0))
  evaluationTable$addRows(row)
}

.kMeansClusteringInformationTable <- function(options, res, jaspResults, ready){

  if(!is.null(jaspResults[["clusterInfoTable"]])) return() #The options for this table didn't change so we don't need to rebuild it

  if (options[['tableClusterInformation']]){

    clusterInfoTable                        <- createJaspTable("Cluster Information")
    jaspResults[["clusterInfoTable"]]       <- clusterInfoTable
    clusterInfoTable$dependOn(options =c("tableClusterInformation","predictors", "modelOpt",
                                        "noOfClusters","noOfRandomSets", "tableClusterInfoSize", "tableClusterInfoSilhouette",
                                        "tableClusterInfoSumSquares", "tableClusterInfoCentroids", "scaleEqualSD", "tableClusterInfoWSS",
                                        "tableClusterInfoBetweenSumSquares", "tableClusterInfoTotalSumSquares", "maxClusters"))
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

.kMeansWithinSumOfSquaresPlot <- function(options, res, jaspResults, ready){
  if(!ready && options[['withinssPlot']] && options[["modelOpt"]] != "validationManual"){
    p <- createJaspPlot(plot = NULL, title= "Within Sum of Squares Plot", width = 400, height = 300)
    p$setError("Plotting not possible: No analysis has been run.")
    return()
  } else if(ready && options[['withinssPlot']] && options[["modelOpt"]] != "validationManual"){
     if(is.null(jaspResults[["optimPlot"]])){
       
       values <- res[['WithinSumSquares_store']]
       d <- data.frame(x = 1:options[["maxClusters"]], y = values)
       
       xBreaks <- JASPgraphs::getPrettyAxisBreaks(d$x)
       yBreaks <- JASPgraphs::getPrettyAxisBreaks(d$y)
   
       p <- ggplot2::ggplot(data = d, ggplot2::aes(x = x, y = y)) +
         JASPgraphs::geom_point()
   
     	p <- p + ggplot2::scale_x_continuous(name = "Cluster", breaks = xBreaks, limits = range(xBreaks))
     	p <- p + ggplot2::scale_y_continuous(name = "Within Sum of Squares", breaks = yBreaks, limits = range(yBreaks))
      p <- p + JASPgraphs::geom_point(data = data.frame(x = res[['clusters']], y = values[res[['clusters']]]), ggplot2::aes(x = x, y = y), fill = "red")
   
      p <- JASPgraphs::themeJasp(p)
   
     jaspResults[["optimPlot"]] 		 <- createJaspPlot(plot = p, title= "Within Sum of Squares Plot", height = 300, width = 400)
     jaspResults[["optimPlot"]]		   $dependOn(options =c("predictors", "noOfClusters","noOfRandomSets", "noOfIterations", "algorithm",
                                         "aicweights", "modelOpt", "ready", "seed", "withinssPlot", "scaleEqualSD", "maxClusters"))
     jaspResults[["optimPlot"]] 		 $position <- 4
     }
  }
}
