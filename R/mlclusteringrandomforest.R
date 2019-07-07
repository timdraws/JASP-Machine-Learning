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

MLClusteringRandomForest <- function(jaspResults, dataset, options, ...) {

# Preparatory work
dataset <- .readDataClusteringAnalyses(dataset, options)
.errorHandlingClusteringAnalyses(dataset, options)

# Check if analysis is ready to run
ready  <- .clusterAnalysesReady(options)

# Run the analysis and save the results in the state
.clustering(dataset, options, jaspResults, ready, type = "randomForest")

# create the model summary table
.clusteringTable(options, jaspResults, ready, type = "randomForest")

# create the cluster information table
.clusterInformationTable(options, jaspResults, ready, type = "randomForest")

# Create the within sum of squares plot
.clusterOptimizationPlot(dataset, options, jaspResults, ready, position = 3)

# Create the cluster plot
.tsneClusterPlot(dataset, options, jaspResults, ready, type = "randomForest", position = 4)

}

.randomForestClustering <- function(dataset, options, jaspResults){

if(options[["modelOpt"]] == "validationManual"){
      
    rfit <- randomForest::randomForest(x = dataset[, .v(options[["predictors"]])], 
										y = NULL, 
										ntree = options[["noOfTrees"]], 
										proximity = TRUE, 
										oob.prox = TRUE)

    clusters <- options[['noOfClusters']]

  } else {

    avg_silh <- numeric(options[["maxClusters"]] - 1)
    wssStore <- numeric(options[["maxClusters"]] - 1)
    clusterRange <- 2:options[["maxClusters"]]
    aicStore <-  numeric(options[["maxClusters"]] - 1)
    bicStore <-  numeric(options[["maxClusters"]] - 1)

    jaspResults$startProgressbar(length(clusterRange))

	rfit_tmp <- randomForest::randomForest(x = dataset[, .v(options[["predictors"]])], 
									y = NULL, 
									ntree = options[["noOfTrees"]], 
									proximity = TRUE, 
									oob.prox = TRUE)
	hrfit_tmp <- hclust(as.dist(1 - rfit_tmp$proximity), method = "ward.D2")

    for (i in clusterRange) {

  	  pred.values <- cutree(hrfit_tmp, k = i)
      silh <- summary(cluster::silhouette(pred.values, dist(dataset[, .v(options[["predictors"]])])))
      avg_silh[i - 1] <- silh[["avg.width"]]

	  m <- dim(as.data.frame(dataset[, .v(options[["predictors"]])]))[2]
  
    wss_tmp <- numeric(i)
    for(j in 1:i) {
      if (m == 1) {
        wss_tmp[j] <- .ss(dataset[, .v(options[["predictors"]])][pred.values == j])
      } else {
        wss_tmp[j] <- .ss(dataset[, .v(options[["predictors"]])][pred.values == j,])
      }
    }

	wssStore[i - 1] <- sum(wss_tmp)

	n <- length(pred.values)
	k <- i
	D <- sum(wss_tmp)
	aicStore[i - 1] <- D + 2*m*k
	bicStore[i - 1] <- D + log(n)*m*k
      
    jaspResults$progressbarTick()
  }

  clusters <- base::switch(options[["modelOpt"]],
                            "validationSilh" = clusterRange[which.max(avg_silh)],
                            "validationAIC" = clusterRange[which.min(aicStore)],
                            "validationBIC" = clusterRange[which.min(bicStore)])

	rfit <- randomForest::randomForest(x = dataset[, .v(options[["predictors"]])], 
										y = NULL, 
										ntree = options[["noOfTrees"]], 
										proximity = TRUE, 
										oob.prox = TRUE)

  }

  hrfit <- hclust(as.dist(1 - rfit$proximity), method = "ward.D2")
  pred.values <- cutree(hrfit, k = clusters)

  clusters <- clusters
  size <- as.numeric(table(pred.values))

  m <- dim(as.data.frame(dataset[, .v(options[["predictors"]])]))[2]
  
  wss <- numeric(clusters)
  for(i in 1:clusters) {
    if (m == 1) {
      wss[i] <- .ss(dataset[, .v(options[["predictors"]])][pred.values == i])
    } else {
      wss[i] <- .ss(dataset[, .v(options[["predictors"]])][pred.values == i,])
    }
  }

  tss <- .tss(dist(dataset[, .v(options[["predictors"]])]))

  n <- length(pred.values)
  k <- clusters
  D <- sum(wss)
  aic <- D + 2*m*k
  bic <- D + log(n)*m*k

  silhouettes <- summary(cluster::silhouette(pred.values, dist(dataset[, .v(options[["predictors"]])])))
  Silh_score <- silhouettes[["avg.width"]]
  silh_scores <- silhouettes[["clus.avg.widths"]]

  clusterResult <- list()
  clusterResult[["pred.values"]] <- pred.values
  clusterResult[['clusters']] <- clusters
  clusterResult[["N"]] <- nrow(dataset)
  clusterResult[['size']] <- size
  clusterResult[['WSS']] <- wss
  clusterResult[['TSS']] <- tss
  clusterResult[['BSS']] <- clusterResult[['TSS']] - sum(clusterResult[['WSS']])
  clusterResult[['AIC']] <- aic
  clusterResult[['BIC']] <- bic
  clusterResult[['Silh_score']] <- Silh_score
  clusterResult[['silh_scores']] <- silh_scores

  if(options[["modelOpt"]] != "validationManual"){
    clusterResult[['silhStore']] <- avg_silh
    clusterResult[["aicStore"]] <- aicStore
    clusterResult[["bicStore"]] <- bicStore
    clusterResult[["wssStore"]] <- wssStore
  }

  return(clusterResult)
}

