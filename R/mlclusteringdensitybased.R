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
  
  #install.packages("dbscan", repos="https://cloud.r-project.org")
  #install.packages("dbscan", repos="https://cloud.r-project.org")

  # read variables ##
  dataset             <- .densityClusteringReadData(dataset, options)
  
  # error handling & code variable names in base64
  .densityClusteringErrorHandling(dataset, options)
  ready               <- length(options[["predictors"]][options[["predictors"]] != ""] > 0)
  
  # Run the analysis and save the results
  res                 <- .densityClustering(dataset, options, jaspResults, ready)
  
  # create the evaluation table
  .densityClusteringSummaryTable(res, options, jaspResults, ready)
  
  # create the cluster information table
  .densityClusteringInformationTable(options, res, jaspResults, ready)
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
  
  if(!is.null(jaspResults[["res"]]$object)) return(jaspResults[["res"]]$object)
  
  # set the seed so that every time the same set is chosen (to prevent random results) ##
  if(options[["seedBox"]])
    set.seed(options[["seed"]])
  
  if(ready){
    if(options[["modelOpt"]] == "validationManual"){
      res <- .densityClusteringManual(dataset, options)
    } else {
      res <- .densityClusteringOptimized(dataset, options)
    }
  } else {
    res <- list()
  }
  jaspResults[["res"]] <- createJaspState(res)
  jaspResults[["res"]]$dependOn(c("predictors", "eps", "minPts", "modelOpt", "seed", 
                                         "maxClusters", "seedBox", "scaleEqualSD"))
  
  return(jaspResults[["res"]]$object)
}

.densityClusteringManual <- function(dataset, options){
  
  densityfit <- dbscan::dbscan(as.data.frame(dataset[, .v(options[["predictors"]])]),
                        eps = options[['eps']],
                        minPts = options[['minPts']])
  
  
  dlabels <- densityfit$cluster

  res <- list()
  res[['Predictions']] <- data.frame(
    'Observation' = 1:nrow(dataset),
    'Cluster' = densityfit$cluster
  )
  res[["pred.values"]] <- densityfit$cluster
  res[['eps']] <- options[['eps']]
  res[["N"]] <- nrow(dataset)
  #res[['size']] <- cfit$size
  return(res)
}

.densityClusteringOptimized <- function(dataset, options){
  
  df <- as.data.frame(dataset[, .v(options[["predictors"]])])
  
  minPts <- dim(df)[2]*2
  optics_eps <- dbscan::optics(df, minPts = minPts) 
  eps <- optics_eps$eps
  densityfit_opt <- dbscan::dbscan(df, eps = eps, minPts = minPts)
  
  
  dlabels <- densityfit_opt$cluster
  
  res <- list()
  res[['Predictions']] <- data.frame(
    'Observation' = 1:nrow(dataset),
    'Cluster' = densityfit_opt$cluster
  )
  res[["pred.values"]] <- densityfit_opt$cluster
  res[['eps']] <- options[['eps']]
  res[["N"]] <- nrow(dataset)
  #res[['size']] <- cfit$size
  return(res)
}
  
.densityClusteringSummaryTable <- function(res, options, jaspResults, ready){
  
  if(!is.null(jaspResults[["evaluationTable"]])) return() #The options for this table didn't change so we don't need to rebuild it
  
  evaluationTable                       <- createJaspTable("Density based Clustering Model Summary")
  jaspResults[["evaluationTable"]]      <- evaluationTable
  jaspResults[["evaluationTable"]]$position <- 1
  evaluationTable$dependOn(c("predictors", "eps", "minPts", "modelOpt", "seed", "maxClusters", "scaleEqualSD"))
  
  evaluationTable$addColumnInfo(name = 'eps', title = 'eps', type = 'integer')
  evaluationTable$addColumnInfo(name = 'n', title = 'N', type = 'number', format = 'dp:1')
  
  if(!ready)
    return()
    
  row <- data.frame(eps = res[['eps']], n = round(res[["N"]], 0))
  evaluationTable$addRows(row)
    
  }

.densityClusteringInformationTable <- function(options, res, jaspResults, ready){
  
  if(!is.null(jaspResults[["clusterInfoTable"]])) return() #The options for this table didn't change so we don't need to rebuild it
  
  if (options[['tableClusterInformation']]){
    
    clusterInfoTable                        <- createJaspTable("Cluster Information")
    jaspResults[["clusterInfoTable"]]       <- clusterInfoTable
    clusterInfoTable$dependOn(c("tableClusterInformation","predictors", "eps", "minPts", "modelOpt", "scaleEqualSD", "maxClusters"))
    clusterInfoTable$position               <- 2
    clusterInfoTable$transpose              <- TRUE
    
    clusterInfoTable$addColumnInfo(name = 'cluster', title = 'Cluster', type = 'integer')
    #clusterInfoTable$addColumnInfo(name = 'size', title = 'Size', type = 'integer')

    if(!ready)
      return()
    
    #size <- res[["size"]]
    
    row <- data.frame(cluster = cluster)
    
    clusterInfoTable$addRows(row)
  }
}





