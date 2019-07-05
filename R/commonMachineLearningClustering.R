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

.tsneClusterPlot <- function(dataset, options, clusterResult, type, jaspResults, ready, position){

  if(!is.null(jaspResults[["plot2dCluster"]]) || !options[["plot2dCluster"]]) return()

  clusterPlot <- createJaspPlot(plot = NULL, title = "T-sne Cluster Plot", width = 400, height = 300)
  clusterPlot$position <- position
  clusterPlot$dependOn(options = c("predictors", "noOfClusters","noOfRandomSets", "algorithm", "eps", "minPts", "distance",
                                          "noOfIterations", "modelOpt", "ready", "seed", "plot2dCluster", "maxClusters", "scaleEqualSD", "seedBox",
                                          "linkage", "m"))
  jaspResults[["plot2dCluster"]] <- clusterPlot

  if(!ready) return()
 
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