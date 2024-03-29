% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/clust.R
\name{elbow}
\alias{elbow}
\title{Use this function to find the best number of clusters}
\usage{
elbow(M, max.num.clusters = 10, metric = "euclidean",
  bnc_threshold = 0.2, doPlot = T)
}
\arguments{
\item{M}{A matrix containing vectors for clustering}

\item{max.num.clusters}{Maximum number of clusters}

\item{metric}{Either 'euclidean' or 'spherical' determining the metric used for clustering}

\item{bnc_threshold}{Specifies the threshold for reduction ratio in within group sum of squares}

\item{doPlot}{logical: Would you like to see the elbow plot to determine the number of clusters?}
}
\value{
list: containing three elements: wgss (Within group sum of squares), clst (list of clustering objects), bnc(best number of clusters)
}
\description{
Use this function to find the best number of clusters
}
\examples{
a = elbow(iris[,1:4], doPlot = T)

a$wgss
[1] 681.37060 152.34795  78.85144  71.44525  46.44618  43.68323  37.39514  36.09792  31.79668  29.28663

a$clst[[a$bnc]]$cluster \%>\% table
.
1  2  3  4  5
12 50 24 39 25
}
