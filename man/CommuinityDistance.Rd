% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/GetDistance_v2.r
\name{CommuinityDistance}
\alias{CommuinityDistance}
\title{module distance function}
\usage{
CommuinityDistance(
  network,
  module.genelist,
  randomMethod = c("None", "RandC", "RandCD", "RandCM", "RandCDM"),
  random = 1000,
  ratio = 0.1,
  nCore = 1,
  method = c("closest", "shortest", "kernel", "centre", "separation")
)
}
\arguments{
\item{network}{network dataframe}

\item{module.genelist}{module genes list}

\item{random}{the number of random network formation and distance calculation}

\item{method}{distance measuring method}

\item{overlap_filtering}{overlap genes filtering (TRUE/FALSE)}
}
\value{
distance results, module.genelist, network
}
\description{
Calculate the distance and z_score of the communities
use.f can be choosed in the DistFunction.r
ex) get.shortest.dist, get.kernel.dist, get.centre.dist, get.separation.dist, get.closest.dist
As a default, get.closest.dist function is used to measure the distance between communities
User also can make the dist function and use it for calculating module distance
This function is made to know the z-score of a measured distance from distances of degree-preserved random networks
}
