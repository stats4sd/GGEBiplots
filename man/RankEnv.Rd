% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/RankEnv.R
\name{RankEnv}
\alias{RankEnv}
\title{Ranking Environments Biplot}
\usage{
RankEnv(GGEModel, ...)
}
\arguments{
\item{GGEModel}{An object of class \code{GGEModel} or \code{gge}}

\item{...}{Other arguments sent to \code{\link[GGEBiplots]{GGEPlot}}}
}
\description{
Ranking environments with respect to the ideal environment
}
\examples{
data(Ontario)
GGE1<-GGEModel(Ontario)
RankEnv(GGE1)
}
\keyword{GGE}
