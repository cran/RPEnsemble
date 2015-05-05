\name{RPGenerate}
\alias{RPGenerate}
\title{Generate a random p by d matrix}
\description{Generates a random projection according to Haar measure or a uniform distribution on axis-aligned projections.}
\usage{RPGenerate(p = 100, d = 10, method = "Haar")}
\arguments{
  \item{p}{The original data dimension}
  \item{d}{The lower dimension}
  \item{method}{Projection distribution: either \code{"Haar"} for Haar distributed projections, \code{"axis"} for uniformly distributed axis-aligned projections, or \code{"other"} for user defined method}
  }
\value{Returns a \code{p} by \code{d} random matrix}
\references{Cannings, T. I. and Samworth, R. J. (2015) Random projection ensemble classification. 
\url{http://arxiv.org/abs/1504.04595}}
\author{Timothy I. Cannings and Richard J. Samworth}



\examples{
R1 <- RPGenerate(20, 2, "Haar")
t(R1)\%*\%R1
R2 <- RPGenerate(20, 2, "axis")
colSums(R2)
rowSums(R2)
}
