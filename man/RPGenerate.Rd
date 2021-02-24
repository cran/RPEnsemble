\name{RPGenerate}
\alias{RPGenerate}
\title{Generates random matrices}
\description{Generates \code{B2} random \code{p} by \code{d} matrices according to Haar measure, Gaussian or axis-aligned projections}
\usage{RPGenerate(p = 100, d = 10, method = "Haar", B2 = 10)}
\arguments{
  \item{p}{The original data dimension}
  \item{d}{The lower dimension}
  \item{method}{Projection distribution, either \code{"Haar"} for Haar distributed projections, \code{"Gaussian"} for Gaussian distributed projections with i.i.d. \code{N(0,1/p)} entries, \code{"axis"} for uniformly distributed axis aligned projections, or \code{"other"} for user defined method}
  \item{B2}{the number of projections}
  }
\value{returns \code{B2} \code{p} by \code{d} random matrices as a single \code{p} by \code{d*B2} matrix}
\references{Cannings, T. I. and Samworth, R. J. (2017) Random-projection ensemble classification, J. Roy. Statist. Soc., Ser. B. (with discussion), 79, 959--1035}
\author{Timothy I. Cannings and Richard J. Samworth}



\examples{
R1 <- RPGenerate(p = 20, d = 2, "Haar", B2 = 3)
t(R1)\%*\%R1
R2 <- RPGenerate(p = 20, d = 2, "Gaussian", B2 = 3)
t(R2)\%*\%R2
R3 <- RPGenerate(p = 20, d = 2, "axis", B2 = 3)
colSums(R3)
rowSums(R3)
}
