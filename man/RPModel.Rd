\name{RPModel}
\alias{RPModel}
\title{Generate pairs from joint distribution}
\description{Generates data from the models described in Cannings and Samworth (2015).}
\usage{
RPModel(Model.No, n, p, Pi = 1/2)
}
\arguments{
  \item{Model.No}{Model number}
  \item{n}{Sample size}
  \item{p}{Data dimension}
  \item{Pi}{Class one prior probability}
}
\value{
\item{x}{An \code{n} by \code{p} data matrix -- \code{n} observations of the \code{p}-dimensional features}
\item{y}{A vector of length \code{n} containing the classes (either 1 or 2)}
}
\references{Cannings, T. I. and Samworth, R. J. (2015) Random projection ensemble classification. 
\url{http://arxiv.org/abs/1504.04595}}
\author{Timothy I. Cannings and Richard J. Samworth}
\note{Model 4 requires p = 50.}


\examples{
Data <- RPModel(Model.No = 1, 100, 50, Pi = 1/2)
table(Data$y)
colMeans(Data$x[Data$y==1,])
colMeans(Data$x[Data$y==2,])
}
