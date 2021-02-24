\name{RPModel}
\alias{RPModel}
\title{Generate pairs \code{(x,y)} from joint distribution}
\description{Generates data from the models described in Cannings and Samworth (2017)}
\usage{
RPModel(Model.No, n, p, Pi = 1/2)
}
\arguments{
  \item{Model.No}{Model Number}
  \item{n}{Sample size}
  \item{p}{Data dimension}
  \item{Pi}{Class one prior probability}
}
\value{
\item{x}{An \code{n} by \code{p} data matrix -- \code{n} observations of the \code{p}-dimensional features}
\item{y}{A vector of length \code{n} containing the classes (either 1 or 2)}
}
\references{Cannings, T. I. and Samworth, R. J. (2017) Random-projection ensemble classification, J. Roy. Statist. Soc., Ser. B. (with discussion), 79, 959--1035}
\author{Timothy I. Cannings and Richard J. Samworth}
\note{Models 1 and 2 require \code{p = 100} or \code{1000}.}


\examples{
Data <- RPModel(Model.No = 1, 100, 100, Pi = 1/2)
table(Data$y)
colMeans(Data$x[Data$y==1,])
colMeans(Data$x[Data$y==2,])
}
