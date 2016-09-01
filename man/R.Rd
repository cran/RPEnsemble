\name{R}
\alias{R}
\docType{data}
\title{A rotation matrix}
\description{The 100 by 100 rotation matrix used in Model 2 in Cannings and
  Samworth (2016).}
\usage{data(R)}
\format{A 100 by 100 rotation matrix}
\references{
Cannings, T. I. and Samworth, R. J. (2016) Random projection ensemble classification. 
\url{http://arxiv.org/abs/1504.04595}
}
\examples{
data(R)
head(R\%*\%t(R))
}
