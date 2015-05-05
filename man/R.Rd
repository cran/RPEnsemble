\name{R}
\alias{R}
\docType{data}
\title{A rotation matrix}
\description{The 50 by 50 rotation matrix used in Model 4 in Cannings and
  Samworth (2015).}
\usage{data("R")}
\format{A 50 by 50 matrix}
\references{
Cannings, T. I. and Samworth, R. J. (2015) Random projection ensemble classification. 
\url{http://arxiv.org/abs/1504.04595}
}
\examples{
data(R)
R\%*\%t(R)
}
