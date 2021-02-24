\name{R}
\alias{R}
\docType{data}
\title{A rotation matrix}
\description{The 100 by 100 rotation matrix used in Model 2 in Cannings and Samworth (2017).}
\usage{data(R)}
\format{A 100 by 100 rotation matrix}
\references{Cannings, T. I. and Samworth, R. J. (2017) Random-projection ensemble classification, J. Roy. Statist. Soc., Ser. B. (with discussion), 79, 959--1035}
\examples{
data(R)
head(R\%*\%t(R))
}
