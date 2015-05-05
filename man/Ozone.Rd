\name{Ozone}
\alias{Ozone}
\docType{data}
\title{Ozone dataset available from the UCI repository}
\description{Ozone dataset available from UCI repository, sub-sampled so that the prior probability of class 1 is approximately 0.25. Sample size = 512. Dimension = 72.}
\usage{data("Ozone")}
\format{a list of
\itemize{
\item{\code{x}} {a 512 by 72 matrix containing the feature vectors}
\item{\code{y}} {a vector of length 512 containing the class labels}
}}
\source{Original data available at 

\url{https://archive.ics.uci.edu/ml/datasets/Ozone+Level+Detection}}
\references{Lichman, M. (2013). UCI Machine Learning Repository \url{http://archive.ics.uci.edu/ml}.}
\examples{
data("Ozone")
table(Ozone$y)
}
