\name{RPParallel}
\alias{RPParallel}
\title{Chooses a projection from each block in parallel}
\description{Makes \code{B1} calls to \code{\link{RPChoose}} or \code{\link{RPChooseSS}} in parallel and returns the results as a matrix.}
\usage{
RPParallel(XTrain, YTrain, XVal, YVal, XTest, d, B1 = 500, B2 = 50, 
base = "LDA",projmethod = "Gaussian", estmethod = "training", k = c(3,5,9), 
clustertype = "Default", cores = 1, machines = NULL, seed = 1, ... )
}
\arguments{
  \item{XTrain}{An \code{n} by \code{p} matrix containing the training data feature vectors}
  \item{YTrain}{A vector of length \code{n} containing the classes (either 1 or 2) of the training data}
  \item{XVal}{An \code{n.val} by \code{p} matrix containing the validation data feature vectors}
  \item{YVal}{A vector of length \code{n.val} of the classes (either 1 or 2) of the validation data}
  \item{XTest}{An \code{n.test} by \code{p} matrix containing the test data feature vectors}
  \item{d}{The lower dimension of the image space of the projections}
  \item{B1}{The number of blocks}
  \item{B2}{The size of each block}
  \item{base}{The base classifier one of \code{"knn","LDA","QDA"} or \code{"other"}}
  \item{k}{The options for \code{k} if base is \code{"knn"}}
  \item{projmethod}{\code{"Haar"}, \code{"Gaussian"} or \code{"axis"}}
  \item{estmethod}{Method for estimating the test errors to choose the projection: either training error \code{"training"}, leave-one-out \code{"loo"}, or sample split \code{"samplesplit"}}
  \item{clustertype}{The type of cluster: \code{"Default"} uses just one core, \code{"Fork"} uses a single machine, \code{"Socket"} uses many machines. Note \code{"Fork"} and \code{"Socket"} are not supported on windows.}
  \item{cores}{Required only if \code{clustertype==Fork}: the number of computer cores to use (note: \code{cores > 1} not supported on Windows)}
  \item{machines}{Required only if \code{clustertype==Socket}: the names of the machines to use e.g. \code{c("Computer1", "Computer2")} (not supported on Windows)}
  \item{seed}{If not \code{NULL}, sets random seed for reproducible results}
  \item{\dots}{Optional further arguments if \code{base = "other"}}
}
\details{Makes \code{B1} calls to \code{\link{RPChoose}} or \code{\link{RPChooseSS}} in parallel.}
\value{If \code{estmethod == "training"} or \code{"loo"} , then returns an \code{n+n.test} by \code{B1} matrix, each row containing the result of a call to \code{\link{RPChoose}}. If \code{estmethod == "samplesplit"}, then returns an \code{n.val+n.test} by \code{B1} matrix, each row containing the result of a call to \code{\link{RPChooseSS}}.}
\references{Cannings, T. I. and Samworth, R. J. (2015) Random projection ensemble classification.
\url{http://arxiv.org/abs/1504.04595}}
\author{Timothy I. Cannings and Richard J. Samworth}

\seealso{\code{\link{RPChoose}}, \code{\link{RPChooseSS}}}
\examples{
Train <- RPModel(1, 50, 100, 0.5)
Test <- RPModel(1, 100, 100, 0.5)
Out <- RPParallel(XTrain = Train$x, YTrain = Train$y, XTest = Test$x, d = 2, B1 = 10, 
B2 = 10, base = "LDA", projmethod = "Haar", estmethod = "training")
colMeans(Out)
}
