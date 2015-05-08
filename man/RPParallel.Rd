\name{RPParallel}
\alias{RPParallel}
\title{Chooses a projection from each block in parallel}
\description{Makes \code{B1} calls to \code{\link{RPChoose}} or \code{\link{RPChooseSS}} in parallel and returns the results as a matrix.}
\usage{
RPParallel(XTrain, YTrain, XVal, YVal, XTest, d, B1 = 100, B2 = 100, 
base = "LDA", projmethod = "Haar", estmethod = "resub", k = c(3, 5), 
cores = 2, splitsample = FALSE,  ...)
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
  \item{k}{The options for k if base is \code{"knn"}}
  \item{projmethod}{Either \code{"Haar"} or \code{"axis"}}
  \item{estmethod}{Method for estimating the test errors to choose the
    projection: either resubstitution \code{"resub"} or leave-one-out \code{"loo"}}
  \item{cores}{The number of computer cores to use (note: \code{cores > 1} not supported on Windows)}
  \item{splitsample}{If \code{TRUE} will use the validation set \code{(XVal, YVal)} to estimate the test error in \code{\link{RPChooseSS}}. If \code{FALSE} will use \code{estmethed} to estimate the test error in \code{\link{RPChoose}}}
  \item{\dots}{Optional further arguments if \code{base = "other"}}
}
\details{Makes \code{B1} calls to \code{\link{RPChoose}} or \code{\link{RPChooseSS}} in parallel.}
\value{If \code{splitsample = FALSE}, then returns an \code{n+n.test} by \code{B1} matrix, each row containing the result of a call to \code{\link{RPChoose}}. If \code{splitsample = TRUE}, then returns an \code{n.val+n.test} by \code{B1} matrix, each row containing the result of a call to \code{\link{RPChooseSS}}.}
\references{Cannings, T. I. and Samworth, R. J. (2015) Random projection ensemble classification.
\url{http://arxiv.org/abs/1504.04595}}
\author{Timothy I. Cannings and Richard J. Samworth}

\seealso{\code{\link{RPChoose}}, \code{\link{RPChooseSS}}}
\examples{
Train <- RPModel(1, 50, 20, 0.5)
Test <- RPModel(1, 100, 20, 0.5)
Out <- RPParallel(XTrain = Train$x, YTrain = Train$y, XTest = Test$x, 
d = 2, B1 = 100, B2 = 10, base = "LDA", projmethod = "Haar", 
estmethod = "resub", cores = 1)
colMeans(Out)
}
