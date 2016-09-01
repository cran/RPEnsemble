\name{RPChoose}
\alias{RPChoose}
\title{Chooses projection}
\description{Chooses a the best projection from a set of size \code{B2} based on a test
  error estimate, then classifies the training and test sets using the chosen projection.}
\usage{
RPChoose(XTrain, YTrain, XTest, d, B2 = 10, base = "LDA", k = c(3,5), 
projmethod = "Haar", estmethod = "training", ...)
}
\arguments{
  \item{XTrain}{An \code{n} by \code{p} matrix containing the training data feature vectors}
  \item{YTrain}{A vector of length \code{n} of the classes (either 1 or 2) of the training data}
  \item{XTest}{An \code{n.test} by \code{p} matrix of the test data}
  \item{d}{The lower dimension of the image space of the projections}
  \item{B2}{The block size}
  \item{base}{The base classifier one of \code{"knn","LDA","QDA"} or \code{"other"}}
  \item{k}{The options for \code{k} if base is \code{"knn"}}
  \item{projmethod}{Either \code{"Haar"}, \code{"Gaussian"} or \code{"axis"}}
  \item{estmethod}{Method for estimating the test errors to choose the projection: either training error \code{"training"} or leave-one-out \code{"loo"}}
  \item{\dots}{Optional further arguments if \code{base = "other"}}
}
\details{Randomly projects the the data \code{B2} times. Chooses the projection yielding the smallest estimate of the test error. Classifies the training set (via the same method as \code{estmethod}) and test set using the chosen projection.}
\value{Returns a vector of length \code{n + n.test}: the first \code{n} entries are the estimated classes of the training set, the last \code{n.test} are the estimated classes of the test set.} 
\references{Cannings, T. I. and Samworth, R. J. (2015) Random projection ensemble classification. \url{http://arxiv.org/abs/1504.04595}}
\author{Timothy I. Cannings and Richard J. Samworth}
\note{Resubstitution method unsuitable for the \code{k}-nearest neighbour classifier.}

\seealso{\code{\link{RPParallel}},  \code{\link{RPChooseSS}}, \code{lda}, \code{qda}, \code{knn}}
\examples{
set.seed(100)
Train <- RPModel(1, 50, 100, 0.5)
Test <- RPModel(1, 100, 100, 0.5)
Choose.out5 <- RPChoose(XTrain = Train$x, YTrain = Train$y, XTest = Test$x, 
d = 2, B2 = 5, base = "QDA", projmethod = "Haar", estmethod = "loo")
Choose.out10 <- RPChoose(XTrain = Train$x, YTrain = Train$y, XTest = Test$x, 
d = 2, B2 = 10, base = "QDA", projmethod = "Haar", estmethod = "loo")
sum(Choose.out5[1:50] != Train$y)
sum(Choose.out10[1:50] != Train$y)
sum(Choose.out5[51:150] != Test$y)
sum(Choose.out10[51:150] != Test$y)
}
