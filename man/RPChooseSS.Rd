\name{RPChooseSS}
\alias{RPChooseSS}
\title{A sample splitting version of \code{\link{RPChoose}}}
\description{Chooses the best projection based on an estimate of the
  test error of the classifier with training data \code{(XTrain, YTrain)}, the estimation method counts the number of errors made on the validation set \code{(XVal, YVal)}.}
\usage{
RPChooseSS(XTrain, YTrain, XVal, YVal, XTest, d, B2 = 100, base = "LDA",  
k = c(3, 5), projmethod = "Haar", ...)
}
\arguments{
  \item{XTrain}{An \code{n} by \code{p} matrix containing the training data feature vectors}
  \item{YTrain}{A vector of length \code{n} of the classes (either 1 or 2) of the training data}
  \item{XVal}{An \code{n.val} by \code{p} matrix containing the validation data feature vectors}
  \item{YVal}{A vector of length \code{n.val} of the classes (either 1 or 2) of the validation data}
  \item{XTest}{An \code{n.test} by \code{p} matrix of the test data feature vectors}
  \item{d}{The lower dimension of the image space of the projections}
  \item{B2}{The block size}
  \item{base}{The base classifier one of \code{"knn","LDA","QDA"} or \code{"other"}}
  \item{k}{The options for \code{k} if \code{base = "knn"}}
  \item{projmethod}{Either \code{"Haar"}, \code{"Gaussian"} or \code{"axis"}}
  \item{\dots}{Optional further arguments if \code{base = "other"}}
}
\details{Maps the the data using \code{B2} random projections. For each projection the validation set is classified using the the training set and the projection yielding the smallest number of errors over the validation set is retained. The validation set and test set are then classified using the chosen projection.}  
\value{Returns a vector of length \code{n.val + n.test}: the first \code{n.val} entries are the estimated classes of the validation set, the last \code{n.test} are the estimated classes of the test set.} 
\references{Cannings, T. I. and Samworth, R. J. (2017) Random-projection ensemble classification, J. Roy. Statist. Soc., Ser. B. (with discussion), 79, 959--1035}
\author{Timothy I. Cannings and Richard J. Samworth}

\seealso{\code{\link{RPParallel}}, \code{\link{RPChoose}}, \code{lda}, \code{qda}, \code{knn}}
\examples{
set.seed(100)
Train <- RPModel(1, 50, 100, 0.5)
Validate <- RPModel(1, 50, 100, 0.5)
Test <- RPModel(1, 100, 100, 0.5)
Choose.out5 <- RPChooseSS(XTrain = Train$x, YTrain = Train$y, XVal = Validate$x, 
YVal = Validate$y, XTest = Test$x, d = 2, B2 = 5, base = "QDA", projmethod = "Haar")
Choose.out10 <- RPChooseSS(XTrain = Train$x, YTrain = Train$y, XVal = Validate$x, 
YVal = Validate$y, XTest = Test$x, d = 2, B2 = 10, base = "QDA", projmethod = "Haar")
sum(Choose.out5[1:50] != Validate$y)
sum(Choose.out10[1:50] != Validate$y)
sum(Choose.out5[51:150] != Test$y)
sum(Choose.out10[51:150] != Test$y)
}
