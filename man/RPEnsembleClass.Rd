\name{RPEnsembleClass}
\alias{RPEnsembleClass}
\title{Classifies the test set using the random projection ensemble classifier}
\description{Performs a biased majority vote over \code{B1} base classifications to assign the test set.}
\usage{RPEnsembleClass(RP.out, n , n.val, n.test, p1, samplesplit, alpha, ...)}
\arguments{
  \item{RP.out}{The result of a call to \code{\link{RPParallel}}}
  \item{n}{Training set sample size}
  \item{n.test}{Test set sample size}
  \item{n.val}{Validation set sample size}
  \item{p1}{Prior probability estimate}
  \item{samplesplit}{\code{TRUE} if using sample-splitting method}
  \item{alpha}{The voting threshold}
  \item{\dots}{Optional further arguments if \code{base = "other"}}
}
\details{An observation in the test set is assigned to class 1 if \code{B1*alpha} or more of the base classifications are class 1 (otherwise class 2).}
\value{A vector of length \code{n.test} containing the class predictions of the test set (either 1 or 2).}
\references{Cannings, T. I. and Samworth, R. J. (2017) Random-projection ensemble classification, J. Roy. Statist. Soc., Ser. B. (with discussion), 79, 959--1035}
\author{Timothy I. Cannings and Richard J. Samworth}

\seealso{\code{\link{RPParallel}}, \code{\link{RPalpha}}, \code{\link{RPChoose}}}
\examples{
Train <- RPModel(1, 50, 100, 0.5)
Test <- RPModel(1, 100, 100, 0.5)
Out <- RPParallel(XTrain = Train$x, YTrain = Train$y, XTest = Test$x, 
d = 2, B1 = 50, B2 = 10, base = "LDA", projmethod = "Haar", 
estmethod = "training", clustertype = "Default")
Class <- RPEnsembleClass(RP.out = Out, n = length(Train$y), 
n.test = nrow(Test$x), p1 = sum(Train$y == 1)/length(Train$y),  
splitsample = FALSE,  alpha = RPalpha(Out, Y = Train$y, 
p1 = sum(Train$y == 1)/length(Train$y)))
mean(Class != Test$y)
}
