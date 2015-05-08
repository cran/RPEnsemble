\name{RPalpha}
\alias{RPalpha}
\title{Choose alpha}
\description{Chooses the best empirical value of the cutoff \code{alpha}, based on the
leave-one-out, resubstitution or sample-split estimates of the class labels.}
\usage{RPalpha(RP.out, Y, p1)}
\arguments{
  \item{RP.out}{The result of a call to \code{\link{RPParallel}}}
  \item{Y}{Vector of length \code{n} or \code{n.val} containing the training or validation dataset classes}
  \item{p1}{(Empirical) prior probability}
}
\details{See precise details in Cannings and Samworth (2015, Section 5.1).}
\value{
  \item{alpha}{The value of \code{alpha} that minimises the empirical error}
}
\references{Cannings, T. I. and Samworth, R. J. (2015) Random projection ensemble classification. 
\url{http://arxiv.org/abs/1504.04595}
}
\author{Timothy I. Cannings and Richard J. Samworth}

\seealso{\code{\link{RPParallel}}}
\examples{
Train <- RPModel(1, 50, 20, 0.5)
Test <- RPModel(1, 100, 20, 0.5)
Out <- RPParallel(XTrain = Train$x, YTrain = Train$y, XTest = Test$x, 
d = 2, B1 = 100, B2 = 10, base = "LDA", projmethod = "Haar", 
estmethod = "resub", cores = 1)
alpha <- RPalpha(RP.out = Out, Y = Train$y, p1 = sum(Train$y == 1)/length(Train$y))
alpha

}
