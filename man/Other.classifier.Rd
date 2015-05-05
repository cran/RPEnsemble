\name{Other.classifier}
\alias{Other.classifier}
\title{The users favourite classifier}
\description{User defined code to convert existing \code{R} code for a base classifier into the correct format.}
\usage{Other.classifier(x, grouping, xTest, CV, ...)}
\arguments{
  \item{x}{An \code{n} by \code{p} matrix containing the training dataset}
  \item{grouping}{A vector of length \code{n} containing the training data classes}
  \item{xTest}{An \code{n.test} by \code{p} test dataset}
  \item{CV}{If \code{TRUE} perform cross-validation (or otherwise) to classify training set. If \code{FALSE}, then classify the test set.}
  \item{\dots}{Optional arguments e.g. tuning parameters}
}
\details{User editable code for your choice of base classifier. Once edited, it can be used by setting \code{base = "other"} in \code{\link{RPChoose}}, \code{\link{RPChooseSS}} or \code{\link{RPParallel}}.}
\value{
 \item{class}{A vector of classes of the training or test set}
 }
\references{Cannings, T. I. and Samworth, R. J. (2015) Random projection ensemble classification. \url{http://arxiv.org/abs/1504.04595}
}
\author{Timothy I. Cannings and Richard J. Samworth}
