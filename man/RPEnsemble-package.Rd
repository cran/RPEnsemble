\name{RPEnsemble-package}
\alias{RPEnsemble-package}
\alias{RPEnsemble}
\docType{package}
\title{Random Projection Ensemble Classification}
\description{Implements the methodology of Cannings and Samworth (2015).  The random projection ensemble classifier is a very general method for classification of high-dimensional data, based on careful combination of the results of applying an arbitrary base classifier to random projections of the feature vectors into a lower-dimensional space. The random projections are divided into non-overlapping blocks, and within each block the projection yielding the smallest estimate of the test error is selected.  The random projection ensemble classifier then aggregates the results of applying the base classifier on the selected projections, with a data-driven voting threshold to determine the final assignment.}
\details{\code{\link{RPChoose}} chooses the projection from a block of size \code{B2} that minimises an estimate of the test error (see Cannings and Samworth, 2015, Section 3), and classifies the training and test sets using the base classifier on the projected data.  \code{\link{RPParallel}} makes many calls to \code{\link{RPChoose}} in parallel.  \code{\link{RPalpha}} chooses the best empirical value of alpha (see Cannings and Samworth, 2015, Section 5.1).  \code{\link{RPEnsembleClass}} combines the results of many base classifications to classify the test set.

The method can be used with any base classifier, any test error estimate and any distribution of the random projections.  This package provides code for the following options: Classifiers -- linear discriminant analysis, quadratic discriminant analysis and the k-nearest neighbour classifier.  Error estimates -- resubstitution and leave-one-out, we also provide code for the sample-splitting method described in Cannings and Samworth (2015, Section 7) (this can be done by setting \code{estmethod = samplesplit}).  Projection distribution -- Haar, Gaussian or axis-aligned projections. 

The package provides the option to add your own base classifier and estimation method, this can be done by editing the code in the function \code{\link{Other.classifier}}.  Moreover, one could edit the \code{\link{RPGenerate}} function to generate projections from different distributions.}

\author{Timothy I. Cannings and Richard J. Samworth

Maintainer: Timothy I. Cannings <t.cannings@statslab.cam.ac.uk>
}
\references{Cannings, T. I. and Samworth, R. J. (2015) Random projection ensemble classification. \url{http://arxiv.org/abs/1504.04595}}

\examples{
#generate data from Model 1
set.seed(101)
Train <- RPModel(2, 50, 100, 0.5)
Test <- RPModel(2, 100, 100, 0.5)

#Classify the training and test set for B1 = 10 independent projections, each 
#one carefully chosen from a block of size B2 = 10, using the "knn" base 
#classifier and the leave-one-out test error estimate
Out <- RPParallel(XTrain = Train$x, YTrain = Train$y, XTest = Test$x, d = 2, 
B1 = 10, B2 = 10, base = "knn", projmethod = "Haar", estmethod = "loo",  
splitsample = FALSE, k = seq(1, 25, by = 3), clustertype = "Default")

#estimate the class 1 prior probability
phat <- sum(Train$y == 1)/50

#choose the best empirical value of the voting threshold alpha
alphahat <- RPalpha(RP.out = Out, Y = Train$y, p1 = phat)

#combine the base classifications  
Class <- RPEnsembleClass(RP.out = Out, n = 50, 
n.test = 100, p1 = phat, alpha = alphahat)

#calculate the error
mean(Class != Test$y)



#Code for sample splitting version of the above
#n.val <- 25
#s <- sample(1:50,25)
#OutSS <- RPParallel(XTrain = Train$x[-s,], YTrain = Train$y[-s], 
#XVal = Train$x[s,], YVal = Train$y[s],  XTest = Test$x, d = 2, 
#B1 = 50, B2 = 10, base = "knn", projmethod = "Haar", estmethod = "samplesplit",
#k = seq(1,13, by = 2), clustertype = "Fork", cores = 1)
#alphahatSS <- RPalpha(RP.out = OutSS, Y = Train$y[s], p1 = phat)
#ClassSS <- RPEnsembleClass(RP.out = OutSS, n.val = 25, n.test = 100, 
#p1 = phat, samplesplit = TRUE, alpha = alphahatSS)
#mean(ClassSS != Test$y)
}
