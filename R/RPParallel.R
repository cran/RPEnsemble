RPParallel <-
function(XTrain #n by p trining data matrix
       , YTrain #n vector of the classes of the trining samples
       , XVal = NULL#n.val by p validation data matrix
       , YVal = NULL #n.val vector of the classes of the validation samples
       , XTest  #n.test by p test data matrix
       , d      #dimension to project into
       , B1 = 500 #number of blocks
       , B2 = 50 #block size
       , base = "LDA" # base classifier, eg "knn","LDA","QDA", or "Other"
       , projmethod = "Gaussian"
       , estmethod = "training"
       , k = c(3,5,9) # possible k
       , clustertype = "Default" #computing cluster type
       , cores = 1
       , machines = NULL  #names of computers on network to form cluster
       , seed = 1 #reproducability seed
       , ... )
{
  if(clustertype == "Default"){cluster = makePSOCKcluster(1)}
  if(clustertype == "Fork"){cluster = makeForkCluster(cores)}
  if(clustertype == "Socket"){cluster = makePSOCKcluster(names = machines)}
  
  clusterExport(cluster, c(ls(),"knn.cv", "knn", "lda", "qda"), envir = environment())

  if(is.null(seed) == FALSE) {clusterSetRNGStream(cluster, seed)}

  if (estmethod == "samplesplit"){
      n.val <- length(YVal)
      RP.out <- simplify2array(parLapply(cl = cluster, 1:B1, function(x){return(RPChooseSS(XTrain, YTrain, XVal, YVal, XTest, d, B2, base, k, projmethod))}))
  }
  else{
    RP.out <- simplify2array(parLapply(cl = cluster, 1:B1, function(x){return(RPChoose(XTrain, YTrain, XTest, d, B2, base, k, projmethod, estmethod))}))
  }
  
  stopCluster(cluster) 
  
 return (RP.out)
}
