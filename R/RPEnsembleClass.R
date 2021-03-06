RPEnsembleClass <-
function(RP.out # the result of a call to RPParallel
        ,n  # training sample size
        ,n.val #validation set size if samplesplit = TRUE
        ,n.test #test sample size
        ,p1 #(estimate of) prior probability
        ,samplesplit = FALSE #split sample TRUE/FALSE
        ,alpha  #voting cutoff alpha
        ,... )
    {
  if (samplesplit == TRUE){
    Test.Class <- RP.out[n.val + 1:n.test, ]
  }
  else{Test.Class <- RP.out[n + 1:n.test, ]}
  if(n.test == 1){vote <- mean(Test.Class, na.rm = TRUE)}
  if(n.test > 1){vote <- rowMeans(Test.Class, na.rm = TRUE)}
  Class  <- 1 + as.numeric(vote > alpha)
  return(Class)
}
