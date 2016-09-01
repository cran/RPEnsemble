RPChooseSS <-
    function(XTrain #n by p training data matrix
           , YTrain #n vector of the classes of the training samples
           , XVal #n.val by p validation data matrix
           , YVal #n.val vector of the classes of the validation samples
           , XTest  #n.test by p test data matrix
           , d      #dimension to project into
           , B2 = 100 #block size
           , base = "LDA" # base classifier, eg "knn","LDA","QDA" or other
           , k = c(3,5) # possible k if base = "knn"
           , projmethod = "Haar" # projection distribution eg "Haar", "axis"
         ,   ... )
        {
    n <- length(YTrain)
    p <- ncol(XTrain)
    n.val <- length(YVal)
    w <- n.val
    RP <-  RPGenerate(p, d, method = projmethod, B2)
    XRP <- crossprod(t(XTrain), RP)
    XRPVal <- crossprod(t(XVal), RP)
    if (base == "knn")
    {
      weight.test <- sapply(1:B2, function(j){min(sapply(k,function(x){mean(knn(XRP[, d*(j-1) + 1:d ], XRPVal[, d*(j-1) + 1:d ], YTrain, x) != YVal, na.rm = TRUE)}))})
      cols1 <- d*(which.min(weight.test) - 1) + 1:d
      kcv.voteRP <- sapply(k,function(x){mean(knn(XRP[, cols1], XRPVal[, cols1], YTrain, x) != YVal, na.rm = TRUE)})
      k1 <- k[which.min(kcv.voteRP)]
      Val.Class <- as.numeric(knn(XRP[,cols1], XRPVal[,cols1], YTrain, k1))
      XRPTest <- crossprod(t(XTest),RP[,cols1])
      Test.Class <- as.numeric(knn(XRP[,cols1], XRPTest, YTrain, k1))
    }
    
    if (base == "LDA") 
    {
      weight.test <- sapply(1:B2, function(j){mean(predict(lda(x =  XRP[,d*(j-1) + 1:d], grouping = YTrain),  XRPVal[, d*(j-1) + 1:d])$class != YVal, na.rm = TRUE)})
      cols1 <- d*(which.min(weight.test) - 1) + 1:d
      Val.Class <- as.numeric(predict(lda(x = XRP[, cols1], grouping = YTrain), XRPVal[, cols1])$class)
      XRPTest <- crossprod(t(XTest),RP[,cols1])
      Test.Class <- as.numeric(predict(lda(x = XRP[, cols1], grouping = YTrain), XRPTest)$class)
    }
    if (base == "QDA") 
    {      
        weight.test <- sapply(1:B2, function(j){mean(predict(qda(x =  XRP[, d*(j-1) + 1:d], grouping = YTrain),  XRPVal[,d*(j-1) + 1:d])$class != YVal, na.rm = TRUE)})
        cols1 <-  d*(which.min(weight.test) - 1) + 1:d
        Val.Class <- as.numeric(predict(qda(x = XRP[, cols1], grouping = YTrain), XRPVal[, cols1])$class)
        XRPTest <- crossprod(t(XTest),RP[,cols1])
        Test.Class <- as.numeric(predict(qda(x = XRP[, cols1], grouping = YTrain), XRPTest)$class)
    }
    if (base == "Other") {
      weight.test <- sapply(1:B2, function(j){mean(Other.classifier(x = XRP[, d*(j-1) + 1:d], grouping = YTrain, CV = TRUE)$class != YTrain, na.rm = TRUE)})
      cols1 <- d*(which.min(weight.test) - 1) + 1:d
      Val.Class <- as.numeric(Other.classifier(x = XRP[, cols1], grouping = YTrain, XRPVal[, cols1])$class)
      XRPTest <- crossprod(t(XTest),RP[,cols1])
      Test.Class <- as.numeric(Other.classifier(x = XRP[, cols1], grouping = YTrain, XRPVal[, cols1])$class)
    }
    return(c(Val.Class, Test.Class)) 
}
