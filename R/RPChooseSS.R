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
    n.test <- nrow(XTest)
    w <- n.val
    RP <-  RPGenerate(p, d, method = projmethod, B2)
    XRP <- as.matrix(crossprod(t(XTrain), RP),n,B2*d)
    XRPVal <- as.matrix(crossprod(t(XVal), RP),n.val,B2*d)
    if (base == "knn")
    {
      weight.test <- sapply(1:B2, function(j){min(sapply(k,function(x){mean(knn(as.matrix(XRP[, d*(j-1) + 1:d],n,d), as.matrix(XRPVal[, d*(j-1) + 1:d],n.val,d), YTrain, x) != YVal, na.rm = TRUE)}))})
      cols1 <- d*(which.min(weight.test) - 1) + 1:d
      kcv.voteRP <- sapply(k,function(x){mean(knn(as.matrix(XRP[,cols1],n,d), as.matrix(XRPVal[,cols1],n.val,d), YTrain, x) != YVal, na.rm = TRUE)})
      k1 <- k[which.min(kcv.voteRP)]
      Val.Class <- as.numeric(knn(as.matrix(XRP[,cols1],n,d), as.matrix(XRPVal[,cols1],n.val,d), YTrain, k1))
      XRPTest <- as.matrix(crossprod(t(XTest),RP[,cols1]),n.test,d)
      Test.Class <- as.numeric(knn(as.matrix(XRP[,cols1],n,d), XRPTest, YTrain, k1))
    }
    if (base == "LDA") 
    {
      weight.test <- sapply(1:B2, function(j){mean(predict(lda(x =  as.matrix(XRP[, d*(j-1) + 1:d],n,d), grouping = YTrain),  as.matrix(XRPVal[, d*(j-1) + 1:d],n.val,d))$class != YVal, na.rm = TRUE)})
      cols1 <- d*(which.min(weight.test) - 1) + 1:d
      Val.Class <- as.numeric(predict(lda(x = as.matrix(XRP[,cols1],n,d), grouping = YTrain), as.matrix(XRPVal[,cols1],n.val,d))$class)
      XRPTest <- as.matrix(crossprod(t(XTest),RP[,cols1]),n.test,d)
      Test.Class <- as.numeric(predict(lda(x = as.matrix(XRP[,cols1],n,d), grouping = YTrain), XRPTest)$class)
    }
    if (base == "QDA") 
    {      
        weight.test <- sapply(1:B2, function(j){mean(predict(qda(x =  as.matrix(XRP[, d*(j-1) + 1:d],n,d), grouping = YTrain),  as.matrix(XRPVal[, d*(j-1) + 1:d],n.val,d))$class != YVal, na.rm = TRUE)})
        cols1 <-  d*(which.min(weight.test) - 1) + 1:d
        Val.Class <- as.numeric(predict(qda(x = as.matrix(XRP[,cols1],n,d), grouping = YTrain), as.matrix(XRPVal[,cols1],n.val,d))$class)
        XRPTest <- as.matrix(crossprod(t(XTest),RP[,cols1]),n.test,d)
        Test.Class <- as.numeric(predict(qda(x = as.matrix(XRP[,cols1],n,d), grouping = YTrain), XRPTest)$class)
    }
    if (base == "Other") {
      weight.test <- sapply(1:B2, function(j){mean(Other.classifier(x = as.matrix(XRP[, d*(j-1) + 1:d],n,d), grouping = YTrain, CV = TRUE)$class != YTrain, na.rm = TRUE)})
      cols1 <- d*(which.min(weight.test) - 1) + 1:d
      Val.Class <- as.numeric(Other.classifier(x = as.matrix(XRP[,cols1],n,d), grouping = YTrain, as.matrix(XRPVal[,cols1],n.val,d))$class)
      XRPTest <- as.matrix(crossprod(t(XTest),RP[,cols1]),n.test,d)
      Test.Class <- as.numeric(Other.classifier(x = as.matrix(XRP[,cols1],n,d), grouping = YTrain, XRPTest)$class)
    }
    return(c(Val.Class, Test.Class)) 
}
