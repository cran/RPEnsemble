RPChoose <-
    function(XTrain #n by p trining data matrix
           , YTrain #n vector of the classes of the trining samples
           , XTest  #n.test by p test data matrix
           , d      #dimension to project into
           , B2 = 10 #block size
           , base = "LDA" # base classifier, eg "knn","LDA","QDA" or other
           , k = c(3,5) # possible k if base = "knn"
           , projmethod = "Haar" # projection distribution eg "Haar", "axis"
           , estmethod = "training"
         ,   ... )
        {
      #psnice(value = 19)
      n <- length(YTrain)
      ntest <- nrow(XTest)
      p <- ncol(XTrain)
      k1 <- 1
      RP <-  RPGenerate(p, d, method = projmethod, B2)
      XRP <- crossprod(t(XTrain), RP)
      if (base == "knn")
      {
        if(estmethod == "training") stop("training error estimate unsuitable for knn classifier") 
        if (estmethod == "loo"){
        weight.test <- sapply(1:B2, function(j){min(sapply(k,function(x){mean(knn.cv(XRP[, d*(j-1) + 1:d ], YTrain, x) != YTrain, na.rm = TRUE)}))})
        }
        cols1 <- d*(which.min(weight.test) - 1) + 1:d
        kcv.voteRP <- sapply(k,function(x){mean(knn.cv(XRP[, cols1], YTrain, x) != YTrain, na.rm = TRUE)})
        k1 <- k[which.min(kcv.voteRP)]
        Train.Class <- as.numeric(knn.cv(XRP[,cols1], YTrain, k1))
        XRPTest <- crossprod(t(XTest),RP[,cols1])
        Test.Class <- as.numeric(knn(XRP[,cols1], XRPTest, YTrain, k1))
      }
      
      if (base == "LDA") 
      {
        if(estmethod == "training") {
        weight.test <- sapply(1:B2, function(j){mean(predict(lda(x =  XRP[,d*(j-1) + 1:d], grouping = YTrain),  XRP[1:n, d*(j-1) + 1:d])$class != YTrain, na.rm = TRUE)})
        cols1 <- d*(which.min(weight.test) - 1) + 1:d
        Train.Class <- as.numeric(predict(lda(x = XRP[, cols1], grouping = YTrain), XRP[1:n, cols1])$class)
        XRPTest <- crossprod(t(XTest),RP[,cols1])
        Test.Class <- as.numeric(predict(lda(x = XRP[1:n, cols1], grouping = YTrain), XRPTest)$class)
        }
        if (estmethod == "loo") {
        weight.test <- sapply(1:B2, function(j){mean(lda(x = XRP[1:n, d*(j-1) + 1:d], grouping = YTrain, CV = TRUE)$class != YTrain, na.rm = TRUE)})
        cols1 <-  d*(which.min(weight.test) - 1) + 1:d
        Train.Class <- as.numeric(lda(x = XRP[, cols1], grouping = YTrain, CV = TRUE)$class)
        XRPTest <- crossprod(t(XTest),RP[,cols1])
        Test.Class <- as.numeric(predict(lda(x = XRP[1:n, cols1], grouping = YTrain), XRPTest)$class)
        }
      }
      if (base == "QDA") 
      {      
        if(estmethod == "training") {
        weight.test <- sapply(1:B2, function(j){mean(predict(qda(x =  XRP[, d*(j-1) + 1:d], grouping = YTrain),  XRP[,d*(j-1) + 1:d])$class != YTrain, na.rm = TRUE)})
        cols1 <-  d*(which.min(weight.test) - 1) + 1:d
        Train.Class <- as.numeric(predict(qda(x = XRP[, cols1], grouping = YTrain), XRP[, cols1])$class)
        XRPTest <- crossprod(t(XTest),RP[,cols1])
        Test.Class <- as.numeric(predict(qda(x = XRP[, cols1], grouping = YTrain), XRPTest)$class)
        }
        if (estmethod == "loo"){      
        weight.test <- sapply(1:B2, function(j){mean(qda(x = XRP[, d*(j-1) + 1:d], grouping = YTrain, CV = TRUE)$class != YTrain, na.rm = TRUE)})
        cols1 <-  d*(which.min(weight.test) - 1) + 1:d
        Train.Class <- as.numeric(qda(x = XRP[, cols1], grouping = YTrain, CV = TRUE)$class)
        XRPTest <- crossprod(t(XTest),RP[,cols1])
        Test.Class <- as.numeric(predict(qda(x = XRP[, cols1], grouping = YTrain), XRPTest)$class)
        }
      }
      if (base == "Other") 
      {
        weight.test <- sapply(1:B2, function(j){mean(Other.classifier(x = XRP[, d*(j-1) + 1:d], grouping = YTrain, CV = TRUE)$class != YTrain, na.rm = TRUE)})
        cols1 <- d*(which.min(weight.test) - 1) + 1:d
        Train.Class <- as.numeric(Other.classifier(x = XRP[, cols1], grouping = YTrain, CV = TRUE)$class)
        XRPTest <- crossprod(t(XTest),RP[,cols1])
        Test.Class <- as.numeric(Other.classifier(x = XRP[, cols1], grouping = YTrain,  XRPTest)$class)
      }
      return(c(Train.Class, Test.Class)) 
    }
   
