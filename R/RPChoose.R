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
      XRP <- as.matrix(crossprod(t(XTrain), RP),n,d*B2)
      if (base == "knn")
      {
        if(estmethod == "training") stop("training error estimate unsuitable for knn classifier") 
        if (estmethod == "loo"){
        weight.test <- sapply(1:B2, function(j){min(sapply(k,function(x){mean(knn.cv(as.matrix(XRP[,d*(j-1) + 1:d ],n,d), YTrain, x) != YTrain, na.rm = TRUE)}))})
        }
        cols1 <- d*(which.min(weight.test) - 1) + 1:d
        kcv.voteRP <- sapply(k,function(x){mean(knn.cv(as.matrix(XRP[, cols1],n,d), YTrain, x) != YTrain, na.rm = TRUE)})
        k1 <- k[which.min(kcv.voteRP)]
        Train.Class <- as.numeric(knn.cv(as.matrix(XRP[, cols1],n,d), YTrain, k1))
        XRPTest <- as.matrix(crossprod(t(XTest),RP[,cols1]),ntest,d)
        Test.Class <- as.numeric(knn(as.matrix(XRP[, cols1],n,1), XRPTest, YTrain, k1))
      }
      
      if (base == "LDA") 
      {
        if(estmethod == "training") {
        weight.test <- sapply(1:B2, function(j){mean(predict(lda(x = as.matrix(XRP[,d*(j-1) + 1:d],n,d), grouping = YTrain),  as.matrix(XRP[1:n, d*(j-1) + 1:d], n,d))$class != YTrain, na.rm = TRUE)})
        cols1 <- d*(which.min(weight.test) - 1) + 1:d
        Train.Class <- as.numeric(predict(lda(x = as.matrix(XRP[, cols1],n,d), grouping = YTrain), as.matrix(XRP[, cols1],n,d))$class)
        XRPTest <- as.matrix(crossprod(t(XTest),RP[,cols1]),ntest,d)
        Test.Class <- as.numeric(predict(lda(x = as.matrix(XRP[, cols1],n,d), grouping = YTrain), XRPTest)$class)
        }
        if (estmethod == "loo") {
        weight.test <- sapply(1:B2, function(j){mean(lda(x = as.matrix(XRP[1:n, d*(j-1) + 1:d],n,d), grouping = YTrain, CV = TRUE)$class != YTrain, na.rm = TRUE)})
        cols1 <-  d*(which.min(weight.test) - 1) + 1:d
        Train.Class <- as.numeric(lda(x = as.matrix(XRP[, cols1],n,d), grouping = YTrain, CV = TRUE)$class)
        XRPTest <- as.matrix(crossprod(t(XTest),RP[,cols1]),ntest,d)
        Test.Class <- as.numeric(predict(lda(x = as.matrix(XRP[1:n, cols1],n,d), grouping = YTrain), XRPTest)$class)
        }
      }
      if (base == "QDA") 
      {      
        if(estmethod == "training") {
        weight.test <- sapply(1:B2, function(j){mean(predict(qda(x =  as.matrix(XRP[, d*(j-1) + 1:d],n,d), grouping = YTrain),  as.matrix(XRP[,d*(j-1) + 1:d],n,d))$class != YTrain, na.rm = TRUE)})
        cols1 <-  d*(which.min(weight.test) - 1) + 1:d
        Train.Class <- as.numeric(predict(qda(x = as.matrix(XRP[, cols1],n,d), grouping = YTrain), as.matrix(XRP[, cols1],n,d))$class)
        XRPTest <- as.matrix(crossprod(t(XTest),RP[,cols1]),ntest,d)
        Test.Class <- as.numeric(predict(qda(x = as.matrix(XRP[, cols1],n,d), grouping = YTrain), XRPTest)$class)
        }
        if (estmethod == "loo"){      
        weight.test <- sapply(1:B2, function(j){mean(qda(x = as.matrix(XRP[, d*(j-1) + 1:d],n,d), grouping = YTrain, CV = TRUE)$class != YTrain, na.rm = TRUE)})
        cols1 <-  d*(which.min(weight.test) - 1) + 1:d
        Train.Class <- as.numeric(qda(x = as.matrix(XRP[, cols1],n,d), grouping = YTrain, CV = TRUE)$class)
        XRPTest <- as.matrix(crossprod(t(XTest),RP[,cols1]), ntest, d)
        Test.Class <- as.numeric(predict(qda(x = as.matrix(XRP[, cols1],n,d), grouping = YTrain), XRPTest)$class)
        }
      }
      if (base == "Other") 
      {
        weight.test <- sapply(1:B2, function(j){mean(Other.classifier(x = as.matrix(XRP[, d*(j-1) + 1:d],n,d), grouping = YTrain, CV = TRUE)$class != YTrain, na.rm = TRUE)})
        cols1 <- d*(which.min(weight.test) - 1) + 1:d
        Train.Class <- as.numeric(Other.classifier(x = as.matrix(XRP[, cols1],n,d), grouping = YTrain, CV = TRUE)$class)
        XRPTest <- as.matrix(crossprod(t(XTest),RP[,cols1]),ntest,d)
        Test.Class <- as.numeric(Other.classifier(x = as.matrix(XRP[, cols1],n,d), grouping = YTrain,  XRPTest)$class)
      }
      return(c(Train.Class, Test.Class)) 
    }
   
