RPModel <-
function(Model.No #Model number
                , n # sample size
                , p # dimension
                , Pi = 1/2 # class 1 prior
                    )
{
   if (Model.No == 1)
    {
      Y1 <- rmultinom(1,n,c(Pi,1-Pi))
      Y <- c(rep(1,Y1[1,1]),rep(2,Y1[2,1]))
      Y11 <- rmultinom(1,Y1[1,1],c(1/2,1/2))
      Y22 <- rmultinom(1,Y1[2,1],c(1/2,1/2))
      mu1 <- c(2, 2, rep(0,p-2))
      mu2 <- c(2,-2, rep(0,p-2))
      if(p == 100) {X1 <- rbind(t(matrix(mu1,p,Y11[1,1])),t(matrix(-mu1,p,Y11[2,1]))) + matrix(rnorm(Y1[1,1]*p),Y1[1,1],p)}
      if(p == 100) {X2 <- rbind(t(matrix(mu2,p,Y22[1,1])),t(matrix(-mu2,p,Y22[2,1]))) + matrix(rnorm(Y1[2,1]*p),Y1[2,1],p)}
      if(p == 1000) {X1 <- rbind(t(matrix(mu1,p,Y11[1,1])),t(matrix(-mu1,p,Y11[2,1]))) + cbind(matrix(rnorm(Y1[1,1]*2),Y1[1,1],2), matrix(rnorm(Y1[1,1]*(p-2))/4,Y1[1,1],p-2))}
      if(p == 1000) {X2 <- rbind(t(matrix(mu2,p,Y22[1,1])),t(matrix(-mu2,p,Y22[2,1]))) + cbind(matrix(rnorm(Y1[2,1]*2),Y1[2,1],2), matrix(rnorm(Y1[2,1]*(p-2))/4,Y1[2,1],p-2))}
      X <- rbind(X1,X2)
    }
    if (Model.No == 2)
    {
      R <- NULL
      if(p == 100) data(R, envir = environment())
      #if(p == 1000) load(R1000.RData)
      Y1 <- rmultinom(1,n,c(Pi,1-Pi))
      Y <- c(rep(1,Y1[1,1]),rep(2,Y1[2,1]))
      mu <- c(rep(3,3), rep(0,p-3))
      Sigma1 <- 0.5*diag(c(rep(1,3),rep(0,p-3)))+0.5*c(rep(1,3),rep(0,p-3))%*%t(c(rep(1,3),rep(0,p-3))) + 0.5*diag(c(rep(0,3),rep(1,p-3)))+0.5*c(rep(0,3),rep(1,p-3))%*%t(c(rep(0,3),rep(1,p-3)))
      Sigma2 <- 1.5*diag(c(rep(1,3),rep(0,p-3)))+0.5*c(rep(1,3),rep(0,p-3))%*%t(c(rep(1,3),rep(0,p-3))) + 0.5*diag(c(rep(0,3),rep(1,p-3)))+0.5*c(rep(0,3),rep(1,p-3))%*%t(c(rep(0,3),rep(1,p-3)))
      X1 <- mvrnorm(Y1[1,1],R%*%rep(0,p),R%*%Sigma1%*%t(R))
      X2 <- mvrnorm(Y1[2,1],R%*%mu,R%*%Sigma2%*%t(R))
      X <- rbind(X1,X2)
      rm(R, envir = environment())
    }
    if (Model.No == 3)
    {
      Y1 <- rmultinom(1,n,c(Pi,1-Pi))
      Y <- c(rep(1,Y1[1,1]),rep(2,Y1[2,1]))
      mu <- c(rep(1/sqrt(p),p/2),rep(0,p/2))
      D <- DExp(1)
      X1 <- cbind(matrix(r(D)(Y1[1,1]*p),Y1[1,1],p))
      X2 <- mvrnorm(Y1[2,1],mu,diag(p))
      X  <- rbind(X1,X2)
    }
    if (Model.No == 4)
    {
      Y1 <- rmultinom(1,n,c(Pi,1-Pi))
      Y <- c(rep(1,Y1[1,1]),rep(2,Y1[2,1]))
      mu <- c(rep(1,10),rep(0,p-10))
      U1 <- rchisq(Y1[1,1],1)
      U2 <- rchisq(Y1[2,1],2)
      Sigma1 <- diag(p)
      Sigma2 <- 0.5*diag(p)+0.5*c(rep(1,10),rep(0,p-10))%*%t(c(rep(1,10),rep(0,p-10))) + 0.5*diag(c(rep(0,10),rep(1,p-10)))
      X1 <- mvrnorm(Y1[1,1],rep(0,p),Sigma1)/sqrt(U1/1)
      X2 <- t(mu + t(mvrnorm(Y1[2,1],rep(0,p),Sigma2)/sqrt(U2/2)))
      X  <- rbind(X1,X2)
    }
  
    return(list(x=X,y=Y))
  }
