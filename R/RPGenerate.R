RPGenerate <-
function(p=100 #higher dimension
                    ,d=10   #lower dimension
                    ,method = "Haar" # Random projection method
                    ,B2 = 10 )
    {
     if (p < d) stop("d must be less than p")
         if (method == "Gaussian")
            {
                Q <-matrix(1/sqrt(p)*rnorm(p*d*B2,0,1),p,d*B2)
            }   
        if (method == "Haar")
            {
                R0 <- matrix(1/sqrt(p)*rnorm(p*d*B2,0,1),p,d*B2)
                Q <- matrix(sapply(1:B2-1, FUN = function(s){qr.Q(qr(R0[,s*d+1:d]))[,1:d]}),p,B2*d)
            }           
        if (method == "axis")
            {
          Q <- NULL
                for(i in 1:B2)
                {
                S <- sample(1:p,d)
                R <- matrix(0,p,d)
                for (D in 1:d)
                    {
                        R[S[D],D] <- 1
                }
                Q <- cbind(Q,R)
                }
            }
       if (method == "other")
            {
                Q <- matrix(0,p,d)
            }
        return(Q)
    }
