# nbdprogram/R/REstd.R
# Copyright (c) 2017 Daniele Medri
# Distributed under the GNU GPL-2 license

.fnrod <- function(i, j) {
  r <- ((i * (10.0 ^ j)) + 0.5) / (10.0 ^ j)
  return(r)
}

.REstd <- function(np) {

  p <- as.numeric(1.0 - np$b)
  m <- as.numeric(np$b * np$w)
  c <- as.numeric((-1 * m) / log(p))
  k <- as.numeric(0.045)
  z <- as.numeric(0.0)

  np$bval <- matrix(0, ncol = 7)
  np$qval <- matrix(0, nrow = 6, ncol = 7)
  np$wval <- matrix(0, ncol = 7)
  np$mval <- matrix(0, ncol = 7)
  np$dval <- matrix(0, ncol = 7)
  np$zval <- matrix(0, nrow = 5, ncol = 7)
  np$eval <- matrix(0, ncol = 7)
  np$fval <- matrix(0, ncol = 7)
  
  a <- matrix(0, ncol = 7)


  while((.fnrod(z, 5.0) != .fnrod(p, 5.0)) || (.fnrod(z, 3.0) != .fnrod(p, 3.0))) {
    y <- (1.0 + m / k)
    z <- (1.0 / (y^k))
  
    if (np$o < 498) {
      np$o <- np$o + 1
    } else if (np$o >= 700) {
      message("Warning: values outside the estimation range.")
    }
    
    if (z < p) {
      np$x <- (np$x - np$v * np$x)
      k <- (k - np$x)
      if (k <= 0) {
        k <- as.numeric(0.00001)
      }
    } else {
      np$x <- (np$x - np$v * np$x)
      k <- (k + np$x)
    }
  }


  np$bval[1,] <- as.numeric(1-1/(1+np$n*m*(np$g[1,]/k))^k)
  np$qval[1,] <- as.numeric(1.0-np$bval[1,])
  np$wval[1,] <- as.numeric((np$n*m*np$g[1,])/(1-1/((1+m*np$n*np$g[1,]/k)^k)))
  np$dval[1,] <- as.numeric((100/np$bval[1,])*(1-2/((1+np$bval[1,]*np$wval[1,]/k)^k)+1/((1+2*np$bval[1,]*np$wval[1,]/k)^k)))
  np$mval[1,] <- as.numeric(np$wval[1,]*np$bval[1,]*(1-1/((1 + np$wval[1,]*np$bval[1,]/k)^(k+1))))
  np$eval[1,] <- as.numeric(np$mval[1,]/(np$dval[1,]*np$bval[1,]/100))
  np$dval[1,] <- as.numeric(.fnrod((np$dval[1,]),(np$l+1)))    
  np$fval[1,] <- as.numeric(((1+np$bval[1,]*np$wval[1,]/k)^(k+1))*(np$bval[1,]-(1-2*((1+np$bval[1,]*np$wval[1,]/k)^(-k))+((1+2*np$bval[1,]*np$wval[1,]/k)^(-k)))))
  a[1,] <- as.numeric(np$bval[1,]*np$wval[1,]/k)
 

  for (j in seq(1:5)) {
    jj <- (j+1)
    for (i in seq(1:7)) {
      np$qval[jj,i] <- as.numeric((a[i]/(1+a[i]))*(1-(a[i]-k*a[i])/(a[i]*j))*np$qval[j,i])
      np$zval[j,i]  <- as.numeric((j*np$qval[jj,i])/(np$wval[i]*np$bval[i]))
    }
  }
  
  class(np) <- "nbdprogram"
  return(np)
}
