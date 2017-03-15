# nbdprogram/R/REstd.R
# Copyright (c) 2017 Daniele Medri
# Distributed under the GNU GPL-2 license

.fnrod <- function(i, j, r) {
  if (missing(i))
    stop("Argument 'i' must be present.", call. = FALSE)
  if (missing(j))
    stop("Argument 'j' must be present.", call. = FALSE)
  if (missing(r))
    r <- as.numeric(0.0)
  
  out <- .C("fnrod",
            as.numeric(i),
            as.numeric(j),
            r)
  return(out[[3]])
}

.fnbval <- function(n, m, g, k, r) {
  if (missing(n))
    stop("Argument 'n' must be present.", call. = FALSE)
  if (missing(m))
    stop("Argument 'm' must be present.", call. = FALSE)
  if (missing(g))
    stop("Argument 'g' must be present.", call. = FALSE)
  if (missing(k))
    stop("Argument 'k' must be present.", call. = FALSE)
  if (missing(r))
    r <- as.numeric(rep(0.0, length(g)))

  out <- .C("fnbval",
            as.numeric(n),
            as.numeric(m),
            as.numeric(g),
            as.numeric(k),
            r)
  return(out[[5]])
}

.fnqval <- function(bval, r) {
  if (missing(bval))
    stop("Argument 'bval' must be present.", call. = FALSE)
  if (missing(r))
    r <- as.numeric(rep(0.0, length(bval)))

  out <- .C("fnqval",
            as.numeric(bval),
            r)
  return(out[[2]])
}

.fnwval <- function(n, m, g, k, r) {
  if (missing(n))
    stop("Argument 'n' must be present.", call. = FALSE)
  if (missing(m))
    stop("Argument 'm' must be present.", call. = FALSE)
  if (missing(g))
    stop("Argument 'g' must be present.", call. = FALSE)
  if (missing(k))
    stop("Argument 'k' must be present.", call. = FALSE)
  if (missing(r))
    r <- as.numeric(rep(0.0, length(g)))

  out <- .C("fnwval",
            as.numeric(n),
            as.numeric(m),
            as.numeric(g),
            as.numeric(k),
            r)
  return(out[[5]])
}

.fndval <- function(bval, wval, k, r) {
  if (missing(bval))
    stop("Argument 'bval' must be present.", call. = FALSE)
  if (missing(wval))
    stop("Argument 'wval' must be present.", call. = FALSE)
  if (missing(k))
    stop("Argument 'k' must be present.", call. = FALSE)
  if (missing(r))
    r <- as.numeric(rep(0.0, length(bval)))

  out <- .C("fndval",
            as.numeric(bval),
            as.numeric(wval),
            as.numeric(k),
            r)
  return(out[[4]])
}

.fnmval <- function(bval, wval, k, r) {
  if (missing(bval))
    stop("Argument 'bval' must be present.", call. = FALSE)
  if (missing(wval))
    stop("Argument 'wval' must be present.", call. = FALSE)
  if (missing(k))
    stop("Argument 'k' must be present.", call. = FALSE)
  if (missing(r))
    r <- as.numeric(rep(0.0, length(bval)))

  out <- .C("fnmval",
            as.numeric(bval),
            as.numeric(wval),
            as.numeric(k),
            r)
  return(out[[4]])
}

.fneval <- function(bval, dval, mval, r) {
  if (missing(bval))
    stop("Argument 'bval' must be present.", call. = FALSE)
  if (missing(dval))
    stop("Argument 'dval' must be present.", call. = FALSE)
  if (missing(mval))
    stop("Argument 'mval' must be present.", call. = FALSE)
  if (missing(r))
    r <- as.numeric(rep(0.0, length(bval)))

  out <- .C("fneval",
            as.numeric(bval),
            as.numeric(dval),
            as.numeric(mval),
            r)
  return(out[[4]])
}

.fnfval <- function(bval, wval, k, r) {
  if (missing(bval))
    stop("Argument 'bval' must be present.", call. = FALSE)
  if (missing(wval))
    stop("Argument 'wval' must be present.", call. = FALSE)
  if (missing(k))
    stop("Argument 'k' must be present.", call. = FALSE)
  if (missing(r))
    r <- as.numeric(rep(0.0, length(bval)))

  out <- .C("fnfval",
            as.numeric(bval),
            as.numeric(wval),
            as.numeric(k),
            r)
  return(out[[4]])
}

.fnqvalrest <- function(bval, wval, qval, k) {
  if (missing(bval))
    stop("Argument 'bval' must be present.", call. = FALSE)
  if (missing(wval))
    stop("Argument 'wval' must be present.", call. = FALSE)
  if (missing(qval))
    stop("Argument 'qval' must be present.", call. = FALSE)
  if (missing(k))
    stop("Argument 'k' must be present.", call. = FALSE)

  out <- .C("fnqvalrest",
            as.numeric(bval),
            as.numeric(wval),
            as.numeric(qval),
            as.numeric(k))
  return(out[[3]])
}

.fnzval <- function(bval, wval, qval, k, zval) {
  if (missing(bval))
    stop("Argument 'bval' must be present.", call. = FALSE)
  if (missing(wval))
    stop("Argument 'wval' must be present.", call. = FALSE)
  if (missing(qval))
    stop("Argument 'qval' must be present.", call. = FALSE)
  if (missing(k))
    stop("Argument 'k' must be present.", call. = FALSE)
  if (missing(zval))
    stop("Argument 'zval' must be present.", call. = FALSE)

  out <- .C("fnzval",
            as.numeric(bval),
            as.numeric(wval),
            as.numeric(qval),
            as.numeric(zval))
  return(out[[4]])
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


  np$bval[1,] <- .fnbval(np$n, m, np$g[1,], k)
  np$qval[1,] <- .fnqval(np$bval[1,])
  np$wval[1,] <- .fnwval(np$n, m, np$g[1,], k)
  np$dval[1,] <- .fndval(np$bval[1,], np$wval[1,], k)
  np$mval[1,] <- .fnmval(np$bval[1,], np$wval[1,], k)  
  np$eval[1,] <- .fneval(np$bval[1,], np$dval[1,], np$mval[1,])
  np$dval[1,] <- .fnrod((np$dval[1,]), (np$l + 1))
  np$fval[1,] <- .fnfval(np$bval[1,], np$wval[1,], k)

  a <- matrix(0, ncol = 7)
  a[1,] <- as.numeric(np$bval[1,]*np$wval[1,]/k)
 
  #np$qval <- matrix(fnqvalrest(np$bval, np$wval, np$qval, k), 5, 7)
  #np$zval <- matrix(fnzval(np$bval, np$wval, np$qval, k, np$zval), 5, 7)

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
