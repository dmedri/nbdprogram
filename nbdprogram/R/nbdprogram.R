# nbdprogram/R/nbdprogram.R
# Copyright (c) 2017 Daniele Medri
# Distributed under the GNU GPL-2 license

nbdprogram <- function(b = 0.20, w = 2.1, t = 4) {
  np <- list()

  # b (penetration)
  if (!is.numeric(b))
    stop("Argument 'b' must be numeric.")
  if (!(b > 0) || !(b < 1))
    stop("Argument 'b' must in the range [0,1].")
  
  # w (frequency)
  if (!is.numeric(w))
    stop("Argument 'w' must be numeric.")
  if (!(w >= 1))
    stop("Argument 'w' must equal or more than 1.")
  
  # t (period of time)
  if (!(t %in% c(1, 2, 3, 4, 5, 6, 7, 8)))
    stop("Argument 't' must be in range [1-8]")


  np$l <- as.numeric(2.0)
  np$v <- as.numeric(0.006)
  np$x <- as.numeric(0.045)
  np$o <- 0


  # b (check)
  np$b <- as.numeric(b)
  if (np$b <= 0.025) np$v <- 0.01

  # w (check)
  np$w <- as.numeric(w)
  if (np$w >= 6.0) np$v <- 0.1

  # t (check)
  np$t <- as.integer(t)

  gv <- c(1, 7, 28, 84, 168, 336, 365)
  np$g <- matrix(gv, ncol = 7)
  np$n <- (1.0 / np$g[np$t])    
  
  np <- .REstd(np)

  return(np)
}
