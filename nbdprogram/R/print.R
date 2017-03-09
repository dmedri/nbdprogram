# nbdprogram/R/print.R
# Copyright (c) 2017 Daniele Medri
# Distributed under the GNU GPL-2 license

print.nbdprogram <- function(x, ...) {

  message("NBD Program")
  message("Predicting purchase rates in stationary markets\n")
  tdesc <- c("1 day", "1 week", "4 weeks", "12 weeks",
               "24 weeks", "48 weeks", "365 days")
  message(paste("(b):",x$b))
  message(paste("(w):",x$w))
  message(paste("(t):",tdesc[x$t]))

  message("\nPenetration (b)")
  p <- as.data.frame(round(x$bval,3), row.names="")
  names(p) <- tdesc
  print(p)

  message("\nPurchase frequency (w)")
  p <- as.data.frame(round(x$wval,3), row.names="")
  names(p) <- tdesc
  print(p)

  message("\nRepat purchase (%)")
  p <- as.data.frame(round(x$dval,3), row.names="")
  names(p) <- tdesc
  print(p)

  message("\nPurchase frequency of buyers")
  p <- as.data.frame(round(x$eval,3), row.names="")
  names(p) <- tdesc
  print(p)

  message("\nPurchase frequency of new buyers")
  p <- as.data.frame(round(x$fval,3), row.names="")
  names(p) <- tdesc
  print(p)
  
  # 2nd table
  
  message("\nProportion -\n")
  rn <- c("not buying", "buying once", "twice", "3 times", "4 times", "5 times")
  p <- as.data.frame(round(x$qval,3), row.names=rn)
  names(p) <- tdesc
  print(p)

  # 3rd table
  
  message("\nProportion of sales due to those buying -\n")
  p <- as.data.frame(round(x$zval,3), row.names=rn[2:6])
  names(p) <- tdesc
  print(p)

}
