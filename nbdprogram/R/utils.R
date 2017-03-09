# nbdprogram/R/utils.R
# Copyright (c) 2017 Daniele Medri
# Distributed under the GNU GPL-2 license

.dateseq <- function(startdate) {
  if (missing(startdate))
    startdate <- as.Date(format(Sys.time(), "%Y-%m-%d"))
  enddate <- as.Date(startdate) + 364  
  r <- seq(as.Date(startdate), as.Date(enddate), by="days")
  
  return(r)
}

.check_enum <- function(x, set) {
  if (missing(x) || missing(set))
    stop("Missing needed arguments.", call. = FALSE)
  if (is.null(x) || is.null(set))
    stop("Arguments must be not null.", call. = FALSE)
  if ( (is.character(x) && is.character(set)) ||
        (is.numeric(x) && is.numeric(set))   ||
        (is.logical(x) && is.logical(set)) ) {
    r <- (x %in% set)
  } else {
    stop("Arguments must be of the same type.", call. = FALSE)
  }
  return(r)
}
