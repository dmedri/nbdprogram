# nbdprogram/R/zzz.R
# Copyright (c) 2017 Daniele Medri
# Distributed under the GNU GPL-2 license

.onLoad <- function(libname, pkgname) {

}

.onAttach <- function(libname, pkgname) {
  msg <- c("\nWelcome to NBD Program.\n type '?nbdprogram' for more infos.\n type 'nbdprogram()' for a demo.\n")
  packageStartupMessage(msg)
}
