#!/usr/local/bin/Rscript
system("make clean")
library(Rcpp)
compileAttributes(".")
system("R CMD INSTALL .")
