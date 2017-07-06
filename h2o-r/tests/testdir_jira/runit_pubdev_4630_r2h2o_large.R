setwd(normalizePath(dirname(R.utils::commandArgs(asValues=TRUE)$"f")))
source("../../scripts/h2o-r-test-setup.R")
library(slam)
library(tidyverse)
library(tidytext)
library(tm)
library(testthat)
library(stringr)
library(forcats)
library(data.table)
library(Matrix)
library(pryr)

# This test aims to test that our PCA works with wide datasets for the following PCA methods:
# 1. GramSVD: PUBDEV-3694;
# 2. Power: PUBDEV-3858
#
# It will compare the eigenvalues and eigenvectors obtained with the various methods and they should agree
# to within certain tolerance.
#
# To cut down on execution time, I will only compare GramSVD with one of the following models built at random
#  at each test: R, Power or Randomized

check.pca.widedata <- function() {
  browser()
  nRow = 90000
  nCol = 1000
  probOne = 0.0001   # generate sparse matrix here
  m <- matrix(rbinom(nRow*nCol, 1, probOne), ncol = nCol)
  object_size(m)
  
  sparseM = Matrix(m, sparse=TRUE) # convert matrix to sparse matrix
  object_size(sparseM)
  
  print("Converting sparse matrix using as.h2o")
  ptm = proc.time()
  dr2h2o = as.h2o(sparseM)
  timepassed = proc.time()-ptm
  print(timepassed)
  h2o.rm(dr2h2o)
    
  drs = as.simple_triplet_matrix(m)   # convert matrix to simple_triplet_matrix
  print("Using bigdata solution proposed by someone..")
  options("h2o.use.data.table"=TRUE)
  ptm = proc.time()
  thefile <- tempfile()
  write_stm_svm(drs, file = thefile)
  enron_sparse_h2o <- h2o.uploadFile(thefile, parse_type = "SVMLight")
  unlink(thefile)
  timepassed = proc.time()-ptm
  print(timepassed)
  h2o.rm(enron_sparse_h2o)
  
  drs = as.simple_triplet_matrix(sparseM)   # convert sparse matrix to simple_triplet_matrix
  print("Using bigdata solution proposed by someone and convert sparse matrix to simple triplet matrix")
  options("h2o.use.data.table"=TRUE)
  ptm = proc.time()
  thefile <- tempfile()
  write_stm_svm(drs, file = thefile)
  enron_sparse_h2o <- h2o.uploadFile(thefile, parse_type = "SVMLight")
  unlink(thefile)
  timepassed = proc.time()-ptm
  print(timepassed)
  h2o.rm(enron_sparse_h2o)
}

# Convert a simple triplet matrix to svm format
#' @author Peter Ellis
#' @return a character vector of length n = nrow(stm)
calc_stm_svm <- function(stm, y){
  # returns a character vector of length y ready for writing in svm format
  if(!"simple_triplet_matrix" %in% class(stm)){
    stop("stm must be a simple triple matrix")
  }
  if(!is.vector(y) | nrow(stm) != length(y)){
    stop("y should be a vector of length equal to number of rows of stm")
  }
  n <- length(y)
  
  # data.table solution thanks to @roland at http://stackoverflow.com/questions/41477700/optimising-sapply-or-for-paste-to-efficiently-transform-sparse-triplet-m/41478999#41478999
  stm2 <- data.table(i = stm$i, j = stm$j, v = stm$v)
  res <- stm2[, .(i, jv = paste(j, v, sep = ":"))][order(i), .(res = paste(jv, collapse = " ")), by = i][["res"]]
  
  out <- paste(y, res)
  
  return(out)
}


#' @param stm a simple triplet matrix (class exported slam) of features (ie explanatory variables)
#' @param y a vector of labels.  If not provided, a dummy of 1s is provided
#' @param file file to write to.
#' @author Peter Ellis
write_stm_svm <- function(stm, y = rep(1, nrow(stm)), file){
  out <- calc_stm_svm(stm, y)
  writeLines(out, con = file)
}

doTest("PUBDEV-3694 and PUBDEV-3858: PCA with wide dataset for GramSVD, Power", check.pca.widedata)