setwd(normalizePath(dirname(R.utils::commandArgs(asValues=TRUE)$"f")))
source("../../scripts/h2o-r-test-setup.R")
library(data.table)
library(slam)
library(tidyverse)
library(tidytext)
library(tm)
library(testthat)
library(stringr)
library(forcats)

# I copied this whole thing from https://ellisp.github.io/blog/2017/02/18/svmlite and
# try to understand how it works and hence evaluate it against the original as.h2o
# and future as.h2o if necessary.
test.pubdev_4630 = function(){
    # what is R doing
    #=============import to R================
    # Adapting the example at http://tidytextmining.com/usenet.html
    browser()
    a = locate("bigdata/laptop/enron_mess")
    folders <- paste0(a, "/enron", 1:6)  # find path to directories containing enron emails

    spam <- data_frame(file = dir(paste0(folders, "/spam"), full.names = TRUE)) %>%
        mutate(text = map(file, readLines, encoding = "Latin-1")) %>%
        transmute(id = basename(file), text) %>%
        unnest(text) %>%
        mutate(SPAM = "spam")

    ham <- data_frame(file = dir(paste0(folders, "/ham"), full.names = TRUE)) %>%
        mutate(text = map(file, readLines, encoding = "Latin-1")) %>%
        transmute(id = basename(file), text) %>%
        unnest(text) %>%
        mutate(SPAM = "ham")

    enron_raw <- rbind(spam, ham)

    #============error checks - failing!===================
    # Check against the counts provided at http://csmining.org/index.php/data.html
    # Turns out some 3013 spam messages have gone missing
    # should be 20170 spam messages and 16545 ham messages:
    # returns an error
    expect_equal(length(unique(enron_raw$id)), 20170 + 16545)

    enron_raw %>%
        select(id, SPAM) %>%
        distinct() %>%
        summarise(spam_count = sum(SPAM == "spam"), ham_count = sum(SPAM == "ham"))
    # For my purposes I decide not to worry about this.

    #=================further processing==================
    enron <- enron_raw %>%
    # will just remove the "Subject:" and "Subject :" and treat subject words like any other
    mutate(text = gsub("^Subject *: *", "", text),
    text = gsub("<U.....>", "", text, fixed = FALSE))

    enron_words <- enron %>%
        unnest_tokens(word, text) %>%
        select(-SPAM)

    # First I'm creating a summary, dense data frame with some numeric info on each document
    enron_sum1 <- enron %>%
        mutate(number_characters = nchar(text)) %>%
        group_by(id, SPAM) %>%
        summarise(number_characters = sum(number_characters))

    enron_sum2 <- enron_words %>%
        group_by(id) %>%
        summarise(number_words = length(word))

    enron_sum3 <- enron_words %>%
        anti_join(stop_words, by = "word") %>%
        group_by(id) %>%
        summarise(number_nonstop_words = length(word))

    enron_sum_total <- enron_sum1 %>%
        left_join(enron_sum2, by = "id") %>%
        left_join(enron_sum3, by = "id") %>%
        mutate(number_stop_words = number_words - number_nonstop_words,
        proportion_stop_words = number_stop_words / number_words) %>%
        select(-number_nonstop_words)

    enron_sum_total
    used_words <- enron_words %>%
    # knock out stop words:
        anti_join(stop_words, by = "word") %>%
    # knock out numerals, and words with only 2 or 1 letters:
        mutate(word = gsub("[0-9]", "", word),
        wordlength = nchar(word)) %>%
        filter(wordlength > 2) %>%
        group_by(word) %>%
        summarise(count = length(word)) %>%
        ungroup() %>%
    # knock out words used less than 10 times:
        filter(count >= 10)

    enron_dtm <- enron_words %>%
        right_join(used_words, by = "word") %>%
        cast_dtm(id, word, count)

    # we need a version of the dense data in the same order as the document-term-matrix, to do a sort of
    # manual join of the sparse matrix with the dense one later in H2O.
    rows <- data_frame(id = rownames(enron_dtm))
    enron_dense <- left_join(rows, enron_sum_total, by = "id")
    expect_equal(nrow(enron_dtm), nrow(enron_dense))
    expect_equal(rownames(enron_dtm), enron_dense$id)


    #================import to h2o and join up there============
    h2o.init(nthreads = -1, max_mem_size = "8G")

    # Load up the dense matrix with counts of stopwords etc:
    enron_dense_h2o <- as.h2o(enron_dense)

    # Load up the sparse matrix with columns for each word:
    thefile <- tempfile()
    write_stm_svm(enron_dtm, file = thefile)
    enron_sparse_h2o <- h2o.uploadFile(thefile, parse_type = "SVMLight")
    unlink(thefile)

    # Number of rows should be equal:
    expect_equal(nrow(enron_sparse_h2o), nrow(enron_dtm))
    # Number of columns should be 1 extra in H2O, dummy variable of labels (1) added by write_stm_svm:
    expect_equal(ncol(enron_sparse_h2o), ncol(enron_dtm) + 1)

    # First column should be the dummy labels = all one
    expect_equal(mean(enron_sparse_h2o[ , 1]), 1)

    enron_fulldata <- h2o.cbind(enron_sparse_h2o, enron_dense_h2o)
    head(colnames(enron_fulldata), 10)

    # Convert the target variable to a factor so h2o.glm and other modelling functions
    # know what to do with it:
    enron_fulldata[ , "SPAM"] <- as.factor(enron_fulldata[ , "SPAM"])

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

doTest("Test the as.h2o for sparse matrix.", test.pubdev_4630)