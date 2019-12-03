source("topdown-halfspacemass-pseudocode.R")
library(depth)
library(testthat)

depth((1:2), mtcars[,c(1,2)], method = "Liu")
