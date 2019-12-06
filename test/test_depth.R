source("topdown-halfspacemass-plotme.R")
require(depth)
require(testthat)



# depth-package returns you the same results as own witten function
test_that("same results for plotted data", {
  expect_equal(
    sapply(1:nrow(data_fig3),
           function(x) depth(data_fig3[x,], data_fig3,
                             method = "Tukey", ndir = 1e4))
    ,
    evaluate_depth(data_fig3, halfspaces = depth_fig3, metric = "depth")
    )
  })
# generate thredimensional data
set.seed(4163)
simulate_three <- matrix(data = c(rnorm(3), rep(c(3, 1), each = 3)), ncol = 3)


test_that("same results for plotted data with 3 dimesions", {
  expect_equal(
    sapply(1:nrow(simulate_three),
           function(x) depth(simulate_three[x,], simulate_three,
                             method = "Tukey", ndir = 1e4, approx = TRUE))
    ,
    evaluate_depth(simulate_three,
                   train_depth(simulate_three, n_halfspace = 1e4,
                               scope = 1, seed = 4163),
                   metric = "depth")
  )
})








