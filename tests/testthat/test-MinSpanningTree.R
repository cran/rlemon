# Title            : Minimum Spanning Tree
# File             : R/minimumspanningtree.R
# Exported         : MinSpanningTree
# Valid Algorithms : "Kruskal" (default)
# Runners          : KruskalRunner

# 1) Ensure runner functions run without error and return the "expected
# objects".
test_that("min spanning tree runner", {

  s <- c(1, 1, 2, 3, 4, 4)
  t <- c(2, 3, 3, 4, 1, 2)
  d <- c(1, 10, 3, 2, 0, 8)

  out <- KruskalRunner(s, t, d, 4)
  expect_true(is.list(out))
  expect_length(out, 3)
  expect_equal(length(out[[1]]), length(out[[2]]))
  expect_length(out[[3]], 1)
})

test_that("min spanning tree functions", {
  s <- c(1, 1, 2, 3, 4, 4)
  t <- c(2, 3, 3, 4, 1, 2)
  d <- c(1, 10, 3, 2, 0, 8)

  # 2) Ensure exported functions run without error and return the "expected
  # objects".
  out <- MinSpanningTree(s, t, d, 4)
  expect_true(is.list(out))
  expect_length(out, 3)
  expect_equal(length(out[[1]]), length(out[[2]]))
  expect_length(out[[3]], 1)

  # 3) Ensure exported functions with `algorithm=`default runs without error, and
  # returns the same if passed no argument
  out2 <- MinSpanningTree(s, t, d, 4, algorithm = "Kruskal")
  expect_identical(out, out2)

  # 4) Ensure exported functions work with all valid algorithms.
  # No alternate algorithsm

  # 5) Ensure exported functions fail if passed an invalid algorithm.
  expect_error(MinSpanningTree(s, t, d, 4, algorithm = "abc"),
               "Invalid")
  expect_error(MinSpanningTree(s, t, d, 4, algorithm = 1),
               "must be a string")
  expect_error(MinSpanningTree(s, t, d, 4, algorithm = NULL),
               "must be a string")
})

test_that("mincostarborescence works", {
  s <- c(1, 2, 3, 4, 5, 6)
  t <- c(2, 3, 4, 5, 6, 1)
  d <- c(1, 10, 3, 2, 0, 8)
  out <- MinCostArborescence(s, t, d, 1, 6)
  expect_equal(out[[1]], c(1, 2, 3, 4, 5))
  expect_equal(out[[2]], c(2, 3, 4, 5, 6))
  expect_equal(out[[3]], 16)
  expect_error(MinCostArborescence(s, t, d, 1, 6, "abc"), "Invalid")
})
