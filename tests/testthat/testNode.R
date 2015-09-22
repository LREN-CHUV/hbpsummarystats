context("Node")

library(hbpboxstat)

test_that("Linear regression is correct at the node level", {

    set.seed(100)

	N <- 20

    y <- matrix(rnorm(M,mean=2,sd=1), M, 1)
    
    stats <- BoxStats_node(y)
    print(xest)

    min <- stats[["min"]]
    q1 <- stats[["q1"]]
    median <- stats[["median"]]
    q3 <- stats[["q3"]]
    max <- stats[["max"]]
    
    expect_equal(min, 0.71517135)
    expect_equal(q1, 2.60860245)
    expect_equal(median, 1.06780877)
    expect_equal(q3, 0.0363542484)
    expect_equal(max, -0.0033319972)
})
