context("Node")

library(hbpboxstats)

test_that("Box statistics are correct at the node level", {

    set.seed(100)

    N <- 20
    y <- matrix(rnorm(N,mean=2,sd=1), N, 1)

    stats <- boxstats(y)

    min <- stats[["min"]]
    q1 <- stats[["q1"]]
    median <- stats[["median"]]
    q3 <- stats[["q3"]]
    max <- stats[["max"]]

    str(min)

    expect_equal(min, 1.086186, tolerance = 1e-6)
    expect_equal(q1, 1.63289,  tolerance = 1e-6)
    expect_equal(median, 2.09308,  tolerance = 1e-6)
    expect_equal(q3, 2.366687,  tolerance = 1e-6)
    expect_equal(max, 4.310297,  tolerance = 1e-6)
})
