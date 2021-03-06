context("Node");

library(testthat);
library(hbpsummarystats);

test_that("Summary statistics are correct at the node level", {

    set.seed(100);

    N <- 20;
    y <- matrix(rnorm(N,mean=2,sd=1), N, 1);

    stats <- summarystats(y);

    type <- stats[["type"]];
    min <- stats[["min"]];
    q1 <- stats[["q1"]];
    median <- stats[["median"]];
    q3 <- stats[["q3"]];
    max <- stats[["max"]];
    std <- stats[["std"]];
    sum <- stats[["sum"]];
    count <- stats[["count"]];

    expect_equal(type,   "numeric");
    expect_equal(min,    1.086186,  tolerance = 1e-6);
    expect_equal(q1,     1.63289,   tolerance = 1e-6);
    expect_equal(median, 2.09308,   tolerance = 1e-6);
    expect_equal(q3,     2.366687,  tolerance = 1e-6);
    expect_equal(max,    4.310297,  tolerance = 1e-6);
    expect_equal(std,    0.7185645, tolerance = 1e-6);
    expect_equal(sum,    42.15734,  tolerance = 1e-6);
    expect_equal(count, N);
})

test_that("Summary statistics are correct at the node level, using another distribution", {

    set.seed(200);

    N <- 10;
    y <- matrix(rnorm(N,mean=4,sd=0.5), N, 1);

    stats <- summarystats(y);

    type <- stats[["type"]];
    min <- stats[["min"]];
    q1 <- stats[["q1"]];
    median <- stats[["median"]];
    q3 <- stats[["q3"]];
    max <- stats[["max"]];
    std <- stats[["std"]];
    sum <- stats[["sum"]];
    count <- stats[["count"]];

    expect_equal(type,   "numeric");
    expect_equal(min,    3.489711,  tolerance = 1e-6);
    expect_equal(q1,     3.964479,  tolerance = 1e-6);
    expect_equal(median, 4.063227,  tolerance = 1e-6);
    expect_equal(q3,     4.190516,  tolerance = 1e-6);
    expect_equal(max,    4.709936,  tolerance = 1e-6);
    expect_equal(std,    0.3125887, tolerance = 1e-6);
    expect_equal(sum,    40.75867,  tolerance = 1e-6);
    expect_equal(count, N);

})

test_that("Summary statistics are correct at the node level, using integers", {

    y <- as.integer(c(8, 9, 7, 5, 4, -1));
    N <- 6;

    stats <- summarystats(y);

    type <- stats[["type"]];
    min <- stats[["min"]];
    max <- stats[["max"]];
    sum <- stats[["sum"]];
    count <- stats[["count"]];

    expect_equal(type,  "integer");
    expect_equal(min,   -1);
    expect_equal(max,   9);
    expect_equal(sum,   32);
    expect_equal(count, N);

})

test_that("Summary statistics are correct at the node level, using string factors", {

    y <- c("a", "b", "c", "a", "a", "b");
    N <- 6;

    stats <- summarystats(y);

    type <- stats[["type"]];
    count <- stats[["count"]];
    factors <- stats[["factors"]];

    expect_equal(type,   "character");
    expect_equal(count, N);
    expect_equal(factors, c("a", "b", "c"));

})
