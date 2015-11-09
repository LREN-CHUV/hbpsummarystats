context("Node");

library(hbpsummarystats);

test_that("Table summary statistics are correct at the node level", {

    N <- 20;
    set.seed(100);
    rowA <- rnorm(N,mean=2,sd=1);
    set.seed(200);
    rowB <- rnorm(N,mean=4,sd=0.7);

    y <- data.frame(id=1:20, a=rowA, b=rowB);

    stats <- tablesummarystats(y, c("a", "b"));

    expect_equal(stats[['min','a']],    1.086186,  tolerance = 1e-6);
    expect_equal(stats[['q1','a']],     1.63289,   tolerance = 1e-6);
    expect_equal(stats[['median','a']], 2.09308,   tolerance = 1e-6);
    expect_equal(stats[['q3','a']],     2.366687,  tolerance = 1e-6);
    expect_equal(stats[['max','a']],    4.310297,  tolerance = 1e-6);
    expect_equal(stats[['mean','a']],   2.107867,  tolerance = 1e-6);
    expect_equal(stats[['std','a']],    0.7185645, tolerance = 1e-6);
    expect_equal(stats[['sum','a']],    42.15734,  tolerance = 1e-6);
    expect_equal(stats[['count','a']],  N);

    expect_equal(stats[['min','b']],    2.689778,  tolerance = 1e-6);
    expect_equal(stats[['q1','b']],     3.643046,  tolerance = 1e-6);
    expect_equal(stats[['median','b']], 3.979684,  tolerance = 1e-6);
    expect_equal(stats[['q3','b']],     4.194589,  tolerance = 1e-6);
    expect_equal(stats[['max','b']],    5.009475,  tolerance = 1e-6);
    expect_equal(stats[['mean','b']],   3.95327,   tolerance = 1e-6);
    expect_equal(stats[['std','b']],    0.5498117, tolerance = 1e-6);
    expect_equal(stats[['sum','b']],    79.0654,   tolerance = 1e-6);
    expect_equal(stats[['count','b']], N);

})
