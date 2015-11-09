context("Federation");

library(hbpsummarystats);

test_that("Table summary statistics are correct at the federation level", {

    N <- 20;
    set.seed(100);
    rowA1 <- rnorm(N,mean=2,sd=1);
    set.seed(200);
    rowB1 <- rnorm(N,mean=4,sd=0.7);

    y1 <- data.frame(id=1:20, a=rowA1, b=rowB1);

    stats1 <- tablesummarystats(y1, c("a", "b"));

    set.seed(100);
    rowA2 <- rnorm(N,mean=3,sd=0.8);
    set.seed(200);
    rowB2 <- rnorm(N,mean=4,sd=1.2);

    y2 <- data.frame(id=1:20, a=rowA2, b=rowB2);

    stats2 <- tablesummarystats(y2, c("a", "b"));

    intermediateStats <- list(stats1,stats2);

    stats <- tablesummarystats_group(intermediateStats);

    expect_equal(stats[['min','a']],    1.086186,  tolerance = 1e-6);
    # TODO: quartiles are far from good enough
    #expect_equal(stats[['q1','a']],     2.169601,  tolerance = 1e-6);
    #expect_equal(stats[['median','a']], 2.583772,  tolerance = 1e-6);
    #expect_equal(stats[['q3','a']],     2.830018,  tolerance = 1e-6);
    expect_equal(stats[['max','a']],    4.848237,  tolerance = 1e-6);
    expect_equal(stats[['mean','a']],   2.59708,   tolerance = 1e-6);
    expect_equal(stats[['std','a']],    0.8140788, tolerance = 1e-6);
    expect_equal(stats[['sum','a']],    103.8832,  tolerance = 1e-6);
    expect_equal(stats[['count','a']],  40);

    expect_equal(stats[['min','b']],    1.753906,  tolerance = 1e-6);
    #expect_equal(stats[['q1','b']],     3.515562,  tolerance = 1e-6);
    #expect_equal(stats[['median','b']], 3.972428,  tolerance = 1e-6);
    #expect_equal(stats[['q3','b']],     4.264085,  tolerance = 1e-6);
    expect_equal(stats[['max','b']],    5.730528,  tolerance = 1e-6);
    expect_equal(stats[['mean','b']],   3.936581,  tolerance = 1e-6);
    expect_equal(stats[['std','b']],    0.7717581, tolerance = 1e-6);
    expect_equal(stats[['sum','b']],    157.4632,  tolerance = 1e-6);
    expect_equal(stats[['count','b']],  40);

})
