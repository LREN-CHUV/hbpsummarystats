context("Federation")

library(hbpsummarystats)

test_that("Summary statistics are correct at the federation level", {

    set.seed(100);

    N1 <- 20;
    y1 <- matrix(rnorm(N1,mean=2,sd=1), N1, 1);

    stats1 <- summarystats(y1);

    set.seed(200);

    N2 <- 10;
    y2 <- matrix(rnorm(N2,mean=4,sd=0.5), N2, 1);

    stats2 <- summarystats(y2);

    stats_check <- summarystats(c(y1,y2));

    intermediateStats <- cbind(stats1, stats2);

    stats_fed <- summarystats_group(intermediateStats);

    expect_equal(stats_fed["min"], stats_check["min"], tolerance = 1e-6);
    # TODO: quartiles are far from good enough
    #expect_equal(stats_fed["q1"], stats_check["q1"],  tolerance = 1e-6)
    #expect_equal(stats_fed["median"], stats_check["median"],  tolerance = 1e-6)
    #expect_equal(stats_fed["q3"], stats_check["q3"],  tolerance = 1e-6)
    expect_equal(stats_fed["max"], stats_check["max"],  tolerance = 1e-6);
    expect_equal(stats_fed["std"], stats_check["std"],  tolerance = 1e-1);
    expect_equal(stats_fed["sum"], stats_check["sum"],  tolerance = 1e-6);
    expect_equal(stats_fed["count"], stats_check["count"]);
})
