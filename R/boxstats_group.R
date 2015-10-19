
#' Computes the statitics used to display the boxplot for a vector of values.
#'
#' @param partialStats the results of boxstat function applied to multiple datasets and packed in one dataframe.
#' @return min : Minimum value in the vector
#'         q1 :  First quartile
#'         median : Median value
#'         q3 :  Third quartile
#'         max : Maximum value in the vector
#'         mean : Mean value
#'         std : Standard deviation
#' @keywords boxplot
#' @export
boxstats_group <- function(partialStats) {

	tsum   <- sum(partialStats[["sum"]])
	tcount <- sum(partialStats[["count"]])
    tmin   <- min(partialStats[["min"]]);
    tmax   <- max(partialStats[["max"]]);
    tmean <- tsum / tcount;
    # Assuming non overlapping populations
    nx <- partialStats[["count"]]
    stdx <- partialStats[["std"]]
    mx <- partialStats[["mean"]]
    nxy <- outer(nx, nx, "*")
    diffxy <- outer(mx, mx, "-")
    nddxy <- nxy * diffxy * diffxy
    tstd <- sqrt(sum(nx * stdx * stdx) / tcount + sum(nddxy[lower.tri(nddxy)]) / tcount / tcount);
    # TODO: rubbish maths by LC
    # To replace by a T-digest implementation in R
    # See https://github.com/tdunning/t-digest
    tmedian <- mean(partialStats[["median"]]);
    q1 <- mean(partialStats[["q1"]]);
    q3 <- mean(partialStats[["q3"]]);

    rout <- list(tmin, q1, tmedian, q3, tmax, tmean, tstd, tsum, tcount);
    names(rout) <- c("min", "q1", "median", "q3", "max", "mean", "std", "sum", "count");

    return(rout)
}
