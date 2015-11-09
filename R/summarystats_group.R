
#' Computes the summary statistics used to display the summaryplot for a vector of values.
#'
#' @param partialStats the results of summarystats function applied to multiple datasets and packed in one dataframe.
#' @return min : Minimum value in the vector
#'         q1 :  First quartile
#'         median : Median value
#'         q3 :  Third quartile
#'         max : Maximum value in the vector
#'         mean : Mean value
#'         std : Standard deviation
#'         sum: Sum of all values
#'         count: Number of values N
#' @keywords summarystats federation
#' @export
summarystats_group <- function(partialStats) {

    partialStats <- as.data.frame(partialStats);
    tStats <- as.data.frame(t(partialStats));

	tsum   <- sum(unlist(tStats$sum));
	tcount <- sum(unlist(tStats$count));
    tmin   <- min(unlist(tStats$min));
    tmax   <- max(unlist(tStats$max));
    tmean  <- tsum / tcount;
    # Assuming non overlapping populations
    nx     <- unlist(tStats$count);
    stdx   <- unlist(tStats$std);
    mx     <- unlist(tStats$mean);
    nxy    <- outer(nx, nx, "*");
    diffxy <- outer(mx, mx, "-");
    nddxy  <- nxy * diffxy * diffxy;
    tstd   <- sqrt(sum(nx * stdx * stdx) / tcount + sum(nddxy[lower.tri(nddxy)]) / tcount / tcount);
    # TODO: rubbish maths by LC
    # To replace by a T-digest implementation in R
    # See https://github.com/tdunning/t-digest
    tmedian <- mean(unlist(tStats$median));
    q1 <- mean(unlist(tStats$q1));
    q3 <- mean(unlist(tStats$q3));

    rout <- list(min=tmin, q1=q1, median=tmedian, q3=q3, max=tmax, mean=tmean, std=tstd, sum=tsum, count=tcount);

    return(rout)
}
