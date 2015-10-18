
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

    ymean <- mean(y);
    ymedian <- median(y);
    ystd <- sd(y);
    yq <- quantile(y);
    q1 <- yq[[2]];
    q3 <- yq[[4]];

    rout <- list(tmin, q1, ymedian, q3, tmax, ymean, ystd, tsum, tcount);
    names(rout) <- c("min", "q1", "median", "q3", "max", "mean", "std", "sum", "count");

    return(rout)
}
