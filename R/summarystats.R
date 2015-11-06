
#' Computes the summary statistics used to display the boxplot for a vector of values.
#'
#' @param y Column vector of size N x 1
#' @return min : Minimum value in the vector
#'         q1 :  First quartile
#'         median : Median value
#'         q3 :  Third quartile
#'         max : Maximum value in the vector
#'         mean : Mean value
#'         std : Standard deviation
#'         sum: Sum of all values
#'         count: Number of values N
#' @keywords summarystats
#' @export
summarystats <- function(y) {

    # Lester Melie-Garcia LREN, CHUV.  Lausanne, September 11th, 2015

    ymean   <- mean(y);
    ymedian <- median(y);
    ystd    <- sd(y);
    ymin    <- min(y);
    ymax    <- max(y);
    yq      <- quantile(y);
    q1      <- yq[[2]];
    q3      <- yq[[4]];
    ysum    <- sum(y);
    ycount  <- length(y);

    rout <- list(ymin, q1, ymedian, q3, ymax, ymean, ystd, ysum, ycount);
    names(rout) <- c("min", "q1", "median", "q3", "max", "mean", "std", "sum", "count");

    return(rout)
}
