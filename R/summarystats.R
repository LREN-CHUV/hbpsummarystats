
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

    if (class(y) == 'matrix' && ncol(y) == 1) {
        y <- as.vector(y);
    }

    rout <- switch(class(y),
        numeric = {
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
      
          list(type='numeric',
               min=ymin,
               q1=q1,
               median=ymedian,
               q3=q3,
               max=ymax,
               mean=ymean,
               std=ystd,
               sum=ysum,
               count=ycount,
               factors=NA);
        },
        integer = {
          ymin    <- min(y);
          ymax    <- max(y);
          ysum    <- sum(y);
          ycount  <- length(y);
      
          list(type='integer',
               min=ymin,
               q1=NA,
               median=NA,
               q3=NA,
               max=ymax,
               mean=NA,
               std=NA,
               sum=ysum,
               count=ycount,
               factors=NA);
        }, {
          ycount  <- length(y);
          factors <- levels(factor(y));

          if (length(factors) == 0 || length(factors) > 1000) {
            factors <- NA;
          }

          list(type=class(y),
               min=NA,
               q1=NA,
               median=NA,
               q3=NA,
               max=NA,
               mean=NA,
               std=NA,
               sum=NA,
               count=ycount,
               factors=factors);
        }
    );

    return (rout)
}
