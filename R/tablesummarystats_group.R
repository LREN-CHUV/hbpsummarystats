
#' Computes the statistics used to display the summaryplot for a vector of values.
#'
#' @param listStats the results of tablesummarystat function applied to multiple datasets and packed in one list.
#' @return min : Minimum value in the vector
#'         q1 :  First quartile
#'         median : Median value
#'         q3 :  Third quartile
#'         max : Maximum value in the vector
#'         mean : Mean value
#'         std : Standard deviation
#'         sum: Sum of all values
#'         count: Number of values N
#' @keywords summarystats table tablesummarystats federation
#' @export
tablesummarystats_group <- function(listStats) {

	groups <- names(as.data.frame(listStats[1]));
	regrouped <- list();

	for (g in groups) {
		ext <- sapply(listStats, function(df) df[,g]);
		rn <- rownames(ext)
		if (is.null(rn)) {
		    rownames(ext) <- c("min", "q1", "median", "q3", "max", "mean", "std", "sum", "count");
	    }
		regrouped <- cbind(regrouped, summarystats_group(ext));
	}

	colnames(regrouped) <- groups;

    return(regrouped)
}
