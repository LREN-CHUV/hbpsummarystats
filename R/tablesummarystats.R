#' Computes the minimum, maximum, media, quantiles and other parameters to be used for plotting.
#' Only those variables that are numeric will be return with the previous statistics.
#'
#' @param data Data table type data.frame.
#' @param columns Names of the variables in the Table that will be plotted.
#' @return fstats : Statistic necessary for plotting: minimum, maximum, media, quantiles. This is data.frame type.
#' @keywords summarystats table tablesummarystats
#' @export
tablesummarystats <- function(data, columns) {

  # Lester Melie-Garcia
  # LREN, CHUV.
  # Lausanne, September 22nd, 2015

  yvarnames <- colnames(data);
  # The list of columns is sometimes wrongly wrapped into a list
  # following a call to strsplit for example
  columns <- unlist(columns);
  ysubset <- as.data.frame(data[ , which(yvarnames %in% columns)]);
  colnames(ysubset) <- columns;
  fstats <- sapply(ysubset, summarystats);

  return(fstats)
}
