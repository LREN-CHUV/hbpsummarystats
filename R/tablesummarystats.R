#' Computes the minimum, maximum, media, quantiles and other parameters to be used for plotting.
#' Only those variables that are numeric will be return with the previous statistics.
#'
#' @param ytable Data table type data.frame.
#' @param ycols2plot Names of the variables in the Table that will be plotted.
#' @return fstats : Statistic necessary for plotting: minimum, maximum, media, quantiles. This is data.frame type.
#' @keywords summarystats table tablesummarystats
#' @export
tablesummarystats <- function(ytable, ycols2plot) {

  # Lester Melie-Garcia
  # LREN, CHUV.
  # Lausanne, September 22nd, 2015

  yvarnames <- colnames(ytable);
  # The list of columns is sometimes wrongly wrapped into a list
  # following a call to strsplit for example
  ycols2plot <- unlist(ycols2plot);
  inds <- which(yvarnames %in% ycols2plot);
  ysubset <- ytable[,inds];
  if (length(ycols2plot) == 1) {
    yfinal <- cbind(ysubset)
    colnames(yfinal) <- ycols2plot
  } else {
  
    # To know the data type of each column in the table.
    ydatatype <- sapply(ysubset, class);
    indst <- which(ydatatype == "integer" | ydatatype == "numeric");
    yfinal <- ysubset[,indst];
    if (length(ycols2plot) == 1) {
      yfinal <- cbind(yfinal)
      colnames(yfinal) <- ycols2plot
    }
  }

  if (ncol(yfinal) == 1) {
    fstats <- cbind(summarystats(yfinal));
    colnames(fstats) <- ycols2plot
  } else {
    fstats <- sapply(yfinal, summarystats);
  }

  return(fstats)
}
