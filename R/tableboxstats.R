#' Computes the minimum, maximum, media, quantiles and other parameters to be used for plotting.
#' Only those variables that are numeric will be return with the previous statistics.
#'
#' @param ytable Data table type data.frame.
#' @param ycols2plot Names of the variables in the Table that will be plotted.
#' @return fstats : Statistic necessary for plotting: minimum, maximum, media, quantiles. This is data.frame type.
#' @keywords boxplot
#' @export
tableboxstats <- function(ytable, ycols2plot) {

  # Lester Melie-Garcia
  # LREN, CHUV.
  # Lausanne, September 22nd, 2015

  yvarnames <- colnames(ytable);
  nc <- length(ycols2plot);
  inds <- NULL;
  for (i in 1:nc) {
    inds[i] <- which(yvarnames == ycols2plot[i]);
  }

  if (length(inds) == 1) {
    ysubset <- ytable;
  } else {
      ysubset <- ytable[,inds];
  }

  # To know the data type of each column in the table.
  ydatatype <- sapply(ysubset, class);
  indst <- which(ydatatype == "integer" | ydatatype == "numeric");

  yfinal <- ysubset[,indst];
  M <- length(indst);
  fstats <- matrix(0, 5, M);
  ynum <- data.matrix(yfinal);
  for (i in 1:M){
    fstats[1,i] <- min(ynum[,i]);
    yq <- quantile(ynum[,i]); q1 <- yq[[2]]; q3 <- yq[[4]];
    fstats[2,i] <- q1;
    fstats[3,i] <- median(ynum[,i]);
    fstats[4,i] <- q3;
    fstats[5,i] <- max(ynum[,i]);
  }

  fstats <- data.frame(fstats);
  if (length(inds) == 1) {
    names(fstats) <- colnames(ytable);
  } else {
    names(fstats) <- colnames(yfinal);
  }

  row.names(fstats) <- c("min", "q1", "median", "q3", "max");

  return(fstats)
}
