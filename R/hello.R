highLow <- function(x, threshold) {
  if(is.na(x)) {
    return(NA)
  }

  if(!is.numeric(x)){
    return(NA)
  }

  if ( x > threshold) {
    return('High')
  }

  return('Low')
}

#' Generate a vector with "High" or "Low" values depending on the original
#' value compared to the median of the data.
#'
#' @param data A numeric dataframe.
#' @return A string dataframe with only 2 possible values.
binarizeMedian <- function(data) {
  if(anyNA(data) || !is.numeric(data))  {
    stop('Data contains NA or non numeric values');
  }
  lapply(data, function(x) highLow(x, median(data)))
}

#' Draw a pre-post line for 2 given dataframes.
#'
#' @param data x A numeric dataframe of length n
#' @param data y A numeric dataframe of length n
prePost <- function(x, y, main="PRE/POST", ylab="", xlab="", labels=c('PRE', 'POST')) {
  if(length(x) != length(y)) {
    stop('Data has to have the same length in order to plot the pre/post graph.')
  }

  plot(
    c(0, 0),
    c(0,0),
    type="n",
    xlab=xlab,
    ylab=ylab,
    xlim=c(0.95, 2.05),
    ylim=c( min( min(x, na.rm=T), min(y, na.rm=T)), max(max(x, na.rm=T), max(y, na.rm=T))),
    xaxt = "n",
    main = main
  )

  # Add x ticks
  axis(1, at=c(1, 2), labels=labels)

  for(i in 1:length(x)){
    lines(c(x[i], y[i]), col=ifelse(x[i]>y[i],"green2", ifelse(x[i]==y[i],"blue", "red")))
  }
}
