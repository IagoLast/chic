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


preparePrePostData <- function(data, patiend_id_name, timepoint_label, pre_name, post_name, variable_name) {
  clean <- data.frame();

  for (id in unique(data[[patiend_id_name]])) {

    i <- data[data[[patiend_id_name]] == id, c(timepoint_label, variable_name)]

    itemPre   <- suppressWarnings(as.numeric(i[ i[[timepoint_label]] == pre_name,  variable_name ]))
    itemPost  <- suppressWarnings(as.numeric(i[ i[[timepoint_label]] == post_name, variable_name ]))

    if(!is.na(itemPre) && !is.na(itemPost) ) {
      newItem <- data.frame(itemPre, itemPost)
      names(newItem)<-c("PRE","POST")
      clean <- rbind(clean, newItem)
    }
  }

  return(clean)
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
    color <- "gray";

    if (x[i] > 10) {
      color <- "red"
    } else {
      color <- "green"
    }

    lines(c(x[i], y[i]), col=color)
  }
}


chic_pre_post <- function(data, patient_id_name, timepoint_label, pre_name, post_name, variable_name) {
  
  d <- preparePrePostData(data, patient_id_name, timepoint_label, pre_name, post_name, variable_name);
  prePost(d$PRE, d$POST, main="PRE/POST", ylab=variable_name, xlab="", labels=c('PRE', 'POST'))
}

