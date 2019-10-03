#' picks experiment start date
#'
#' This function takes a matrix of tumour volumes per date and returns the
#' first date measured above a set weight.
#'
#' @param data matrix of the format: rows = mice, columns = dates,
#' fill = tumour volume.
#' @param weight weight above which the animal is ready for treatment.
#' @return vector of start dates.
#' @export
startpick <- function(data = micemat , weight = 90){
  startdate <- c()
  dat <- data
  for (row in seq_along(1:nrow(dat))) {
    startdate[row] <- colnames(dat)[which(dat[row, ] >= 90,
                                          arr.ind = TRUE)[1]]
    startdate <- as.charater.Date(startdate, format = "%d/%m/%Y")

  }
  return(startdate)
}

#' experiment time
#'
#' This function calculates how many days have passed for each animal since
#' they were eligble for the experiment.
#'
#' @param start Start date of the animal/experiment. As calculated by startpick
#' @param curdate The current date, or the date at which point the
#' experiment ended.
#' @return A vector of days.
#' @export
exprun <- function(start = begin, curdate = Sys.Date()) {
  # (TODO) : what if an animal has been culled before the current date?
  start <- as.Date(start, format = "%d/%m/%Y")
  curdate <- as.Date(curdate, format = "%d/%m/%Y")
  treatment_time <- curdate - start
  return(as.vector(treatment_time))
}

#' tumour growth
#'
#'This function grabs the earliest measured tumour size sinds experiment start
#' and substracts the last measured tumour size.
#'
#' @param matrix matrix of the format: rows = mice, columns = dates,
#' fill = tumour volume.
#' @param startdate Start date of the animal/experiment.
#' As calculated by startpick
#' @return A vector of difference in tumour size.
#' @export
tumgrowth <- function(matrix = micematrix, startdate = begin) {
  # grabs the earliest measured tumour size above 90 mm and
  # substracts the last measured tumour size.
  #
  # Args: matrix = a matrix trimmed of everything but mice per row and
  #                date as column.
  # start = vector of starting dates
  # (note these should be R Date format, not characters)
  #
  # Returns: a vector with size differences per mice.
  miceDT <- as.data.table(matrix)
  startweight <- c()
  endweight <- c()
  #grab the tumour size on the start day
  startchar <- as.character(startdate)
  for (n in seq_along(1 : length(startchar))) {
    startweight[n] <- miceDT[n, startchar[n], with = F]
  }
  startweight <- unlist(startweight, use.names = F)

  #grab tumour size at the end.

  for (n in seq_along(1 : nrow(miceDT))) { # go along the mice (rows)
    DTle <- length(miceDT)
    # DTle is the last entry of the matrix
    endweight[n] <- miceDT[n, DTle, with = F]
    if(is.na(endweight[n])) { #if the last entry of the matrix is NA
      # go into while loop to return to the latest available value
      while (is.na(endweight[n])) {
        DTle <- DTle - 1
        endweight[n] <- miceDT[n, DTle, with = F]
      }
    }
  }
  endweight <- unlist(endweight, use.names = F)
  #calculate tumour size difference.
  g <- startweight - endweight
  return(g)
}

#' measurement intervals
#'
#' This function calculates the interval in days between measurements
#'
#' @param volumematrix matrix of the format: rows = mice, columns = dates,
#' fill = tumour volume.
#' @param datecolumn Vector of dates of measurements, in case they differ from
#' the column names.
#' @return A vector of difference in tumour size.
#' @export
exptime <- function(volumematrix = micematrix, datecolumn = col ){

   # calculate the days between time points.
  intervalcount <- ncol(volumematrix)-1
  int <- c()
  coldate <- as.Date(datecolumn, format = "%d/%m/%Y")
  for (i in seq_along(1: intervalcount)){
    print(coldate[i+1])
    print(coldate[i])
    int[i] <- coldate[i + 1] - coldate[i]
  }
  return(int)
}

#' growth over intervals
#'
#' This function calculates the growth of a tumour over each of the measurement
#'  interval.
#'
#' @param volumematrix matrix of the format: rows = mice, columns = dates,
#' fill = tumour volume.
#' @param datecolumn Vector of dates of measurements, in case they differ from
#' the column names.
#' @return A matrix of growth of each mice (rows) per intervan (column).
#' @export
growthinterval <- function(volumematrix = micematrix, datecolumn = col) {

  intervalcount <- ncol(volumematrix)-1
  coldate <- as.Date(datecolumn, format = "%d/%m/%Y")
  # for each mice calculate if there as growth or remission in that timeframe.
  intgrowth <- matrix(nrow = nrow(volumematrix), ncol = intervalcount)

  for ( n in seq_along(1 : nrow(volumematrix))) {
    i <- 1

    for (i in seq_along(1 : intervalcount)) {
      x <- i+1
      intgrowth[n, i] <- volumematrix[n , i] - volumematrix[n, x]
      # you could make a heatmap of this or use it for different plotting purposes.

    }
  }
  nam <- c()
  for (q in seq_along(1:intervalcount)) {

    nam[q] <- c(paste0("int ", q))
  }
  colnames(intgrowth) <- nam
  return(intgrowth)
}


#' summary of tumour progression
#'
#' This function summates the amount of days a tumour sees growth, recession or
#' remains at the same volume over the course of treatment.
#'
#' @param intermatrix a matrix of tumour volume over intervals, as generated by
#' growthinterval. The matrix should be mice (rows) x intervals (columns).
#' @param intervaltime  Vector of the length in days of each interval between
#' two measurements.
#'
#' @return a data.frame of three columns (growth, remission, stable) with
#' counts for each mice.
#' @export
growthindicator <- function(intermatrix = intgrowth, intervaltime = int){
  # sum the days of growth vs days of remission per mice.
  growthsum <- apply(intermatrix, MARGIN = 1, function(x){
    pos  <- 0
    neg  <- 0
    stab <- 0
    temp <- c()
    ret <-c()
    q <- x[!is.na(x)]
    for (i in seq_along(1 : length(q))) {

      if(q[i] > 0) {
        pos <- pos + (1 * intervaltime[i])
      } else if (q[i] < 0) {
        neg <- neg + (1 * intervaltime[i])
      } else {
        stab <- stab + (1* intervaltime[i])
      }
      temp <- c(pos,neg,stab)
    }
    ret <- rbind(ret, temp)

  })
  rownames(growthsum)<- c("growth", "remissiom", "stable")
  return(growthsum)
}
