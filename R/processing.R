#' picks experiment start date
#'
#' This function takes a matrix of tumour volumes per date and returns the
#' first date measured above a set weight.
#'
#' @param data matrix of the format: rows = mice, columns = dates,
#' fill = tumour volume.
#' @param threshold weight above which the animal is ready for treatment.
#' @return vector of start dates.
#' @export
startpick <- function(data = micemat , threshold = 90){

  startdate <- c()
  dat <- data
  for (row in seq_along(1:nrow(dat))) {
    startdate[row] <- colnames(dat)[which(dat[row, ] >= threshold,
                                          arr.ind = TRUE)[1]]
  }
  startdate <- as.Date(startdate, tryFormats = c("%d/%m/%Y",
                                                 "%Y-%m-%d"))
  return(startdate)
}

#' experiment time
#'
#' This function calculates how many days have passed for each animal since
#' they were eligble for the experiment.
#'
#' @param startdate Start date of the animal/experiment. As calculated by startpick
#' @param curdate The current date, or the date at which point the
#' experiment ended.
#' @return A vector of days.
#' @export
exprun <- function(startdate = begin, curdate = Sys.Date()) {

  start <- startdate
  curdate <- as.Date(curdate, tryFormats = c("%d/%m/%Y",
                                             "%Y-%m-%d",
                                             "%Y/%m/%d"))
  treatment_time <- curdate - start
  return(treatment_time)
}

#' tumour growth
#'
#' This function grabs the earliest measured tumour size sinds experiment start
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
  colnames(miceDT) <- as.character(as.Date.character(colnames(matrix),
                                        tryFormats = c("%d/%m/%Y",
                                                       "%Y-%m-%d")))
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
  g <- endweight - startweight
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
  coldate <- as.Date(datecolumn, tryFormats = c("%d/%m/%Y",
                                                "%Y-%m-%d",
                                                "%Y/%m/%d"))
  for (i in seq_along(1: intervalcount)){
    int[i] <- coldate[i + 1] - coldate[i]
  }
  return(int)
}

#' matrix gap fill.
#'
#' Before we can plot anything we need to know how the data looks with days
#' between measurements included, we do this by filling in the missing columns
#' using this function.
#'
#'
#' @param data matrix of the format: rows = mice, columns = dates,
#' fill = tumour volume.
#' @param intervaltime  Vector of the length in days of each interval between
#' two measurements.
#' @param datecolumn A vector of dates of tumour measurement.
#' @return a matrix where every column is a day, if no measurements were made
#' that day the column is filled with NA.
#' @export
filldate <- function(data, intervaltime, datecolumn = colnam){

  coldate <- as.Date(datecolumn, tryFormats = c("%d/%m/%Y",
                                                "%Y-%m-%d",
                                                "%Y/%m/%d"))
  explength <- sum(intervaltime)
  alldays <- seq(coldate[1], length=explength+1, by="+1 day")
  newmat<-matrix(ncol = length(alldays), nrow = nrow(data))
  matchdates<- match(coldate,alldays)
  for (i in seq_along(1:ncol(data))){
    colu<-matchdates[i]
    newmat[,colu]<-data[,i]
  }
  colnames(newmat) <- as.character(alldays)
  return(newmat)
}

#' experimental matrix setup
#'
#' This function creates a matrix of the tumour size per date with all data
#' starting at the experimental start point. e.g., the first column is
#' experiment day 1 for all mice.
#'
#'
#' @param filledmatrix matrix of the format: rows = mice, columns = dates,
#' fill = tumour volume. This function does not allow columns to be skipped.
#' each day must be represented as a column.
#' @param intervaltime  Vector of the length in days of each interval between
#' two measurements.
#' @param startdate Start date of the animal/experiment.
#' As calculated by startpick
#' @return A matrix where the first column is day 1 of experiment. column names
#'  are days since onset experiment, columns are different mice.
#' @export
plotmatrix <- function(filledmatrix = micematrix,
                       startdate = first,
                       intervaltime = int) {
  miceDT <- as.data.table(filledmatrix)
  end <- ncol(filledmatrix)
  plotmat <- matrix(ncol = end)
  alldays <- as.Date(colnames(filledmatrix), tryFormats = c("%d/%m/%Y",
                                                            "%Y-%m-%d",
                                                            "%Y/%m/%d"))
  for (i in seq_along(1:length(startdate))) {
  index <- match(as.Date(startdate[i], tryFormats = c("%d/%m/%Y",
                                                      "%Y-%m-%d",
                                                      "%Y/%m/%d")), alldays)
  # cycle over the begin dates,
  # as this vector corresponds to each row in the matrix.
  row <- miceDT[i ,c(index:end), with = F]
  # fill up remaining space with NA.
  row <- c(unlist(row), rep(NA, times = (index-1)))
  plotmat <- rbind(plotmat, row)
  }
  plotmat <- plotmat[-1, ]
  # dont need this anymore, could be a usefull snippet to create interval collnames from.
  # x <- c()
  # z <- c()
  # for ( i in seq_along(1:length(int))) {

  #   z <-sum(int[1:i])
  #   x <- c(x, z)
  #   y<- c(1, (x))
  # }

  colnames(plotmat) <- c(1:length(alldays))
  return(plotmat)
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
  coldate <- as.Date(datecolumn, tryFormats = c("%d/%m/%Y",
                                                "%Y-%m-%d",
                                                "%Y/%m/%d"))
  # for each mice calculate if there as growth or remission in that timeframe.
  intgrowth <- matrix(nrow = nrow(volumematrix), ncol = intervalcount)

  for ( n in seq_along(1 : nrow(volumematrix))) {
    i <- 1

    for (i in seq_along(1 : intervalcount)) {
      x <- i+1
      intgrowth[n, i] <- volumematrix[n , x] - volumematrix[n, i]
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
  rownames(growthsum)<- c("growth", "remission", "stable")
  return(growthsum)
}

#' Exclude mice
#'
#' This function determines which mice to exclude from the study. I does this
#' by checking if a mice which lack entry data in the final measurement has
#' got a date of death.
#'
#' @param filledmat matrix of the format: rows = mice, columns = dates,
#' fill = tumour volume. This function does not allow columns to be skipped.
#' each day must be represented as a column.
#' @param culdat A dataframe which contains the date of death of each mice.
#' expected format is: 1. Cage, 2. Treatment, 3. Mice ID, 4. Date of death in
#' format day/month/Year.
#'
#' @return A TRUE/FALSE vector which indicates if a mice should be excluded or
#' not.
#' @export
exclude <- function(filledmat, culdat) {
  lastdat <- filledmat[,ncol(filledmat)]
  end <- as.character.Date(culdat[[4]], tryFormats = c("%d/%m/%Y",
                                             "%Y-%m-%d",
                                             "%Y/%m/%d"))
  excl <- c()
  for (i in seq_along(1:length(end))) {
    if (is.na(end[i]) && is.na(lastdat[i])) {
      excl[i]<-TRUE
    } else {excl[i] <- FALSE}
  }
  return(excl)
}

#' Mice survival data preparation
#'
#' This function prepares the data for a survival analysis. calculating
#' the survival time of each animal and if the data is censored or not.
#'
#' @param startdate Start date of the animal/experiment as calculated by
#'  startpick
#' @param treatmenttime how many days have passed for each animal since
#' they were eligble for the experiment.
#' @param culdat A dataframe which contains the date of death of each mice.
#' expected format is: 1. Cage, 2. Treatment, 3. Mice ID, 4. Date of death in
#' format day/month/Year.
#'
#' @return a table with the Treatment, survival time and binary survival
#' indication (1 = death, 0 = alive).
#' @export
survdata <- function(startdate, treatmenttime, culdat) {
  start <- startdate
  end <- as.Date(culdat[[4]], tryFormats = c("%d/%m/%Y",
                                             "%Y-%m-%d",
                                             "%Y/%m/%d"))
  frame <- culdat[,c(1:3)]
  censored <- is.na(end)
  survtime <- (end - start)

  for (i in seq_along(1:length(censored))) {
    if (censored[i]) {
      survtime[i] <- treatmenttime[i]
    }
  }
  binarysurvi <- sapply(censored, function(z) {
    if (z == FALSE) {1} else
      if (z == TRUE) {0}
  }, USE.NAMES = F, simplify = T)

  survivaldat <- cbind(frame[,2], survtime, binarysurvi)
  colnames(survivaldat) <- c("Treatment", "survtime", "binarysurvi")

  return(survivaldat)
}


