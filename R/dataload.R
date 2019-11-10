#' calculate sphere volume
#'
#' This function takes height and width of a tumour and calculates the
#' volume. As the width and height are interchangable in a mice use the highest
#' value as height.
#'
#' @param x widght of a tumour in mm.
#' @param y height of a tumour in mm.
#' @return volume in mm3.
#' @export
calcsize <- function(x, y) {

  size <- (x * y^2 * pi) / 6
  return (size)
}
#' calculate sphere volume
#'
#' This function takes height and width of a tumour and calculates the
#' area. Some lisences require this parameter to decide if an animal requires
#' to be culled.
#'
#' @param x widght of a tumour in mm.
#' @param y height of a tumour in mm.
#' @param type either "square" or "circle".
#' @return area in mm2.
#' @export
calcarea <- function(x, y, type = "square") {

  if (type == "square") {
    area <- x * y
  } else if (type == "circle" ) {

    if (x < 0.5*y && x > 1.5*y) {
      warning("x and y of tumour differ more then 50%, perhaps the tumour area
      is not circular.")
    }

    largest <- sort(c(x,y),decreasing = TRUE)[1]
    area <- pi*largest^2
  }
}

#' create tumour volume matrix
#'
#' This function reads an input file containing  height and width data and
#' returns a matrix with rows = mice and columns = dates of measurement.
#'
#' @param measure_data A table with the first 3 columns indicating sample data:
#' cage, treatment, mouse/repeat. The remaining columns are dates with the
#' height and width measurements. e.g. 01-01-2020 | 01-01-2020.
#' Make sure the heigher value (height) is in the first column and the lower
#' (width) in the second.
#' @param precolumns An numeric indicator how many columnes in the file
#' before the volumetric data.
#' #' @param max A value above which tumour entries will be removed from the
#' analysis.
#' @return A matrix of tumour volumes per mice (rows) for each
#'  measurement date (columns).
#' @import data.table
#' @export
tumcalc <- function(measure_data, precolumns = 3, max = NULL) {
  vols <- data.table::fread(measure_data, header = TRUE)
  voluse <- vols[, -c(1:eval(precolumns)), with = F]
  sizedat <- vols[, c(1:(eval(precolumns-1))), with = F]
  for (i in seq(1, ncol(voluse), by=2)) {

    foo1 <- voluse[, c(i : (i + 1)), with = F]
    bar <- mapply(calcsize, foo1[, 1], foo1[, 2])
    sizedat <- cbind(sizedat,bar)
    micematrix <- as.matrix(sizedat[, -c(1:eval(precolumns-1)), with = F])
    col <- colnames(micematrix)
    colnames(micematrix) <- as.character.Date(
        col, tryFormats = c("%d/%m/%Y", "%Y-%m-%d", "%Y/%m/%d"))
  }
  micematrix[micematrix < 0] <- 0
  if(!is.null(max)) {
    micematrix[micematrix > max] <- NA
  }
  return(micematrix)
}

#' create tumour volume matrix
#'
#' This function reads an input file containing tumour volume data and
#' returns a matrix with rows = mice and columns = dates of measurement.
#'
#' @param file A table with the first 3 columns indicating sample data:
#' cage, treatment, mouse/repeat. The remaining columns are dates with the
#' tumour volume on that day. e.g. 01-01-2020 | 02-01-2020
#' @param max A value above which tumour entries will be removed from the
#' analysis.
#' @param precolumns An numeric indicator how many columnes in the file
#' before the volumetric data.
#' @return A matrix of tumour volumes per mice (rows) for each
#'  measurement date (columns).
#' @import data.table
#' @export
dataprep <- function(file, precolumns = 3,  max=NULL) {

  # returns a matrix of the volumetric data, with dates as column names.
  micedata <- data.table::fread(file, header = TRUE)
  micematrix <- as.matrix(micedata[, -c(1:eval(precolumns)), with = F])
  col <- colnames(micematrix)
  colnames(micematrix) <- as.character.Date(
      col, tryFormats = c("%d/%m/%Y", "%Y-%m-%d", "%Y/%m/%d"))
  micematrix[micematrix < 0] <- 0
  if(!is.null(max)) {
  micematrix[micematrix > max] <- NA
  }

  return(micematrix)
}
