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
  (x * y^2 * pi) / 6
}

#' create tumour volume matrix
#'
#' This function reads an input file containing  height and width data and
#' returns a matrix with rows = mice and columns = dates of measurement.
#'
#' @param volumes A table with the first 3 columns indicating sample data:
#' cage, treatment, mouse/repeat. The remaining columns are dates with the
#' height and width measurements. e.g. 01-01-2020 | 01-01-2020.
#' Make sure the heigher value (height) is in the first column and the lower
#' (width) in the second.
#' @return A matrix of tumour volumes per mice (rows) for each
#'  measurement date (columns).
#' @export
tumcalc <- function(volumes) {
  vols <- data.table::fread(volumes)
  voluse <- vols[, -c(1:3)]
  sizedat <- vols[, c(1:3)]
  for (i in seq(1, ncol(voluse), by=2)) {

    foo1 <- voluse[, c(i : (i + 1)), with = F]
    bar <- mapply(calcsize, foo1[, 1], foo1[, 2])
    sizedat <- cbind(sizedat,bar)
    micematrix <- as.matrix(sizedat[, -c(1:3)])
    col <- colnames(micematrix)
    colnames(micematrix) <- as.character.Date(
        col, format ="%d/%m/%Y")
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
#' @return A matrix of tumour volumes per mice (rows) for each
#'  measurement date (columns).
#' @export
dataprep <- function(file) {
  # returns a matrix of the volumetric data, with dates as column names.
  micedata <- data.table::fread(file)
  micematrix <- as.matrix(micedata[, -c(1:3)])
  col <- colnames(micematrix)
  colnames(micematrix) <- as.character.Date(
      col, format ="%d/%m/%Y")
  return(micematrix)
}
