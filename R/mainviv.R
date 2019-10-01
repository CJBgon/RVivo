#' main function of Rvivo
#'
#' This function combines all previous functions to run, it takes an table of
#' either tumour volumes or measurements. Calculates the start date of the
#' experiment when mice are above a designated weight. It then calculates,
#' from this start date, how much the tumour has grown / shrunk since the
#' starting size. Additionally, it determines progression of the tumour over
#' the intervals. and how many days of growth, regression or stability the
#' tumour has shown over the course of the treatment.
#'
#' provide either volumes or measures, not both.
#'
#' @param volumes A table with three columns of sample data
#' (e.g. Cage, treatment, mice tag) and following columns of tumour volumes
#' in mm3. Column names should be in format day/month/Year, e.gg.01/01/2020.
#' @param measures A table with three columns of sample data
#' (e.g. Cage, treatment, mice tag) and following columns of Height and Width
#' in side by side columns, both with the date of measurement as column name.
#' @param minvol minimum volume of the tumour before animal can start treatment
#'  (default = 90)
#' @param figures TRUE/FALSE, should Rvivo plot tumour growth and survival?
#'  (WIP, default = FALSE)
#' @param table TRUE/FALSE, Should Rvivo write the results to
#' in_vivo_summary.csv? (default = TRUE)
#' @param output The folder Rvivo will ouptut to. (default = Working directory)
#'
#' @return A data.frame of three columns (growth, remission, stable) with
#' counts for each mice. and depending on table & figures, optionally a
#' summary file: in_vivo_summary.csv and figures.
#' @export
mainvivo <- function(volumes = NULL,
                     measures = NULL,
                     minvol = NULL,
                     figures = FALSE,
                     table = FALSE,
                     output = getwd()) {
  setwd(output)

  if(hasArg(measures) & is.null(volumes)) {
    # volume do volume calculations first and use that matrix as input.
    micemat <- tumcalc(measures)
    frame <- data.table::fread(file= measures, select = c(1:3))
  }else if (is.null(volumes) & hasArg(measures)) {
    #read pre-calulated volumes and use that matrix as input.
    micemat <- dataprep(volumes)
    frame <- data.table::fread(file = volumes, select = c(1:3))
  } else if (hasArg(measures) & hasArg(volumes)) {
    stop("please provide either tumour measurements or pre-calculated volumes,
       not both.")
  } else  {
    stop("Provide either precalculated tumour volumes (volumes),
       or height and width data (measures)")
  }

col <- as.Date(colnames(micemat))
first <- startpick(data = micemat, weight = minvol)
treatmenttime <- exprun(start = first, curdate = Sys.Date())
tumourgrowth <- tumgrowth(matrix = micemat, startdate = first)
experimentinterval <- exptime(volumematrix = micemat, datecolumn = col)
intervalmatrix <- growthinterval(volumematrix = micemat, datecolumn = col)
tumoursum <- growthindicator(intermatrix = intervalmatrix,
                             intervaltime = experimentinterval )
results <- cbind.data.frame(frame,
                            first,
                            treatmenttime,
                            tumourgrowth,
                            intervalmatrix,
                            t(tumoursum))
if (table == T){
data.table::fwrite(x = results,
                   file = paste0(output, "/in_vivo_summary.csv"),
                   sep =",",
                   col.names=T,
                   row.names=F)
}
return(results)
}
