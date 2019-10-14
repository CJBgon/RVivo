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
#' @param volumes A table with three columns of sample information
#' (e.g. Cage, treatment, mice tag) and subsequent columns of tumour volumes
#' in mm3. Column names should be in format Year-month-day, e.gg.2020-01-01.
#' @param measures A table with three columns of sample information
#' (e.g. Cage, treatment, mice tag) and subsequent columns of Height and Width
#' in side by side columns, both with the date of measurement as column name.
#' @param measures A table with four columns, the firs three are the same
#' information as measures or volumes. the fourth column contains the date if
#' death for each culled animal. live animals, or animals which died because
#' of external reasons should be left blank.
#' @param minvol minimum volume of the tumour before animal can start treatment
#'  (default = 90)
#' @param figures TRUE/FALSE, Indicates if  Rvivo generates plots.
#'  (default = TRUE)
#' @param table TRUE/FALSE, Inidcates if Rvivo outputs a table with results.
#'  (default = TRUE)
#' @param output The folder Rvivo will ouptut to. (default = Working directory)
#'
#' @return A data.frame of three columns (growth, remission, stable) with
#' counts for each mice. and depending on table & figures, optionally a
#' summary file: in_vivo_summary.csv and figures.
#' @import data.table
#' @import ggplot2
#' @export
rvivo <- function(volumes = NULL,
                  measures = NULL,
                  cul = NULL,
                  minvol = NULL,
                  treatcolours = NULL,
                  colstyle = "Dark2",
                  figures = TRUE,
                  table = TRUE,
                  survival = TRUE,
                  output = getwd()) {
  setwd(output)

  if(hasArg(measures) & is.null(volumes)) {
    # volume do volume calculations first and use that matrix as input.
    micemat <- tumcalc(measures)
    culdat <- data.table::fread(cul)
    frame <- data.table::fread(file= measures, select = c(1:3))
  }else if (is.null(volumes) & hasArg(measures)) {
    #read pre-calulated volumes and use that matrix as input.
    micemat <- dataprep(volumes)
    frame <- data.table::fread(file = volumes, select = c(1:3))
    culdat <- data.table::fread(cul)
  } else if (hasArg(measures) & hasArg(volumes)) {
    stop("please provide either tumour measurements or pre-calculated volumes,
       not both.")
  } else  {
    stop("Provide either precalculated tumour volumes (volumes),
       or height and width data (measures)")
  }
  # create table out.
  colnam <- colnames(micemat)
  first <- startpick(data = micemat, weight = minvol)
  treatmenttime <- exprun(start = first, curdate = last(colnam))
  tumourgrowth <- tumgrowth(matrix = micemat, startdate = first)
  experimentinterval <- exptime(volumematrix = micemat, datecolumn = colnam)
  # intervalmatrix <- growthinterval(volumematrix = micemat, datecolumn = col)
  # tumoursum <- growthindicator(intermatrix = intervalmatrix,
  #                             intervaltime = experimentinterval )
  filledmat <- filldate(data = micemat,
                        intervaltime = experimentinterval,
                        datecolumn = colnam)
  plotmat <- plotmatrix(filledmatrix = filledmat,
                        startdate = first,
                        intervaltime = experimentinterval)
  experimentmat <- cbind(frame, plotmat)

  Ex <- exclude(filledmat = filledmat, culdat = culdat)
  excl_experimentmat <-experimentmat[!Ex]
  results <- cbind.data.frame(frame,
                              first,
                              treatmenttime,
                              tumourgrowth,
                              plotmat)
  ex_results <- results[!Ex]

  if (table == T) {
  data.table::fwrite(x = ex_results,
                     file = paste0(output, "/in_vivo_summary.csv"),
                     sep =",",
                     col.names=T,
                     row.names=F)
  }

  if (figures == T) {
    # plot growth over time.

    plotgrowth <- plotdat(excl_experimentmat, date = F)
      if(is.null(treatcolours)){
        colour <- colourpick(vars = frame$Treatment, colourtype = colstyle)
      } else {
        colour <- treatcolours
      }

    growthplots <- vivoplot_treatment(data = plotgrowth,
                                      line = T,
                                      colours = colour
                                      )
    ggplot2::ggsave(filename= paste0(Sys.Date(), "_rvivo_growthplot.pdf"),
           plot = growthplots, device = "pdf",
           width = 24,
           height = 16,
           units = "cm")


    # plot growth without error bars.
    # growthplot_line <- vivoplot_overal(data = plotgrowth,
    #                                   cols = colour,
    #                                   error = T,
    #                                   dots = T)
    # growthplot_error <- vivoplot_overal(data = plotgrowth,
    #                                         cols = colour,
    #                                         error = T,
    #                                         dots = T)
    # growthplot <- vivoplot_overal(data = plotgrowth,
    #                                        cols = colour,
    #                                         error = F,
    #                                         dots = T)
  }

  if (survival == TRUE && is.null(culdat)) {
    stop("When trying to calculate survival data no survival data found.
         Has `cull` been provided?")
  } else if (survival == TRUE && !is.null(culdat)) {

      if (is.null(treatcolours)) {
        colour <- colourpick(vars = frame$Treatment, colourtype = colstyle)
      } else {
        colour <- treatcolours
      }

    survivaldata <- survdata(startdate = start, treatmenttime = treatmenttime,
                             culdat = culdat)
    excl_survivaldata <- survivaldata[!Ex]
    survivalplot <- survdataplot(survivaldat = survivaldata, colours = colour)
    ggplot2::ggsave(filename= paste0(Sys.Date(), "_rvivo_survival.pdf"),
                    plot = survivalplot, device = "pdf",
                    width = 24,
                    height = 16,
                    units = "cm")
  }
  return(ex_results)
}




