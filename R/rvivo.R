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
#' in mm3. Column names should be in format Year-month-day, e.g. 2020-01-01.
#' @param measures A table with three columns of sample information
#' (e.g. Cage, treatment, mice tag) and subsequent columns of Height and Width
#' in side by side columns, both with the date of measurement as column name.
#' @param cul A table with three columns of sample information followed with
#' the date a mice was culled. mice still undergoing treatment or those that
#' have been excluded from the study should be left blank.
#' @param min minimum volume of the tumour before animal can start treatment
#'  (default = 0)
#' @param max A value above which tumour entries will be removed from the
#' analysis.
#' @param treatcolours For when you want to use the same colours per treatment
#' over several images. Input should be a named vector with hex style colour
#' codes. The names should correspond to the treatments in the data.
#' @param colstyle Indicate the type of colour coding that should be assigned
#' to each treatment group. follows RColorBrewer style. (default = "Dark2")
#' @param figures TRUE/FALSE, Indicates if  Rvivo generates plots.
#'  (default = TRUE)
#' @param axis "linear" or "log", passes on axis infomation for the plots.
#' @param output image extension types: "tiff", "pdf".
#' @param table TRUE/FALSE, Inidcates if Rvivo folders a table with results.
#'  (default = TRUE)
#' @param survival TRUE/FALSE, inidcates if Rvivo folders Kaplan-meier
#' survival curves.
#'  (default = TRUE)
#' @param folder The folder Rvivo will ouptut to. (default = Working directory)
#'
#' @return A data.frame of three columns (growth, remission, stable) with
#' counts for each mice. and depending on table & figures, optionally a
#' summary file: in_vivo_summary.csv and figures.
#' @import data.table
#' @import ggplot2
#' @importFrom methods hasArg
#' @export
rvivo <- function(volumes = NULL,
                  measures = NULL,
                  cul = NULL,
                  min = 0,
                  max = NULL,
                  treatcolours = NULL,
                  colstyle = "Dark2",
                  figures = TRUE,
                  axis = "linear",
                  output = "pdf",
                  table = TRUE,
                  survival = TRUE,
                  folder = getwd()) {

  # check inputs:
  if (hasArg(measures) & hasArg(volumes)) {
    stop("please provide either tumour measurements or pre-calculated volumes,
         not both.")
  } else if (!hasArg(measures) & !hasArg(volumes)) {
    stop("Provide either precalculated tumour volumes (volumes),
         or height and width data (measures)")
  }
  setwd(folder)
  # input can be either [culdat + volumes or measures] or [volumes or measures]
  # in which case the exclusion data should be a column in that data.
  if (hasArg(cul)) {
    culdat <- data.table::fread(cul, header = TRUE,
                                fill = TRUE,
                                 na.strings = "")
    if (hasArg(measures) & is.null(volumes)) {
        # volume do volume calculations first and use that matrix as input.
        micemat <- tumcalc(measures, precolumns = 3, max = max)
        frame <- data.table::fread(file = measures,
                                   header = TRUE,
                                   select = c(1:3))
    } else if (is.null(measures) & hasArg(volumes)) {
        # read pre-calulated volumes and use that matrix as input.
        micemat <- dataprep(volumes, precolumns = 3, max = max)
        frame <- data.table::fread(file = volumes,
                                   header = TRUE,
                                   select = c(1:3))
    }
  } else if (!hasArg(cul) & hasArg(measures) & !hasArg(volumes)) {
      # try and extract cul from measures otherwise return stop error.
    micemat <- tumcalc(measures, precolumns = 4, max = max)
    frame <- data.table::fread(file = measures,
                               header = TRUE,
                               select = c(1:3))
    Exdat <- data.table::fread(measures, header = TRUE,
                                fill = TRUE,
                                na.strings = "",
                                select = c(4))
    # If values have been excluded set the Ex to TRUE which corresponds to
    # the exclude function
    Ex <- ifelse(is.na(Exdat), yes = FALSE, no = TRUE)[,1]
    culdat <- endcalc(micemat)
  } else if(!hasArg(cul) & !hasArg(measures) & hasArg(volumes)) {
      # try and extract cul from volumes otherwise return stop error
    micemat <- dataprep(volumes, precolumns = 4, max = max)
    frame <- data.table::fread(file= volumes,
                               header = TRUE,
                               select = c(1:3))
    Exdat <- data.table::fread(volumes, header = TRUE,
                               fill = TRUE,
                               na.strings = "",
                               select = c(4))
    Ex <- ifelse(is.na(Exdat), FALSE, TRUE)[,1]
    culdat <- endcalc(micemat)
  }

  # check input data, does it contain the right amount of columns?
  if (ncol(frame) != 3) {
    stop(paste0("ERROR: Unexpected amount of columns ",
                "in cul or volumes/measurements. ",
                "\nPerhaps your Treatment column contains a lot of spaces ",
                "or symbols that could be interpreted as delimiters?"))
  }
  # clean out empty rows at the bottom of frame.
  remfram <- frame[, fifelse(Cage == '', FALSE, TRUE)]
  # don't need the is.na() case unless fread gets changed. Right now empty
  # character columns get read in as '' instead of NA. this is not the case
  # for integers.
  # remframe2 <- frame[, fifelse(is.na(Cage), TRUE, FALSE)]

  frame <- frame[remfram, c(1:3)]
  # create table out.
  colnam <- colnames(micemat)
  first <- startpick(data = micemat, threshold = min)
  treatmenttime <- exprun(startdate = first, curdate = last(colnam))
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
  percentmat <- apply(plotmat, 2, function(x){
    (x/plotmat[,1])*100
  })
  experimentmat <- cbind(frame, plotmat)
  per_experimentmat <- cbind(frame, percentmat)


  if (hasArg(cul)) {
      Ex <- exclude(filledmat = filledmat, culdat = culdat)
  }
  if (!hasArg(cul) & all(Ex)) {
    stop(paste0("ERROR: All data will be excluded",
                "\nThe fourth column in your data should have an indicator (such as 'x'",
                " or 'y'), only at mice excluded from the analysis."))
  }
  # if (is.matrix(Ex)) {Ex -> Ex[,1]}
  # remove rows at the end of the input file (like we've done for frame.)
  # this is only neccessary because excell might save empty ',,,,,,,,,,' rows.
  Ex <- Ex[remfram]
  excl_experimentmat <- experimentmat[!Ex]
  per_experimentmat <- per_experimentmat[!Ex]
  results <- cbind.data.frame(frame,
                              first,
                              treatmenttime,
                              tumourgrowth,
                              plotmat)
  ex_results <- results[!Ex]

  if (table == T) {
  data.table::fwrite(x = ex_results,
                     file = paste0(folder, "/in_vivo_summary.csv"),
                     sep =",",
                     col.names=T,
                     row.names=F)
  }

  if (is.null(treatcolours)) {
    colour <- colourpick(vars = frame[[2]], colourtype = colstyle)
  } else {
    colour <- treatcolours
  }

  if (figures == T) {
    # plot growth over time.

    plotgrowth <- plotdat(excl_experimentmat, date = F)
    sumplot <- vivoplot_overall(data = plotgrowth,
                                type = 'reg',
                                colours = colour,
                                )

    growthplots <- vivoplot_treatment(data = plotgrowth,
                                      type = 'reg',
                                      line = T,
                                      colours = colour,
                                      ax = axis)


    per_plotgrowth <- plotdat(per_experimentmat, date = F)
    per_growthplots <- vivoplot_treatment(data = per_plotgrowth,
                                          type = 'per',
                                          line = T,
                                          colours = colour,
                                          ax = axis)

    ggplot2::ggsave(filename= paste0(Sys.Date(),
                                     "_rvivo_sumplot.",
                                     output),
                    plot = sumplot,
                    device = output,
                    width = 24,
                    height = 16,
                    units = "cm")

    ggplot2::ggsave(filename= paste0(Sys.Date(),
                                     "_rvivo_growthplot.",
                                     output),
                    plot = growthplots,
                    device = output,
                    width = 24,
                    height = 16,
                    units = "cm")

    ggplot2::ggsave(filename= paste0(Sys.Date(),
                                     "_rvivo_percentageplot.",
                                     output),
                    plot = per_growthplots,
                    device = output,
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
  if (survival == TRUE) {
    if (hasArg(cul)) {
      DOD <- culdat[[4]]
    } else {
      DOD <- culdat
    }
    survivaldata <- survdata(startdate = first,
                             treatmenttime = treatmenttime,
                             culdat = culdat,
                             frame = frame)
    excl_survivaldata <- survivaldata[!Ex]
    statistics <- survstats(survivaldat = excl_survivaldata)
    data.table::fwrite(x = statistics,
                       file = paste0(folder, "/survival_stats.csv"),
                       sep =",",
                       col.names=T,
                       row.names=F)
    survivalplot <- survdataplot(survivaldat = excl_survivaldata,
                                 colours = colour)
    ggplot2::ggsave(filename= paste0(Sys.Date(), "_rvivo_survival.", output),
                    plot = print(survivalplot),
                    device = output,
                    width = 24,
                    height = 16,
                    units = "cm")
  }
  return(ex_results)
}




