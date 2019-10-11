# rvivo_expsetup

# development: ####################################################
# micedat <- tumcalc("~/Documents/car/vol_mice_sheet2.csv")
#col <- colnames(micedat)
#last(col)

#start <- startpick(data = micedat, weight = 90)
#treatmenttime <- exprun(start = start, curdate = last(col))
#int <- exptime(volumematrix = micedat, datecolumn = col)
#culdat <- fread("~/Documents/car/cul_mice_sheet2.csv")
#end <- as.Date(culdat[[4]], format = "%d/%m/%Y")

#filledmat <- filldate(data = micedat, intervaltime = int, datecolumn = col)
#plotmat <- plotmatrix(filledmatrix = filledmat,
#                      startdate = start,
#                      intervaltime = int)
#frame <- data.table::fread(file= "~/Documents/car/vol_mice_sheet2.csv", select = c(1:3))
#experimentmat <- cbind(frame, plotmat)
#excl_experimentmat <-experimentmat[!Ex]

# what do we want to do: ###############################################

# create function that takes mm2 and weight -> returns if an animal should be culled or not.

# creates a function that gives a rough prediction of when an animal will need to be
# culled from trend in mm2 gain and weight loss. (processing.R)

# create function that excludes mice if they are culled for any reason other
# than death. (processing.R)
#end <- as.Date(culdat[[4]], format = "%d/%m/%Y")

# create function that determines survival time. (processing.R)
survdata <- function(start, treatmenttime, culdat){
  start <- as.Date(start)
  end <- as.Date(culdat[[4]], format = "%d/%m/%Y")
  censored <- is.na(end)
  survtime <- end - start

  for (i in seq_along(1:length(censored))) {
    if(censored[i]){
      survtime[i] <- treatmenttime[i]
    }
  }
  binarysurvi <- sapply(censored, function(z){
    if(z == FALSE){1}else
      if(z == TRUE){0}
  }, USE.NAMES = F, simplify = T)

  survivaldat <- cbind(survtime, binarysurvi)

  return(survivaldat)
}
# create function that calculates survival (processing.R)
# per condition compared to control

# create function that plots KM-survival. (plotting.R)

# weightdat <- fread("~/Documents/car/weight_mice_sheet2.csv")
# culdat <- fread("~/Documents/car/cul_mice_sheet2.csv")
# micedat <- dataprep(file = "~/Documents/car/vol_mice_sheet2.csv")


