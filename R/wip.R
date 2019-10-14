# rvivo_expsetup

# development: ####################################################
# micedat <- tumcalc("~/Documents/car/vol_mice_sheet2.csv")
# col <- colnames(micedat)
# last(col)
#
# start <- startpick(data = micedat, weight = 90)
# treatmenttime <- exprun(start = start, curdate = last(col))
# int <- exptime(volumematrix = micedat, datecolumn = col)
# culdat <- fread("~/Documents/car/cul_mice_sheet2.csv")
# end <- as.Date(culdat[[4]], format = "%d/%m/%Y")
#
# filledmat <- filldate(data = micedat, intervaltime = int, datecolumn = col)
# plotmat <- plotmatrix(filledmatrix = filledmat,
#                       startdate = start,
#                       intervaltime = int)
# frame <- data.table::fread(file= "~/Documents/car/vol_mice_sheet2.csv",
#                            select = c(1:3))
#
# experimentmat <- cbind(frame, plotmat)
# Ex <- exclude(filledmat = filledmat, culdat = culdat)
# excl_experimentmat <-experimentmat[!Ex]
#

# what do we want to do: ###############################################
# interval plots?

# create function that takes mm2 and weight -> returns if an animal should be culled or not.

# creates a function that gives a rough prediction of when an animal will
# need to be culled from trend in mm2 gain and weight loss. (processing.R)


# function(survstats) {}

# weightdat <- fread("~/Documents/car/weight_mice_sheet2.csv")
# culdat <- fread("~/Documents/car/cul_mice_sheet2.csv")
# micedat <- dataprep(file = "~/Documents/car/vol_mice_sheet2.csv")


