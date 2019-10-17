# rvivo_expsetup

# development: ####################################################
# micedat <- tumcalc("~/Documents/car/vol_mice_sheet2.csv")
# col <- colnames(micedat)
# start <- startpick(data = micedat, threshold = 90)
# treatmenttime <- exprun(start = start, curdate = last(col))
# int <- exptime(volumematrix = micedat, datecolumn = col)
# culdat <- fread("~/Documents/car/cul_mice_sheet2.csv", fill = T,na.strings = "" )
# end <- as.Date(culdat[[4]], format = "%d/%m/%Y")
#
# filledmat <- filldate(data = micedat, intervaltime = int, datecolumn = col)
# plotmat <- plotmatrix(filledmatrix = filledmat,
#                    startdate = start,
#                    intervaltime = int)
# frame <- data.table::fread(file= "~/Documents/car/vol_mice_sheet2.csv",
#                           select = c(1:3))
# experimentmat <- cbind(frame, plotmat)
# Ex <- exclude(filledmat = filledmat, culdat = culdat)
# excl_experimentmat <-experimentmat[!Ex]
# plotdata_exl<-plotdat(data = excl_experimentmat, date = F)
# plotdata_exl$value[plotdata_exl$value == 0] <- 1
#
# survivaldata <- survdata(startdate = start, treatmenttime = treatmenttime, culdat = culdat)


# what do we want to do: ###############################################
# interval plots?

# create function that takes mm2 and weight -> returns if an animal should be culled or not.

# creates a function that gives a rough prediction of when an animal will
# need to be culled from trend in mm2 gain and weight loss. (processing.R)

# We need some sort of scaling y / x factor for the growthplots.

# function(survstats) {}

