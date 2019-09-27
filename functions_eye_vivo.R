# Christian J Bouwens
# 2019-09-26
# Calulate neccesary and interesting parameters of in-vivo experiments.

# dataprep
dataprep <- function(file) {
  # returns a matrix of the volumetric data, with dates as column names.
  if (!require(data.table)) install.packages('data.table')
  suppressPackageStartupMessages(library(data.table))
  
  micedata <- fread(file)
  micematrix <- as.matrix(micedata[,-c(1:4)])
  col <- colnames(micematrix)
  colnames(micematrix) <- as.character(as.Date(col, format ="%d/%m/%Y"))
  return(micematrix)
}

startpick <- function(data = micemat , weight = 90){ 
  # returns experimental starting date from a matrix of the format:
  # rows = mice, columns = dates, fill = tumour volume.
  #
  # 
  # Args: data a matrix trimmed of everything but mice per row and
  # date as column.
  #       weight = the parameter from which experiment start should be counted.
  startdate <- c()
  dat <- data
  for (row in seq_along(1:nrow(dat))) {
  startdate[row] <- colnames(dat)[which(dat[row, ] >= 90,
                                          arr.ind = TRUE)[1]]
  startdate <- as.Date(startdate, format = "%d/%m/%Y")
  return(startdate)
  }
}

exprun <- function(start = begin, curdate = Sys.Date()) {
  # how many days has each mice been in treatment?
  # 
  # Args: startdate = the date from when the mice started treatment.
  #       curdate = the current date, or the date to which experiment end.
  # (TODO) : what if an animal has been culled before the current date?
  treatment_time <- curdate - start
  return(as.vector(treatment_time))
}

growth <- function(matrix = micematrix, startdate = begin) {
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
    startweight[n] <- miceDT[n, get(startchar)[n], with = F]
  }
  startweight <- unlist(startweight, use.names = F)
  
  #grab tumour size at the end.
  
  for (n in seq_along(1 : nrow(miceDT))) { # go along the mice (rows)
    DTle <- length(miceDT)
    # DTle is the last entry of the matrix
    endweight[n] <- miceDT[n, DTle, with = F] 
    if(is.na(endweight[n])){ #if the last entry of the matrix is NA
      # go into while loop to return to the latest available value
      while (is.na(endweight[n])) { 
        DTle<- DTle - 1
        endweight[n] <- miceDT[n, DTle, with = F]
      } 
    }
  }
  endweight <- unlist(endweight, use.names = F)
  #calculate tumour size difference.
  g <- startweight - endweight
  return(g)
}


  
  
 # (TODO) days of remission days of growth sum. 
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

  # for each mice calculate if there as growth or remission in that timeframe.
  for (i in seq_along(1 : intervalcount)){
    print(coldate[i+1])
    print(coldate[i])
    int[i] <- coldate[i + 1] - coldate[i]
  }
  intgrowth <- matrix(nrow = nrow(volumematrix), ncol = intervalcount)

  for ( n in seq_along(1 : nrow(volumematrix))) {
  i <- 1

    for (i in seq_along(1 : 7)) {
      x <- i+1
      intgrowth[n, i] <- volumematrix[n , i] - volumematrix[n, x]
  # you could make a heatmap of this or use it for different plotting purposes.
   
  }  
  }
  return(intgrowth)
}

growthindicator <- function(intermatrix = intgrowth, intervaltime = int)
# sum the days of growth vs days of remission per mice.
growthwum <- apply(intermatrix, MARGIN = 1, function(x){
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
  ret <- cbind(ret, temp)
  return(ret)
} )

# out: dataframe (list per animal/group):
#     interval, growth or remiss, how much?, total growth and remis intervals

setwd("~/Documents/car")
micemat <- dataprep("~/Downloads/mice_volume.csv")
col <- colnames(micemat)
first <- startpick(data = micemat, weight = 90)  #invalid first argument. am i in the wrong calling scope?
treatmenttime <- exprun(start = first, curdate = Sys.Date())
tumourgrowth <- growth(matrix = micemat, startdate = first)
  
  results <- cbind(startdate, treatmenttime, tumourgrowth)
  return(results)
  

mainvivo(data = "~/Downloads/mice_volume.csv", weight = 90, output = "~/Documents/car")
# (TODO) create growth graphs
# tumrouprog <- function(){}