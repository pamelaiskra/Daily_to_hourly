#PUB summer school 

#--Pre-processing

library(PUBexamples)
library(rworldmap)    # has to be installed from CRAN-Mirror
library(zoo)

data(data4chapter10)
idnr_ungauged <- names(ModelInput)[!(names(ModelInput) %in% names(ObsDischarges))]
CatChar_ungauged <- CatChar[CatChar$idnr %in% idnr_ungauged,]
ModelInput_ungauged <- ModelInput[idnr_ungauged]
idnr_target <- "212613"
data_target <- ModelInput_ungauged[[idnr_target]]
CatChar_target <- CatChar_ungauged[CatChar_ungauged$idnr == idnr_target,]


#---MAF - Mean annual rainfall - "Easy Budyko"

#PET and PREC is given on a daily basis in data_target and cannot be used
#Aridity-index is given in CatChar_ungauged
#Calculation of E/P
EdivP <- 1-exp(-CatChar_ungauged[5,15])
EdivP

#Calculation of the Q/P-ratio
QdivP <- 1-EdivP
QdivP

#P can be extracted from data_target[,4]
Pmean <- mean(data_target[,4])*365.25
Pmean

#Calculation of MAR Mean Annual Runoff
MAR1 <- QdivP*Pmean

#---MAF - Mean annual rainfall - "Other Budyko"
#####
#other methods to be tested
####

#---Parde-Coefficients

#Here we use EU-catchments. However, the list could be extended by the 6 gauged catchments from ObsDischarges[]

help(data4chapter5and6)
data(data4chapter5and6)
head(CatchmentsEU, 15)

#Calculation of Parde-coefficients from the EU-catchments (see exercise 6)
MAQ <- apply(meanQmon, 1, mean) # m3/s
PkQ <- meanQmon/matrix(MAQ, nrow=dim(meanQmon)[1], ncol=12, byrow=FALSE)

#Looking or the nearest catchments of the EU-data set-geogr. distance
numcatch <- length(CatchmentsEU[,1])
numcatch
for(i in 1:numcatch){
  distgeogr <- sqrt((CatchmentsEU[,5]-CatChar_target[,4])^2+(CatchmentsEU[,4]-CatChar_target[,5])^2)
}
Minimum <- which.min(distgeogr[])
Qseasongeogr <-  PkQ[Minimum,]*MAR1/12

#Looking or the nearest catchments of the EU-data set-geogr. distance & elevation
for(i in 1:numcatch){
  distgeogrelev <- sqrt((CatchmentsEU[,5]-CatChar_target[,4])^2+(CatchmentsEU[,4]-CatChar_target[,5])^2+(CatchmentsEU[,6]-CatChar_target[,9])^2)
}
Minimum <- which.min(distgeogrelev[])
Qseasongeogrelev <-  PkQ[Minimum,]*MAR1/12
Qseasongeogrelev

#Looking or the nearest catchments of the EU-data set-geogr. distance & area
for(i in 1:numcatch){
  distgeograrea <- sqrt((CatchmentsEU[,5]-CatChar_target[,4])^2+(CatchmentsEU[,4]-CatChar_target[,5])^2+(CatchmentsEU[,7]-CatChar_target[,8])^2)
}
Minimum <- which.min(distgeograrea[])
Qseasongeograrea <-  PkQ[Minimum,]*MAR1/12
Qseasongeograrea

#Looking or the nearest catchments of the EU-data set-geogr. distance & area & elevation 
for(i in 1:numcatch){
  distgeogrelevarea <- sqrt((CatchmentsEU[,5]-CatChar_target[,4])^2+(CatchmentsEU[,4]-CatChar_target[,5])^2+(CatchmentsEU[,6]-CatChar_target[,9])^2+(CatchmentsEU[,7]-CatChar_target[,8])^2)
}
Minimum <- which.min(distgeogrelevarea[])
Qseasongeogrelevarea <-  PkQ[Minimum,]*MAR1/12
Qseasongeogrelevarea
#->elevation and area leads to the same results as elevation only

plot(c(1:12), PkQ[Minimum,], type="b", xlab="", ylab="Parde",
     main=paste(CatchmentsEU$code[Minimum], ": ", CatchmentsEU$river[Minimum], " at ", CatchmentsEU$station[Minimum], sep=""),
     cex.main=1, font.main=1, ylim=c(0,4))


























# Loading the discharge data
valCatChar <- CatChar[CatChar$idnr %in% names(ObsDischarges),]

#idnr="200287"   # we focus first on this catchment
idnr=200287    # ID of the catchment to predict
#Code<-c(200287,204925,205799,208835,212076,212647)
Code<-c('200287','204925','205799','208835','212076','212647')
Target.Code<-idnr # Target site code
numgauges <- length(Code)
#creating array datag with all discharges
data[] <- ObsDischarges[[Code[1]]]
dataq <- array(dim=c(length(data[,1]),6))
for(i in 1:numgauges){
  data[] <- ObsDischarges[[Code[i]]] # Read the observed discharges
  dataq[,i] <- data[,4]
}

data <- ObsDischarges[[-which('data.frame'==idnr)]] # Read the observed discharges

data
days <- as.Date(strptime(paste(data[,1], data[,2], data[,3]), format="%d %m %Y"))
Q <- zoo(data[,4], order.by=days) # daily discharge (m3/s), which can be plotted with the following code:

#--Data-plotting

newMap <- getMap(resolution="coarse") # you may use resolution="low"

colori <- rainbow(length(valCatChar$idnr), start=0, end=.8)
#names(colori) <- valCatChar$idnr
names(colori) <- valCatChar$station
plot(newMap, xlim=c(11.5, 15.5), ylim=c(46, 49), usePolypath=FALSE)
points(valCatChar$lon, valCatChar$lat, pch=21,
       bg=colori,
       cex=log10(valCatChar$area))
#text(valCatChar$lon, valCatChar$lat, valCatChar$idnr,
#     pos=4, col=4)
text(valCatChar$lon, valCatChar$lat, valCatChar$station,
     pos=4, col=4)



help(data4chapter10)  # explains the data structure
data(data4chapter10)
str(ObsDischarges)    # shows the structures - 6 catchments with observed discharge
                      # Catchments are named by ID's: 200287, 204925,...
                      # structure: ID name river m.a.d


valCatChar <- CatChar[CatChar$idnr %in% names(ObsDischarges),]
valCatChar
str(valCatChar)       # includes 6 catchments with its 15 attributes


## Time series

idnr="200287"   # we focus first on this catchment
#idnr="212647"

library(zoo)
data <- ObsDischarges[[idnr]] # Read the observed discharges
head(data, 20)                # shows only the twenty first rows of the table

days <- as.Date(strptime(paste(data[,1], data[,2], data[,3]), format="%d %m %Y"))
Q <- zoo(data[,4], order.by=days) # daily discharge (m3/s), which can be plotted with the following code:

plot(Q)   # all timeseries
plot(Q, xlim=as.Date(c("2001-09-01","2003-08-31"))) # zoom
plot(window(Q, start=as.Date("2001-09-01"), end=as.Date("2003-08-31"))) # alternatively, window of a timeseries

data <- ModelInput[[idnr]]
head(data, 20)

days <- as.Date(strptime(paste(data[,1], data[,2], data[,3]), format="%d %m %Y"))
P <- zoo(data[,4], order.by=days) # daily catchment precipitation (mm/d)
T <- zoo(data[,5], order.by=days) # mean daily catchment temperature (deg C)

area <- CatChar[CatChar$idnr == idnr, "area"]
Qmm <- 24*3.6*Q/area # conversion of discharge from m3/s to mm/d

#------------------

plot_3timeseries <- function (prec, et, dis, ...) {         # Plotting of all three varriables in one plot
  # ... can be used to give xlim
  plot(dis, ylim=c(-max(dis), 2*max(dis)), yaxt="n",
       xlab="", ylab="", ...)
  axis(2, at=round(signif(seq(0, max(dis), length=5), 2)))
  abline(h=0, lty=3)
  par(new=TRUE)
  plot(time(prec), as.numeric(prec), col="blue", type="l",
       ylim=rev(c(0, 3*max(prec))),
       xlab="", ylab="", axes=FALSE, ...)
  axis(4, at=round(signif(seq(0, max(prec), length=5), 2)))
  par(new=TRUE)
  deltaet <- max(et) - min(et)
  plot(time(et), as.numeric(et), col="orange", type="l",
       ylim=c(min(et), max(et) + 2.5*deltaet),
       xlab="", ylab="", axes=FALSE, ...)
  axis(4, at=round(signif(seq(min(et), max(et), length=5), 2), 1))
}
plot_3timeseries(prec=P, et=EP, dis=Qmm, xlim=as.Date(c("2001-09-01","2003-08-31")))
title("rainfall, runoff and potential evaporation (mm/d)", cex.main=1, font.main=1)

#-----------------

t=1
Deltat=365          # investigating the time series on an annual basis
while (t <= length(P) - Deltat - 1) {
  plot_3timeseries(P[seq(t, t + Deltat)],
                   EP[seq(t, t + Deltat)],
                   Qmm[seq(t, t + Deltat)])
  title(main=paste(as.Date(range(time(P[seq(t, t + Deltat)]))), collapse=" --> "),
        cex.main=1, font.main=1)
  readline()
  t <- t + Deltat
}


#-----------------


Qmm_yr <- aggregate(Qmm, format(time(Qmm), "%Y"), FUN=sum) # mm/yr
P_yr <- aggregate(P, format(time(P), "%Y"), FUN=sum)
EP_yr <- aggregate(EP, format(time(P), "%Y"), FUN=sum)
plot(Qmm_yr, type="b", ylim=c(0,3000), xlab="", ylab="(mm/yr)")
lines(P_yr, type="b", pch=6, col="blue")
lines(EP_yr, type="b", pch=2, col="orange")

Qmm_m <- aggregate(Qmm, as.yearmon(time(Qmm)), FUN=function(x){30.43*mean(x)}) # mm/mon
P_m <- aggregate(P, as.yearmon(time(P)), FUN=function(x){30.43*mean(x)})
EP_m <- aggregate(EP, as.yearmon(time(EP)), FUN=function(x){30.43*mean(x)})
plot_3timeseries(prec=P_m, et=EP_m, dis=Qmm_m)
title("rainfall, runoff and potential evaporation (mm/mon)", cex.main=1, font.main=1)


