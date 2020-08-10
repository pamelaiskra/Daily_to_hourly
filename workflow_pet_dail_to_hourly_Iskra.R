#workflow to derive hourly PET data from daily PET data

######load and install packages

    library("ggplot2")
    install.packages("reshape",repos="https://cran.rstudio.com/bin/windows/contrib/3.4/reshape_0.8.7.zip")
    library("reshape") 
    
    #packages to be installed to make the source file work - only call these lines once
    install.packages("RAtmosphere")
    install.packages("gamlss.dist")
    #install.packages("RAtmosphere",repos="https://cran.rstudio.com/bin/windows/contrib/3.4/RAtmosphere_1.1.zip")
    #install.packages("gamlss.dist",repos="https://cran.rstudio.com/bin/windows/contrib/3.4/gamlss.dist_5.0-2.zip")
    library("RAtmosphere")
    library("gamlss.dist")
    

##### call in source functions
    
    setwd("E:/DynaTOPMODEL/R_stats")
    source("source_pet_daily_to_hourly.R")

##### call in example data
    data <- read.table("E:/DynaTOPMODEL/R_stats/GB_50m_catchcut_daily_PET_gauge_num_39020_GEMMA.txt", header = TRUE)

    #subset data to required time period
    dat <- cbind(data[,2],data[,3],data[,4],data[,6])
    
    dat <- dat[dat[,1] > 1999 & dat[,1] < 2001,]

##### calculate hourly PET time-series from input data
    latitude <- 51.7
    longitude <- -1.8
    petHourly <- petTimeSeries(dat, latitude, longitude)

##### wrie output to file
    write.table(petHourly,"E:/DynaTOPMODEL/R_stats/GB_revisdar-GEMMA-codigo.txt")

#####check output
    
    #plot data to check where it works or not...
    plot(petHourly[1:nrow(petHourly),5], type = "l")
    
    #total sum check
    sum(petHourly$PET)
    sum(dat[,4])




   





    