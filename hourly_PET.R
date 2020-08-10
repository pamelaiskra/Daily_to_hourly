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
    
    setwd("C:My_directory")
    source("source_code.R")

##### call in example data
    data <- read.table("sample_file.txt", header = TRUE)

##### calculate hourly PET time-series from input data
    latitude <- 51.7
    longitude <- -1.8
    petHourly <- petTimeSeries(data, latitude, longitude)

##### wrie output to file
    write.table(petHourly,"C:/PET_hourly.txt")

#####check output
    
    #plot data to check where it works or not...
    plot(petHourly[1:nrow(petHourly),5], type = "l")
    
    #total sum check
    sum(petHourly$PET)
    sum(data[,4])




   





    
