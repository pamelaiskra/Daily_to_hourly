#install.packages("RAtmosphere")
#install.packages("gamlss.dist")
#library("RAtmosphere")
#library("gamlss.dist")


#function converts the day, month, year, to a specific dayofyear (1 to 365)
dayYear <- function(day, month, year){
  
  leapYears <- seq(1932,2012, by = 4)
  dayInMonth <- c(31,28,31,30,31,30,31,31,30,31,30,31)
  ly <- sum((year - leapYears) == 0)
  if(ly == 1) dayInMonth[2] = dayInMonth[2] + 1
  
  dayofyear <- day
  if(month > 1) dayofyear <- dayofyear + sum(dayInMonth[1:(month-1)])
  
  remove(leapYears)
  remove(dayInMonth)
  
  return(dayofyear)
}

#calculate hourly PET from dailyPET input, day, month, year, latitude and longitude parameters 
petHour <- function(dailyPET, day, month, year, lat, long){
  
  #derive the day of the year from the day month and year
  dayofyear <- dayYear(day,month,year)
  
  #calculate sunrise and sunset using the day of the year, latitude and longitude as inputs
  sunshine <- suncalc(dayofyear, lat, long, UTC = TRUE)
    
  #distribution used SEP distribution
  zcross <- 0.04
  x <- seq(-5,5, by = 0.001)
  y <- dSEP(x, mu=0,sigma=1, nu=0, tau=3) - zcross
  #tau = kurtosis. 2 gaussian, less than 2 heavy tailed.greater than 10 is uniform.  
  plot(x,y)
  
  #re-scale x axis across the daylight hours...
  dist <- cbind(x,y)
  dist <- dist[dist[,2] > 0,]
  #plot(dist[,1], dist[,2])
  z <- dist[,1]
  z[1] <- sunshine$sunrise
  diff <- (sunshine$sunset-sunshine$sunrise)/length(z)
  for(i in 2:length(z)) z[i] <- z[i-1] + diff
  #plot(z,dist[,2])
  
  petDist <- z
   
  #re-scale PET to fit the distribution
  petDist <- dailyPET*(dist[,2]/sum(dist[,2]))
  
  #plot(z,petDist, type = "l")
  
  petDist <- cbind(z,petDist)
  
  #turn the distribution into PET as a function of the hour of the day
  dayHour <- seq(1, 24, by = 1)
  petHour <- vector(length = 24)
  for(i in 1:24){
    petHour[i] <- 0
    petHour[i] <- sum(petDist[petDist[,1] >= (i-1) & petDist[,1] < (i),2])
  }
  
  petHour <- data.frame(cbind(dayHour,petHour))
  colnames(petHour) <- c("Hour", "PET")
  #plot(petHour[,1],petHour[,2])
  
  remove(x)
  remove(y)
  remove(dist)
  remove(z)
  remove(petDist)
  remove(dayHour)
   
  return(petHour)
}

#calls petHour sequentially for a supplied time-series
petTimeSeries <- function(data, lat, long){
  
  #data: table, with 4 columns in the following order: YEAR, MONTH, DAY, PET
  #PET units should be: length/day (e.g. mm/day), as the code divides daily total 
  #across hours using predefined distribution
  
  len <- length(data[,1])
  i <- 1
  petHourly <- petHour(data[i,4], data[i,3], data[i,2], data[i,1],lat, long)
  store <- cbind(rep(data[i,1],24), rep(data[i,2],24), rep(data[i,3],24), petHourly)  
  for(i in 2:len){
    petHourly <- petHour(data[i,4], data[i,3], data[i,2], data[i,1],lat, long)
    temp <- cbind(rep(data[i,1],24), rep(data[i,2],24), rep(data[i,3],24), petHourly)
    store <- rbind(store,temp)
    #print(i)
  }
  
  colnames(store) <- c("Year", "Month", "Day", "Hour", "PET")
        
  return(store)
}










