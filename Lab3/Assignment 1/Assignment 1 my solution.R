set.seed(1234567890)
library(geosphere)
#Function fr calculating kernel for distance
k1=function(x1,x2) {
  distance=distHaversine(x1,x2)
  return(exp(-abs(distance^2)/(2*h_distance^2)))
}

#Function for calculating kernel for date difference
k2=function(date1, date2) {
  x_month=as.integer(format(as.Date(st$date, format="%Y-%m-%d"), format="%m"))
  y_month=as.integer(format(as.Date(date2, format="%Y-%m-%d"), format="%m"))
  x_day=as.integer(format(as.Date(st$date, format="%Y-%m-%d"), format="%d"))
  y_day=as.integer(format(as.Date(date2, format="%Y-%m-%d"), format="%d"))
  
  #Calculate date diff, only considering day and month not year
  diff=abs(30.5*(y_month-x_month)+(y_day-x_day))
  
  #Adjusting so cycles are correct
  for(i in 1:length(diff)) {
    if(diff[i]>182.5) {
      diff[i]=365-diff[i]
    }
  }
  return(exp(-abs(diff^2)/(2*h_date^2)))
}

#Function to calculate time kernel
k3=function(hour1, hour2) {
  diff=as.numeric(hour2-hour1, unit="hours")
  
  #Adjusting so cycles are correct
  for(i in 1:length(diff)) {
    if(diff[i]>12){
      diff[i]=12-diff[i]
    }
  }
  return(exp(-abs(diff^2)/(2*h_date^2)))
}

#Choosing h value for distance to be used in kernel
h_distance=100000

#Choosing h value for date to be used in kernel
h_date=15

#Choosing h value for time to be used in kernel
h_time=2

#Plot to verify reasonable h for distance
dist=seq(0,300000)
plot(exp(-abs(dist^2)/(2*h_distance^2)), type="l", xlab="Distance", ylab="Kernel")

#Plot to verify reasonable h for date
date=seq(0,60)
plot(exp(-abs(date^2)/(2*h_date^2)), type="l", xlab="Date diff", ylab="Kernel")

#Plot to verify reasonable h for time
time=seq(0,10)
plot(exp(-abs(time^2)/(2*h_time^2)), type="l", xlab="Time diff", ylab="Kernel")

#Adding coordinates, date and times for prediction
a=58.4247
b=14.826
date=as.Date("2013-07-04")
times <- c("04:00:00", "06:00:00", "08:00:00", "10:00:00", "12:00:00",
           "14:00:00", "16:00:00", "18:00:00", "20:00:00", "22:00:00", "24:00:00")
temp=vector(length=(length(times)))

#Read csv:s and merge them into one data frame
stations=read.csv("stations.csv", fileEncoding="latin1")
temps=read.csv("temps50k.csv")
st=merge(stations,temps,by="station_number")

#Removing dates that "has not happened yet"
st=st[!as.Date(st$date)>=date,]

#Function to predict temperature
temp_predict=function(st) {
  #Calculating dist. kernel
  distance_k=k1(data.frame(st$longitude, st$latitude), data.frame(b,a))
  #Calculating date kernel
  date_k=k2(as.Date(st$date), date)
  
  #Creating lists to store predictions for each timeslot
  sum_predictions=rep(0,11)
  mult_predictions=rep(0,11)
  
  #Looping for length of time vector to make predictions every second hour
  for(i in 1:length(times)) {
    time_k=k3(strptime(st$time, format="%H:%M:%S"), 
              strptime(times[i], format="%H:%M:%S"))
    #Calculating time kernel
    #Adds all kernels for sum prediction
    sum_k=distance_k+date_k+time_k
    #Making prediction
    sum_predictions[i]=sum(sum_k*st$air_temperature)/sum(sum_k)
    #Adds all kernels for mult prediction
    mul_k=distance_k*date_k*time_k
    #Making prediction
    mult_predictions[i]=sum(mul_k*st$air_temperature)/sum(mul_k)
  }
  
  #Storing predictions in one data fram
  final=data.frame(sum_predictions, mult_predictions)
  return(final)

}

list=temp_predict(st)

#Plot result of sum kernel
plot(list[,1], type="o", col="red", xlab="Time", ylab="Temp", xaxt="n")
axis(1, at=1:length(times), labels=times)

#Plot result of mult. kernel
plot(list[,2], type="o", col="red", xlab="Time", ylab="Temp", xaxt="n")
axis(1, at=1:length(times), labels=times)
