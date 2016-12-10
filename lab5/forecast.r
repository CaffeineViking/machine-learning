library("ggplot2")
library("reshape2")
library("grDevices")
library("geosphere")

epoch <- as.Date("1970-01-01")
stations <- read.csv("stations.csv")
temps50k <- read.csv("temps50k.csv")
weather <- merge(stations, temps50k,
              by = "station_number")

# Language R has a very shitty library for doing dates/times.
weather$time <- as.POSIXlt(paste("1970-01-01", weather$time))
weather$day <- sub("^\\d{4}", "1970", weather$date)
weather$day <- as.Date(weather$day) - epoch
weather$day <- as.numeric(weather$day)

gaussian_kernel <- function(distance, sdspread)  {
    # The Gaussian Radial Basis Function.
    gamma_spread   <-  1 / (2*sdspread^2)
    return(exp(-gamma_spread*distance^2))
}

forecast_kernel <- function(longitudes, latitudes,
                            days,   time_points) {
    location_distance <- distHaversine(c(longitudes[1], latitudes[1]),
                                       c(longitudes[2], latitudes[2]))
    location_distance <- (location_distance / 1000) # Kilometers.
    day_distance <- ((days[1] - 182.5) - (days[2] - 182.5)) %% 365
    time_point_distance <- abs(time_points[1] - time_points[2])
    day_kernel <- gaussian_kernel(as.numeric(day_distance / 182.5), 0.5)
    location_kernel <- gaussian_kernel(as.numeric(location_distance / 1572), 0.5)
    time_point_kernel <- gaussian_kernel(as.numeric(time_point_distance / 14), 0.5)
    return((day_kernel + location_kernel + time_point_kernel) / 3.0)
}


forecast <- function(date, location, weather) {
    day <- as.numeric(date - epoch)
    clock <- paste0(seq(4,24,by=2),":00:00")
    clock[1:6] <- paste0("0", clock[1:3])
    clock <- as.POSIXlt(paste("1970-01-01", clock))
    temperature <- vector(length = length(clock))
    similarities <- matrix(0, nrow(weather), length(clock))
    for (i in 1:nrow(weather)) { # Shitty apply fails...
        if (i%%25==0) cat("Progress", (i/nrow(weather))*100,"%\n")
        for (j in 1:length(clock)) { # Need to predict for each...
            similarities[i, j] <- forecast_kernel(c(location[1], weather[i,]$longitude),
                                                  c(location[2], weather[i,]$latitude),
                                  c(day, weather[i,]$day), c(clock[j], weather[i,]$time))
        }
    } ; return(similarities)
}

day <- as.Date("2013-11-04")
longitude <- 58.4274 ; latitude <- 14.826
temperatures <- forecast(day, c(longitude, latitude),
                         weather) # Our observations.
