library("ggplot2")
library("reshape2")
library("grDevices")
library("geosphere")

stations <- read.csv("stations.csv")
temps50k <- read.csv("temps50k.csv")
weather <- merge(stations, temps50k,
              by = "station_number")

# Language R has a very shitty library for doing dates/times.
weather$time <- as.POSIXlt(paste("1970-01-01", weather$time))
weather$day <- sub("^\\d{4}", "1970", weather$date)
weather$day <- as.Date(weather$day) # Kill me...

gaussian_kernel <- function(distance, sdspread)  {
    # The Gaussian Radial Basis Function.
    gamma_spread   <-  1 / (2*sdspread^2)
    return(exp(-gamma_spread*distance^2))
}

forecast_kernel <- function(longitudes, latitudes,
                            days,   time_points) {
    location_distance <- distHaversine(c(longitudes[1], latitudes[1]),
                                       c(longitudes[2], latitudes[2]))
    time_points <- abs(time_points[1] - time_points[2])
    day_distance <- abs(days[1] - days[2])
}


forecast <- function(date, location) {
}
