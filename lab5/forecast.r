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
    location_distance <- location_distance / 1000 # Convert to km.
    time_point_distance <- abs(time_points[1] - time_points[2])
    day_distance <- abs(days[1] - days[2])
    day_kernel <- gaussian_kernel(as.numeric(day_distance), 0.5)
    location_kernel <- gaussian_kernel(as.numeric(location_distance), 0.5)
    time_point_kernel <- gaussian_kernel(as.numeric(time_point_distance), 0.5)
    return(day_kernel + location_kernel + time_point_kernel)
}


forecast <- function(date, location, measurements) {
    time <- paste0(seq(4, 24), ":00:00")
    time[1:6] <- paste0("0", time[1:6])
    time <- as.POSIXlt(paste("1970-01-01", time))
    temperature <- vector(length = length(time))
    predicted <- apply(measurements, 1,
                       function(measurement) {
        forecast_kernel(c(location[1], location[2]),
                        c(measurement$longitude, measurement$latitude),
                        c(date, measurement$date),
                        c(time, measurement$time))
    })
}

day <- as.Date("2013-11-04")
latitude <- 58.4274 ; longitude <- 14.826
temperatures <- forecast(day, c(longitude, latitude))
