library("ggplot2")
library("reshape2")
library("grDevices")
library("geosphere")

# Fulhax for finding day diff.
epoch <- as.Date("1970-01-01")
epocht <- as.POSIXlt(paste(epoch,
                     "00:00:00"))
stations <- read.csv("stations.csv")
temps50k <- read.csv("temps50k.csv")
weather <- merge(stations, temps50k,
              by = "station_number")

# Language R has a very shitty library for doing dates/times.
weather$time <- as.POSIXlt(paste("1970-01-01", weather$time))
weather$hour <- round(difftime(weather$time, epocht,
                               units="hours"))
weather$day <- sub("^\\d{4}", "1970", weather$date)
weather$day <- as.Date(weather$day) - epoch
weather$day <- as.numeric(weather$day)

# Find faulty convert (because of leap year).
working <- which(complete.cases(weather$day))
weather <- weather[working,] # Remove errors.
indx <- sample(1:nrow(weather),nrow(weather))

gaussian_kernel <- function(distance, sdspread)  {
    # The Gaussian Radial Basis Function.
    gamma_spread   <-  1 / (2*sdspread^2)
    return(exp(-gamma_spread*distance^2))
}

date_time_modulo_distance <- function(x, y, n) {
    numeric_distance <- abs(x - y)
    modulo_distance <- n - numeric_distance
    if (numeric_distance < modulo_distance)
        return(numeric_distance)
    else return(modulo_distance)
}

forecast_kernel <- function(longitudes, latitudes,
                            days,   time_points) {
    time_points <- as.numeric(time_points) # I dunno why. FUCKING R.
    location_distance <- distHaversine(c(longitudes[1], latitudes[1]),
                                       c(longitudes[2], latitudes[2]))
    location_distance <- (location_distance / 1000) # Kilometers.
    day_distance <- date_time_modulo_distance(days[1], days[2], 365)
    day_kernel <- gaussian_kernel(as.numeric(day_distance / 182.5), 0.256)
    time_point_distance <- date_time_modulo_distance(time_points[1], time_points[2], 24)
    time_point_kernel <- gaussian_kernel(as.numeric(time_point_distance / 12), 0.256)
    location_kernel <- gaussian_kernel(as.numeric(location_distance / 1572), 0.192)
    # cat("(", location_distance, day_distance, time_point_distance, ")",
    #     "=> (", location_kernel, day_kernel, time_point_kernel, ") \n")
    return((day_kernel + location_kernel + time_point_kernel))
}

forecast <- function(date, location, weather) {
    clock <- paste0(seq(4,24,by=2))
    date <- sub("^\\d{4}", "1970", date)
    date <- as.Date(date) # Kill me.....
    day <- abs(as.numeric(date - epoch))
    # See lecture slides for predicting yhat-kernels.
    total_kernel_sum <- vector(length = length(clock))
    weighted_temperatures <- vector(length = length(clock))
    similarities <- matrix(0, nrow(weather), length(clock))
    stp <- 512 # Print steps for showing the progress...

    # Loop through each observation and desired time.
    for (i in 1:nrow(weather)) { # Shitty apply fails...
        if (i%%stp==0) cat("Progress",(i/nrow(weather))*100,"%\n")
        for (j in 1:length(clock)) { # Need to predict for each...
            # Calculate the similarity for this observation and our desired response
            similarities[i, j] <- forecast_kernel(c(location[1], weather[i,]$longitude),
                                                  c(location[2], weather[i,]$latitude),
                                 c(day, weather[i,]$day), c(clock[j], weather[i,]$hour))
            total_kernel_sum[j] <- total_kernel_sum[j] + similarities[i, j]
            weighted_temperature <- similarities[i, j] * weather[i,]$air_temperature
            weighted_temperatures[j] <- weighted_temperatures[j] + weighted_temperature
        }
        # Apply the Nadaraya-Watson kernel regression.
    } ; return(weighted_temperatures / total_kernel_sum)
}

args <- commandArgs(TRUE)
day <- if (!is.na(args[1])) as.Date(args[1]) else as.Date("2013-11-04")
latitude <- if (!is.na(args[2])) as.numeric(args[2]) else 58.4274
longitude <- if (!is.na(args[3])) as.numeric(args[3]) else 14.826
temperatures <- forecast(day, c(longitude, latitude),
                         weather[indx,]) # Kill me...
cat("Forecast (in oC) for the", as.character(day),
    "at 04:00:00 - 24:00:00 in", longitude, latitude,
    "(longitude, latitude)\n")
cat(temperatures, "\n")
