distribution <- function(x, theta) {
    # An exponential distribution.
    exponential <- exp((-theta)*x)
    return(theta*exponential)
}

lnlikelihood <- function(x, theta) {
    p <- log(distribution(x, theta))
    return(sum(p)) # log-likelihood.
}

polikelihood <- function(x, theta) {
    jp<-prod(distribution(x, theta))
    posterior <- 10*exp(-10*theta)
    return(log(jp*posterior))
}
