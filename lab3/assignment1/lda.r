library("ggplot2")

mu <- function(X)  { return(colMeans(X)) }
sigma <- function(X, mu) {
    distance <- X - mu
    X <- data.matrix(X)
    distance <- data.matrix(distance)
    projection <- apply(distance, 1,
             function(d) t(d) %*% d)
    return(mean(projection))
}

lda <- function(X, y)   {
    classes <- levels(y) # Only c = 2
    class1  <- which(y == classes[1])
    class2  <- which(y == classes[2])
    X1     <- data.matrix(X[class1,])
    y1     <- data.matrix(y[class1]);
    X2     <- data.matrix(X[class2,])
    y2     <- data.matrix(y[class2]);

    mu1 <- mu(X1) ; mu2 <- mu(X2)
    pi1 <- length(y1) / length(y)
    pi2 <- length(y2) / length(y)
    sigma1 <- sigma(X1, mu1)
    sigma2 <- sigma(X2, mu2)

    sigma <- sigma1*length(y1) +
             sigma2*length(y2)
    sigma <- sigma / length(y)

    w01 <- t(mu1) * (1 / sigma) * mu1 + log(pi1)
    w01 <- -(w01 / 2) # Needed, because of log..
    wx1 <- (1 / sigma) * mu1 # Some like magic.

    w02 <- t(mu2) * (1 / sigma) * mu2 + log(pi2)
    w02 <- -(w02 / 2) # Needed, because of log..
    wx2 <- (1 / sigma) * mu2 # Some like magic.
}

crabs <- read.csv("crabs.csv")
X <- crabs[,c("CL", "RW")]
y <- crabs[,c("sex")]

# setEPS()
# postscript("crabs.eps")
print(qplot(CL, RW, data = crabs, color = sex,
      geom = c("point", "smooth"),
      xlab = "Carapace Length",
      ylab = "Rear Width"))
# dev.off()
