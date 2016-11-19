library("ggplot2")

mu <- function(X)  { return(colMeans(X)) }
softmax <- function(X, wi, wj) {
    X <- data.matrix(X)
    ihypothesis <- exp(X %*% wi[-1] + wi[1])
    jhypothesis <- exp(X %*% wj[-1] + wj[1])
    jhypothesis <- jhypothesis +
                   exp(X %*% wi[-1] + wi[1])
    return(ihypothesis / jhypothesis)
}

lda <- function(X, y) {
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
    sigma <- cov(X1)*nrow(X1)+
             cov(X2)*nrow(X2)
    sigma <- sigma/nrow(X)

    w01 <- (-(t(mu1)/2)) %*% solve(sigma) %*% mu1 + log(pi1)
    wx1 <- solve(sigma) %*% mu1 # Some sort of magic.
    w1 <- matrix(c(w01, wx1), 1, 3)

    w02 <- (-(t(mu2)/2)) %*% solve(sigma) %*% mu2 + log(pi2)
    wx2 <- solve(sigma) %*% mu2 # Some magic here too.
    w2 <- matrix(c(w02, wx2), 1, 3)

    Xm <- data.matrix(X)
    w1p <- softmax(X, w1, w2)
    w2p <- softmax(X, w2, w1)
    y_hat <- w1p > w2p
    y_hat[y_hat == TRUE] = "Male"
    y_hat[y_hat == FALSE] = "Female"
    return(y_hat)
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
