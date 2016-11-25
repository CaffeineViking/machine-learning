library("glmnet")
library("ggplot2")
library("grDevices")

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

    w01 <- -0.5 * mu1 %*% solve(sigma) %*% mu1 + log(pi1)
    wx1 <- solve(sigma) %*% mu1 # Some sort of weird magic.
    w1 <- matrix(c(w01, wx1), 1, 3)

    w02 <- -0.5 * mu2 %*% solve(sigma) %*% mu2 + log(pi2)
    wx2 <- solve(sigma) %*% mu2 # Some more magic here too.
    w2 <- matrix(c(w02, wx2), 1, 3)
    return(rbind(w1, w2)) # w1, w2.
}

classify <- function(X, d) {
    return(d[1] + d[2]*X[,1] +
                  d[3]*X[,2])
}

crabs <- read.csv("crabs.csv")
X <- crabs[,c("CL", "RW")]
y <- crabs[,c("sex")]

setEPS()
cairo_ps("crabs.eps")
print(qplot(CL, RW, data = crabs, color = sex,
      geom = c("point"),
      xlab = "Carapace Length",
      ylab = "Rear Width"))
dev.off()

parameters <- lda(X, y)
difference <- parameters[1,]-parameters[2,]
intercept <- difference[1] / difference[3]
slope <- difference[2] / difference[3]

sex <- classify(X, difference) > 0.0
sex[sex == FALSE] = "Female"
sex[sex == TRUE] = "Male"

setEPS()
cairo_ps("boundarylda.eps")
print(qplot(CL, RW, data = crabs, color = sex,
      geom = c("point"),
      xlab = "Carapace Length",
      ylab = "Rear Width") +
      geom_abline(intercept = -intercept,
                  slope = -slope, colour='purple'))
dev.off()

fit <- cv.glmnet(data.matrix(X), data.matrix(y),
    family = "binomial", type.measure = "class")
yhat <- predict(fit, data.matrix(X), type="class")

setEPS()
cairo_ps("boundarylr.eps")
print(qplot(X$CL, X$RW, color = yhat,
      geom = c("point"),
      xlab = "Carapace Length",
      ylab = "Rear Width") +
      geom_abline(intercept = -coef(fit)[1] / coef(fit)[3],
                  slope = -coef(fit)[2] / coef(fit)[3],
                  colour='purple'))
dev.off()

cat("Decision boundary with linear discriminant analysis:",
    -intercept, "+", -slope, "* k\n")
cat("Decision boundary with linear regression:",
    -coef(fit)[1] / coef(fit)[3], "+",
    -coef(fit)[2] / coef(fit)[3], "* k\n")
