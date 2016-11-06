source("likelihood.r") # sum of ln(p(x|theta)).
lifetimes <- read.csv("machines.csv") # Matrix?
parameter <- seq(0.1, 8.0, by=0.1) # Testing...
# Apply each parameter theta individually gives
# the log-likelihood for each of the parameters
p <- sapply(parameter,lnlikelihood,x=lifetimes)
average_lnlikelihoods <- p / dim(lifetimes)[1];
mle <- order(average_lnlikelihoods)[length(p)];
mle <- mle * 0.1 ; cat("MLE(theta): ",mle,"\n")

# Plot the relation between theta = logl.
plot(parameter,p, xlab="theta-parameter",
     ylab = "loge/post-likelihood", type = "l",
     xlim=c(0,8), ylim=c(-242,-2), col="orange")
points(mle, lnlikelihood(lifetimes, mle),
       col="orange", lwd=c(2, 2), pch="x");
title("Max Log/Posteriori-Likelihood Estimation")

lifetimes6 <- t(data.matrix(lifetimes)[1:6])
p6 <- sapply(parameter,lnlikelihood,x=lifetimes6)
average_lnlikelihoods6 <- p6 / dim(lifetimes6)[1]
mle6 <- order(average_lnlikelihoods6)[length(p6)]
mle6 <- mle6 * 0.1 ; cat("MLE(theta): ",mle6,"\n")

# Plot the relation between theta = ln-l.
par(new=TRUE) # Seems a little bit hacky.
plot(parameter,p6, xlab="theta-parameter",
     ylab = "loge/post-likelihood", type = "l",
     xlim=c(0,8), ylim=c(-242, -2), col="purple")
points(mle6, lnlikelihood(lifetimes6, mle6),
       col="purple", lwd=c(2, 2), pch="x")

po <- sapply(parameter, polikelihood, x=lifetimes)
average_polikelihoods <- po / dim(lifetimes)[1]
mpe <- order(average_polikelihoods)[length(po)]
mpe <- mpe * 0.1 ; cat("MPE(theta): ",mpe,"\n")

# Plot the relation between theta = po-li.
par(new=TRUE) # Seems a little bit hacky.
plot(parameter,po, xlab="theta-parameter",
     ylab = "loge/post-likelihood",type="l",
     xlim=c(0,8), ylim=c(-242, -2),
     col = "Green")
points(mpe, polikelihood(lifetimes, mpe),
       col="Green", lwd=c(2, 2), pch="x")
legend(x = "bottomleft", c("loge-likelihood, |x| = 48",
                           "loge-likelihood, |x| = 6",
                           "post-likelihood, |x| = 48"),
       lty = c(1,1), lwd = c(2,2),
       col=c("Orange", "Purple", "Green"))
