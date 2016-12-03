library("tree")
library("boot")
library("ggplot2")
library("grDevices")

setEPS() # Enable save to eps.
state <- read.csv2("state.csv")
# Reorder data according to $MET.
state <- state[order(state$MET),]

cairo_ps("state.eps")
# Plotting MET vs EX. See file.
print(qplot(MET, EX, data = state,
    xlab = "Metropolitan Population Ratio",
    ylab = "Capita/Public Expenditures ($)",
    geom = c("point")))
dev.off()

set.seed(12345) # Required for cross-validation.
# Make sure that there are at least 8 leaves now.
control <- tree.control(nrow(state), minsize = 8)
# Fit our model by using regression trees (8 leaves).
fit <- tree(EX ~ MET, data = state, control = control)
optimal <- cv.tree(fit) # Do k-fold cross-validation.
least_deviance_index <- which.min(optimal$dev)
leaves <- optimal$size[least_deviance_index]
best_tree <- prune.tree(fit, best = leaves)
# Gives "best" tree according to k-fold cv.
yhat <- predict(best_tree, newdata = state)
# Predict EX by using best regression tree.

cairo_ps("tree.eps")
plot(best_tree)
text(best_tree)
dev.off()

cairo_ps("predicted_state.eps")
# Plotting MET vs EX. See the file.
print(qplot(MET, yhat, data = state,
    xlab = "Metropolitan Population Ratio",
    ylab = "Capita/Public Expenditures ($)",
    geom = c("point")))
dev.off()

cairo_ps("histogram.eps")
hist(residuals(best_tree))
dev.off()

bootstrap_predictor <- function(data, indices) {
    sample <- data[indices,] # Pick a subset of data.
    control <- tree.control(nrow(sample), minsize = 8)
    # Fit our model by using regression trees (8 leaves).
    fit <- tree(EX ~ MET, data = sample, control = control)
    leaves <- optimal$size[least_deviance_index]
    best_tree <- prune.tree(fit, best = leaves)
    # Gives "best" tree according to k-fold cv.
    yhat <- predict(best_tree, newdata = data)
    return(yhat) # Prediction from subset.
}

set.seed(12345) # Required for the bootstrapping.
# Apply non-parametric bootstrap to our regression tree
# model, picking out 1024 different indices from state.
bootstrap <- boot(state, bootstrap_predictor, R = 1024)

cairo_ps("bootstrap.eps")
plot(bootstrap)
dev.off()

# Find the confidence bands.
bands <- envelope(bootstrap)

cairo_ps("bands.eps")
# Plotting MET vs EX. See the file.
print(qplot(MET, yhat, data = state,
    xlab = "Metropolitan Population Ratio",
    ylab = "Capita/Public Expenditures ($)",
    geom = c("point")) + geom_line(data = state, aes(x = MET, y = bands$point[1,], col = "c.b")) +
                         geom_line(data = state, aes(x = MET, y = bands$point[2,], col = "c.b")))
dev.off()

bootstrap_prediction <- function(data) {
    control <- tree.control(nrow(data), minsize = 8)
    # Fit our model by using regression trees (8 leaves).
    fit <- tree(EX ~ MET, data = data, control = control)
    leaves <- optimal$size[least_deviance_index]
    best_tree <- prune.tree(fit, best = leaves)
    # Gives "best" tree according to k-fold cv.
    yhat <- predict(best_tree, newdata = data)
    sample <- rnorm(nrow(data), yhat, sd(resid(fit)))
    return(yhat) # Prediction from random subset.
}

bootstrap_confidence <- function(data) {
    control <- tree.control(nrow(data), minsize = 8)
    # Fit our model by using regression trees (8 leaves).
    fit <- tree(EX ~ MET, data = data, control = control)
    leaves <- optimal$size[least_deviance_index]
    best_tree <- prune.tree(fit, best = leaves)
    # Gives "best" tree according to k-fold cv.
    yhat <- predict(best_tree, newdata = data)
    return(yhat) # Prediction from subset.
}

bootstrap_random <- function(data, mle) {
    data$EX <- rnorm(nrow(data), predict(mle, newdata = data), sd(resid(mle)))
    return(data) # MLE is basically the best tree model from the prediction.
}

set.seed(12345) # Required for the paramatric bootstrapping using confidence bands instead.
bootstrapc <- boot(state, bootstrap_confidence, R = 1024, mle = best_tree, ran.gen = bootstrap_random, sim = "parametric")
set.seed(12345) # Required for the paramatric bootstrapping using prediction bands instead.
bootstrapp <- boot(state, bootstrap_prediction, R = 1024, mle = best_tree, ran.gen = bootstrap_random, sim = "parametric")
confidence_bands <- envelope(bootstrapc) ; prediction_bands <- envelope(bootstrapp) # Bands for the Parametric Boostrap...

cairo_ps("npcbootstrap.eps")
plot(bootstrapc)
dev.off()
cairo_ps("nppbootstrap.eps")
plot(bootstrapp)
dev.off()

cairo_ps("npbands.eps")
# Plotting MET vs EX. See the file.
print(qplot(MET, yhat, data = state,
    xlab = "Metropolitan Population Ratio",
    ylab = "Capita/Public Expenditures ($)",
    color = "predicted",
    geom = c("point")) + geom_line(data = state, aes(x = MET, y = prediction_bands$point[1,], col = "p.b")) +
                         geom_line(data = state, aes(x = MET, y = prediction_bands$point[2,], col = "p.b")) +
                         geom_line(data = state, aes(x = MET, y = confidence_bands$point[1,], col = "c.b")) +
                         geom_line(data = state, aes(x = MET, y = confidence_bands$point[2,], col = "c.b")) +
                         geom_point(data = state, aes(x = MET, y = EX, color="real")))
dev.off()
