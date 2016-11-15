library("MASS")
library("glmnet")
data <- read.csv("tecator.csv");
data_matrix <- data.matrix(data)

setEPS()
postscript("linear.eps")
plot(data$Protein, data$Moisture,
     xlab="Protein", ylab="Moisture")
dev.off()

# Do you think these data are described well by a linear model?
# Answer: yes, definitely. Both data are rising similar ratios.
# Moisture ~ N(mu = Xw, sigma^2), a linear model should work...
# Moisture_hat ~ Mi = polynomial function dependent on Protein:
#                     w0 + x1*w1 + x2*w2^2 + ... xi*wi^i = f(x)

set.seed(12345) # Set seed for getting same results...
indices <- sample(1:nrow(data), floor(nrow(data)*0.5))
training <- data[indices,] # Subset for training data.
validation <- data[-indices,] # Subset for validation.
validation <- validation[-nrow(validation),] # Shhh...

polynomials <- 1:6
imse <- data.frame(Model = polynomials,
                   Training.MSE = c(0.0),
                   Validation.MSE = c(0))
for (degree in polynomials) {
    model <- lm(Moisture ~ poly(Protein, degree),
                training)
    tprediction <- predict(model, training)
    vprediction <- predict(model, validation)
    vmse <- (vprediction - validation$Moisture)^2
    tmse <- (tprediction - training$Moisture)^2
    vmse <- mean(vmse) # MSE for validation set.
    tmse <- mean(tmse) # MSE for training set...
    imse$Validation.MSE[degree] = vmse # For i.
    imse$Training.MSE[degree] = tmse # For i.
}

setEPS()
postscript("depends.eps")
plot(imse$Model, imse$Training.MSE,
     xlab="Model", ylab="M.S.E.", "b", col="purple",
     ylim=c(31.0, 34.0))
points(imse$Model, imse$Validation.MSE, col="orange",
     type = "b", ylim=c(31.0, 34.0))
legend("bottomright", legend=c("Training", "Validation"),
       col=c("purple", "orange"), lty=1)
dev.off()

# Which model is best according to the plot?
# Answer: model where i = 1, since the validation predictions
# are becoming more erronous as i gets larger, while training
# becomes more accurate (this is caused by overfitting data).
# How do the M.S.E. values change and why?
# Answer: the error for the training set seems to become less
# as i gets larger while validation becomes more error prone.
# This is caused because the model overfits,  with a bias to-
# wards the training set (since the model is based on these).
# Ppecifically, it overfits:  the models become more complex.
# Interpret this picture in terms of bias-variance tradeoffs.
# Answer: it seems to be biased towards the training dataset.
# Since the E[yhat(x0) - f(x)] isn't close to zero, biased...

model <- lm(Fat ~ . - Moisture - Protein, data)
feature_selection <- stepAIC(model, direction="both")
# How many were selected? Answer: 64,  coeffiecients.

y <- data_matrix[,102] # Select Fat as the only response...
X <- data_matrix[,2:101] # Select Channel1-Channel-100 features.
ridge <- glmnet(X, y, alpha = 0) # Fit with the Ridge regression.
lasso <- glmnet(X, y, alpha = 1) # Fit with the Lasso regression.

setEPS()
postscript("ridge.eps")
# Explicit about x-axis variable.
plot(ridge, xvar="lambda", label=TRUE)
dev.off()

# Report on how the coeffiecients change with lambda.
# Answer: ridge penalizes all features equally with lambda.
# Therefore, it will take longer to coverge all features...

setEPS()
postscript("lasso.eps")
# Explicit about x-axis variable.
plot(lasso, xvar="lambda", label=TRUE)
dev.off()

# Conclusions on the Ridge vs Lasso resulting plots?
# Answer: the Lasso regression seems to converge individual
# features, therefore converging faster, towards the values
# of some features. While Ridge converges simultaniously...

kfoldcv <- cv.glmnet(X, y,  type.measure="mse", nfolds=20)
feature_selection <- coef(kfoldcv, s = "lambda.min")

# Report the optimal lambda and how many variables were selected.
# Answer: the optimal lambda was 0.02985605 and 14 were selected.
# Conclusions: the interval area shown in the graph shows optimal
# number of feature selections,  basically with the lowest M.S.E.

setEPS()
postscript("kfold.eps")
plot(kfoldcv)
dev.off()

# Compare the results from steps (4) and (7). Basically, compare
# stepAIC() and the glmnet CV (using k-fold).    Answer: stepAIC
# gives a lot of features, 64 for the selection, while glmnet cv
# for Lasso gives 13 features. Therefore, stepAIC() seems to not
# penalize the features very well, and therefore chooses more...
