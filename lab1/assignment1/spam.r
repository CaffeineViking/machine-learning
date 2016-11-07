library("kknn")
source("distance.r")
source("knearest.r")

sensitivity <- function(x, y) {
    tp <- sum(x == 1 & y == 1)
    fn <- sum(x == 0 & y == 1)
    return(tp / (tp + fn))
}

specificity <- function(x, y) {
    tn <- sum(x == 0 & y == 0)
    fp <- sum(x == 1 & y == 0)
    return(tn / (tn + fp))
}

set.seed(12345) # For debugging.
data <- read.csv("spambase.csv")
# Pick randomly around half of the rows in dataset.
samples <- sample(1:nrow(data), floor(0.5*nrow(data)))
# Split given dataset evenly for training and tests.
learning <- data.matrix(data[samples,]) # Training.
testing <- data.matrix(data[-samples,]) # Testing.

# Predict spam for testing data K=5.
cat("\nknearest: predicting k = 5\n")
k5 <- knearest(learning, 5, testing)
kr5 <- round(k5) # Classify >0.5 -> 1|0.
# Generate the confusion matrix for K=5.
cm5 <- table(kr5, testing[,ncol(testing)])
# Calculate given missclassification.
mc5 <- 1 - sum(diag(cm5)) / sum(cm5)
# Report confusion matrix and error.
print(cm5) ; print(mc5)

# Predict spam for testing data K=1.
cat("\nknearest: predicting k = 1\n")
k1 <- knearest(learning, 1, testing)
kr1 <- round(k1) # Classify >0.5 -> 1|0.
# Generate the confusion matrix for K=1.
cm1 <- table(kr1, testing[,ncol(testing)])
# Calculate given missclassification.
mc1 <- 1 - sum(diag(cm1)) / sum(cm1)
# Report confusion matrix and error.
print(cm1) ; print(mc1)

cat("\nkknn: training and predicing with k = 5\n")
m5 <- train.kknn(Spam ~ ., data = data.frame(learning), ks = c(5))
p5 <- predict(m5, data.frame(testing)) # Predict spam with k = 5.
pr5 <- round(p5) # Classify with the function >0.5 -> 1 else 0.
cm5 <- table(pr5, testing[,ncol(testing)])
# Calculate given missclassification.
mc5 <- 1 - sum(diag(cm5)) / sum(cm5)
# Report confusion matrix and error.
print(cm5) ; print(mc5)

cat("\nkknn: training and predicing with k = 1\n")
m1 <- train.kknn(Spam ~ ., data = data.frame(learning), ks = c(1))
p1 <- predict(m1, data.frame(testing)) # Predict spam with k = 1.
pr1 <- round(p1) # Classify with the function >0.5 -> 1 else 0.
cm1 <- table(pr1, testing[,ncol(testing)])
# Calculate given missclassification.
mc1 <- 1 - sum(diag(cm1)) / sum(cm1)
# Report confusion matrix and error.
print(cm1) ; print(mc1)

# Classify dataset by 0.05 steps...
response <- testing[,ncol(testing)]
classify <- seq(0.05, 0.95, by=0.05)
# Apply the classification rule for all.
kc5 <- sapply(k5, function(x) x > classify)
pc5 <- sapply(p5, function(x) x > classify)

# Find the sensitivity and specificity of knearest.
ksensitivity <- apply(kc5, 1, sensitivity, response)
kspecificity <- apply(kc5, 1, specificity, response)
psensitivity <- apply(pc5, 1, sensitivity, response)
pspecificity <- apply(pc5, 1, specificity, response)

plot(1 - kspecificity, ksensitivity, xlim=c(0.05,0.95), ylim=c(0.05,0.95), xlab="Specificity", ylab="Sensitivity", type='l')
lines(1 - kspecificity, ksensitivity, col="Orange") ; lines(1 - pspecificity, psensitivity, col="Purple") 
legend(x = "bottomright", c("knearest", "kknn"), lty = c(1,1), lwd = c(2,2), col=c("Orange", "Purple"))
lines(0:1, 0:1, col="Red", xlim=c(0.05, 0.95), ylim=c(0.05,0.95))
title("ROC Curve for the K-NN Spam Predictors")
