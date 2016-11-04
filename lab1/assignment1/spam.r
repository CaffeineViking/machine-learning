library("kknn")
source("distance.r")
source("knearest.r")

data <- read.csv("spambase.csv")
# Pick randomly around half of the rows in dataset.
samples <- sample(1:nrow(data), floor(0.5*nrow(data)))
# Split given dataset evenly for training and tests.
learning <- data.matrix(data[samples,]) # Training.
testing <- data.matrix(data[-samples,]) # Testing.

# Predict spam for testing data K=5.
k5 <- knearest(learning, 5, testing)
# Generate the confusion matrix for K=5.
cm5 <- table(k5, testing[,ncol(testing)])
# Calculate given missclassification.
mc5 <- 1 - sum(diag(cm5)) / sum(cm5)
# Report confusion matrix and error.
print(cm5) ; print(mc5)

# Predict spam for testing data K=1.
k1 <- knearest(learning, 1, testing)
# Generate the confusion matrix for K=1.
cm1 <- table(k1, testing[,ncol(testing)])
# Calculate given missclassification.
mc1 <- 1 - sum(diag(cm1)) / sum(cm1)
# Report confusion matrix and error.
print(cm1) ; print(mc1)
