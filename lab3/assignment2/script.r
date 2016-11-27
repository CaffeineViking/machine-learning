library("tree")
library("ggplot2")
library("reshape2")
library("grDevices")
library("e1071")

set.seed(12345) # As always.....
scores <- read.csv("scores.csv")
n <- nrow(scores) # Observation.
samples <- sample(1:n,  n / 2.0)
others <- setdiff(1:n, samples)
halves <- sample(others, n/4.0)

training <- scores[samples,]
trainingX <- training[,-ncol(training)]
trainingy <- training[,ncol(training)]

validation <- scores[halves,]
validationX <- validation[,-ncol(validation)]
validationy <- validation[,ncol(validation)]

testing  <- scores[-halves,]
testingX <- testing[,-ncol(testing)]
testingy <- testing[,ncol(testing)]

fit <- tree(good_bad ~ ., data = training, split = c("deviance"))
training_prediction <- predict(fit, trainingX, type= "class")
testing_prediction <- predict(fit, testingX, type="class")
cat("Missclassifications only with deviance impurity: (",
    mean(training_prediction != trainingy), "," ,
    mean(testing_prediction != testingy), ")\n")

fit <- tree(good_bad ~ ., data = training, split = c("gini"))
training_prediction <- predict(fit, trainingX, type= "class")
testing_prediction <- predict(fit, testingX, type="class")
cat("Missclassifications only with the gini impurity: (",
    mean(training_prediction != trainingy), "," ,
    mean(testing_prediction != testingy), ")\n")

fit <- tree(good_bad ~ ., data = training, split = c("deviance", "gini"))
training_prediction <- predict(fit, trainingX, type= "class")
testing_prediction <- predict(fit, testingX, type="class")
cat("Missclassifications only with deviance and gini: (",
    mean(training_prediction != trainingy), "," ,
    mean(testing_prediction != testingy), ")\n")

max_depth <- 15
training_deviance <- rep(0, max_depth)
validation_deviance <- rep(0, max_depth)
for (depth_level in 2:max_depth) {
    pruned <- prune.tree(fit, best = depth_level)
    pred <- predict(pruned, validation, type="tree")
    training_deviance[depth_level] <- deviance(pruned)
    validation_deviance[depth_level] <- deviance(pred)
}

deviances <- data.frame(2:max_depth, training_deviance[-1], validation_deviance[-1])
colnames(deviances) <- c("Leaves", "Training", "Validation")
collapsed_deviances <- melt(deviances, id="Leaves")

setEPS()
cairo_ps("deviance.eps")
# It seems depth 12 is good, since validation goes hayware after that...
print(ggplot(data=collapsed_deviances, aes(x=Leaves, y=value, color=variable)) +
      geom_smooth() + labs(x="Leaves", y="Deviance", color="data set"))
dev.off()

# The final tree has depth 6, see output of `final_tree`.
# The parameters chosen are: savings, duration, history, age,
# purpose, amount, other, resident, in `summary(final_tree)`.
final_tree <- prune.tree(fit, best = 12) # The best choice...
prediction <- predict(final_tree, testing, type = "class")
cat("Missclassification for the optimal tree depth: (",
    mean(prediction != testingy), ")\n")

setEPS()
cairo_ps("tree.eps")
plot(final_tree)
text(final_tree)
dev.off()

fit <- naiveBayes(good_bad ~ ., data = training)
training_prediction <- predict(fit, training, type = "class")
testing_prediction <- predict(fit, testing, type = "class")
cat("\nMissclassifications using Naive Bayes method:  (",
    mean(training_prediction != trainingy), "," ,
    mean(testing_prediction != testingy), ")\n")
cat("Confusion matrices for using Naive Bayes:\n")
print(table(training_prediction, trainingy))
print(table(testing_prediction, testingy))

training_probability <- predict(fit, training, type = "raw")
training_loss <- training_probability[,1] / training_probability[,2] > 10
training_loss[training_loss == FALSE] = "good"
training_loss[training_loss == TRUE] = "bad"
training_loss <- as.factor(training_loss)

testing_probability <- predict(fit, testing, type = "raw")
testing_loss <- testing_probability[,1] / testing_probability[,2] > 10
testing_loss[testing_loss == FALSE] = "good"
testing_loss[testing_loss == TRUE] = "bad"
testing_loss <- as.factor(testing_loss)

cat("\n\nMissclassifications using Naive Bayes loss method:  (",
    mean(training_loss != trainingy), "," ,
    mean(testing_loss != testingy), ")\n")
cat("Confusion matrices for using Naive Bayes loss:\n")
print(table(training_loss, trainingy))
print(table(testing_loss, testingy))
