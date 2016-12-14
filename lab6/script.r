library("ggplot2")
library("reshape2")
library("neuralnet")
library("grDevices")
set.seed(1234567890)

variable <- runif(50, 0, 10)
sine <- data.frame(x=variable, sin=sin(variable))
training <- sine[1:25,] ; testing <- sine[26:50,]

candidate_error <- Inf
units <- 10 # Hidden baby!
candidate_threshold <- Inf
weights <- runif(50, -1, +1)

for (threshold_attempt in 1:10) {
    thresholdi <- threshold_attempt / 1000
    nn <- neuralnet(sin~x, training, units,
                    startweights = weights,
                    threshold = thresholdi)

    predicted <- compute(nn, testing$x)
    error <- sum((testing$sin - predicted$net.result)^2)
    cat("NN Threshold", thresholdi, "->", error, "SSE \n")
    if (error < candidate_error) {
        candidate_error = error
        candidate_threshold = thresholdi
    }
}

nn <- neuralnet(sin~x, training, units,
                   candidate_threshold,
                startweights = weights)
predicted <- compute(nn, testing$x)

setEPS()
cairo_ps("network.eps")
plot(nn, file="network.png")
dev.off()

setEPS()
cairo_ps("predictions.eps")
plot(testing$x, predicted$net.result, col = "orange",
     xlab = "x", ylab = "sin")
points(sine, col = "blue")
dev.off()
