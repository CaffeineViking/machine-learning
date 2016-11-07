source("distance.r") # cos-distance d.

# spam(i, t) - gets spam vector in i.
spam <- function(indices, training) {
    spamid <- ncol(training) #  last.
    return(mean(training[indices, spamid]))
}

# knearest(t, k, t') - predicts values
# given in t', for training data in t.
# Done with using k-nearest neighbors.
knearest <- function(train, k, test) {
    # Don't include the 'Spam' column, not feature.
    test_features <- data.matrix(test[,-ncol(test)])
    train_features <- data.matrix(train[,-ncol(train)])

    # Compute the distance matrix between train and test
    # using the cosine distance formula (see distance.r)
    # which is then sorted, for picking the k-neighbors.
    distances <- distance(train_features, test_features)
    sorted_distance_ids <- as.matrix(t(apply(distances, 2, order))[,1:k])

    # Finally, retrieve if the training data is spam or
    # not, selecting the k-closest classifications, for
    # later determining the most likely classification.
    spamv <- apply(sorted_distance_ids, 1, spam, train)
    kspam_vector <- spamv # Select only first K.
    mean_spam <- data.matrix(kspam_vector)
    # Still need to classify data by e.g. >0.5 -> 1.
    return(mean_spam) # This step is done in spam.r.
}
