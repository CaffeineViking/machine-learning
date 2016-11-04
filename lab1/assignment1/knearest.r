source("distance.r") # cos-distance d.

# spam(i, t) - gets spam vector in i.
spam <- function(indices, training) {
    spamid <- ncol(training) #  last.
    return(training[indices, spamid])
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
    sorted_distance_ids <- t(apply(distances, 1, order))

    # Finally, retrieve if the training data is spam or
    # not, selecting the k-closest classifications, for
    # later determining the most likely classification.
    spamv <- apply(sorted_distance_ids, 1, spam, train)
    kspam_vector <- spamv[,1:k] # Select only first K.
    mean_spam <- rowMeans(data.matrix(kspam_vector))
    # Spam if >0.5, not-spam iff <=0.5.
    return(round(mean_spam))
}
