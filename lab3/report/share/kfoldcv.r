source("linrhat.r")

# ------disjoin(X, k)------
# Produce a k disjoint sets
# of the training matrix X.
# Useful for kfoldcv below.
# Note: only returns index.
disjoin <- function(X, k) {
    row <- 1:nrow(X)
    folds <- nrow(X) / k
    # Resulting disjoints.
    S <- matrix(, k, folds)
    U <- c() # Picked sets.
    for (fold_row in 1:k) {
        D <- setdiff(row, U)
        Si <- sample(D, folds)
        S[fold_row,] <- Si
        U <- union(U, Si)
    }

    return(S)
}

# ------egerror(x, y)------
# Locate the generalization
# error within in our model
# by comparing the results:
# x and y targets. Returns:
# the difference for x & y.
egerror <- function(x, y) {
    targets <- length(x)
    # Using good old MSE...
    sdiff <- sum((x - y)^2)
    return(sdiff / targets)
}

# ------kfoldcv(X, y, k)-------
# Returns the estimated genera-
# lization error of the feature
# set according to a linear mo-
# del. It does this by applying
# an k-folding cross validation
# method, splitting X randomly,
# gives k disjoint subets of X.
kfoldcv <- function(X, y, k)  {
    kfolding <- 1:k
    sets <- disjoin(X, k)
    ege <- c() # Empty set of errors.
    for (i in kfolding) { # Every set.
        kset <- sets[-i,] # Remove 'i'
        iset <- sets[i,]  # Only  'i'.
        # Pick dataset for all but 'i'
        Xi <- X[kset,] ; yi <- y[kset]
        # Estimated parameters w. Xi.
        hypothesis <- linrhat(Xi, yi)
        # Predict for the unused 'i'.
        p <- X[iset,]%*%hypothesis
        # Esimate error of this.
        e <- egerror(p, y[iset])
        # Add to list of these.
        ege <- c(ege, abs(e))
    }

    # Oh yea baby,
    # average errors.
    return(mean(ege))
}
