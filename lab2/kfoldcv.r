# ------disjoin(X, k)------
# Produce n/k disjoint sets
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

# ------kfoldcv(X, y, k)-------
# Returns the estimated genera-
# lization error of the feature
# set according to a linear mo-
# del. It does this by applying
# an k-folding cross validation
# method, splitting X randomly,
# gives k disjoint subets of X.
kfoldcv <- function(X, y, k)  {
}
