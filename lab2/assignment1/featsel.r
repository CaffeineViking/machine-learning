source("kfoldcv.r")

# ------kfold(i, X, y, k)------
# Wrapper for applying kfoldcv,
# since it needs to apply rows.
kfold <- function(i, X, y, k) {
    Xi <- data.matrix(X[,i]);
    return(kfoldcv(Xi, y, k))
}

# ------featsel(X, y, k)------
# Applies feature selection to
# X, giving the best subset of
# X which minimizes the error.
# This is done by applying the
# k-fold cross validation, for
# each possible subset of X's.
featsel <- function(X, y, k) {
    features <- c() # Nothing.
    lowerror <- Inf # Not good
    for (i in 1:k) { # Wrapper
        # Produce all combins.
        fi <- t(combn(1:k, i))
        # Apply combinations for
        # each of the featureset
        # giving the error value
        ei <- apply(fi, 1, kfold,
                    X, y, k)

        # Order by error...
        err <- ei[order(ei)]
        fea <- fi[order(ei),]
        fea<-data.matrix(fea)

        # Update best estimates.
        if (lowerror > err[1]) {
            lowerror <- err[1];
            features <- fea[1,]
        }
    }

    # Best features.
    cat("Error: ", lowerror, "\n")
    return(features)
}
