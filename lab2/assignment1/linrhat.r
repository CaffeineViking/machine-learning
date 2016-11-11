# ------linrhat(X, y)------
# Predicts the parameters w
# for the given features X,
# and the targets y through
# the use of an hat matrix.
linrhat <- function(X, y) {
    # Nice hat good sir...
    return(solve(t(X)%*%X)
           %*% t(X)%*%y)
}
