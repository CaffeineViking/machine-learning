library("ggplot2")
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
    relation <- data.frame(K   = 1:k,
                           MSE = c(0))
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

        relation$MSE[i]<-mean(err)
        # Update best estimates.
        if (lowerror > err[1]) {
            lowerror <- err[1];
            features <- fea[1,]
        }
    }

    setEPS()
    postscript("kvsmse.eps")
    # Plot relation k v.s. M.S.E.
    plot(relation$K, relation$MSE,
         type="both", xlab="k",
         ylab = "Average M.S.E.")
    dev.off() # Write...
    # Best features.
    return(features)
}

set.seed(12345)
X <- data.matrix(swiss[,-1])
y <- data.matrix(swiss[,1]);
features <- featsel(X, y, 5)
X <- data.matrix(X[,features])
what <- linrhat(X, y)
yhat <- X %*% what

graph <- data.frame(X)
graph$Fertility <- yhat

setEPS()
postscript("education.eps")
plot(graph$Education, graph$Fertility,
   xlab="Education", ylab="Fertility")
dev.off()

postscript("catholic.eps")
plot(graph$Catholic, graph$Fertility,
   xlab="Catholic", ylab="Fertility")
dev.off()

postscript("mortality.eps")
plot(graph$Infant.Mortality, graph$Fertility,
   xlab="Infant Mortality", ylab="Fertility")
dev.off()
