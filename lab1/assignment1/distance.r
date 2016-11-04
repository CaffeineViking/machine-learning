# distance(X, Y) - distances between X, Y.
# Uses the usual cosine distance function.
# Batch operation into a matrix -> fast...
distance <- function(matrix_x, matrix_y) {
    x_squared_sum <- rowSums(matrix_x^2)
    y_squared_sum <- rowSums(matrix_y^2)
    x_prime <- matrix_x / sqrt(x_squared_sum)
    y_prime <- matrix_y / sqrt(y_squared_sum)
    similarity_matrix <- x_prime %*% t(y_prime)
    distance_matrix <- 1.0 - similarity_matrix
    return(distance_matrix)
}
