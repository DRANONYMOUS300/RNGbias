# Define the function to apply is_schnapszahl to a matrix/dataframe
is_schnapszahl2 <- function(m) {
  result <- matrix(NA, nrow=nrow(m), ncol=ncol(m))

  for (i in 1:nrow(m)) {
    for (j in 1:ncol(m)) {
      result[i, j] <- is_schnapszahl(m[i, j])
    }
  }

  result
}
