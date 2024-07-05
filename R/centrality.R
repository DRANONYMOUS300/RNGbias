#' Calculate Centrality Measure
#'
#' This function calculates a centrality measure based on input numbers, maxs, and mins. TThe central3 measure quantifies the relative position of each element in numbers by assessing its deviation from the midpoint of its range defined by maxs and mins, normalized across the column's range.
#'
#' @param numbers A matrix or data frame of numeric values.
#' @param maxs A vector defining upper bounds for each column of `numbers`.
#' @param mins A vector defining lower bounds for each column of `numbers`.
#' @return A matrix of centrality measures corresponding to each element in `numbers`.
#' @export


centrality <- function(numbers, maxs, mins) {
  # Ensure maxs and mins are vectors with the same length as the number of columns in numbers
  if (length(maxs) != ncol(numbers) | length(mins) != ncol(numbers)) {
    stop("maxs and mins must have the same length as the number of columns in numbers")
  }

  # Centrality calculation
  central <- 1 - 2 * abs(numbers - matrix((maxs + mins)/2, nrow = nrow(numbers), ncol = ncol(numbers), byrow = TRUE)) / matrix((maxs - mins), nrow = nrow(numbers), ncol = ncol(numbers), byrow = TRUE)

  return(central)
}
