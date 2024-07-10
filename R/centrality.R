#' Calculate Centrality Measure
#'
#' This function calculates a centrality measure based on input numbers, maxs, and mins. TThe central3 measure quantifies the relative position of each element in numbers by assessing its deviation from the midpoint of its range defined by maxs and mins, normalized across the column's range.
#'
#' @param numbers A matrix or data frame of numeric values.
#' @param maxs A vector defining upper bounds for each column of `numbers`.
#' @param mins A vector defining lower bounds for each column of `numbers`.
#' @return A matrix of centrality measures corresponding to each element in `numbers`.
#' @export


centrality <- function(numbers) {
  # Standardize the numbers using the scale function


  # Calculate decentrality as the row-wise mean of the absolute values of the standardized numbers
  decentral <- as.matrix(abs(scale(numbers)))

  # Apply the custom logarithmic transformation
  central <- log(1 / (1 + decentral))

  return(central)
}
