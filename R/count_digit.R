#' Count Occurrences of a Target Digit in a Data Frame
#'
#' This function counts the occurrences of a specified digit in each element of a data frame.
#'
#' @param input A data frame. The data frame whose elements will be searched for the target digit.
#' @param target_digit A numeric or character value. The digit to count within the elements of the data frame.
#'
#' @return A data frame of the same dimensions as the input, where each element represents the count of the target digit in the corresponding element of the input data frame.
#' @examples
#' df <- data.frame(a = c(123, 456, 789), b = c(321, 654, 987))
#' count_digit(df, 2)
#' count_digit(df, '5')
#'
#' @export


count_digit <- function(input, target_digit) {
  # Ensure that the target_digit is a character
  target_digit <- as.character(target_digit)

  if (!is.data.frame(input)) {
    stop("Input must be a data frame.")
  }

  # Define a function to count occurrences of target_digit in a vector
  count_in_vector <- function(vec) {
    vec_chars <- strsplit(as.character(vec), split = "")
    count <- sapply(vec_chars, function(chars) sum(chars == target_digit))
    return(count)
  }

  # Apply count_in_vector function to each element of the data frame
  counts <- as.data.frame(lapply(input, count_in_vector))

  return(counts)
}
