# Define the function to check for schnapszahl
is_schnapszahl <- function(n) {
  n <- abs(n)  # Ensure the number is positive

  # If the number is NA, return NA
  if (is.na(n)) {
    return(NA)
  }

  # Convert the number to a string
  n_str <- as.character(n)

  # Check if all characters in the string are the same
  if (nchar(n_str) > 1 && all(substr(n_str, 1, 1) == strsplit(n_str, "")[[1]])) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}


