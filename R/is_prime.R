
is_prime <- function(n) {

  n <- abs(n)
  if (is.na(n)){return(NA)}



  if (n==0){n<-1}
  if (n <= 1) {
    return(FALSE)
  }
  if (n <= 3) {
    return(TRUE)
  }
  if (n %% 2 == 0 || n %% 3 == 0) {
    return(FALSE)
  }

  i <- 5
  while (i * i <= n) {
    if (n %% i == 0 || n %% (i + 2) == 0) {
      return(FALSE)
    }
    i <- i + 6
  }





  return(TRUE)
}

