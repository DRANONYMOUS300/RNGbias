
is_prime2 <- function(m){

  result <- matrix(NA,nrow=nrow(m),ncol=ncol(m))

  for (i in 1:nrow(m)){
    for (j in 1:ncol(m)){

      result[i,j] <-is_prime(m[i,j])
    }

  }
  result

}

