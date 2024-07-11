lownumber <- function(numbers, mins, maxs){

  numbersrel <- matrix(NA,nrow=nrow(numbers),ncol=ncol(numbers))

  for (j in 1:ncol(numbers)){

      numbersrel[,j] <- (numbers[,j] - mins[j])/(maxs[j]-mins[j])

  }

  lownumbers <- matrix(data=numbersrel<0.15,nrow=nrow(numbers),ncol=ncol(numbers))
  lownumbers

}
