popularity <- function(numbers, mins, maxs) {


typical <- matrix(NA,nrow=nrow(numbers),ncol=ncol(numbers))

for (i in 1:ncol(numbers)){

  u <- unique(numbers[,i])
  tot <- length(na.omit(numbers[,i]))
  exp <- 1/length(seq(mins[i],maxs[i],1))

  for (j in 1:length(u)){


    sel <- which(numbers[,i]==u[j])
    typical[sel,i] <- length(sel)/tot/exp

  }





}

result <- log(typical)

result

}
