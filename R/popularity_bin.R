popularity_bin <- function(numbers, mins, maxs) {


typical_bin <- matrix(NA,nrow=nrow(numbers),ncol=ncol(numbers))

for (i in 1:ncol(numbers)){

  t <- table(numbers[,i])
  t <- sort(t,decreasing=T)

  range <- maxs[i] - mins[i]

  n20 <- round(length(unique(numbers[,i]))/10,digits=0)

  if (n20 == 0) {n20 <- 1}


  top20 <- as.numeric(names(t[1:n20]))
  nottop20 <- as.numeric(names(t[(n20+1):length(t)]))





    r <- numbers[,i] %in% top20
    nr <- numbers[,i] %in% nottop20


    typical_bin[which(r==TRUE),i] <- 1
    typical_bin[which(nr==TRUE),i] <- 0








}



typical_bin

}
