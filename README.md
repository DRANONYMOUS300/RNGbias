# RNGbias

This package calculates common biases in simple random number generation tasks (RNG). These biases are (1) prime number avoidance, (2) even number preference, (3) digit zero preference, (4) digit five preference, (5) multiples of 5 preference, (6) multiples of 10 preference and (7) centrality bias. The package also calculates a composite score indicating the latent dimension behind these biases (Simplex number bias). The package compares the answers given to a simulated dataset as a benchmark for true randomness (uniform distribution). 

## Installation

```R 
library(devtools)
install_github("dranonymous300/RNGbias")
library(RNGbias)
```



## Examples


### Example on RNG task on openpsychometrics.org

Openpsychometrics.org provides a dataset on N=1,369 answers in twenty random number generation tasks.

```R 
data <- openpsychometricsRNG

numbers <- data[,2:21]

mins <- c(1,1,44,100,1,123,1,7,1,60,80,1,5,600,1,600,1,1,77,30)
maxs <- c(100,1000,99,500,100,1234,100,50,100,140,90,100,8,800,100,800,4,100,4012,60)

bias <- RNGbiases(numbers, mins, maxs)


```




### Example on manual data entry

This is an example with three random number generation tasks (r1,r2 and r3). The questions are:
Please select a random number
r1: between 1 and 50
r2: between 1 and 10
r3: between 1000 and 5000

```R 
data <- data.frame(r1=c(10,20,30,40),r2=c(1,2,3,4),r3=c(1000,2000,3000,4000))

lowerlimits <- c(1,1,1000)
upperlimits <- c(50,10,5000)

bias <- RNGbiases(data, lowerlimits, upperlimits)


```





### Example on replication data

This is an example with a replication study on N=415 students. In this study the same tasks as on openpsychometrics.org were used. However, three different instructions were tested to quantifiy the malleability of Simplex Number Bias: (1) no specific instruction (2) lottery (3) the task was to give unusual answers 

```R 

openpsychometrics <- openpsychometricsRNG
students <- replicationRNG

unspecific_discovery <- openpsychometrics[,2:21]
unspecific_replication <- students[,4:23]
uncommonanswers_students <- students[,24:43]
lottery_students <- students[,45:64]


biases <- RNGbiases(numbers, mins=mins,maxs=maxs,nsim=1000)
biases1 <- RNGbiases(numbers1,mins=mins,maxs=maxs,nsim=1000)
biases2 <- RNGbiases(numbers2,mins=mins,maxs=maxs,nsim=1000)
biases3 <- RNGbiases(numbers3,mins=mins,maxs=maxs,nsim=1000)

print(biases1$OddsRatios)



```





