#' Calculates the presence of simplex number biases in Random Number Generation tasks
#'
#' This function simulates datasets as a benchmark for true randomness.

#'
#' @param data A numeric matrix or data frame containing the individual random number selections (rows = individuals, columns = tasks)
#' @param mins A numeric vector specifying the lower limit of each random number generation task (e.g. 1, if the task is 1-100)
#' @param maxs A numeric vector specifying the lower limit of each random number generation task (e.g. 100, if the task is 1-100)
#' @param nsim Number of simulations to perform. Default is 10,000
#' @param seed Seed for reproducibility of simulations. Default is 123456789.
#'
#' @return A list containing:
#'   - `compositeZ`: A composite score indicating a person's propensity to simplex number bias (standardized scores compared to simulated data)
#'   - `rawbias`: Raw bias percentages for seven typical biases
#'   - `biasZ`: Standardized bias measures (Z scores for seven typical biases. Mean and SD are derived from simulated dataset).
#'   - `primes`: Matrix indicating prime numbers in the original dataset.
#'   - `even`: Matrix indicating even numbers in the original dataset.
#'   - `mult5`: Matrix indicating multiples of 5 in the original dataset.
#'   - `mult10`: Matrix indicating multiples of 10 in the original dataset.
#'   - `c0`: Matrix indicating the presence of digit 0 in the original dataset.
#'   - `c5`: Matrix indicating the presence of digit 5 in the original dataset.
#'   - `Central`: Matrix indicating the centrality of each value in the original dataset.
#'
#'
#' @details
#' This function generates simulated datasets (`sim1M` and `sim1Mwithoutmissing`) where random number
#' selection is uniform but introduces biases similar to those observed in the original dataset.
#' Biases are quantified using functions such as `is_prime2`, `is_multiple`, `count_digit`, and `centrality`.
#' The function calculates raw bias percentages (`rawbiasperc`), standardizes biases (`biaspercZ`),
#' and computes composite bias scores (`compositebiasZ`).
#'
#' @examples
#' data <- RNGopenpsychometrics
#' numbers <- data[,2:21]
#' mins <- c(1,1,44,100,1,123,1,7,1,60,80,1,5,600,1,600,1,1,77,30)
#' maxs <- c(100,1000,99,500,100,1234,100,50,100,140,90,100,8,800,100,800,4,100,4012,60)
#' bias <- RNGbiases(numbers, mins, maxs)
#'
#' @seealso
#' `is_prime2`, `is_multiple`, `count_digit`, `centrality`
#'
#' @export



RNGbiases <- function(data, mins, maxs, nsim = 10000, seed = 123456789) {


  # Simulates a dataset where random number selection is completely random (uniform distribution)

  set.seed(seed)
  sim1M <- data.frame(matrix(NA,nrow=nsim,ncol=length(mins)))
  sim1Mwithoutmissing <- data.frame(matrix(NA,nrow=nsim,ncol=length(mins)))

  numbers <- data

  missing <- matrix()

  for (i in 1:length(mins)){

    missing[i] <- length(which(is.na(numbers[,i])))/length(numbers[,i])

    sim1M[,i] <- trunc(runif(nsim,mins[i],maxs[i]+0.9999999999999999999999))
    sim1Mwithoutmissing[,i] <- sim1M[,i]
    sel <- sample(1:nsim,round(nsim*missing[i],digits=0))

    sim1M[sel,i] <- NA


  }


  primes <- is_prime2(numbers)
  even <- is_multiple(numbers, 2)
  mult5 <- is_multiple(numbers, 5)
  mult10 <- is_multiple(numbers, 10)
  c0 <- count_digit(numbers, 0)
  c5 <- count_digit(numbers, 5)
  central <- centrality(numbers)
  repdigit <- is_schnapszahl2(numbers)
  pop <- popularity(numbers, mins, maxs)
  lnb <- lownumber(numbers, mins, maxs)


  primes_sim <- is_prime2(sim1M)
  even_sim <- is_multiple(sim1M, 2)
  mult5_sim <- is_multiple(sim1M, 5)
  mult10_sim <- is_multiple(sim1M, 10)
  c0_sim <- count_digit(sim1M, 0)
  c5_sim <- count_digit(sim1M, 5)
  central_sim <- centrality(sim1M)
  repdigit_sim <- is_schnapszahl2(sim1M)
  pop_sim <- popularity(sim1M, mins, maxs)
  lnb_sim <- lownumber(sim1M, mins, maxs)

  primeavoidanceperc <- rowMeans(1 - primes, na.rm = T)
  evenperc <- rowMeans(even, na.rm = T)
  mult5perc <- rowMeans(mult5, na.rm = T)
  mult10perc <- rowMeans(mult10, na.rm = T)
  c0perc <- rowMeans(c0, na.rm = T)
  c5perc <- rowMeans(c5, na.rm = T)
  centralperc <- rowMeans(central, na.rm = T)
  repdigitperc <- rowMeans(repdigit, na.rm = T)
  popperc <- rowMeans(pop, na.rm = T)
  lnbperc <- rowMeans(lnb, na.rm = T)

  primeavoidanceperc_sim <- rowMeans(1 - primes_sim, na.rm = T)
  evenperc_sim <- rowMeans(even_sim, na.rm = T)
  mult5perc_sim <- rowMeans(mult5_sim, na.rm = T)
  mult10perc_sim <- rowMeans(mult10_sim, na.rm = T)
  c0perc_sim <- rowMeans(c0_sim, na.rm = T)
  c5perc_sim <- rowMeans(c5_sim, na.rm = T)
  centralperc_sim <- rowMeans(central_sim, na.rm = T)
  repdigitperc_sim <- rowMeans(repdigit_sim, na.rm = T)
  popperc_sim <- rowMeans(pop_sim, na.rm = T)
  lnbperc <- rowMeans(lnb, na.rm = T)

  rawbiassim <- data.frame(cbind(primeavoidanceperc_sim,evenperc_sim,mult5perc_sim,mult10perc_sim,c0perc_sim,c5perc_sim,centralperc_sim,repdigitperc_sim, popperc_sim, lnbperc_sim))

  rawbiasperc <- data.frame(cbind(primeavoidanceperc,evenperc,mult5perc,mult10perc,c0perc,c5perc,centralperc,repdigitperc, popperc, lnbperc))
  biaspercZ <- data.frame(matrix(NA,nrow=nrow(numbers),ncol=ncol(rawbiasperc)))

  for (i in 1:nrow(numbers)){

    temp <- rawbiassim
    temp[nsim + 1, ] <- rawbiasperc[i,]
    tempZ <- scale(temp)
    biaspercZ[i,] <- tempZ[nsim + 1, ]

  }

  colnames(biaspercZ) <- c("primeavoidanceZ","evenZ","mult5Z","mult10Z","c0Z","c5Z","centralityZ","repdigitZ", "popularityZ", "lownumberZ")




  compositeZ <- rowMeans(biaspercZ,na.rm=T)

  result <- list(compositebiasZ = compositeZ, rawbias = rawbiasperc, biasZ = biaspercZ, biasSim = rawbiassim, primes = primes, even = even, mult5 = mult5, mult10 = mult10, c0 = c0, c5 = c5, central = central, repdigit = repdigit, popularity = pop, lownumber = lnb)

  result


}
