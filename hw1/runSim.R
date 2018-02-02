for (arg in commandArgs(TRUE)) {
  # parsing command arguments
  eval(parse(text = arg))
}

isPrime = function(n) {
  # check if a given integer is prime
  if ( n <= 3 ) {
    return (TRUE)
  }
  if (any((n %% 2:floor(sqrt(n))) == 0)) {
    return (FALSE)
  }
  return (TRUE)
}

estMeanPrimes = function (x) {
  # estimate mean only using observation with prime indices
  n = length(x)
  ind = sapply(1:n, isPrime)
  return (mean(x[ind]))
}

MSE = function (seed,n,dist,rep){
  # Mean Squared Error Calculation for classical and prime mean.
  #
  # Args: 
  #   seed=seed number.
  #   n = sample size.
  #   dist = distribution type.
  #   rep = number of replicates.
  classicalmean <- vector()
  meanPrimes <- vector()
  set.seed(seed)
  for (i in 1:rep){
    if (dist == "gaussian"){
      x = rnorm(n)
    }
    else if (dist == "t1"){
      x = rt(n,1)
    }
    else if (dist == "t5"){
      x = rt(n,5)
    }
    classicalmean[i] <- mean(x)  # returns classical mean.
    meanPrimes[i] <- estMeanPrimes(x) # returns prime mean.
  }
  classicalMSE = sum((classicalmean)^2)/rep # returns classical MSE.
  primeMSE = sum((meanPrimes)^2)/rep # returns prime MSE.
  return(c(classicalMSE, primeMSE))
}
MSE(seed, n, dist, rep)

