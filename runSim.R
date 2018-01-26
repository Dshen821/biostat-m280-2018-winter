## parsing command arguments
for (arg in commandArgs(TRUE)) {
  eval(parse(text=arg))
}


## check if a given integer is prime
isPrime = function(n) {
  if (n <= 3) {
    return (TRUE)
  }
  if (any((n %% 2:floor(sqrt(n))) == 0)) {
    return (FALSE)
  }
  return (TRUE)
}

## estimate mean only using observation with prime indices
estMeanPrimes = function (x) {
  n = length(x)
  ind = sapply(1:n, isPrime)
  return (mean(x[ind]))
}

MSE = function (seed,n,dist,rep){
  classicalmean <-vector()
  meanPrimes<-vector()
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
  classicalmean[i]<-mean(x)
  meanPrimes[i]=estMeanPrimes(x)
}
 # return(classicalmean)
  classicalMSE = sum((classicalmean)^2)/rep
  primeMSE = sum((meanPrimes)^2)/rep
  return(c(classicalMSE,primeMSE))
}
MSE(seed,n,dist,rep)
