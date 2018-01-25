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
      x = rt(1)
    }
  else if (dist == "t5"){
      x = rt(5)
  }
  classicalmean[i]<-mean(x)
  
  meanPrimes[i]=estMeanPrimes(x)
}
 # return(classicalmean)
  classicalMSE = sum((classicalmean)^2)/rep
  primeMSE = sum((meanPrimes)^2)/rep
  return(c(classicalMSE,primeMSE))
  
}

#Expand the runSim.R script to include arguments seed (random seed), n (sample size), 
#dist (distribution) and rep (number of simulation replicates). When dist="gaussian", 
#generate data from standard normal; when dist="t1", generate data from t-distribution 
#with degree of freedom 1 (same as Cauchy distribution); when dist="t5", generate data 
#from t-distribution with degree of freedom 5. Calling runSim.R will (1) set random seed
#according to argument seed, (2) generate data according to argument dist, (3) compute the
#primed-indexed average estimator in class and the classical sample average estimator for each simulation replicate, (4) report the average mean squared error (MSE)