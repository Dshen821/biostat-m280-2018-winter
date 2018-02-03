rep <- 50
seed <- 280
nVals <- seq(100, 500, by = 100)
distributions <- c("gaussian", "t1", "t5")

  # Runs simulations with combinations of sample sizes
  # nVals = seq(100, 500, by=100) and distTypes = c("gaussian", "t1", "t5")
  # and write output to appropriately named files.
for (n in nVals){
  for (dist in distributions) {
    oFile <- paste(dist, "n", n, ".txt", sep = "_")
    arg <- paste(paste("seed=", seed, " n=", n, sep = ""),
                 paste("\'dist=", '\"', dist, '\"', "\'", sep = ""),
                 paste(" rep=", rep, " n=", n, sep = ""), sep = " ")
    sysCall <- paste("nohup Rscript runSim.R ", arg, " > ", oFile)
    system(sysCall)
    print(paste("sysCall=", sysCall, sep = ""))
  }
}

