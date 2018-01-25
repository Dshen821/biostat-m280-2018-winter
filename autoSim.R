# autoSim.R

#Modify the autoSim.R script to run simulations with combinations of sample sizes
#nVals = seq(100, 500, by=100) and distributions  distTypes = c("gaussian", "t1", "t5")
#and write output to appropriately named files. Use rep = 50, and seed = 280
rep = 50
seednum=280
nVals = seq(100, 500, by=100)
distTypes = c("gaussian", "t1", "t5")
for (i in distTypes){
  for (n in nVals) {
    oFile = paste(distTypes,"_n_", n, ".txt", sep="")
    arg = paste("seed=",seednum," n=",n," dist=",distTypes, " rep=",rep, " n=",n, sep="")
    sysCall = paste("nohup Rscript runSim.R ", arg, " > ", oFile)
    system(sysCall)
    print(paste("sysCall=", sysCall, sep=""))
  }
}

