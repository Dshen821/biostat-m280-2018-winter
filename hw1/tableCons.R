dat <- lapply(Sys.glob("*_.txt"), read.table)  # reading in all _.txt files.
  # unlist the list and create a matrix and remove the first column of 1's.
unlDat <- matrix(unlist(dat), byrow = TRUE, ncol = 3)[, -1] 
unlDat2 <- unlDat[c(6:15, 1:5), c(2, 1)]  # rearranging the matrix.
unlDat2Trans <- c(t(unlDat2))  # transpose.
matDat <- matrix(unlDat2Trans, nrow = 10) # 10 row matrix with the desired form.

datFrmDat <- as.data.frame(matDat)  # dataframe creation.
  # vector of mean types alternative
avgNames <- rep(c("PrimeAvg", "ClassAvg"), times = 5)
nObs <- rep(c(100, 200, 300, 400, 500), each = 2) # vector of sample sizes.
  # binding created vectors to dataframe of classical and prime means.
labeledDatFrmDat <- cbind(nObs, avgNames, datFrmDat) 
  # label columns of the data frame.
colnames(labeledDatFrmDat) <- c("n", "Method", "t1", "t5", "Gaussian")

library(knitr)  # use kable function to create a table with appropriate labels.
knitr::kable(labeledDatFrmDat,  
             col.names = c("$n$",  
                           "Method", "$t_1$", "$t_5$","Gaussian"), align = "l")
