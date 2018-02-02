dat <- lapply(Sys.glob("*_.txt"), read.table) #reading in all _.txt files.

unl_dat <- matrix(unlist(dat), byrow = TRUE, ncol = 3)[, -1] 
  # unlist the list and create a matrix and remove the first column of 1's.
unl_dat2 <- unl_dat[c(6:15, 1:5), c(2, 1)]  # rearranging the matrix.
unl_dat2_trans <- c(t(unl_dat2))  # transpose.
matdat <- matrix(unl_dat2_trans, nrow = 10)
  # 10 row matrix with the desired form.

datfrmdat <- as.data.frame(matdat)  # dataframe creation.
avgNames <- rep(c("Prime", "Class"), times = 5)  # vector of mean types altern.
nObs <- rep(c(100, 200, 300, 400, 500), each = 2) # vector of sample sizes.
labeled_datfrmdat <- cbind(nObs, avgNames, datfrmdat) 
  # binding created vectors to dataframe of classical and prime means
colnames(labeled_datfrmdat) <- c("n", "Method", "t1", "t5", "Gaussian")
  # label columns of the data frame
library(knitr)  #use kable function to create a table with appropriate labels.
knitr::kable(labeled_datfrmdat,  
             col.names = c("$n$             ",  
                           "            Method", "$t_1$", "$t_5$","Gaussian"))
