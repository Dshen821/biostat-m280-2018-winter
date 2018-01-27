dat <- lapply(Sys.glob("*.txt"), read.table)

unl_dat <- matrix(unlist(dat), byrow = TRUE, ncol = 3)[, -1]; 
unl_dat2 <- unl_dat[c(6:15, 1:5), c(2, 1)]; 
unl_dat2_trans <- c(t(unl_dat2)) #transpose
matdat <- matrix(unl_dat2_trans, nrow = 10)

datfrmdat <- as.data.frame(matdat)
avgNames <- rep(c("Prime", "Class"), times = 5)
nObs <- rep(c(100, 200, 300, 400, 500), each = 2)
labeled_datfrmdat <- cbind(nObs, avgNames, datfrmdat)
colnames(labeled_datfrmdat) <- c("n", "Method", "t1", "t5", "Gaussian")
kable(labeled_datfrmdat)
