data <- lapply(Sys.glob("*.txt"), read.table)
library(knitr)
kable(final.table)
print(data)