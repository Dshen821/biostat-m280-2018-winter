library(tidyverse)
setwd("/home/m280-data/hw1")

individual <- read_delim("merge-geno.fam", delim = " ", col_names = FALSE)
snp <- read_delim("merge-geno.bim", delim = "\t ", col_names = FALSE)
colnames(individual) <- c("Family.ID", "Person.ID", "Father.ID", "Mother.ID", "Sex", "Affection.Status")
colnames(snp) <- c("Chromosome", "SNP.ID", "Genetic.Distance", "BP", "Allele.1", "Allele 2")



#How many persons are in the data set (statisticians call this n)? How many SNPs are in the data set (statisticians call this p)?
count(individual)
count(snp)

#Which chromosomes does this data set contain? How many SNPs are in each chromosome?
snpCount <- snp %>%
  group_by(Chromosome) %>%
  summarise(
    count = n()
  )

#MAP4 (microtubule-associated protein 4 is a gene on chromosome 3 spanning positions 47,892,180 bp – 48,130,769 bp. How many SNPs are located within MAP4 gene?
MAP4 <- snp %>%
  filter(Chromosome == 3, BP >= 47892180, BP<= 48130769) %>%
  summarise(count = n())
MAP4

# Mendel’s SNP definition file is similar to the plink bim file but has format
# SNP ID, Chromosome, Base Pair Position
# with each field separated by a comma. Write a Linux shell command to convert merge-geno.bim to Mendel SNP definition file. The first few lines of the Mendel SNP definition file should look like

snpDef <- dplyr::select(snp, SNP.ID, Chromosome, BP)
write_lines("     2.40 = FILE FORMAT VERSION NUMBER.\n8348674  = NUMBER OF SNPS LISTED HERE.", "/home/shendarrick821/biostat-m280-2018-winter/hw2/mendelSnpDef.txt", append = FALSE)
write_delim(snpDef, "/home/shendarrick821/biostat-m280-2018-winter/hw2/mendelSnpDef.txt", delim = ",", col_names = FALSE, append = TRUE)


#Mendel’s pedigree file is similar to the plink fam file but has format
#Family ID, Person ID, Father ID, Mother ID, Sex coded as M or F, Twin Status
#with each field separated by a comma. Write a Linux shell command to convert merge-geno.fam
#to Mendel pedigree file. Since twin status is not available in plink format, we put nothing for that field.
#Also Mendel limits Person ID to have length less than or equal to 8 characters, so we have to strip the string T2DG from the IDs. First few lines of the Mendel pedigree should look like
library(tidyverse)
mendPed <- select(individual, Family.ID, Person.ID, Father.ID, Mother.ID, Sex)
mendPed <- mutate(mendPed, Twin.Status = 0)
mendPed[mendPed == 0] <- NA
replace_na(mendPed, list(Father.ID = " ", Mother.ID = " ", Twin.Status = " "))

finalMendPed <- mendPed %>% 
  mutate(Sex = if_else(Sex == 2, "F", "M", missing = NULL)) %>%
  mutate(Person.ID = str_replace_all(Person.ID, "T2DG", "")) %>%
  mutate(Father.ID = str_replace_all(Father.ID, "T2DG", "")) %>%
  mutate(Mother.ID = str_replace_all(Mother.ID, "T2DG", ""))
write_delim(finalMendPed, "/home/shendarrick821/biostat-m280-2018-winter/hw2/mendelPedigree.txt", delim = ",", col_names = FALSE, append = FALSE, na = "")

print(head("mendelPedigree.txt"))