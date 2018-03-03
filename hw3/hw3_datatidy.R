# Here we read in the payroll dataset and rename column names to exclude spaces.
setwd(".")
LAdata2 <- read_csv("/home/m280-data/la_payroll/LA_City_Employee_Payroll.csv")
colnames(LAdata2) <- str_replace_all(names(LAdata2), " ", "_")
colnames(LAdata2)[24] <- "Other_Paye"
head(LAdata2)

LAEmp <- LAdata2 %>%
  select(Row_ID, Year, Department_Title, Job_Class_Title, Total_Payments,
         Base_Pay, Other_Paye, Overtime_Pay, Total_Payments,
         Average_Benefit_Cost)

#Changing character type to numeric for those in character type, removing $.
LAEmp$Total_Payments <- ifelse(!is.na(LAEmp$Total_Payments), 
                                       as.numeric(str_replace
                                                  (LAEmp$Total_Payments,
                                                    "\\$", "")), NA)

LAEmp$Base_Pay <- ifelse(!is.na(LAEmp$Base_Pay), 
                                 as.numeric(str_replace
                                            (LAEmp$Base_Pay,
                                              "\\$", "")), NA)

LAEmp$Other_Paye <- ifelse(!is.na(LAEmp$Other_Paye), 
                                   as.numeric(str_replace
                                               (LAEmp$Other_Paye,
                                                 "\\$", "")), NA)

LAEmp$Overtime_Pay <- ifelse(!is.na(LAEmp$Overtime_Pay), 
                                     as.numeric(str_replace
                                                 (LAEmp$Overtime_Pay,
                                                   "\\$", "")), NA)
 
LAEmp$Average_Benefit_Cost <- ifelse(!is.na
                                     (LAEmp$Average_Benefit_Cost),
                                      as.numeric(str_replace
                                                  (LAEmp$Average_Benefit_Cost,
                                                    "\\$", "")), NA)
#Renaming column names to be shorter.
LAEP <- LAEmp %>% rename(Total.Payments = Total_Payments,
                         Base.Pay = Base_Pay, Other.Pay = Other_Paye,
                         OT.Pay = Overtime_Pay,
                         Avg.Ben.Cost = Average_Benefit_Cost)
head(LAEP)
# Saving compressed RDS file.
saveRDS(LAEP, "LAPayroll.rds")
