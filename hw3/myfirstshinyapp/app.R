
library(tidyverse)
library(ggplot2)
library(shiny)
library(dplyr)

  # Here we read in the payroll dataset and rename column names to exlcude spaces.
#  LAdata<- read.csv("/home/m280-data/la_payroll/LA_City_Employee_Payroll.csv")
LAdata2<- read_csv("/home/m280-data/la_payroll/LA_City_Employee_Payroll.csv")
colnames(LAdata2) <- str_replace_all(names(pay.raw), " ", "_")
colnames(LAdata2)[24] <- "Other_Paye"
head(LAdata2)

LAEmployeePay <- LAdata2 %>%
  select(Row_ID, Year, Department_Title, Job_Class_Title, Total_Payments,
         Base_Pay, Other_Paye, Overtime_Pay, Total_Payments,
         Average_Benefit_Cost, Average_Dental_Cost)
  
LAEmployeePay$Total_Payments <- ifelse(!is.na(LAEmployeePay$Total_Payments), 
                                       as.numeric(str_replace
                                                  (LAEmployeePay$Total_Payments,
                                                    "\\$", "")), NA)

LAEmployeePay$Base_Pay <- ifelse(!is.na(LAEmployeePay$Base_Pay), 
                                       as.numeric(str_replace
                                                  (LAEmployeePay$Base_Pay,
                                                    "\\$", "")), NA)

LAEmployeePay$Other_Paye <- ifelse(!is.na(LAEmployeePay$Other_Paye), 
                                 as.numeric(str_replace
                                            (LAEmployeePay$Other_Paye,
                                              "\\$", "")), NA)

LAEmployeePay$Overtime_Pay<- ifelse(!is.na(LAEmployeePay$Overtime_Pay), 
                                   as.numeric(str_replace
                                              (LAEmployeePay$Overtime_Pay,
                                                "\\$", "")), NA)

LAEmployeePay$Average_Benefit_Cost<- ifelse(!is.na(LAEmployeePay$Average_Benefit_Cost), 
                                    as.numeric(str_replace
                                               (LAEmployeePay$Average_Benefit_Cost,
                                                 "\\$", "")), NA)
LAEmployeePay$Average_Dental_Cost<- ifelse(!is.na(LAEmployeePay$Average_Dental_Cost), 
                                            as.numeric(str_replace
                                                       (LAEmployeePay$Average_Dental_Cost,
                                                         "\\$", "")), NA) 

LAEP <-LAEmployeePay %>% rename(Total.Payments = Total_Payments,
                                Base.Pay = Base_Pay, Other.Pay = Other_Paye,
                                Overtime.Pay = Overtime_Pay,
                                Avg.Ben.Cost = Average_Benefit_Cost,
                                Avg.Den.Cost = Average_Dental_Cost)
head(LAEP)

saveRDS(LAEP, "LAPayroll.rds")
LA.payroll <- readRDS("LAPayroll.rds")
  # We only want to keep complete data.
LA.payroll <- LA.payroll[complete.cases(LA.payroll),]

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Old Faithful Geyser Data"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         sliderInput("bins",
                     "Number of bins:",
                     min = 1,
                     max = 50,
                     value = 30)
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("distPlot")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$distPlot <- renderPlot({
      # generate bins based on input$bins from ui.R
      x    <- faithful[, 2] 
      bins <- seq(min(x), max(x), length.out = input$bins + 1)
      
      # draw the histogram with the specified number of bins
      hist(x, breaks = bins, col = 'darkgray', border = 'white')
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

