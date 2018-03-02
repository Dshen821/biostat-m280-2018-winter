
library(tidyverse)
library(ggplot2)
library(shiny)
library(dplyr)

LApayroll <- readRDS("LAPayroll.rds")
  # We only want to keep complete data.
# LApayroll <- LApayroll[complete.cases(LApayroll),]

# Total payroll by LA City. Visualize the total LA City payroll of each year, 
# with breakdown into base pay, overtime pay, and other pay.


# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("LA City Employee Payroll"),
   
   # Sidebar with a slider input for years and pay types
   sidebarLayout(
      sidebarPanel(
        selectInput("Pay",
                    label = "Select Pay Category",
                    choices = c("Base Pay", "OT Pay", "Other Pay", "Total Pay"),
                    selected = "Base Pay")
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
     LApayroll$Pay <-switch(input$Pay, 
                       "Base Pay" = LApayroll$Base.Pay,
                       "OT Pay" = LApayroll$OT.Pay,
                       "Other Pay" = LApayroll$Other.Pay,
                       "Total Pay" = LApayroll$Total.Payments)
     LApayroll %>%
       select(Year, Pay) %>%
       group_by(Year) %>%
       summarise(TotalPay = sum(Pay, na.rm = TRUE)) %>%
       ggplot(mapping = aes(x = Year, y = TotalPay)) +
       geom_col()
     
     
     #payroll <- LApayroll %>%
     # select(Row_ID, Year, Base.Pay, Other.Pay, OT.Pay) %>%
     #group_by(Year) %>%
     #summarise(sumOfBase = sum(Base.Pay),
     #         sumOfOther = sum(Other.Pay),
     #        sumOfOT = sum(OT.Pay)) %>%
     #gather(sumOfBase, sumOfOther, sumOfOT, value = "amount", key = "type")
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

