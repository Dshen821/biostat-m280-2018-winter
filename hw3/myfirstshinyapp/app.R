 #  Read in Librarys.
library(tidyverse)
library(ggplot2)
library(shiny)
library(dplyr)

 # Read in Compressed RDS file.
LApayroll <- readRDS("LAPayroll.rds")

# Define UI for application that draws a histogram
ui <- fluidPage(
    # Application title
    titlePanel("LA City Employee Payroll Statistics"),
   
    # Sidebar with a slider input for years and pay types.
    # Q2:
    tabsetPanel(
      tabPanel("Total Pay Roll by City",
               p("Visualize the total LA City payroll of each year,
                 with breakdown into base pay, overtime pay, and other pay."),
        sidebarLayout(
          sidebarPanel(
            selectInput(inputId = "Pay",
                        label = "Select a Pay Type:",
                        choices = c("Base Pay", "OT Pay",
                                    "Other Pay", "Total Pay"),
                        selected = "Base Pay")
          ),
          mainPanel = plotOutput("distPlot")
        )
      ),
    # Q3:
      tabPanel("Who earned most?",
               p("Visualize the payroll information (total payment with
                 breakdown into base pay, overtime pay, and other pay,
                 Department, Job Title) of the top n highest paid LA City
                 employees in a specific year. "),
        sidebarLayout(
          sidebarPanel(
            selectInput(inputId = "Year",
                        label = "Select a Year",
                        choices = c("2013", "2014", "2015", "2016", "2017"),
                        selected = "2017"),
         
            sliderInput("nTopEarners",
                        label = "Information for Top Earners",
                        max = 10,
                        min = 1,
                        value = 10)
          ),
          mainPanel = tableOutput("distTable")   
        )
      ),
    # Q4:
      tabPanel("Which departments earn most?",
               p("Visualize the mean or median payroll, with breakdown into
                 base pay, overtime pay, and other pay,
                 of top n earning departments."),
        sidebarLayout(
          sidebarPanel(
            selectInput(inputId = "YearQ4",
                        label = "Select a Year:",
                        choices = c("2013", "2014", "2015", "2016", "2017"),
                        selected = "2017"),
            sliderInput("nDepts",
                        label = "Number of Departments Shown:",
                        max = 10,
                        min = 1,
                        value = 5),
            selectInput(inputId = "sumStat",
                        label = "Select Summary Statistic:",
                        choices = c("mean", "median"),
                        selected = "mean")
          ),
          mainPanel = tableOutput("distTablesumStat")   
        )
      ),
    # Q5:
      tabPanel("Which departments cost most?",
               p("Visualize the total payroll, with breakdown into base pay,
                 overtime pay, and other pay, of top n expensive departments."),
        sidebarLayout(
          sidebarPanel(
            selectInput(inputId = "YearQ5",
                        label = "Select a Year:",
                        choices = c("2013", "2014", "2015", "2016", "2017"),
                        selected = "2017"),
            sliderInput("nDeptsQ5",
                        label = "Number of Departments Shown:",
                        max = 10,
                        min = 1,
                        value = 5)
          ),
          mainPanel = tableOutput("distTableQ5")
        )
      ),
    # Q6:
      tabPanel("Which departments earn the most Overtime Pay?",
               p("Visualize what type of job leads to the most OT Pay."),
        sidebarLayout(
          sidebarPanel(
            selectInput(inputId = "YearQ6",
                        label = "Select a Year:",
                        choices = c("2013", "2014", "2015", "2016", "2017"),
                        selected = "2017"),
            sliderInput("nDeptsQ6",
                        label = "Number of Departments Shown:",
                        max = 10,
                        min = 1,
                        value = 5)
          ),
          mainPanel = plotOutput("distPlotQ6")
        )
      )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    #  Q2:
  output$distPlot <- renderPlot({
    LApayroll$Pay <- switch(input$Pay, 
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
  })
     # Q3:
  output$distTable <- renderTable({
    LApayroll %>%
      select(Year, Department_Title, Job_Class_Title, Total.Payments,
             Base.Pay, Other.Pay, OT.Pay) %>%
      filter(Year == input$Year) %>%
      arrange(desc(Total.Payments)) %>%
      head( n = input$nTopEarners)
  })
     # Q4:
  output$distTablesumStat <- renderTable({
    if(input$sumStat == "mean") {
      LApayroll %>%
        filter(Year == input$YearQ4) %>%
        group_by(Department_Title) %>%
        summarise(
          Total = mean(Total.Payments),
          Base = mean(Base.Pay),
          Other = mean(Other.Pay),
          OT = mean(OT.Pay)) %>%
          arrange(desc(Total)) %>%
          select(Department_Title, Total, Base, Other, OT) %>%
        head(input$nDepts)
    } else {
      LApayroll %>%
        filter(Year == input$YearQ4) %>%
        group_by(Department_Title) %>%
        summarise(
          Total = median(Total.Payments),
          Base = median(Base.Pay),
          Other = median(Other.Pay),
          OT = median(OT.Pay)
        ) %>%
        arrange(desc(Total)) %>%
        select(Department_Title, Total, Base, Other, OT ) %>%
        head(input$nDepts)
      }
  })
    # Q5:
  output$distTableQ5 <- renderTable({
    LApayroll %>%
      filter(Year == input$YearQ5) %>%
      select(Department_Title, Avg.Ben.Cost, Total.Payments,
             Base.Pay, Other.Pay, OT.Pay) %>%
      group_by(Department_Title) %>%
      summarise(TotalCost = sum(Avg.Ben.Cost),
                sumTotalPay = sum(Total.Payments),
                sumBasePay = sum(Base.Pay),
                sumOtherPay = sum(Other.Pay),
                sumOTPay = sum(OT.Pay)) %>%
      arrange(desc(TotalCost)) %>%
      head(input$nDeptsQ5)
  })
    # Q6: Visualize the top OT earning Departments.
  output$distPlotQ6 <- renderPlot({
    LApayroll %>%
      filter(Year == input$YearQ6) %>%
      select(Department_Title, OT.Pay) %>%
      group_by(Department_Title) %>%
      summarise(OT = mean(OT.Pay)) %>%
      arrange(desc(OT)) %>%
      head(input$nDeptsQ6) %>%
      ggplot() +
        geom_col(mapping = aes(x = Department_Title, y = OT)) +
        coord_flip()
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

