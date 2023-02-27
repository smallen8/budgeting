library(shiny)
library(DT)
# library(plotly)
library(reshape2)
library(dplyr)

spendingCategoriesSelected <- c('Auto & Transport','Bills & Utilities','Entertainment','Food & Dining','Gifts & Donations','Health & Fitness','Home','Income','Shopping','Travel')
startDate <- min(df3$date)
endDate <- max(df3$date)

# summaryTab <- fluidPage(
# 
# )

cashFlowTab <- fluidPage(
  fluidRow(column(width=6,
                  dateRangeInput(inputId = 'dateRangeCashFlow',
                                 label = 'Date Range',
                                 start = startDate,
                                 end = endDate,
                                 min = startDate,
                                 max = endDate)
                  ),
           column(width=6,
                  selectInput(inputId = 'spendingCategoriesCashFlow', multiple = TRUE, 
                              label = 'Spending Categories', 
                              choices = sort(unique(df3$category)),
                              selected = unique(df3$category))
                  )),
  plotOutput('incomeVsSpending'),
  plotOutput('netIncome'),
  DTOutput('incomeVsSpendingDf')
)

spendingTab <- fluidPage(
  fluidRow(column(6,
                  dateRangeInput(inputId = 'dateRangeSpending',
                                 label = 'Date Range',
                                 start = startDate,
                                 end = endDate,
                                 min = startDate,
                                 max = endDate)),
           column(6,
                  selectInput(inputId = 'spendingCategoriesSpending', multiple = TRUE,
                              label = 'Spending Categories',
                              choices = sort(unique(df3$category)),
                              selected = spendingCategoriesSelected)
  )),

  fluidRow(column(3,
                  radioButtons(inputId = 'chartTypeSpending',
                              label = 'Chart Type',
                              choices = c('stack','fill'),
                              selected = 'stack')),
           column(9, plotOutput('spendingSummary'))),
  
  fluidRow(column(3,
                  selectInput(inputId = 'spendingCategoryDrilldown', multiple = FALSE, 
                              label = 'Spending Category',
                              choices = sort(unique(df3$category)),
                              selected = 'Food & Dining',
                              selectize = FALSE)),
           column(9, plotOutput('spendingSummarySubcategory'))),
  
  fluidRow(column(9, offset=3,
                  DTOutput('spendingSummaryDf')))
)

incomeTab <- fluidPage(
  dateRangeInput(inputId = 'dateRangeIncome',
                 label = 'Date Range',
                 start = startDate,
                 end = endDate,
                 min = startDate,
                 max = endDate),
  
  plotOutput('incomeSummary'),
  DTOutput('incomeYTDSummaryDf')
)

shinyUI(fluidPage(

    # Application title
    titlePanel("Team Zack and Sarah Budget"),

    tabsetPanel(
      # tabPanel('Summary', summaryTab),
      tabPanel('Cash Flow', cashFlowTab),
      tabPanel('Spending', spendingTab),
      tabPanel('Income', incomeTab)
    )
))
