adminTab <- fluidPage(
  fileInput(inputId = 'mintDataUpload', label = 'Select Mint file'),
  actionButton(inputId = 'cleanData', label = 'Upload File')
)

summaryTab <- fluidPage(
  plotOutput('summaryPlot'),
  DTOutput('summaryDf')
)

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
                              choices = sort(unique(mintData$category)),
                              selected = unique(mintData$category))
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
                              choices = sort(unique(mintData$category)),
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
                              choices = sort(unique(mintData$category)),
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
      tabPanel('Admin', adminTab),
      tabPanel('Summary', summaryTab),
      tabPanel('Cash Flow', cashFlowTab),
      tabPanel('Spending', spendingTab),
      tabPanel('Income', incomeTab)
    )
))
