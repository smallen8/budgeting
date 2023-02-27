library(shiny)

shinyServer(function(input, output) {

  # summary tab
  output$summaryDf <- renderDT({
    getSummary(df3)
  })
  
  output$summaryPlot <- renderPlot({
    getSummary(df3) %>%
      mutate(Category = as.factor(Category)) %>%
      ggplot() +
      geom_col(aes(reorder(Category, Amount), Amount)) +
      geom_point(aes(Category, `Average Amount`), color='red', size=3) +
      coord_flip() +
      xlab('Category') + ylab('$') + ggtitle("Spending This Month Compared to Average")
  })
  
  
  # cash flow tab
  output$incomeVsSpending <- renderPlot({
    df3 %>%
      filter(date>=input$dateRangeCashFlow[1],
             date<=input$dateRangeCashFlow[2],
             category %in% input$spendingCategoriesCashFlow) %>%
      group_by(year_month, transaction_type) %>%
      summarize(amount = sum(amount)) %>%
      ungroup() %>%
      ggplot() +
      geom_col(aes(year_month, amount, fill=transaction_type), position = 'dodge') +
      scale_fill_manual(values=c('darkgreen','red'), name = 'Transaction Type') +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
      xlab('Month') + ylab('$') + ggtitle('Income and Spending by Month')
  })
  
  output$netIncome <- renderPlot({
    df3 %>%
      filter(date>=input$dateRangeCashFlow[1],
             date<=input$dateRangeCashFlow[2],
             category %in% input$spendingCategoriesCashFlow) %>%
      mutate(amount = ifelse(transaction_type=='credit', amount, -amount)) %>%
      group_by(year_month) %>%
      summarize(net_income = sum(amount)) %>%
      mutate(positive_negative = ifelse(net_income>0, 'credit', 'debit')) %>%
      ggplot() +
      geom_col(aes(year_month, net_income, fill = positive_negative)) +
      scale_fill_manual(values=c('darkgreen','red'), name = 'Positive or Negative') +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
      xlab('Month') + ylab('$') + ggtitle('Income and Spending by Month')
  })
  
  output$incomeVsSpendingDf <- renderDT({
    df3 %>%
      filter(date>=input$dateRangeCashFlow[1],
             date<=input$dateRangeCashFlow[2],
             category %in% input$spendingCategoriesCashFlow) %>%
      group_by(year_month, transaction_type) %>%
      summarize(amount = sum(amount)) %>%
      ungroup() %>%
      dcast(year_month ~ transaction_type, value.var='amount') %>%
      rename(Spent = debit,
             Income = credit,
             Month = year_month) %>%
      mutate(`Net Income` = Income - Spent)
  })

  output$spendingSummary <- renderPlot({
    savings <- df3 %>%
      filter(date>=input$dateRangeSpending[1],
             date<=input$dateRangeSpending[2]) %>%
      mutate(amount = ifelse(transaction_type=='credit', amount, -amount)) %>%
      group_by(year_month) %>%
      summarize(amount = sum(amount)) %>%
      mutate(category = 'Savings') %>%
      mutate(amount = ifelse(amount<0, 0, amount))
    
    temp <- df3 %>%
      filter(date>=input$dateRangeSpending[1],
             date<=input$dateRangeSpending[2],
             category %in% input$spendingCategoriesSpending) %>%
      filter(transaction_type=='debit') %>%
      group_by(year_month, category) %>%
      summarize(amount = sum(amount)) %>%
      ungroup()
    if(input$chartTypeSpending=='fill') {
      temp <- temp %>%
        bind_rows(savings)
    }
    ggplot(temp) +
      geom_col(aes(year_month, amount, fill=category), position=input$chartTypeSpending) +
      scale_fill_brewer(palette="Set3", name = 'Category') +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
      xlab('Month') + 
      ylab(ifelse(input$chartTypeSpending=='fill', '%/100', '$')) + 
      ggtitle('Spending by Month and Category')
  })
  
  output$spendingSummaryDf <- renderDT({
    averageByCategory <- df3 %>% 
      group_by(year_month, category) %>% 
      summarize(amount = sum(amount)) %>%
      bind_rows(left_join(allYearMonths, allCategories) %>%
                  mutate(amount = 0) %>%
                  select(-temp)) %>%
      group_by(year_month, category) %>% 
      summarize(amount = sum(amount)) %>%
      group_by(category) %>%
      summarize(avg_amount = mean(amount)) %>%
      arrange(desc(avg_amount))
    
    df3 %>%
      filter(date>=input$dateRangeSpending[1],
             date<=input$dateRangeSpending[2],
             category %in% input$spendingCategoryDrilldown) %>%
      filter(transaction_type=='debit') %>%
      group_by(year_month, category) %>%
      summarize(amount = sum(amount)) %>%
      ungroup() %>%
      left_join(averageByCategory, by='category') %>%
      mutate(amount = round(amount),
             avg_amount = round(avg_amount),
             amount_above_average = amount - avg_amount) %>%
      select(Month = year_month,
             Category = category,
             Spending = amount,
             `Average Spending` = avg_amount,
             `Amount Above Average` = amount_above_average)
  })
  
  output$spendingSummarySubcategory <- renderPlot({
    df3 %>%
      filter(date>=input$dateRangeSpending[1],
             date<=input$dateRangeSpending[2],
             category==input$spendingCategoryDrilldown) %>%
      filter(transaction_type=='debit') %>%
      group_by(year_month, subcategory) %>%
      summarize(amount = sum(amount)) %>%
      ungroup() %>%
      ggplot() +
      geom_col(aes(year_month, amount, fill=subcategory)) +
      scale_fill_brewer(palette="Set3", name = 'Subcategory') +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
      xlab('Month') + ylab('$') + ggtitle('Spending by Month and Subcategory')
  })
  
  # income tab
  output$incomeSummary <- renderPlot({
    df3 %>%
      filter(date>=input$dateRangeIncome[1],
             date<=input$dateRangeIncome[2]) %>%
      filter(subcategory %in% c('Paycheck','Deposit','Dividend & Cap Gains')) %>%
      mutate(income_description = case_when(toupper(description) %like% '%HIGHMARK%' ~ 'Sarah Income',
                                            toupper(description) %like% '%BAKER%' ~ 'Zack Income',
                                            toupper(description) %like% '%DIVIDEND%' ~ 'Dividend & Cap Gains',
                                            account_name=='Michael Baker International 401(k) Plan' ~ 'Zack 401K',
                                            account_name=='401K Account' ~ 'Sarah 401K',
                                            TRUE ~ 'Other')) %>%
      group_by(year_month, income_description) %>%
      summarize(amount = sum(amount)) %>%
      ungroup() %>%
      ggplot() +
      geom_col(aes(year_month, amount, fill=income_description)) +
      scale_fill_brewer(palette="Set3", name = 'Category') +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
      xlab('Month') + ylab('$') + ggtitle('Income by Month and Category')
  })
  
  output$incomeYTDSummaryDf <- renderDT({
    df3 %>%
      filter(date>=floor_date(Sys.Date(), unit='year')) %>%
      filter(subcategory %in% c('Paycheck','Deposit','Dividend & Cap Gains')) %>%
      mutate(income_description = case_when(toupper(description) %like% '%HIGHMARK%' ~ 'Sarah Income',
                                            toupper(description) %like% '%BAKER%' ~ 'Zack Income',
                                            toupper(description) %like% '%DIVIDEND%' ~ 'Dividend & Cap Gains',
                                            account_name=='Michael Baker International 401(k) Plan' ~ 'Zack 401K',
                                            account_name=='401K Account' ~ 'Sarah 401K',
                                            TRUE ~ 'Other')) %>%
      group_by(income_description) %>%
      summarize(amount = sum(amount)) %>%
      ungroup() %>%
      arrange(-amount) %>%
      select(`Income Description` = income_description,
             `YTD Amount` = amount)
  })
  
  
})
