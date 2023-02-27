library(lubridate)
library(dplyr)
library(ggplot2)
library(DescTools)
theme_set(theme_minimal())

cleanData <- function(filename) {
  # filename <- 'C:/Users/zacka/OneDrive/Documents/Budgeting/transactions.csv'
  
  # read in data
  df = read.csv(filename, stringsAsFactors = F)
  colnames(df) = tolower(colnames(df))
  colnames(df) = gsub('\\.', '_', colnames(df))
  
  # clean up data
  df2 <- df %>%
    mutate(date = lubridate::mdy(date),
           year = lubridate::year(date),
           month = lubridate::month(date),
           year_month = paste0(year, stringr::str_pad(month, width=2, pad='0'))) %>%
    filter(year_month>='202201') %>%
    filter(!category %in% c('Hide from Budgets & Trends','Credit Card Payment','Transfer'))
  # filter(!category %in% c('Investments','Credit Card Payment','Transfer','Dividend & Cap Gains','Trade Commissions','Interest Income'))
  
  df3 <- df2 %>%
    rename(subcategory = category) %>%
    mutate(category = case_when(subcategory %in% c('Auto & Transport','Auto Insurance','Auto Payment','Gas & Fuel','Parking','Public Transportation','Ride Share','Service & Parts') ~ 'Auto & Transport',
                                subcategory %in% c('Bills & Utilities','Home Phone','Internet','Mobile Phone','Television','Utilities','Electric','Gas','Water') ~ 'Bills & Utilities',
                                subcategory %in% c('Business Services','Advertising','Legal','Office Supplies','Printing','Shipping') ~ 'Business Services',
                                subcategory %in% c('Education','Books & Supplies','Student Loan','Tuition') ~ 'Education',
                                subcategory %in% c('Entertainment','Amusement','Arts','Movies & DVDs','Music','Newspapers & Magazines') ~ 'Entertainment',
                                subcategory %in% c('Fees & Charges','ATM Fee','Bank Fee','Finance Charge','Late Fee','Service Fee','Trade Commissions') ~ 'Fees & Charges',
                                subcategory %in% c('Financial','Financial Advisor','Life Insurance') ~ 'Financial',
                                subcategory %in% c('Food & Dining','Alcohol & Bars','Coffee Shops','Fast Food','Food Delivery','Groceries','Restaurants') ~ 'Food & Dining',
                                subcategory %in% c('Gifts & Donations','Charity','Gift','Eight Days of Hope','Embraced International','Identity Mission','Mission St. Louis','Orchard Hill Church','Reliant','Service Over Self','The Journey Church') ~ 'Gifts & Donations',
                                subcategory %in% c('Health & Fitness','Dentist','Doctor','Eyecare','Gym','Health Insurance','Pharmacy','Sports') ~ 'Health & Fitness',
                                subcategory %in% c('Home','Furnishings','Home Improvement','Home Insurance','Home Services','Home Supplies','Lawn & Garden','Mortgage & Rent') ~ 'Home',
                                subcategory %in% c('Income','Bonus','Interest Income','Paycheck','Reimbursement','Rental Income','Returned Purchase') ~ 'Income',
                                subcategory %in% c('Investments','Buy','Deposit','Dividend & Cap Gains','Sell','Withdrawal') ~ 'Investments',
                                subcategory %in% c('Kids','Allowance','Baby Supplies','Babysitter & Daycare','Child Support','Kids Activities','Toys') ~ 'Kids',
                                subcategory %in% c('Loans','Loan Fees and Charges','Loan Insurance','Loan Interest','Loan Payment','Loan Principal') ~ 'Loans',
                                subcategory %in% c('Personal Care','Hair','Laundry','Spa & Massage') ~ 'Personal Care',
                                subcategory %in% c('Pets','Pet Food & Supplies','Pet Grooming','Veterinary') ~ 'Pets',
                                subcategory %in% c('Shopping','Books','Clothing','Electronics & Software','Hobbies','Sporting Goods') ~ 'Shopping',
                                subcategory %in% c('Taxes','Federal Tax','Local Tax','Property Tax','Sales Tax','State Tax') ~ 'Taxes',
                                subcategory %in% c('Transfer','Credit Card Payment','Transfer for Cash Spending') ~ 'Transfer',
                                subcategory %in% c('Travel','Air Travel','Hotel','Rental Car & Taxi','Vacation') ~ 'Travel',
                                subcategory %in% c('Uncategorized','Cash & ATM','Check') ~ 'Uncategorized',
                                TRUE ~ subcategory)) %>%
    filter(!subcategory %in% c('Buy','Sell'))
  
  allYearMonths <- df3 %>% distinct(year_month) %>% mutate(temp = 1)
  allCategories <- df3 %>% distinct(category) %>% mutate(temp = 1)
  allIncomeSubcategories <- df3 %>% filter(transaction_type=='credit') %>% distinct(subcategory) %>% mutate(temp = 1)
  
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

  # export cleaned data
  write.csv(df3, 'C:/Users/zacka/OneDrive/Documents/Budgeting/transactions_clean.csv', row.names = F)
  write.csv(averageByCategory, 'C:/Users/zacka/OneDrive/Documents/Budgeting/transactions_average.csv', row.names = F)

  return(df3)  
}

filename <- 'C:/Users/zacka/OneDrive/Documents/Budgeting/transactions.csv'
df3 <- cleanData(filename)
# allow user to import new transactions csv into app, clean up within the app, and immediately summarize


getSummary <- function(df3) {
  
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
  
  summary <- df3 %>%
    filter(date>=floor_date(Sys.Date(), unit='month')) %>% # filter on this month
    group_by(year_month, category) %>%
    summarize(amount = sum(amount)) %>%
    ungroup() %>%
    left_join(averageByCategory, by='category') %>%
    mutate(amount = round(amount),
           avg_amount = round(avg_amount),
           amount_above_average = amount - avg_amount) %>%
    select(Month = year_month,
           Category = category,
           Amount = amount,
           `Average Amount` = avg_amount,
           `Amount Above Average` = amount_above_average)
  
  return(summary)
}


# df3 %>%
#   group_by(year_month, category) %>% 
#   summarize(amount = sum(amount)) %>%
#   filter(category %in% c('Auto & Transport','Bills & Utilities','Entertainment','Food & Dining','Gifts & Donations','Health & Fitness','Home','Income','Shopping','Travel')) %>%
#   ggplot() + 
#   geom_col(aes(year_month, amount)) +
#   facet_wrap(~category, scales='free')
# 
# 
# df3 %>%
#   filter(year_month=='202301') %>%
#   arrange(date) %>%
#   View()
# df2 %>%
#   filter(year_month=='202301') %>%
#   arrange(date) %>%
#   View()
# 
# df2 %>%
#   filter(category=='Dividend & Cap Gains') %>%
#   View()
# # filter(!category %in% c('Investments','Credit Card Payment','Transfer','Dividend & Cap Gains','Trade Commissions','Interest Income'))
# 
# 
# 
# # df3 %>%
# #   filter(subcategory %in% c('Paycheck','Deposit','Dividend & Cap Gains')) %>%
# #   mutate(income_description = case_when(toupper(description) %like% '%HIGHMARK%' ~ 'Sarah Income',
# #                                         toupper(description) %like% '%BAKER%' ~ 'Zack Income',
# #                                         account_name=='Michael Baker International 401(k) Plan' ~ 'Zack 401K',
# #                                         account_name=='401K Account' ~ 'Sarah 401K',
# #                                         TRUE ~ 'Other')) %>%
# #   filter(year_month=='202202',
# #          income_description=='Other') %>%
# #   View()
# 
# 
# # summarize spending
# 
# 
# 
# 
# 
# 
# savings <- df3 %>%
#   mutate(amount = ifelse(transaction_type=='credit', amount, -amount)) %>%
#   group_by(year_month) %>%
#   summarize(amount = sum(amount)) %>%
#   mutate(category = 'Savings')
# head(savings)
# 
# df3 %>%
#   # filter(date>=input$dateRange[1],
#   #        date<=input$dateRange[2]) %>%
#   filter(transaction_type=='debit') %>%
#   # filter(category %in% input$spendingCategories) %>%
#   filter(category %in% c('Auto & Transport','Bills & Utilities','Entertainment','Food & Dining','Gifts & Donations','Health & Fitness','Home','Income','Shopping','Travel')) %>%
#   group_by(year_month, category) %>%
#   summarize(amount = sum(amount)) %>%
#   ungroup() %>%
#   bind_rows(savings) %>%
#   ggplot() +
#   geom_col(aes(year_month, amount, fill=category), position='fill') +
#   scale_fill_brewer(palette="Set3", name = 'Category') +
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
#   xlab('Month') + ylab('$') + ggtitle('Spending by Month and Category')
