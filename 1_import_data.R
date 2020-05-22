t = read.csv("~/Downloads/transactions-2.csv", stringsAsFactors=FALSE)
colnames(t) = gsub('\\.', '_', tolower(colnames(t)))
colnames(t)
head(t)
t$subcategory = t$category
t$date = mdy(t$date)
t$year = year(t$date)
t$month = month(t$date)
t$labels = NULL
t$notes = NULL
head(t)

t$category = t$subcategory
t$category = ifelse(t$subcategory %in% c('Auto Insurance','Auto Payment','Gas & Fuel','Parking','Public Transportation','Service & Parts'),
                    'Auto & Transport', t$category)
t$category = ifelse(t$subcategory %in% c('Home Phone','Internet','Mobile Phone','Television','Utilities'),
                    'Bills & Utilities', t$category)
t$category = ifelse(t$subcategory %in% c('Advertising','Legal','Office Supplies','Printing','Shipping'),
                    'Business Services', t$category)
t$category = ifelse(t$subcategory %in% c('Books & Supplies','Student Loan','Tuition'),
                    'Education', t$category)
t$category = ifelse(t$subcategory %in% c('Amusement','Arts','Movies & DVDs','Music','Newspapers & Magazines'),
                    'Entertainment', t$category)
t$category = ifelse(t$subcategory %in% c('ATM Fee','Bank Fee','Finance Charge','Late Fee','Service Fee','Trade Commissions'),
                    'Fees & Charges', t$category)
t$category = ifelse(t$subcategory %in% c('Financial Advisor','Life Insurance'),
                    'Financial', t$category)
t$category = ifelse(t$subcategory %in% c('Alcohol & Bars','Coffee Shops','Fast Food','Groceries','Restaurants'),
                    'Food & Dining', t$category)
t$category = ifelse(t$subcategory %in% c('Charity','Gift'),
                    'Gifts & Donations', t$category)
t$category = ifelse(t$subcategory %in% c('Dentist','Doctor','Eyecare','Gym','Health Insurance','Pharmacy','Sports'),
                    'Health & Fitness', t$category)
t$category = ifelse(t$subcategory %in% c('Furnishings','Home Improvement','Home Insurance','Home Services','Home Supplies','Lawn & Garden','Mortgage & Rent'),
                    'Home', t$category)
t$category = ifelse(t$subcategory %in% c('Bonus','Interest Income','Paycheck','Reimbursement','Rental Income','Returned Purchase'),
                    'Income', t$category)
t$category = ifelse(t$subcategory %in% c('Buy','Deposit','Divident & Cap Gains','"Sell','Withdrawal'),
                    'Investments', t$category)
t$category = ifelse(t$subcategory %in% c('Allowance','Baby Supplies','Babysitter & Dayare','Child Support','Kids Activities','Toys'),
                    'Kids', t$category)
t$category = ifelse(t$subcategory %in% c('Loan Fees and Charges','Loan Insurance','Loan Interest','Loan Payment','Loan Principal'),
                    'Loans', t$category)
t$category = ifelse(t$subcategory %in% c('Hair','Laundry','Spa & Massage'),
                    'Personal Care', t$category)
t$category = ifelse(t$subcategory %in% c('Pet Food & Supplies','Pet Grooming','Veterinary'),
                    'Pets', t$category)
t$category = ifelse(t$subcategory %in% c('Books','Clothing','Electronics & Software','Hobbies','Sporting Goods'),
                    'Shopping', t$category)
t$category = ifelse(t$subcategory %in% c('Federal Tax','Local Tax','Property Tax','Sales Tax','State Tax'),
                    'Taxes', t$category)
t$category = ifelse(t$subcategory %in% c('Credit Card Payment','Transfer for Cash Spending'),
                    'Transfer', t$category)
t$category = ifelse(t$subcategory %in% c('Air Travel','Hotel','Rental Car & Taxi','Vacation'),
                    'Travel', t$category)
t$category = ifelse(t$subcategory %in% c('Cash & ATM'),
                    'Uncategorized', t$category)
head(t)

describe(t)
t$category %>% unique()
t$subcategory %>% unique()

t = t %>%
  filter(!subcategory %in% c('Credit Card Payment','Bank Fee','Hide from Budgets & Trends'),
         description!='Venmo') %>%
  mutate(pos_neg = ifelse(subcategory %in% c('Investments','Paycheck','Income','Transfer','Bonus'), 
                          'income', 'expense'))

