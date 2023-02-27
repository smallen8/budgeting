t %>% 
  filter(month==5, year==2019,
         pos_neg=='expense') %>%
  group_by(category) %>%
  summarize(tot_amount=sum(amount)) %>%
  arrange(-tot_amount)
  

t %>% 
  filter(pos_neg=='expense',
         year==2019) %>%
  group_by(year, month, category) %>%
  summarize(tot_amount=sum(amount)) %>%
  arrange(year, month, -tot_amount) %>%
  ggplot() +
  geom_line(aes(as.factor(month), tot_amount)) +
  geom_point(aes(as.factor(month), tot_amount)) +
  facet_wrap(~category, scales='free')





t %>%
  filter(month==5, year==2019,
         pos_neg=='expense') %>%
  group_by(category) %>%
  summarize(tot_amount = sum(amount)) %>%
  arrange(-tot_amount) %>%
  ggplot() +
  geom_col(aes(reorder(category, tot_amount), tot_amount)) +
  coord_flip() +
  xlab('Category') + ylab('Amount')


t %>%
  filter(pos_neg=='expense') %>%
  group_by(year, month, category) %>%
  summarize(tot_amount = sum(amount)) %>%
  mutate(yr_month = paste0(year, '-', month)) %>%
  ggplot() +
  geom_col(aes(yr_month, tot_amount)) +
  facet_wrap(~category, scales='free') +
  coord_flip()

# average amount spent on each category across 2019 months


