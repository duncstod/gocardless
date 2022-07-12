
# Load libs ---------------------------------------------------------------


library(tidyverse)
library(lubridate)
library(parsnip)
library(tidymodels)
setwd("/Users/duncanstoddard/projects/GoCardless/gocardless_eda")



# Load data ---------------------------------------------------------------


accounts <- read_csv("data/accounts.csv")
opportunities <- read_csv("data/opportunities.csv")


# Join --------------------------------------------------------------------


joined_df <- accounts %>% 
  inner_join(
    opportunities %>% 
      select(account_id, opportunity_type, source_bucket, opportunity_stage_name,
             country, commission_model, opportunity_created_date),
    by = c("id" = "account_id")
  )


# Filter ------------------------------------------------------------------


# source bucket
joined_df %>% 
  count(source_bucket) %>% 
  mutate(source_bucket = ifelse(is.na(source_bucket), 'NA', source_bucket),
         source_bucket = reorder(source_bucket, n)) %>% 
  ggplot(aes(x = source_bucket, y = n, fill = source_bucket)) + 
  geom_col() + 
  coord_flip() + 
  theme_minimal() +
  labs(
    title = 'Source bucket',
    y = 'Number of accounts',
    x = 'Source bucket',
  ) +
  theme(legend.position = 'none') +
  scale_fill_brewer(palette="Set1")

# Filter
joined_df <- joined_df %>% 
  filter(!is.na(source_bucket))

# opportunity_type
joined_df %>% 
  count(opportunity_type) %>% 
  mutate(opportunity_type = reorder(opportunity_type, n)) %>% 
  ggplot(aes(x = opportunity_type, y = n, fill = opportunity_type)) + 
  geom_col() + 
  coord_flip() + 
  theme_minimal() +
  labs(
    title = 'Opportunity type',
    y = 'Number of accounts',
    x = 'Opportunity type',
  ) +
  theme(legend.position = 'none') +
  scale_fill_brewer(palette="Set1")

# Filter
joined_df <- joined_df %>% 
  filter(opportunity_type == 'New')


# opportunity_stage_name
joined_df %>% 
  count(opportunity_stage_name) %>% 
  arrange(desc(n))

joined_df %>% 
  count(opportunity_stage_name) %>% 
  mutate(opportunity_stage_name = reorder(opportunity_stage_name, n),
         tgt = ifelse(opportunity_stage_name == '7. Closed Won', 'y', 'n')) %>% 
  ggplot(aes(x = opportunity_stage_name, y = n, fill = tgt)) + 
  geom_col() + 
  coord_flip() + 
  theme_minimal() +
  theme_minimal() +
  labs(
    title = 'Opportunity stage name',
    y = 'Number of accounts',
    x = 'Opportunity stage name',
    fill = 'Target true'
  ) +
  scale_fill_brewer(palette="Set1")

# Create target
joined_df <- joined_df %>% 
  mutate(
    closed_won = ifelse(opportunity_stage_name == '7. Closed Won', 1, 0)
  )


# commission_model
joined_df %>% 
  count(commission_model) %>% 
  mutate(
    commission_model = ifelse(is.na(commission_model), 'NA', commission_model),
    commission_model = reorder(commission_model, n)
    ) %>%
  ggplot(aes(x = commission_model, y = n, fill = commission_model)) + 
  geom_col() + 
  coord_flip() + 
  theme_minimal() +
  labs(
    title = 'Commission model',
    y = 'Number of accounts',
    x = 'Commission model',
  ) +
  theme(legend.position = 'none') +
  scale_fill_brewer(palette="Set1")

# revenue_contract_type
joined_df %>% 
  count(revenue_contract_type) %>% 
  mutate(
    revenue_contract_type = ifelse(is.na(revenue_contract_type), 'NA', revenue_contract_type),
    revenue_contract_type = reorder(revenue_contract_type, n)
  ) %>%
  ggplot(aes(x = revenue_contract_type, y = n, fill = revenue_contract_type)) + 
  geom_col() + 
  coord_flip() + 
  theme_minimal() +
  labs(
    title = 'Revenue contract type',
    y = 'Number of accounts',
    x = 'Revenue contract type',
  ) +
  theme(legend.position = 'none') +
  scale_fill_brewer(palette="Set1")

# Both
plot(table(joined_df$commission_model, joined_df$revenue_contract_type))

# Filter
joined_df <- joined_df %>% 
  filter(commission_model == 'CRA')


# Look at target ----------------------------------------------------------


# Target variable
joined_df <- joined_df %>% 
  mutate(
    opportunity_created_date = as.Date(gsub(" UTC", "", opportunity_created_date)),
    opportunity_created_date_month = floor_date(opportunity_created_date, 'month')
  )

# Plot
joined_df %>% 
  count(opportunity_created_date_month) %>% 
  ggplot(aes(x = opportunity_created_date_month, y = n)) + 
  geom_line() +
  theme_minimal() +
  labs(
    title = 'Number of opportunities by month',
    x = 'Month',
    y = 'Number of opportunities'
  )

joined_df %>% 
  group_by(opportunity_created_date_month) %>% 
  summarise(
    n = n(),
    conversion_rate = mean(closed_won)
  ) %>% 
  ggplot(aes(x = opportunity_created_date_month, y = conversion_rate)) + 
  geom_line() +
  theme_minimal() +
  labs(
    title = 'Conversion rate by month',
    x = 'Month',
    y = 'Conversion rate'
  )



# Explore categoricals ----------------------------------------------------


# Vertical
joined_df %>% 
  count(vertical) %>% 
  mutate(prop = n / sum(n))

joined_df %>% 
  count(vertical) %>% 
  mutate(vertical = reorder(vertical, n)) %>% 
  ggplot(aes(x = vertical, y = n)) + 
  geom_col() + 
  coord_flip() + 
  theme_minimal() +
  labs(
    title = 'Vertical',
    x = 'Number of accounts',
    y = 'Vertical'
  )


# Conversion rate
joined_df %>% 
  group_by(vertical) %>% 
  summarise(
    n = n(),
    conversion_rate = mean(closed_won)
  )

# Relabel missing values
joined_df <- joined_df %>% 
  mutate(
    vertical = ifelse(is.na(vertical), 'missing', vertical)
  )

# Test
table(joined_df$vertical, joined_df$closed_won)
chisq.test(joined_df$vertical, joined_df$closed_won)


# hq_country
joined_df %>% 
  mutate(
    hq_country = ifelse(is.na(hq_country), 'NA', hq_country),
    hq_country = fct_lump(hq_country, n = 15)
  ) %>% 
  count(hq_country) %>% 
  # filter(n > 50) %>%
  mutate(
    hq_country = reorder(hq_country, desc(n))
    ) %>% 
  ggplot(aes(x = hq_country, y = n)) + 
  geom_col() + 
  # coord_flip() + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90)) + 
  labs(
    title = 'HQ Country',
    x = 'Number of accounts',
    y = 'HQ Country'
  )

# Relabel missing values
joined_df <- joined_df %>% 
  mutate(
    hq_country = ifelse(is.na(hq_country), 'missing', hq_country)
  )

# Revenue
joined_df %>% 
  count(annual_revenue_db) %>% 
  arrange(desc(n)) %>% 
  head()

# Proportion NA
sum(is.na(joined_df$annual_revenue_db)) / length(joined_df$annual_revenue_db)

# Proportion below 1m
sum(joined_df$annual_revenue_db[!is.na(joined_df$annual_revenue_db)] < 1e6) / length(joined_df$annual_revenue_db)


# Histogram
joined_df %>% 
  filter(annual_revenue_db < 5e8) %>% 
  ggplot(aes(x = annual_revenue_db)) +
  geom_histogram(bins=100) +
  labs(
    title = 'Distribution of annual_revenue_db',
    x = 'annual_revenue_db',
    y = 'Freq'
  ) +
  theme_minimal()


# Focus below 5e7
joined_df %>% 
  filter(annual_revenue_db < 5e7) %>% 
  ggplot(aes(x = annual_revenue_db)) +
  geom_histogram()


# Max
max(joined_df$annual_revenue_db[!is.na(joined_df$annual_revenue_db)])


# Top three
joined_df %>%
  mutate(
    ping = ifelse(annual_revenue_db %in% c(5000000, 25000000, 500000, 75000000), 1, 0)
  ) %>% 
  filter(annual_revenue_db < 8e7) %>% 
  ggplot(aes(x = annual_revenue_db, fill = as.factor(ping))) +
  geom_histogram(bins = 200) +
  labs(
    title = 'Distribution of annual_revenue_db',
    subtitle = 'Highlighting most common 4 values',
    x = 'annual_revenue_db',
    y = 'Freq',
    fill = ''
  ) +
  theme_minimal() +
  theme(legend.position = 'none') +
  scale_fill_brewer(palette="Set1")


# Proportion
joined_df %>%
  mutate(
    ping = ifelse(annual_revenue_db %in% c(5000000, 25000000, 500000, 75000000), 1, 0)
  ) %>% 
  group_by(ping) %>% 
  summarise(
    n = n()
  ) %>% 
  mutate(n = n / sum(n))



# gc_potential_usd
joined_df %>%
  filter(gc_potential_usd < 1e6) %>% 
  ggplot(aes(x = gc_potential_usd)) +
  geom_histogram(bins = 100) +
  labs(
    title = 'Distribution of gc_potential_usd',
    x = 'gc_potential_usd',
    y = 'Freq',
    fill = ''
  ) +
  theme_minimal() +
  theme(legend.position = 'none')


# share_of_wallet_percentage
table(joined_df$share_of_wallet_percentage)


# total_mqa_transitions
joined_df %>%
  ggplot(aes(x = total_mqa_transitions)) +
  geom_histogram(bins = 50) +
  labs(
    title = 'Distribution of total_mqa_transitions',
    x = 'total_mqa_transitions',
    y = 'Freq',
    fill = ''
  ) +
  theme_minimal() +
  theme(legend.position = 'none')


# value, current_no_of_monthly_transactions, gc_potential_usd, dnb_growth_engine, growth_engine, annual_revenue_db

# Proportion NA
joined_df %>% 
  summarise(
    value = sum(is.na(value)) / n(),
    current_no_of_monthly_transactions = sum(is.na(current_no_of_monthly_transactions)) / n(),
    gc_potential_usd = sum(is.na(gc_potential_usd)) / n(),
    annual_revenue_db = sum(is.na(annual_revenue_db)) / n()
  )


# current_no_of_monthly_transactions
sum(is.na(joined_df$current_no_of_monthly_transactions)) / length(joined_df$current_no_of_monthly_transactions)

joined_df %>%
  filter(current_no_of_monthly_transactions < 1e6) %>% 
  ggplot(aes(x = current_no_of_monthly_transactions)) +
  geom_histogram(bins = 50) +
  labs(
    title = 'Distribution of current_no_of_monthly_transactions',
    x = 'current_no_of_monthly_transactions',
    y = 'Freq',
    fill = ''
  ) +
  theme_minimal() +
  theme(legend.position = 'none')


# Partner
joined_df %>% 
  count(is_partner) %>% 
  mutate(
    prop = n / sum(n)
  )


# is_partner_marketing_engaged
joined_df %>% 
  count(is_partner_marketing_engaged) %>% 
  mutate(
    prop = n / sum(n)
  )

# partner_score
joined_df %>% 
  count(partner_score) %>% 
  mutate(
    prop = n / sum(n)
  )

# partner_type
joined_df %>% 
  count(partner_type) %>% 
  mutate(
    prop = n / sum(n)
  )

# Partner
joined_df %>% 
  select(contains('partner')) %>% 
  colnames(.)


# partner_type
joined_df %>% 
  count(partner_status) %>% 
  mutate(
    prop = n / sum(n)
  )


# Open banking
joined_df %>% 
  select(contains('open_banking')) %>% 
  colnames(.)

# has_open_banking_use_case
joined_df %>% 
  count(has_open_banking_use_case) %>% 
  mutate(
    prop = n / sum(n)
  )

# open_banking_segment
joined_df %>% 
  count(open_banking_segment) %>% 
  mutate(
    prop = n / sum(n)
  )

# open_banking_segment
joined_df %>% 
  count(open_banking_addressable) %>% 
  mutate(
    prop = n / sum(n)
  )


# segment
joined_df %>% 
  count(segment) %>% 
  mutate(
    prop = n / sum(n)
  )


joined_df %>% 
  count(segment) %>% 
  ggplot(aes(x = segment, y = n)) + 
  geom_col() +
  labs(
    title = 'Segment',
    x = 'Segment',
    y = 'N',
    fill = ''
  ) +
  theme_minimal() +
  theme(legend.position = 'none',
        axis.text.x = element_text(angle=90))


# growth_engine
joined_df %>% 
  count(growth_engine) %>% 
  mutate(
    prop = n / sum(n)
  )

joined_df %>% 
  count(growth_engine) %>% 
  ggplot(aes(x = growth_engine, y = n)) + 
  geom_col() +
  labs(
    title = 'Growth engine',
    x = 'Growth engine',
    y = 'N',
    fill = ''
  ) +
  theme_minimal() +
  theme(legend.position = 'none',
        axis.text.x = element_text(angle=90))


# decision_maker
joined_df %>% 
  count(decision_maker) %>% 
  mutate(
    prop = n / sum(n)
  )


# champion
joined_df %>% 
  count(champion) %>% 
  mutate(
    prop = n / sum(n)
  )


# audience
joined_df %>% 
  count(audience) %>% 
  mutate(
    prop = n / sum(n)
  )


# Failure rates
sum(is.na(joined_df$failure_rate_3m)) / length(joined_df$failure_rate_3m)
sum(is.na(joined_df$failure_rate_6m)) / length(joined_df$failure_rate_6m)


# account_stage
joined_df %>% 
  count(account_stage) %>% 
  mutate(
    prop = n / sum(n)
  )



# Model -------------------------------------------------------------------


# Model
joined_df_complete <- joined_df %>% 
  select(
    id,
    closed_won,
    hq_country,
    vertical,
    annual_revenue_db, 
    growth_engine
  ) %>% 
  filter(complete.cases(.)) %>% 
  distinct()


# Combine categories
joined_df_complete <- joined_df_complete %>% 
  mutate(
    hq_country = fct_lump(hq_country, n = 8),
    vertical = fct_lump_min(vertical, 50)
  )

# Check groups
joined_df_complete %>% 
  count(hq_country)

joined_df_complete %>% 
  count(vertical)


# Heatmaps 
joined_df_complete %>% 
  group_by(hq_country, vertical) %>% 
  summarise(
    n = n(),
    rate = mean(closed_won)
  ) %>% 
  mutate(rate = ifelse(n < 10, NA, rate)) %>% 
  ggplot(aes(vertical, hq_country, fill = rate)) + 
  geom_tile() +
  geom_text(aes(label=n), size = 4, color = 'white') +
  theme(
    title = element_text(size = 16),
    axis.text.x = element_text(angle = 90),
    axis.text = element_text(size = 10)
    ) +
  labs(
    title = 'Conversion rate by Country by Vertical',
    subtitle = 'Number of data points in box',
    x = 'Vertical',
    y = 'Country',
    fill = 'Conversion rate'
  )


# Bin revenue
joined_df_complete <- joined_df_complete %>% 
  mutate(
    annual_revenue_db_cuts = cut(annual_revenue_db, 
                                 breaks = c(0, 1e6, 5e6, 1e7, 5e7, 1e8, 5e8, Inf),
                                 labels = c('0-1m', '1m-5m', '5m-10m', '10m-50m', '50m-100m', '100m-500m', '500m+'),
                                 include.lowest = TRUE)
  ) %>% 
  select(-annual_revenue_db)


# Convert target to fct
joined_df_complete <- joined_df_complete %>% 
  mutate(closed_won = as.character(closed_won))


# Split
set.seed(1353)
df_split <- initial_split(joined_df_complete, strata = closed_won)
train_data <- training(df_split)
test_data <- testing(df_split)
train_id <- train_data$id
test_id <- test_data$id


# Recipe
m1_rec <- 
  recipe(closed_won ~ ., data = train_data %>% select(-id)) %>%
  step_dummy(all_nominal_predictors()) %>%
  prep(training = train_data, retain = TRUE)


# The processed versions are:
train_data <- juice(m1_rec)
test_data  <- bake(m1_rec, test_data)
train_data$id <- train_id
test_data$id <- test_id

# Model
lr_mod <- 
  logistic_reg() %>% 
  set_engine("glm")

rf_mod <- 
  rand_forest(mode = "classification", trees = 2000) %>% 
  set_engine("ranger")


# Fit
fit_lr <- lr_mod %>% 
  fit(closed_won ~ ., data = train_data %>% select(-id))

fit_rf <- rf_mod %>% 
  fit(closed_won ~ ., data = train_data %>% select(-id))


# Coefs
tidy(fit_lr)


# Predictions
m1_prob_preds_lr <- predict(fit_lr, new_data = test_data, type = "prob")
m1_prob_preds_rf <- predict(fit_rf, new_data = test_data, type = "prob")

m1_prob_bin_lr <- as.factor(ifelse(m1_prob_preds_lr$.pred_1 > 0.40, 1, 0))
m1_prob_bin_rf <- as.factor(ifelse(m1_prob_preds_rf$.pred_1 > 0.40, 1, 0))

# Hist of preds
hist(m1_prob_preds_lr$.pred_1)
hist(m1_prob_preds_rf$.pred_1)

# Append to test
test_data$m1_prob_preds_lr <- m1_prob_preds_lr$.pred_1
test_data$m1_prob_bin_lr <- m1_prob_bin_lr

test_data$m1_prob_preds_rf <- m1_prob_preds_rf$.pred_1
test_data$m1_prob_bin_rf <- m1_prob_bin_rf


# Precision
precision(test_data, closed_won, m1_prob_bin_lr, event_level = 'second')
precision(test_data, closed_won, m1_prob_bin_rf, event_level = 'second')

# Recall
recall(test_data, closed_won, m1_prob_bin_lr, event_level = 'second')
recall(test_data, closed_won, m1_prob_bin_rf, event_level = 'second')

# Conf
test_data %>% conf_mat(closed_won, m1_prob_bin_lr)
test_data %>% conf_mat(closed_won, m1_prob_bin_rf)

# Ranking
test_data %>% 
  select(
    id,
    closed_won,
    m1_prob_preds_lr
  ) %>%
  arrange(desc(m1_prob_preds_lr)) %>% 
  rownames_to_column() %>% 
  mutate(
    rowname = as.numeric(rowname),
    group = round(rowname, -2),
    closed_won = as.numeric(closed_won) - 1
  ) %>% 
  group_by(group) %>% 
  summarise(
    mean_prec = mean(m1_prob_preds_lr),
    mean_actual = mean(closed_won)
  ) %>% 
  ggplot(aes(x = group, y = mean_actual)) +
  geom_col() +
  geom_text(aes(label=scales::percent(round(mean_prec, 1))), size = 3.5, position=position_dodge(width=0.9), hjust=1.1, angle = 90, color='white') +
  labs(
    title = 'Conversion rate by opportunity group',
    subtitle = 'Buckets of 100 opportunities, ranked by predicted conversion rate',
    y = 'Actual conversion rate',
    x = 'Opportunity bucket'
  ) +
  theme_minimal() +
  scale_y_continuous(labels = scales::percent)


# Uplift
test_data %>% 
  select(
    id,
    closed_won,
    m1_prob_preds_lr
  ) %>%
  arrange(desc(m1_prob_preds_lr)) %>% 
  rownames_to_column() %>% 
  mutate(
    rowname = as.numeric(rowname),
    group = round(rowname, -2),
    closed_won = as.numeric(closed_won) - 1
  ) %>% 
  group_by(group) %>% 
  summarise(
    mean_prec = mean(m1_prob_preds_lr),
    mean_actual = mean(closed_won)
  ) %>% 
  mutate(
    uplift = mean_actual / mean(as.numeric(test_data$closed_won)-1, na.rm=F)
  )

# RF
test_data %>% 
  select(
    id,
    closed_won,
    m1_prob_preds_rf
  ) %>%
  arrange(desc(m1_prob_preds_rf)) %>% 
  rownames_to_column() %>% 
  mutate(
    rowname = as.numeric(rowname),
    group = round(rowname, -2),
    closed_won = as.numeric(closed_won) - 1
  ) %>% 
  group_by(group) %>% 
  summarise(
    mean_prec = mean(m1_prob_preds_rf),
    mean_actual = mean(closed_won)
  ) %>% 
  ggplot(aes(x = group, y = mean_actual)) +
  geom_col() +
  geom_text(aes(label=scales::percent(round(mean_prec, 1))), size = 3.5, position=position_dodge(width=0.9), hjust=1.1, angle = 90, color='white') +
  labs(
    title = 'Conversion rate by opportunity group',
    subtitle = 'Buckets of 100 opportunities, ranked by predicted conversion rate',
    y = 'Actual conversion rate',
    x = 'Opportunity bucket'
  ) +
  theme_minimal() +
  scale_y_continuous(labels = scales::percent)


# Uplift
test_data %>% 
  select(
    id,
    closed_won,
    m1_prob_preds_rf
  ) %>%
  arrange(desc(m1_prob_preds_rf)) %>% 
  rownames_to_column() %>% 
  mutate(
    rowname = as.numeric(rowname),
    group = round(rowname, -2),
    closed_won = as.numeric(closed_won) - 1
  ) %>% 
  group_by(group) %>% 
  summarise(
    mean_prec = mean(m1_prob_preds_rf),
    mean_actual = mean(closed_won)
  ) %>% 
  mutate(
    uplift = mean_actual / mean(as.numeric(test_data$closed_won)-1, na.rm=F)
  )


# Predict for groups
grid_pred <- expand_grid(
  hq_country = unique(joined_df_complete$hq_country),
  vertical = unique(joined_df_complete$vertical),
  growth_engine = unique(joined_df_complete$growth_engine),
  annual_revenue_db_cuts = unique(joined_df_complete$annual_revenue_db_cuts)
  )

lr_grid_preds <- predict(fit_lr, new_data = bake(m1_rec, grid_pred), type = "prob")
rf_grid_preds <- predict(fit_rf, new_data = bake(m1_rec, grid_pred), type = "prob")

grid_pred$lr_pred <- lr_grid_preds$.pred_1
grid_pred$rf_pred <- rf_grid_preds$.pred_1


# Save data
joined_df %>% 
  write_csv("/Users/duncanstoddard/projects/GoCardless/gocardless_eda/data/model_data.csv")

joined_df %>% 
  summarise(across(everything(), ~ sum(is.na(.x)))) %>% 
  gather(key = 'column', value = 'number_missing') %>% 
  write_csv("/Users/duncanstoddard/projects/GoCardless/gocardless_eda/data/missing_value_counts.csv")

grid_pred %>% 
  write_csv("/Users/duncanstoddard/projects/GoCardless/gocardless_eda/data/predictions.csv")

tidy(fit_lr) %>% 
  write_csv("/Users/duncanstoddard/projects/GoCardless/gocardless_eda/data/lr_coefs.csv")


