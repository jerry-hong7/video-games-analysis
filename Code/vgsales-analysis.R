# name: Jerry Hong, Thomas Joyce, and Hayden Truong

# load necessary packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(estimatr, tidyverse, foreach, doRNG, doSNOW, parallel, boot, targeted, ggplot2, xtable)

# load data
vgsales <- read.csv('Data/vgsales-12-2016.csv')

# rescale user score by 10
vgsales$User_Score <- vgsales$User_Score * 10

# convert Year_of_Release as numeric
vgsales$Year_of_Release <- as.numeric(vgsales$Year_of_Release)

# filter games only up to 2016
vgsales <- vgsales %>% filter(Year_of_Release <= 2016)

# remove outliers
vgsales <- vgsales %>% filter(Global_Sales < 40)

# summary stats selecting numeric cols
summary_vg <- vgsales %>% 
  select_if(is.numeric) %>% 
  summary()
# convert to latex
xtable(summary_vg)

# figure showing the average number of sales by platform
vgsales %>% 
  group_by(Platform) %>% 
  summarise(mean_sales = mean(Global_Sales)) %>% 
  ggplot(aes(x = reorder(Platform, mean_sales), y = mean_sales, fill = Platform)) +
  geom_bar(stat = 'identity') +
  coord_flip() +
  labs(title = 'Average Global Sales by Platform',
       x = 'Platform',
       y = 'Average Global Sales (Millions)') + 
  theme(legend.position = 'none') +
  ggsave('Figures/avg_sales_platform.png')

# bar plot of the average number of sales by region
vgsales %>% 
  gather(key = 'region', value = 'sales', NA_Sales:Other_Sales) %>% 
  group_by(region) %>% 
  summarise(mean_sales = mean(sales)) %>% 
  ggplot(aes(x = reorder(region, mean_sales), y = mean_sales, fill = region)) +
  geom_bar(stat = 'identity') +
  coord_flip() +
  labs(title = 'Average Sales by Region',
       x = 'Region',
       y = 'Average Sales (Millions)') + 
  theme(legend.position = 'none') +
  ggsave('Figures/avg_sales_region.png')

# bar plot of average critic and user score by platform among those that have scores
vgsales %>% 
  filter(!is.na(Critic_Score) & !is.na(User_Score)) %>% 
  group_by(Platform) %>% 
  summarise(mean_critic_score = mean(Critic_Score),
            mean_user_score = mean(User_Score)) %>% 
  gather(key = 'score_type', value = 'score', mean_critic_score:mean_user_score) %>% 
  ggplot(aes(x = reorder(Platform, score), y = score, fill = score_type)) +
  geom_bar(stat = 'identity', position = 'dodge') +
  coord_flip() +
  labs(title = 'Average Critic and User Score by Platform',
       x = 'Platform',
       y = 'Average Score') + 
  theme(legend.position = 'top') +
  ggsave('Figures/avg_scores_platform.png')

# line plot of total global sales by year up to 2016
vgsales %>% 
  group_by(Year_of_Release) %>% 
  summarise(total_sales = sum(Global_Sales)) %>% 
  ggplot(aes(x = Year_of_Release, y = total_sales)) +
  geom_line() +
  labs(title = 'Total Global Sales by Year',
       x = 'Year',
       y = 'Total Global Sales (Millions)') +
  ggsave('Figures/total_sales_year.png')

# scatter plot of global sales by critic score
vgsales %>% 
  ggplot(aes(x = Critic_Score, y = Global_Sales)) +
  geom_point() +
  labs(title = 'Global Sales by Critic Score',
       x = 'Critic Score',
       y = 'Global Sales (Millions)') +
  ggsave('Figures/sales_critic_score.png')

# scatter plot of global sales by user score
vgsales %>% 
  ggplot(aes(x = User_Score, y = Global_Sales)) +
  geom_point() +
  labs(title = 'Global Sales by User Score',
       x = 'User Score',
       y = 'Global Sales (Millions)') +
  ggsave('Figures/sales_user_score.png')

# dummy variable of having a critic and user score
vgsales$has_critic_score <- ifelse(is.na(vgsales$Critic_Score), 0, 1)
vgsales$has_user_score <- ifelse(is.na(vgsales$User_Score), 0, 1)

# linear regression model
linreg <- function(data_in) {
  model <- estimatr::lm_robust(Global_Sales ~ Critic_Score + User_Score + Year_of_Release, data = data_in)
  
  ate_hat <- model$coef['Critic_Score']
  
  return(ate_hat %>% as.numeric())
}

# summary of the linreg model
linreg_model <- estimatr::lm_robust(Global_Sales ~ Critic_Score + User_Score + Year_of_Release, data = vgsales)
summary(linreg_model)

# estimated ate from linreg model 
linreg(vgsales)

# model for interactions between user score and year of release
linreg_int <- estimatr::lm_robust(Global_Sales ~ Critic_Score + User_Score + Year_of_Release + User_Score:Year_of_Release, data = vgsales)
summary(linreg_int)

# function to take the estimated ATE difference
ate_diff <- function(data_in) {
  
  treat <- data_in %>% filter(has_critic_score == 1)
  control <- data_in %>% filter(has_critic_score == 0)
  
  ate_hat <- mean(treat$Global_Sales) - mean(control$Global_Sales)
  
  return(ate_hat)
}

# results for the ate_diff function
ate_diff(vgsales)
