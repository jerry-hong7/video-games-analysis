# name: Jerry Hong

# load necessary packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(estimatr, tidyverse, foreach, doRNG, doSNOW, parallel, boot, targeted, ggplot2)

# load data
vgsales <- read.csv('vgsales-12-2016.csv')

# summary stats
summary(vgsales)

# rescale user score by 10
vgsales$User_Score <- vgsales$User_Score * 10

# convert Year_of_Release as numeric
vgsales$Year_of_Release <- as.numeric(vgsales$Year_of_Release)

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
  theme(legend.position = 'none')

# colored bar plot of the average number of sales by region
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
  theme(legend.position = 'none')

# average critic and user score by platform
vgsales %>% 
  group_by(Platform) %>% 
  summarise(mean_critic_score = mean(Critic_Score),
            mean_user_score = mean(User_Score)) %>% 
  gather(key = 'score_type', value = 'score', mean_critic_score:mean_user_score) %>% 
  ggplot(aes(x = reorder(Platform, score), y = score, fill = score_type)) +
  geom_bar(stat = 'identity', position = 'dodge') +
  coord_flip() +
  labs(title = 'Average Critic and User Scores by Platform',
       x = 'Platform',
       y = 'Average Score') + 
  theme(legend.position = 'bottom')

# line plot of total global sales by year up to 2016
vgsales %>% 
  filter(Year_of_Release <= 2016) %>% 
  group_by(Year_of_Release) %>% 
  summarise(total_sales = sum(Global_Sales)) %>% 
  ggplot(aes(x = Year_of_Release, y = total_sales)) +
  geom_line() +
  labs(title = 'Total Global Sales by Year',
       x = 'Year',
       y = 'Total Global Sales (Millions)')

# dummy variable of having a critic and user score
vgsales$has_critic_score <- ifelse(is.na(vgsales$Critic_Score), 0, 1)
vgsales$has_user_score <- ifelse(is.na(vgsales$User_Score), 0, 1)

# linear regression model
linreg <- function(data_in) {
  model <- estimatr::lm_robust(Global_Sales ~ Critic_Score + User_Score, data = data_in)
  
  ate_hat <- model$coef['Critic_Score']
  
  return(ate_hat %>% as.numeric())
}

# results from linreg model 
linreg(vgsales)

# function to take the estimated ATE difference
ate_diff <- function(data_in) {
  
  treat <- data_in %>% filter(has_critic_score == 1)
  control <- data_in %>% filter(has_critic_score == 0)
  
  ate_hat <- mean(treat$Global_Sales) - mean(control$Global_Sales)
  
  return(ate_hat)
}

# results for the ate_diff function
ate_diff(vgsales)

# function for estimating ate using regression adjustment
linreg_adjust <- function(data_in) {
  model <- estimatr::lm_robust(Global_Sales ~ Critic_Score + User_Score, data = data_in)
  
  y1_hat <- predict(model, newdata = data_in %>% filter(has_critic_score == 1))
  y0_hat <- predict(model, newdata = data_in %>% filter(has_critic_score == 0))
  
  if (length(y1_hat) != length(y0_hat)) {
    stop("Lengths of y1_hat and y0_hat are not the same.")
  }
  ate_hat <- as.numeric(mean(y1_hat - y0_hat))
  
  return(ate_hat)
}

# results from linreg_target model
linreg_adjust(vgsales)
