####AB Testing####

###Mini Case of A/B testing###
##Baseline conversion rate##
#review the data
library(tidyverse) #include ggplot2 package
click_data <- read_csv("click_data.csv")
click_data
#calculate the current conversion rate
click_data %>%
  summarize(conversion_rate = mean(clicked_adopt_today))

#calculate the current conversion rate seasonality
library(tidyverse)
library(lubridate)  #have the month function
click_data <- read_csv("click_data.csv")
click_data
click_data %>%
  group_by(month(visit_date)) %>%
  summarize(conversion_rate = mean(clicked_adopt_today))
#plotting the data
ggplot(click_data_sum, aes(x = `month(visit_date)`, y = conversion_rate)) +
  geom_point() +
  geom_line()

##Design an Experiment-Power Analysis##
#the experiment duration--one of the important problem of AB testing:
#too short: we do not have enough data to test; too long: waste valuable resources on a failed experiment
#power analysis: how many data point (or sample size) that i need to be sure an effect is real
#it is depend on variables: such as how much website hits you get per day
#we need to decide: statistical test, baseline value:value for current control condition,desired value for the test condition, the fraction of the data from the test condition(ideally 0.5),significant level(normally 0.05),power(1-beta, normally 0.8)
library(powerMediation) 
total_sample_size <- SSizeLogisticBin(p1 = 0.2,   #we run a logistic regression,a binary test: click or not-click
                                      p2 = 0.3,  #p1:conversion rate for the control condition
                                      B = 0.5,  #p2:...for the test condition
                                      alpha = 0.05,
                                      power = 0.8)
total_sample_size
total_sample_size / 2
#return:
587
293.5

