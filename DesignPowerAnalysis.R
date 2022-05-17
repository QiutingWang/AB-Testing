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

#look at experiment result#
library(tidyverse)
experiment_data <- read_csv("experiment_data.csv")
experiment_data
#return: one more column add it before: condition-->show it whether a control group or a test group
experiment_data %>%
  group_by(condition) %>%  #compute the entire length of experiment 
  summarize(conversion_rate = mean(clicked_adopt_today))
#return: the conversion rate for the control condition 17%,for test condition was 38%

#we compute it by day of experiment
group_by(visit_date, condition) %>%    
  summarize(conversion_rate = mean(clicked_adopt_today))
#plotting the result
ggplot(experiment_data_sum,
       aes(x = visit_date,
           y = conversion_rate)
          color = condition,
          group = condition)) +geom_point() +geom_line()

#confirm the result statistically
library(tidyverse)
library(broom)
experiment_data <- read_csv("experiment_data.csv")
glm(clicked_adopt_today ~ condition,  #previously we calculate it use logistic regression model in glm(dependent variable~independent variable)
    family = "binomial",  #make clear what kind of distribution is to use
    data = experiment_data)%>%  #provide the dataframe 
  tidy()  #use tidy() from brrom package to see a cleaned up version of results
#result:
     term        estimate std.error  statistic  p-value
1   (Intercept) -1.609438 0.1564922 -10.284464 8.280185e-25
2 conditiontest  1.138329 0.1971401   5.774212 7.731397e-09
#interpretation: p-value of conditiontest<0.05, model estmate is positive about 1 -->the test comdition had a higher conversion rate than control
#The experiment is success!
