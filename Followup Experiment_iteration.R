####Mini-Case study in AB Testing####
###Designing Follow-up Experiment###
##tips:
#1.set up the small follow-up experiments
#2.avoid confounding variables, we test only one variable at once
#3.test small changes/differences, not huge ones

##Example:Following up Experiment#1
#use a picture of a kitten in a hat instead of an adult cat;
#use a picture of two cats or kittens in hats instead of one.

#Design Strategy of Experiment:
#Build one experiment where your test condition is a kitten in a hat. 
#If the experiment works, run a second experiment with a kitten in a hat as the control and two kittens in hats as the test.

# Load package for running power analysis
library(powerMediation)
# Run logistic regression power analysis
total_sample_size <- SSizeLogisticBin(p1 = 0.39,
                                      p2 = 0.59,
                                      B = 0.5,
                                      alpha = 0.05,
                                      power = 0.8)
total_sample_size

##Pre-follow-up Experiment Assumptions:
#compute conversion rate difference:due to the conversion rate varies from time to time
eight_month_checkin_data_sum <- eight_month_checkin_data %>%
  mutate(month_text = month(visit_date, label = TRUE)) %>% #add a new datetime column use mutate function
  group_by(month_text, condition) %>%
  summarize(conversion_rate = mean(clicked_adopt_today))
eight_month_checkin_data_diff <- eight_month_checkin_data_sum %>%
  spread(condition, conversion_rate)  #show the distribution frequency of each condition,changing the format of data. return levels into a column
  mutate(condition_diff = cat_hat - no_hat) #add the new column to show the difference between conditions
#calculate the mean of each differences
mean(eight_month_checkin_data_diff$condition_diff, na.rm = TRUE)  #return:0.1876171
#calculate the std of each differences
sd(eight_month_checkin_data_diff$condition_diff, na.rm = TRUE) #return:0.03893739
#Conclusion of the statistics: the effect is pretty consistent.

# Compute difference over time
no_hat_data_diff <- no_hat_data_sum %>%
  spread(year, conversion_rate) %>%
  mutate(year_diff = `2018` - `2017`)
no_hat_data_diff
# Compute summary statistics
mean(no_hat_data_diff$year_diff, na.rm = TRUE)
sd(no_hat_data_diff$year_diff, na.rm = TRUE)

#re-run the power analysis
# Load package 
library(powerMediation)
# Run power analysis for logistic regression
total_sample_size <- SSizeLogisticBin(p1 = 0.49,
                                      p2 = 0.64,
                                      B = 0.5,
                                      alpha = 0.05,
                                      power = 0.8)
total_sample_size
#return:341
#run our experiment in September,re-run glm() for follow up
# Load package to clean up model outputs
library(broom)
# View summary of data
followup_experiment_data_sep %>%
  group_by(condition) %>%
  summarize(conversion_rate=mean(clicked_adopt_today))
# Run logistic regression
followup_experiment_sep_results <- glm(clicked_adopt_today ~ condition,
                                       family = "binomial",
                                       data = followup_experiment_data_sep) %>%
  tidy()
followup_experiment_sep_results
#returns:
term   estimate std.error  statistic     p.value
1         (Intercept) -0.1288329 0.1532613 -0.8406096 0.400566704
2 conditionkitten_hat  0.5931385 0.2194637  2.7026718 0.006878462
