####Statistical Analysis in A/B Testing####

###Power Analysis###
##Definitions dive deeper:
#Power: 
#the probability of rejecting the null hypothesis when it's false;
#the basic step of estimating the sample size needed to detect an effect of a particular magnitude; 
#the test with higher power is preferred.

#Significant Level:
#the level of probability at which it is agreed that the null hypothesis will be rejected

#Effect Size: the magnitude of the effect you are expecting
#Difference between control and experiment groups population means of a response variable, divided by assumed common population standard deviation

#Power Analysis Relationships: 
#Power↑, # of data point needed↑, holding the significant level and effect size constant
#Significant level↑, # of data point needed↓, holding the power constant
#Effect size↑,# of data point needed↓,holding the power and significant level constant
#Conclusion: we running the experiment with higher power, lower significant level,smaller effect size, and more data we needed is preferred.

##Experiment of T-test##usually used in linear regression for a continuous variable(time spent on website)
library(pwr)
pwr.t.test(power = 0.8,
           sig.level = 0.05,
           d = 0.6) #d: effect size

#return:
two-sample t test power calculation
n = 44.58577
d = 0.6
sig.level = 0.05
power = 0.8
alternative = two.sided
NOTE: n is number in *each* group

#if d=0.2:
Two-sample t test power calculation
n = 393.4057
d = 0.2
sig.level = 0.05
power = 0.8
alternative = two.sided
NOTE: n is number in *each* group

##Statistical Test##
#t-test for AA testing:
viz_website_2018_01 <- read_csv("viz_website_2018_01.csv")
aa_experiment_results <- t.test(time_spent_homepage_sec ~ condition,
                                data = viz_website_2018_01)
#result:
Welch Two Sample t-test
data:  time_spent_homepage_sec by condition
t = -0.87836, df = 30998, p-value = 0.3798
alternative hypothesis: true difference in means is not equal to 0
95 percent confidence interval:
-0.03252741  0.01239578
sample estimates:
mean in group A1 mean in group A2
58.99352         59.00358

#linear regression:has negative t value to show the direction of effect
lm(time_spent_homepage_sec ~ condition, data = viz_website_2018_01) %>%
  summary()
#return:
Coefficients:
              Estimate   Std. Error   t-value    Pr(>|t|)
(Intercept)   58.993518   0.008103   7280.207    <2e-16 ***
 conditionA2  0.010066    0.011460    0.878       0.38


###Stopping Rules and Sequential Analysis###
##Definition:
#Stopping rules:the procedure allows interim analysis in clinical trails at predefined times, while preserving the type one error at some pre-specified level.
#Sequential Analysis:statistical test of significance is conducted repeatedly over time as data are collected.
#1.stop data collection, reject H0 and claim the statistical significance
#2.stop data collection, do not reject H0 and state the results are not statistical significance
#3.continue data selection, since as the collected data are not enough to draw a conclusion
#more peaks, lower significant level

#why we use stop rules:
#1.prevent p-hacking
#2.account for unsure effect size
#3.better allocation of resources

library(gsDesign)
seq_analysis <- gsDesign(k = 4,  #the number of time we want to look at the data
                         test.type = 1, #refers to hypothesis. 1 means one sided test
                         alpha = 0.05,
                         beta = 0.2,
                         sfu = "Pocock") #spending function to figure out how to update our p-values
#result: to get uniform p-value cutoffs at each look point
One-sided group sequential design with
80 % power and 5 % Type I Error.
            Sample  
             Size
Analysis    Ratio*  Z    Nominal p  Spend    #we should put our attention to Nominal p:0.0193
1           0.306 2.07    0.0193   0.0193
2           0.612 2.07    0.0193   0.0132
3           0.918 2.07    0.0193   0.0098
4           1.224 2.07    0.0193   0.0077
Total                     0.0500

++ alpha spending:
  Pocock boundary.
* Sample size ratio compared to fixed design with no interim

#continue to update our p-value, we need to figure out the sample size for our stopping points:
max_n <- 1000
max_n_per_group <- max_n / 2
stopping_points <- max_n_per_group *seq_analysis$timing
stopping_points 
#return:125 250 375 500 data points per group


###Multivariate Testing###--we have more than one independent variables in an experiment
#how two different changes will affect each other,we need to test them at the same time.

##time spend on homepage multivariate analysis:
library(broom)
multivar_results <- lm(time_spent_homepage_sec ~ word_one * word_two,
                       data = viz_website_2018_05) %>%
  tidy()
multivar_results
#results:
          term                     estimate   std.error    statistic     p.value
1   (Intercept)                 48.00829170   0.008056696  5958.80671  0.0000000
2   word_onetools                4.98549854   0.011393888  437.55902   0.0000000
3   word_twobetter               -0.01323206  0.011393888   -1.16133   0.2455122 #only reference to the baseline model,not the entire data set
4   word_onetools:word_twobetter -4.97918356  0.016113391  -309.00904  0.0000000 #only reference to the baseline model,not the entire data set
#R choose word one alphabetically.

#use factor to order the words into a wanted order
library(broom)
multivar_results <- viz_website_2018_05 %>%
  mutate(word_one = factor(word_one,
                           levels = c("tips", "tools"))) %>%
  mutate(word_two = factor(word_two,
                           levels = c("better", "amazing"))) %>%
  lm(time_spent_homepage_sec ~ word_one * word_two,
     data = .) %>%
  tidy()

#results:
      term                           estimate   std.error    statistic   p.value
1(Intercept)                      47.995059637 0.008056696 5957.1643430 0.0000000
2word_onetools                     0.006314972 0.011393888    0.5542421 0.5794152
3word_twoamazing                   0.013232063 0.011393888    1.1613299 0.2455122
4 word_onetools:word_twoamazing  4.979183565  0.016113391  309.0090419 0.0000000

#plotting time homepage:
# Compute summary values for four conditions
viz_website_2018_05_sum <- viz_website_2018_05 %>%
  group_by(word_one, word_two) %>%
  summarize(mean_time_spent_homepage_sec = mean(time_spent_homepage_sec))

# Plot summary values for four conditions
ggplot(viz_website_2018_05_sum,
       aes(x = word_one,
           y = mean_time_spent_homepage_sec,
           fill = word_two)) +
  geom_bar(stat = "identity", position = "dodge")








