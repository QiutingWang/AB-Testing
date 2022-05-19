####Experiment Design in A/B testing####

###A/B testing research questions###
#metrics uses:conversion rate, engagement, drop-off rate,time spend on a website

#time spend in the homepage:
library(tidyverse)
library(lubridate)
str(viz_website_2017) #display the columns vertically
viz_website_2017 %>%
  group_by(month(visit_date)) %>%
  summarize(mean(time_spent_homepage_sec)) #get the average spend in the home page

###Assumptions and types of A/B testing:
##with-in group and between group: generally have higher power
#within: each individual see both conditions;between: different groups see different conditions
#EG:You have a group of email users and for two weeks randomly show one of two "Inbox" designs whenever a person logs in.

##Types of A/B testing:
#A/B:compare a control and test condition 
#A/A:compare two groups of control conditions
#A/B/N:compare a control condition to any number of different test conditions

##plotting A/A test:
# Compute conversion rates for A/A experiment
viz_website_2018_01_sum <- viz_website_2018_01 %>%
  group_by(condition) %>%
  summarize(like_conversion_rate = mean(clicked_like))
viz_website_2018_01_sum
# Plot conversion rates for two conditions
ggplot(viz_website_2018_01_sum,
       aes(x = condition, y = like_conversion_rate)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(limits = c(0, 1), labels = percent)

##Analysis A/A test:
#this time hoping for a null effect, we build a logistic regression.
# Load library to clean up model outputs
library('broom')
# Run logistic regression
aa_experiment_results <- glm(clicked_like ~ condition,
                             family = "binomial",
                             data = viz_website_2018_01) %>%
  tidy()
aa_experiment_results
#result:
        term    estimate  std.error   statistic   p.value
1 (Intercept) -1.38025691 0.02004420 -68.8606672 0.0000000
2 conditionA2 -0.02591398 0.02845802  -0.9106038 0.3625041


###Confounding Variables###affect your test
##confound variable internal:
#Eg: in the first month,the click rate of "tips">the click rate of "tools"-->Reasons:the length of letters/the frequency of words
##confound variable external:
#Eg: in the second month, the click of "tips"<the click rate of "tools"-->Demographic information about the website visitors:ages...
# Compute 'like' conversion rate by week and condition
viz_website_2018_02 %>%
  mutate(week = week(visit_date)) %>%
  group_by(week, condition) %>%
  summarize(like_conversion_rate = mean(clicked_like))
# A tibble: 10 x 3
# Groups:   week [?]
week condition like_conversion_rate
<dbl> <chr>                    <dbl>
1     5 tips                    0.109 
2     5 tools                   0.0156
3     6 tips                    0.124 
4     6 tools                   0.0238
5     7 tips                    0.115 
6     7 tools                   0.0492
7     8 tips                    0.124 
8     8 tools                   0.110 
9     9 tips                    0.114 
10    9 tools                   0.147

# Compute 'like' conversion rate by if article published and condition
viz_website_2018_02 %>%
  group_by(article_published, condition) %>%
  summarize(like_conversion_rate = mean(clicked_like))
# A tibble: 4 x 3
# Groups:   article_published [?]
article_published condition like_conversion_rate
  <chr>             <chr>                    <dbl>
1 no                tips                    0.118 
2 no                tools                   0.0200
3 yes               tips                    0.119 
4 yes               tools                   0.106 

# Plot 'like' conversion rates by date for experiment
ggplot(viz_website_2018_02_sum,
       aes(x = visit_date,
           y = like_conversion_rate,
           color = condition,
           linetype = article_published,
           group = interaction(condition, article_published))) +
  geom_point() +
  geom_line() +
  geom_vline(xintercept = as.numeric(as.Date("2018-02-15"))) +   #Find the date when the article was first published.
  scale_y_continuous(limits = c(0, 0.3), labels = percent)


###Side Effect###
#definition: an unintended consequence of a change we made
##Example:
#in the first month,the click rate of "tips">the click rate of "tools"-->Reasons:the length of letters
#but 1 letter longer, leads to 2 more seconds to load. It will slow down the website.
#amount of information "above the fold":what the person sees without having to do any scrolling. If the logo is too big, the less click rates will include.

#side effect load time plot#
#there was a larger load delay for the "tools" homepage than the "tips" homepage. 
# then added a delay to the "tips" homepage so that they even out. 
# Compute 'like' conversion rate and mean pageload time by day
viz_website_2018_03_sum <- viz_website_2018_03 %>%
  group_by(visit_date, condition) %>%
  summarize(mean_pageload_time = mean(pageload_time),
            like_conversion_rate = mean(clicked_like))
# Plot effect of 'like' conversion rate by pageload time
ggplot(viz_website_2018_03_sum,
       aes(x = mean_pageload_time, y = like_conversion_rate, color = condition)) +
  geom_point()

#seeing if we can find the effect of when the page load delay was added.
# Plot 'like' conversion rate by day
ggplot(viz_website_2018_03_sum,
       aes(x = visit_date,
           y = like_conversion_rate,
           color = condition,
           linetype = pageload_delay_added,
           group = interaction(condition, pageload_delay_added))) +
  geom_point() +
  geom_line() +
  geom_vline(xintercept = as.numeric(as.Date("2018-03-15"))) +
  scale_y_continuous(limits = c(0, 0.3), labels = percent)

