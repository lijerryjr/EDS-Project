######################
###
###  Fall 2019 Exam 1
###
###  andrew id: jerryl
###
######################

library(tidyverse)
library(ggthemes)
library(car)
library(AER)
d<-read_csv("Steak.csv")
d2<-read_csv("sbelts2.csv")
# Data Visualization Question  ----
#
# 1.
ggplot(data=d) +
  geom_bar(mapping=aes(x=steak_prep)) +
  xlab("Steak Types") +
  ylab("Frequency")

#
# 2. ----

d1<-d%>%filter(steak_prep %in% c("Well",
                                 "Medium Well",
                                 "Medium",
                                 "Medium rare",
                                 "Rare"))

#d1$steak_prep<-reorder(d1$steak_prep, d1$hhold_income)
ggplot(data=d1) +
  geom_bar(mapping=aes(x=steak_prep), width=0.2) +
  coord_flip()+
  facet_wrap(~hhold_income) +
  labs(title="Question 2: How Americans Like Their Steak",
       subtitle="Results seperated by household income level",
       y="Number of Respondents",
       x="Preferred Steak Preparation") +
  theme_few()


#
# 3. 
##The plot in the lower left displays the distribution of steak preferences (excluding unknown, or NA types)
##for Americans with unknown, or NA type, household income levels. (Note when I say distribution I mean the 
##frequency of observations of people who prefer different types of steak)



# Regression Question ----
#
# 4.
d2$enforcement<-relevel(factor(d2$enforcement), ref="secondary")
d2$ba08_c<-factor(d2$ba08)
m1<-lm(fatalityrate~enforcement+log(income)+ba08+sb_use, data=d2)
summary(m1)
#
# 5. ----
##Intercept: 66.2012. For a state with secondary enforcement of seat belt laws, max BAC=0.1%,
##log(income per capita)=0 (i.e. income per capita is $1000, which shows up as 1 in the variable),
##and seat belt usage rate of 0, we
##would predict the fatality rate to be 66.2012 per thousand traffic miles. This is an extrapolation
##because we do not observe a log(income per capita)=0 or a seat belt usage rate of 0.

##Enforcementprimary: 0.3104. Holding log of income per capita, maximum allowed blood alcohol limit,
##and seat belt usage rate constant, we expect the fatality rate in states with primary enforcement to be 0.3104
##higher as compared to states with secondary enforcement, on average. This is equivalent to saying that,
##holding the other variables constant, the intercept for states with primary enforcement is
##66.2012+0.3104=66.5116--this intercept can be interepreted similarly to the overall intercept.

##log(income): -15.7103. Holding seat belt enforcement law, maximum allowed blood alcohol limit, and
##seat belt usage rate constant, we expect the fatality rate of a state to decrease by 0.157103 for each
##1% increase in income per capita.

##Note: when I say holding {variables} constant, I mean if two states had the same values for those
##variables.

#
# 6. ----
residualPlots(m1)
##Yes--it seems that the residuals for the variable ba08 are reasonably independent, as indicated
##by the fact that there is no clear correlation (trend) between the residuals and specific values for ba08.


#
# 7. ----
m2<-lm(fatalityrate~enforcement+log(income)+ba08*sb_use, data=d2)
summary(m2)


#
# 8. ----
##ba08: -2.49137. Holding enforcement law, income per capita, and seat belt usage rate constant, we expect
##the fatality rate in a given state with blood alcohol limit capped at 0.08% to be lower by 2.49137 as
##compared to states with max BAC=0.1%.

##log(income): -15.65678. Holding seat belt enforcement law, maximum allowed blood alcohol limit, and
##seat belt usage rate constant, we expect the fatality rate of a state to decrease by 0.1565678 for each
##1% increase in income per capita.

##sb_use: 0.07885. Holding enforcement law and income per capita constant,
##we expect the fatality rate in a state with max BAC=0.1% to increase by 0.07885 for every 1-unit increase
##in the seat belt usage rate. Note that this is an extrapolation because we can't have a seat belt usage rate
##above 1.0 (the variable is in decimal and not percentage form)--I could have transformed 
##the rate by multiplying by 100, which would produce a coefficient of
##0.0007885, indicating that holding enforcement law and income per capita constant, we expect
##the fatality rate in a state with max BAC=0.1% to increase by 0.0007885 for every 1 percentage increase
##in the the seat belt usage rate (representing the usage rate as a percentage out of 100).

#
# 9. ----
anova(m1,m2)
##The F-statistic is 0.3653 with a p-value of 0.5459. Since the p-value is not significant under 0.05,
##we can conclude that the first model does not
##account for significantly more variation in the response variable fatality rate as compared to the
##second model--the first model is not statistically better at predicting fatality rate.

#
# 10. ----
3*(4+1)/436
##The threshold is 0.0344
infIndexPlot(m2, id=T,vars=c("Cook", "hat"))
##Yes, there are data points with leverages above the threshold. From the influence index plot, there 
##seem to be 35 data points with high leverages.

#
# 11. ----
max(cooks.distance(m2))
##The maximum cooks distance is 0.0389. Since this is less than 0.5, the value is not worrisome.

#
# 12. ----
predict(m2, tibble(sb_use=0.45,
                   enforcement="primary",
                   income=30,
                   ba08=0))
##The predicted fatality rate is 13.31757 per thousand traffic miles.






