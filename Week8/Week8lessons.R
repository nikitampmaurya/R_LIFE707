library(survival) # used for analyzing time-to-event data, it provides functions for survival analysis, including Kaplan-Meier plots and Cox proportional hazards.
library(survminer) # used for producing publication standard survival plots

# install.packages("survminer")

#?colon to view the dataset in R studio

View(colon)
# Q. 1. Look at the help for ?colon again. How are sex and status coded? How is this different from the lung data?

# we interested in three columns: 
# sex:	
# 0 = female 
# 1 = male 
# status:
# 1= occurrence of the event (death)
# 0= censoring (the event did not occur during the study period)

colon_death =  subset(colon, colon$etype == "2") # we don't want reoccurence

View(colon_death)

# Q. 2. Create a survival curve separately for males versus females. 
# Run a summary() on this object, showing time points 0, 500, 1000, 1500, and
# 2000. Do males or females appear to fair better over this time period?
  
s = Surv(colon_death$time, colon_death$status)  # creating a survival object using time and status columns from colon dataset
s #to view on console

summary(colon_death) # to check if there are any NA values  

table(colon_death$status) # to check if status is binary 

sfit_sex = survfit(Surv(time, status) ~ sex, data = colon_death)

sfit_sex

# Based on the Survival analysis by sex, males have a median survival time of 2527 days (95% CI: 1976–2910), 
# while females have a median survival time of 2174 days (95% CI: 1752–NA)
# which means that males tend to live slightly longer than females.

summary(sfit_sex, times = seq(0, 2000, 500))

plot <- ggsurvplot(
  sfit_sex,
  risk.table = TRUE, # adding risk table
  risk.table.col = "strata", # add color to our risk table based on their sex
  xlab = "Time (in days)",
  ylab = "Probability of Survival",
  title = "Kaplan-Meier Survival Curve Between Sexes",
  legend = c(0.9, 0.8), # adjust legend position
  legend.title = "Sex",          
  legend.labs = c("Female", "Male"), 
  ggtheme = theme_bw(base_size = 12) +
    theme(plot.title = element_text(hjust = 0.5))
)
plot

# Based on the survival analysis and graph, both sexes show similar survival patterns over time.


sfit_sex2 = coxph(Surv(time, status)~ sex, data=colon)
summary(sfit_sex2)

# The Cox proportional hazards model does not find a statistically significant association between sex and survival time in the colon dataset.
# As p-value for the coefficient is 0.61, which is much greater than the standard threshold of 0.05. 
# These results indicate that sex likely does not play a meaningful role in predicting survival in this dataset.
