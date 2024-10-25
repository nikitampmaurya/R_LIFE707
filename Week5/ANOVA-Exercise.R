library(tidyverse)
library(dplyr)
library(ggplot2)

df = read.table("antid1.txt", header = T, stringsAsFactors = T) 

#wellbeing scores based on different doses (high. low, placebo) of antidepressants. 

View(df) #15 rows and 2 columns

str(df)

summary(df)

head(df)

#Q.1. What test do you need to run on these data?

#A.1. Since there are more than two groups of dosage, we will perform one way anova

#but before proceeding, let us visualize 

#Q.2. Choose a suitable plot for the data 
#and produce a publication ready version using R (base R or ggplot2 is fine)

ggplot(df, aes(x=wellbeing)) +
  geom_histogram(binwidth = 1, fill = "lightblue", color="black") +
  facet_wrap(~Group) + 
  labs(title = "Wellbeing Scores by Antidepressant Dose", x = "wellbeing", y = "count") +
  theme_bw(base_size = 16)

# #according to the graph, the wellbeing score of low dosage and placebo are very similar

#also boxplot

ggplot(df, aes(x = Group, y = wellbeing, fill = Group)) +
  geom_boxplot() +
  labs(title = "Wellbeing Scores by Antidepressant Dose",
       x = "Group (Dose)",
       y = "Wellbeing Score") +
  theme_bw(base_size = 16)

#overall, high dose of antidepressdant is producing better wellbeing score. 

#let' us perform anova by fitting model first

df_model = lm(wellbeing ~ Group,df)

#Null hypothesis: there is no significant difference in the mean wellbeing scores across the different groups (i.e., "High", "Low", and "Placebo" doses of antidepressants).
#Alternative hypothesis: at least one group of dose where the mean wellbeing score is significantly different from the others.

anova(df_model) #now since p value is 0.025, which is below threshold, we accept the alternative hypothesis.

#post hoc multiple comparison test to check ?

df_aov = aov(df_model)

TukeyHSD(df_aov, ordered = TRUE)

#according to the post hoc test, there is only significant difference between high and placebo.
#High dose of the antidepressant tends to improve wellbeing compared to the Placebo.


df2 = read.csv("voles.csv", header=T, stringsAsFactors = T)

View(df2)

str(df2)

summary(df2)

head(df2)

df3 = df2[!is.na(df2$weight), ]

View(df3)

str(df3)

summary(df3)


df4 = df3[df3$sex == 'M',]

View(df4)

summary(df4)

str(df4)

df4$year = as.factor(df4$year)

str(df4)

#One way anova because we have checking weights in more than two different years.

#Null hypothesis: there is no difference in the mean weight of male voles between years.

#Alternative hypothesis: At least one year has a significantly different mean weight of male voles compared to the others.

df4_model = lm(weight ~ year,df4)

anova(df4_model)

#p value is very low , so we reject the null hypothesis

df4_aov = aov(df4_model)

TukeyHSD(df4_aov, ordered = TRUE)

#2017 and 2015 have significant difference as well as 2017 and 2016.

ggplot(df4,aes(x=year,y=weight, fill = year)) +
  geom_boxplot(color = "black") +
  scale_fill_brewer(palette = "Pastel1") +  
  labs(title = "Distribution of Male Vole Weights Across Years", 
       x = "Year", 
       y = "Weight (g)") +
  theme_bw(base_size = 12)

#According to the graph, males in 2017 have highest weights compared to those in 2016 and 2015.

