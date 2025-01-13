# Task: Sex determination in turtles is dependent on the temperature 
# at which eggs are incubated at. 
# Consider the data in the turtles.txt file and describe this relationship.

library(tidyverse)

df1 = read.table("turtle.txt", header = T)

View(df1)

str(df1)

plot1 = ggplot(df1) +
  geom_point(aes(x = male, y = temp, color = "Male"), size = 3) +
  geom_point(aes(x = female, y = temp, color = "Female"), size = 3) +
  geom_smooth(method = "lm") +
  labs(x = "Temperature", y = "Number of Turtles") +
  scale_color_manual(values = c("Male" = "blue", "Female" = "purple")) +
  theme_minimal(base_size = 11) +
  ggtitle("Sex Determination in Turtles Based on Temperature")

plot1

# Based on the graph, more males tend to born at higher temp 
# while more females tend to born at lower temp


summary(df1.model)

# temp is significantly affecting  female/male ratio.
# negative coefficient means as temp increase females decreases

drop1(df1.model,test="Chisq")

#  p = 1.919 × 10⁻¹² confirms that temperature has a statistically significant effect on the sex ratio.
# removing it would worse the fit

df1.pred <- data.frame(temp = seq(27,30,length=20))
View(df1.pred)

# dataframe with 20 rows of temp 

df1.pred$pred.female <- predict(df1.model, type = "response", newdata = df1.pred)
VieW(df1.pred)

# dataframe with a column containing the predicted probability of a turtle being female at each temperature value,

df1.plot1 = ggplot(df1,aes(x=temp,y=female/(male+female))) + geom_point() +
  xlab("Temperature (C)") + ylab("Probability of female") +
  xlim(c(27, 30)) + ylim(c(0, 1)) +
  geom_line(data=df1.pred,aes(x=temp,y=pred.female)) +
  geom_vline(xintercept=27.2,col="red",linetype="dashed",linewidth=2)

df1.plot1

# a decreasing probability of being female as temperature increases. 

df2 = read.table("phage.txt", header = T, stringsAsFactors = T)

View(df2)

str(df2)

summary(df2)

# since both data are not categorical, we can't do chi-square

df2.plot = ggplot(df2, aes(x = treatment ,y = count)) +
  geom_boxplot() +  
  geom_point() + geom_jitter(height=0,width=0.2) +
  theme_bw(base_size = 12)        

df2.plot

df2.plot1 = ggplot(df2, aes(x = count, fill = treatment)) +
  geom_histogram(binwidth = 1, color = "black") +  
  facet_wrap(~treatment) +
  labs(x = "Count", y = "Frequency", title = " No of antibiotic resistant bacteria in wildtype and mutant strains") +
  theme_minimal(base_size = 12)

df2.plot1

df2.model = glm(count ~ treatment,df2, family="poisson")
summary(df2.model)

# We performed a Poisson regression analysis to assess the effect of treatment (mutant vs. wild type) on the count of antibiotic-resistant bacteria.

# The regression model includes count as the response variable and treatment as the predictor variable.

# The p-value is significant meaning the wild type bacteria have lower number of antibiotic-resistant bacteria compared to the mutant strain since its coefficient is negative.

drop1(df2.model,test="Chisq")


# There is a statistically significant difference in antibiotic resistance between the wild-type and mutant bacterial strains. 
# The data suggest that the mutant strain is more likely to exhibit higher antibiotic resistance than the wild-type strain following infection with the lysogenic phage.

