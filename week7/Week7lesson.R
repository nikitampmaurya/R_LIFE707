library(tidyverse)

df1 = read.table("chlorophyll.txt", header = T, stringsAsFactors =  T)

View(df1)

str(df1)

summary(df1)

any(duplicated(df1$Lake)) #to check if there any duplicates

hist(df1$Chlorophyll)

hist(log(df1$Chlorophyll))

hist(df1$Alkalinity)

hist(log(df1$Alkalinity))

hist(df1$Calcium)

hist(log(df1$Calcium))

hist(df1$pH)

df1_model1 = lm(log(Chlorophyll) ~ (pH + log(Alkalinity) + log(Calcium))^2, df1)

summary(df1_model1)

# lower calcium might be associated with high chlorophyll levels.
# a possible interaction effect between pH and calcium on chlorophyll.

df1_model2 = step(df1_model1)

# AIC (Akaike Information Criterion) is a statistical metric used to compare models and evaluate their quality relative to one another. 
#lower the AIC better the model is

drop1(df1_model2,test="F")

# Since the p-value is significant (< 0.05), the interaction term pH:log(Calcium) important for our model.

summary(df1_model2)

# log(Calcium) and the interaction between pH and log(Calcium) are significantly affect log(Chlorophyll) levels.
# higher levels of log(Calcium) are associated with a decrease in log(Chlorophyll).
# The model as a whole is significant (p-value = 4.551e-07), but pH alone does not significantly contribute to explaining chlorophyll levels.

plot(df1_model2, which=2)

df1$Acid <- factor("Acid",levels=c("Acid","Alkali"))
df1$Acid[df1$pH >= 7] <- "Alkali"

df1.plot <- ggplot(df1, aes(x=Calcium,y=Chlorophyll, color = Acid, shape = Acid))

df1.plot + geom_point() + 
  xlab("Calcium (mg/L)") + ylab("Chlorophyll (mg/L)") +
  scale_x_log10() + scale_y_log10() +
  facet_wrap(~Acid) +
  theme_bw(base_size = 16)

# In alkali lakes the amount of chlorophyl is higher.

####################

orings = read.table("orings.txt", header = T)

View(orings)

or1 = glm(cbind(damage, 6 - damage) ~ temp, data = orings, family = "binomial")

# Fits a GLM to predict the number of O-rings damaged using temperature as the predictor variable, based on the orings dataset.

or.predict = data.frame(temp = seq(25,85),
                         pred.damage=predict(or1, type = "response", newdata = data.frame(temp = seq(25,85))))

#  uses the fitted model to make predictions for a range of temperatures, converting log odds into probabilities.

# What data we want to plot
oring.plot1 = ggplot(data=orings, aes(x=temp,y=damage/6))

# plot the points
oring.plot1 + geom_point() +
  
  # put on informative axes labels 
  xlab("Temperature (F)") + ylab("Proportion of damaged orings") +
  
  # set the limits of x and y axes beyond the range of the data (but to include the fatal launch temperature)
  ylim(c(0,1)) + xlim(c(25,85)) +
  
  # add a line for the probability of failure predicted by the model
  geom_line(data=or.predict,aes(x=temp,y=pred.damage)) +
  
  # the fatal launch was at 31F, let's plot this as a thick red line
  geom_vline(xintercept=31,col="red",linetype="dashed",linewidth=2)

#I would not choose to launch at that temperature because the porbability is not zero. 