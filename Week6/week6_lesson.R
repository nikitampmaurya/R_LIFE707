library(tidyverse)

fruitfly = read.table("fruitfly.txt", header = T)

View(fruitfly)
table(fruitfly$activity)

fruitfly$activity = factor(fruitfly$activity, levels = c("isolated","one","many","low","high"))
#to read in this order instead of alphabetically

str(fruitfly)

fly.plot = ggplot(fruitfly, aes(x=thorax,y=longevity))

fly.plot + geom_point() +
  xlab("Thorax length (in mm)") +
  ylab("longevity (in days)") +
  facet_wrap(vars(activity)) +
  theme_bw(base_size = 16)

#if you see the graph, more no of points are present in upper half of the first plot,
#this number slowly decreases, indicating, longevity decreases as sexual activity increases.


ff1 = lm(longevity ~ thorax + activity, fruitfly)
# dependent ~ predictor + predictor 
summary(ff1)
# longevity = − 48.749 + 134.341(thorax) + 2.637(activityone) + 4.139(activitymany) − 7.015(activitylow) − 20.004(activityhigh)
# but here acitvityone and activitymany are not statistically significant
# longevity = − 48.749 + 134.341(thorax) − 7.015(activitylow) − 20.004(activityhigh)

# Thorax size is the strong positive predictor of longevity.
# larger the thorax size the longer lifespans is in fruit flies

# also mating with higher no of virgin female significantly decrease longevity. 

# here R sq is 65% and we know higher R sq value indicate a better fit (closer to 1 being ideal)
# p-value < 2.2e-16 indicates that the overall model is statistically significant, 
# meaning the predictors collectively explain longevity better than a null model.

# Therefore sex reduces male lifespan and the more sex they have the shorter their lives are. 

# task 1:

# For 'low' activity group when Thorax = 0.75mm:

# Longevity = -48.749 + (134.341 * 0.75) - 7.015

# longevity = 44.992 days

# For 'low' activity group when Thorax = 0.85mm:

# Longevity = -48.749 + (134.341 * 0.85) - 7.015

# longevity = 58.426 days

# similarly for'High' Activity Group 

# When Thorax = 0.75mm: longevity = 32.003 days

# When Thorax = 0.85mm: longevity = 45.437 days


# and  'Isolated' Activity Group:

# When Thorax = 0.75mm: longevity = 52.007 days

# When Thorax = 0.85mm: longevity = 65.441 days

# An interaction here might mean something like ‘sex reduces longevity, but only in small flies’, which could be biologically plausible and is worth checking.
  
ff2 <- lm(longevity ~ thorax * activity, data = fruitfly)
summary(ff2)

#  none are significant.

# The F-test in ANOVA compares the residual sums of squares (RSS) between the two models. 
# It tells you if the more complex model (with interactions) provides a significantly better fit to the data compared to the simpler model.

anova(ff2, ff1, test="F")

# p-value of 0.9947 suggests that adding the interaction terms in ff2 does not provide a significant improvement in the model fit. 
# In other words, the simpler model ff1 (without interactions) is just as good as ff2 (with interactions) at explaining the data.

drop1(ff2, test="F")

# drop1() function systematically tests whether each term in the model ff2 is necessary. 

# the interaction term thorax:activity does not statistically improve the model fit (p-value = 0.9947).

drop1(ff1, test="F")
# Both thorax and activity are highly significant predictors of longevity 
# Removing either term would significantly worsen the model fit.

# task 2:

df1 = read.table("hydrolase.txt", header = T, stringsAsFactors = T)

View(df1)

str(df1)

df1_plot = ggplot(df1, aes(x = salt, y = activity)) 

df1_plot + 
  geom_point() +
  xlab("Salt concentration") + 
  ylab("Enzyme activity") +
  facet_wrap(~species) +
  theme_bw(base_size = 16)
  
# it seems as salt concentration increases in species A, activity decreases and vice versa for species B

df1_model = lm(activity ~ salt + species, df1)
summary(df1_model)

# as salt concentration increase, enzyme activity in species A decreases and it is significant.
# as salt concentration increases, enzyme actvity in species B increases and it is significant.

df2_model = lm(activity ~ salt * species, df1)
summary(df2_model)

#the relationship between salt concentration and enzyme activity differs significantly (< 2.10e-11) between the two species.
# which confirms that the effect of salt on enzyme activity depends on the bacterial species.

anova(df1_model, df2_model, test="F")

# p value is 2.102e-11 meaning adding the interaction term between salt and species significantly improves the model fit. 

drop1(df1_model, test="F") # removing either term (salt or species) worsens the model fit

drop1(df2_model, test="F") # removing the interaction term significantly worsens the model fit 

######################

# task 3

motherweight = read.csv("birthweight.csv", header = T)

View(motherweight)

str(motherweight) #to check data type

#motherweight$smoker <- factor(motherweight$smoker, levels = c(0, 1), labels = c("Non-smoker", "Smoker"))

summary(motherweight) #to check na values

ggplot(motherweight, aes(x=mppwt)) + geom_histogram(bins=10)

ggplot(motherweight, aes(x=mppwt)) + geom_density()

#appears to be normally distributed

motherweight_model1 <- lm(Birthweight ~ (Gestation + smoker + mppwt)^2, motherweight)
summary(motherweight_model1)

#here only smoker is significant meaning mom that smoke have baby with lower birthweight, since p value < 0.05 and coefficient is negative
#overall model is significant however none of the interaction are significiant

drop1(motherweight_model1, test="F")

# let remove Gestation:mppwt  since it is highly insignificant

motherweight_model2 = update(motherweight_model1, .~.-Gestation:mppwt)
anova(motherweight_model1, motherweight_model2, test="F")

# since p-value > 0.05 there is no statistically significant difference between the two models. 
# removing the Gestation:mppwt term from the old model does not significantly affect the fit.

drop1(motherweight_model2,test="F")
#here Gestation:smoker is highly insignificant, so let's remove that

motherweight_model3 = update(motherweight_model2, .~.-Gestation:smoker)
anova(motherweight_model2, motherweight_model3, test="F")

#  since p-value > 0.05 there is no statistically significant difference between the two models.
# removing Gestation:smoker from previous model did not significant affect the fit.

drop1(motherweight_model3, test = "F")
# here smoker:mppwt is highly insignificant so we again remove it

motherweight_model4 = update(motherweight_model3, .~.-smoker:mppwt)
drop1(motherweight_model4, test="F") #all significant
summary(motherweight_model4)
# here if gestation time is 38 or 41 and motherweight is 125 lbs,

# -7.165 + 0.313 * 38 + 0.0198 * 125 = 7.20 lbs (birthweight)

# -7.165 + 0.313 * 41 + 0.0198 * 125 = 8.14 lbs (birthweight)
plot(motherweight_model4)

bw.plot2 <- ggplot(motherweight, aes(x=Gestation, y=Birthweight, color=mppwt))

#how to plot it (this is just the code from the workshop)
bw.plot2 + geom_point(size=4) + facet_wrap(~smoker) +
  xlab("Gestation (weeks)") + ylab("Birthweight (lbs)") +
  theme_bw() +scale_color_gradient(low="blue", high="red") 


