library(tidyverse)
library(dplyr)
library(ggplot2)
library(gridExtra)

df1 = read.csv("Deer.csv", header = T, stringsAsFactors = T)
View(df1) # 2 cols and 24 rows

str(df1) #all columns are in proper data types

summary(df1) #no na,nan or missing values

#Before performing let's us visualize

ggplot(df1, aes(x=diet, y=gain, fill=diet)) +
  geom_boxplot() +
  labs(title = "Weight Gain by Diet", x = "Diet", y = "Weight Gain (kg)") +
  theme_classic(base_size = 16)

#Diet B shows the highest median gain, followed by Diet D, Diet A, and Diet C. 
#so there may be a significant effect of diet on weight gain in Père David's deer. 
#To confirm this,Let's us perform an ANOVA test to check whether the differences in weight gain across the diets are statistically significant or not.

#Why anova because there are four diets. 

#also here weight gain based on diets is independent and the spread of values in each boxplot is roughly similar, 
#this suggests homogeneity of variances.

#why one anova because there is only one independent variable diet.

#Null hypoythesis: no significant difference in weight gain among the different diets
#meaning diet does not influence weight gain in Père David's deer.

#Alternative hypothesis: is significant difference in weight gain among the different diets
#meaning diet does influence weight gain in Père David's deer.

ggplot(df1, aes(x = gain, fill = diet)) +
  geom_histogram(binwidth = 5, color = "black") +
  labs(title = "Histogram of Weight Gain by Diet",
       x = "Weight Gain (kg)",
       y = "Frequency") +
  facet_wrap(~ diet) +  
  theme_classic(base_size = 16)


df1_model <- lm(gain ~ diet, df1)

anova(df1_model)

#here p value is 0.00128. So we reject the null hypothesis. 
#and conclude there is a statistically significant effect of diet on weight gain in Père David's deer.

df1_aov = aov(df1_model)

TukeyHSD(df1_aov, ordered = T)

# only d and c, b and c and b and a show significant difference.

sum_stat = df1 %>%
  group_by(diet) %>%
  summarise(
    mean_gain = mean(gain),
    SD_gain = sd(gain),
    SE_gain = sd(gain)/sqrt(n()),
  )
print(sum_stat)


ferm = read.csv("Fermentation.csv", header = T)

View(ferm)

ferm$Process = as.factor(ferm$Process)

str(ferm)

summary(ferm)

plot_glutamate <- ggplot(ferm, aes(x = Process, y = Glutamate_yield, fill = Process)) +
  geom_boxplot() +
  labs(title = "Glutamate Yield by Fermentation Process",
       x = "Fermentation Process",
       y = "Glutamate Yield (g/L)") +
  theme_minimal(base_size = 10)

plot_lysine <- ggplot(ferm, aes(x = Process, y = Lysine_yield, fill = Process)) +
  geom_boxplot() +
  labs(title = "Lysine Yield by Fermentation Process",
       x = "Fermentation Process",
       y = "Lysine Yield (g/L)") +
  theme_minimal(base_size = 10)

grid.arrange(plot_glutamate, plot_lysine, ncol = 2)

#glutamate and lysine yield is higher in during process 1. 

ggplot(ferm, aes(x = Glutamate_yield, fill=Process)) +
  geom_histogram(binwidth = 0.5, color = "black") +
  facet_wrap(~Process)
  labs(title = "Histogram of Glutamate Yield",
       x = "Glutamate Yield (g/L)",
       y = "Frequency") +
  theme_minimal()
  
ggplot(ferm, aes(x = Lysine_yield, fill=Process)) +
  geom_histogram(binwidth = 0.02, color = "black") +
  facet_wrap(~Process)
  labs(title = "Histogram of Lysine Yield",
       x = "Lysine Yield (g/L)",
       y = "Frequency") +
  theme_minimal()
  
#seems normally distributed

sum_stat = ferm %>%
  group_by(Process) %>%
  summarise(
    mean_Glutamate_yield = mean(Glutamate_yield),
    SD_Glutamate_yield = sd(Glutamate_yield),
    SE_Glutamate_yield = sd(Glutamate_yield)/sqrt(n()),
    mean_Lysine_yield = mean(Lysine_yield),
    SD_Lysine_yield = sd(Lysine_yield),
    SE_Lysine_yield = sd(Lysine_yield)/sqrt(n())
  )

T_sum_stat = t(sum_stat)

T_sum_stat_df <- as.data.frame(T_sum_stat)

colnames(T_sum_stat_df) <- T_sum_stat_df[1, ]

# View the updated data frame
View(T_sum_stat_df)

# we will use paired t test because there are two independent variable and two outcome. 

t.test(Glutamate_yield ~ Process, data = ferm)

#since p value is very small (0.004), we reject null hypothesis and we say
#there is significant difference, Glutamate yield in old process is higher than in new process.

t.test(Lysine_yield ~ Process, data = ferm)

#since p value is higher than the threshold, we accept null hypothesis and we say
#there is no significant difference, lysine yield in old process same as in new process.


mushroom = read.csv("mushroom_5k.csv", header = T, stringsAsFactors = T)

View(mushroom)

str(mushroom)

#features such as colour, water content, cap size and stem height.

ggplot(mushroom, aes(x=class,y=stem.height,fill=class)) +
  geom_boxplot() +
  labs(y="Stem Height (in mm)")
  theme_bw(base_size = 16)
  
ggplot(mushroom, aes(x=class,y=cap.diameter,fill=class)) +
  geom_boxplot() +
  labs(y="Cap diameter (in mm)")
  theme_bw(base_size = 16)
  
ggplot(mushroom, aes(x=class,y=water.content,fill=class)) +
  geom_boxplot() +
  labs(y="Water content (in %)")
  theme_bw(base_size = 16)
  

mushroom_stat = mushroom %>% 
  group_by(class) %>% 
  summarise(
    mean_sh=mean(stem.height),
    sd_sh=sd(stem.height),
    se_sh=sd(stem.height)/sqrt(n()),
    mean_cd=mean(cap.diameter),
    sd_sh=sd(cap.diameter),
    se_sh=sd(cap.diameter)/sqrt(n()),
    mean_wc=mean(water.content),
    sd_wc=sd(water.content),
    se_wc=sd(water.content)/sqrt(n())
    )
T_mushroom_stat = t(mushroom_stat)
View(T_mushroom_stat)

t.test(stem.height ~ class,mushroom) 

#There is a significant difference in stem height between edible and poisonous mushrooms, with a p-value < 2.2e-16.
#The mean stem height is greater in edible mushrooms (7.07 cm) compared to poisonous mushrooms (6.18 cm).


t.test(cap.diameter ~ class, mushroom)
#A significant difference exists in cap diameter between edible and poisonous mushrooms, with a p-value < 2.2e-16.
#The mean cap diameter is larger in edible mushrooms (8.07 cm) than in poisonous mushrooms (5.85 cm).


t.test(water.content ~ class, mushroom)

#There is no significant difference in water content between edible and poisonous mushrooms, with a p-value of 0.9642.
#The mean water content is nearly the same for edible (49.87%) and poisonous mushrooms (49.86%).

table(mushroom$class , mushroom$colour)

chisq.test(mushroom$class , mushroom$colour)

#p value is  p-value = 0.06057, we reject null hypothesis
# we can say mushroom color does not significantly influence weather mushroom is edible or not.

ggplot(mushroom,aes(x=stem.height,y=cap.diameter,color=class)) +
  geom_point() + 
  labs(title = "Association Between Stem Height And Cap Diameter ", x = "Stem Height (mm)", y = "Cap Diameter (mm)") +
  theme_classic(base_size=16)

cor.test(mushroom$stem.height, mushroom$cap.diameter)

#The correlation coefficient, r = 0.398, suggests a moderate positive association between stem height and cap diameter.
#meaning as stem height increases, cap diameter tends to increase as well.
#The p-value is extremely low (< 2.2e-16), which indicates that the correlation is statistically significant.
#Thus, we can reject the null hypothesis of no correlation.