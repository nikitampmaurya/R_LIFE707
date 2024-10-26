library(tidyverse)
library(dplyr)
library(ggplot2)


# Q.1.  plot the data and transform it, is this data suitable for linear analysis after transformation? 

df = read.csv("Braindata.csv", header = T, stringsAsFactors = T)

View(df)

head(df)

str(df)

summary(df)

ggplot(df, aes(x= Brain.size , y= Body.size), fill = Order) +
  geom_point() +
  labs(title = "Brain Size vs Body Size", x = "Body Size", y = "Brain Size") +
  theme_bw(base_size = 16)

#according to me points are too near to each other, let's do log transformation

ggplot(df, aes(x= log10(Brain.size) , y= log10(Body.size), fill = Order)) +
  geom_point() +
  labs(title = "Brain Size vs Body Size", x = "Body Size", y = "Brain Size") +
  theme_bw(base_size = 16) 

#now I see positive correlation

#let me fit the model

m1 = lm(log10(Brain.size) ~ log10(Body.size), data = df) 

m1 #here equation is (brain size) = [(0.74 x body size)  - 1.33] 

summary(m1) # R sq is 98% and p value is less than 0.05

#hence we say body size is a very good predictor of brain size in this dataset.
#as body size increases, brain size increases.

#A.1. after log transformation, the relationship between brain size and body size becomes more linear, 
# and the linear model fits the data well. 


df1 = read.table("monkeys.txt", header = T, stringsAsFactors = T)

View(df1)

head(df1)

str(df1) 

summary(df1)

ggplot(df1, aes(x= Group , y= Range), fill = Population) +
  geom_point() +
  labs(title = "Group vs Range", x = "Group", y = "Range") +
  theme_bw(base_size = 16)

#looks like there is some positive relationships

m2 = lm(Range ~ Group, df1)
m2
# Vmb = −3.4567 − 1.094 × diameter

summary(m2) #R sq is 49% and p is very small

#we say there is moderate relationship between group and range.

df2 = data.frame(Group = c(8,14,20,26))
View(df2)

predict(m2, df2)

# Range = −1.955 + 3.457 × Group, we can predict the range size for monkey groups of different sizes. 
#For groups of 8, 14, 20, and 26 individuals, the predicted range sizes are approximately 25.70, 46.44, 67.18, and 87.92 units, respectively.

m3 = lm(Range ~ Rainfall, df1)
m3

#Linear equation is Range = 6.5422 + 0.3389 × Rainfall

summary(m3) #R sq is 84.47% and P is too small

#we can say rainfall is good predictor for range.

df3 = read.csv("retinoblastoma_Y79_data.csv", header = T, stringsAsFactors = T)

View(df3)

head(df3)

str(df3) 

summary(df3)

ggplot(df3, aes(x= diameter , y= Vmb)) +
  geom_point() +
  labs(title = "Diameter vs  resting membrane potential", x = "Diameter (in micrometres)", y = "Resting membrane potential (in microvolts)") +
  theme_bw(base_size = 16)

#Seems like there is inverse relationship

m4 = lm(Vmb ~ diameter, df3)
m4 

#linear equation is Vmb = -1.094 * diameter - 6.205

summary(m4) #R is 69.56 % and p value is very small

# we say a significant inverse relationship between diameter and resting membrane potential (Vmb), with a larger diameter associated with a lower Vmb.


