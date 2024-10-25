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

m1 #here equation is y = 0.74x -1.33 

summary(m1) # R sq is 98% and p value is less than 0.05

#hence we say body size is a very good predictor of brain size in this dataset.

#A.1. after log transformation, the relationship between brain size and body size becomes more linear, 
# and the linear model fits the data well. 
