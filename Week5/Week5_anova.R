library(tidyverse)
library(dplyr)
library(ggplot2)

corncrake1 = read.csv("corncrake.csv", stringsAsFactors = T)

View(corncrake1)

ggplot(corncrake1, aes(x=Supplement,y=WeightGain)) +
  geom_boxplot(fill = "lightblue", color="black") +
  theme_bw(base_size = 16)

#according to the graph, allvit and control group have similar effect on weight gain,
#Earlybird supplement has caused weightgain followed by Sizefast diet and linseed.

cuttlefish <- read.csv("Cuttlefish.csv", stringsAsFactors = TRUE)

head(cuttlefish)

levels(cuttlefish$Time)

ggplot(cuttlefish, aes(x=Strikes)) +
  geom_histogram(binwidth = 1, fill = "lightblue", color="black") +
  facet_wrap(~Time) + 
  labs(title = "Distributions of Strikes by Time", x = "Strikes", y = "Frequency") +
  theme_bw(base_size = 16)

#according to the graph, as no of minutes increasing the no strikes decreases
#also, the graph for each timeline is very variable.


ggplot(cuttlefish, aes(x=Time,y=Strikes)) +
  geom_boxplot(fill = "lightgreen", color = "black") +
  labs(title = "Distributions of Strikes by Time", x = "Strikes", y = "Frequency") +
  theme_bw(base_size = 16)


festuca = read.csv("Festuca.csv", stringsAsFactors = T)

View(festuca)

head(festuca)
str(festuca)
summary(festuca)

festuca_model = lm(Weight ~ pH + Calluna + pH:Calluna, data = festuca)

plot(festuca_model, which = 3, add.smooth = F)

anova(festuca_model)

#by chatgpt 

interaction.plot(x.factor = festuca$pH, # x-axis: pH
                 trace.factor = festuca$Calluna, # lines: Calluna (trace factor)
                 response = festuca$Weight, # dependent variable: Weight
                 xlab = "Soil pH", # x-axis label
                 ylab = "Weight", # y-axis label
                 col = c("blue", "red"), # line colors
                 lty = 1:2, # line types
                 type = "b", # points and lines
                 pch = 19) # point type


#let's perform Post-hoc testing

festuca_aov <- aov(festuca_model)

TukeyHSD(festuca_aov, ordered = TRUE)
 
#according to the result, p value of ph values is very small, so ph do influence the weight.
#in this case, high ph is more preferable.
#absence of calluna increases the growth.
#Ph values plays bigger role than presence and absence of calluna

