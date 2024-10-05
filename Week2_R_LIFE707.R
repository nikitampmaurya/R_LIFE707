library(tidyverse)

vertebrate_age = read.csv("vertebrate_age.csv") #to load data

View(vertebrate_age)

summary(vertebrate_age$Kingdom) #to get statistics summary of a random column

vertebrate_age$Class = factor(vertebrate_age$Class) #to store Class columns as categorical data

summary(vertebrate_age$Class) #to see statistics of categorical data

names(vertebrate_age) 

plot(Male_maturity ~ Female_maturity, data = vertebrate_age, xlab = "Female Maturity (in days)", ylab = "Male Maturity (in days)", log = "xy")

#xlab to change the labels

#log = xy to take log of each values.

plot(Litters_pa ~ Female_maturity, data = vertebrate_age, xlab = 'Female Maturity (in days)', ylab = "Litters per year")

#According to the graph, younger female tend to have more litters per year.

plot(Female_maturity ~ Class, data = vertebrate_age, ylab = "Female Maturity (in days)")

#according to the graph, Reptilia has highest median for female maturity meaning more than 50% of Reptilia take more time to get sexually matured
#and also there's more variability in the maturation age.
#whereas Aves(bird) have the lowest median which indicates they become mature faster than mammalia and amphibian.

plot(Male_maturity ~ Class, data = vertebrate_age, ylab = "Male Maturity (in days)")

#similar pattern has been observed in males too.

p1 = ggplot(data=vertebrate_age, aes(x=Female_maturity,y=Male_maturity))

p1

#aes informs R to add x abd y axes

p1 + geom_point() #to add points

p1 + geom_point() +
  xlab("Female maturity (days)") + ylab("Male maturity (days)") +
  scale_x_log10() + scale_y_log10() +
  theme_bw()

#white background, grey grud and border line

p1 + geom_point() +
  xlab("Female maturity (days)") + ylab("Male maturity (days)") +
  scale_x_log10() + scale_y_log10() +
  theme_minimal()

#same as above but without border

p1 + geom_point() +
  xlab("Female maturity (days)") + ylab("Male maturity (days)") +
  scale_x_log10() + scale_y_log10() +
  theme_bw(base_size=16)

#same as first plot, with larger labels.

p2 = ggplot(data = vertebrate_age, aes(x=Female_maturity,y=Litters_pa))

p2 + geom_point() +
  xlab("Female maturity (in days)") + ylab("Litters per year") +
  scale_x_log10() + scale_y_log10() +
  theme_bw(base_size = 16)

p3 = ggplot(data=vertebrate_age, aes(x=Class,y=Female_maturity))

p3 + geom_boxplot() +
  ylab("Female Maturity") +
  theme_bw(base_size = 16)

p4 = ggplot(data=vertebrate_age, aes(x=Female_maturity, y=Male_maturity, color=Class))

p4 + geom_point() +
  xlab("Female Maturity") + ylab("Male Maturity") +
  scale_x_log10() + scale_y_log10() +
  theme_bw(base_size = 16) +
  facet_wrap(vars(Class)) 

p5 = ggplot(data=vertebrate_age, aes(x=Maximum_longevity,y=Female_maturity))

p5 + geom_smooth() +
  xlab("Maximum longevity (in years)") + ylab("Female Maturity") +
  theme_bw(base_size = 16)

#according to the graph, organism with higher longevity reach their sexual maturity at later years.

pima = read.table("pima.txt",header=T)

View(pima)

h1 = ggplot(data=pima, aes(x=diastolic))

h1 + geom_histogram(bins=12)

#adding bins = 12 changed the data presentation and not data

#bins=12 divided diastolic blood pressure into 12 equal parts.

#increasing bins will give detailed distribution.

h1 + geom_density()

#now y-axis is labelled density and not count

#also here he mean and the median are equivalent and the median is between the 1st and 3rd quartiles.

h2 = ggplot(data=pima, aes(x=insulin)) 

h2 + geom_histogram(bins=30)

h2 + geom_density()

names(pima) 

h3 = ggplot(data=pima, aes(x=bmi)) 

h3 + geom_histogram()

h4 = ggplot(data=pima, aes(x=glucose)) 

h4 + geom_density()
