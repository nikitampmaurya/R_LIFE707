library(tidyverse)

df = read.table("pima.20.txt", header=T)

View(df)

summary(df)

df$test = factor(df$test) #to read summary of test column

summary(df)

df.negative = df[df$test == 'negative',] #all the columns with only negative rows

df.positive = df[df$test == 'positive',] #all the columns with only positive rows

m1 = mean(df.negative$glucose) #113.7
m1

m2 = mean(df.positive$glucose) #137.7
m2

#difference is 24 

s1 = sd(df.negative$glucose) # 22.56
s1

s2 = sd(df.positive$glucose) # 23.09
s2

e1 = s1/sqrt(10) #7.13
e1

e2 = s2/sqrt(10) #7.30
e2

t.test(glucose ~ test, data=df) #p value = 0.03036

#we accept the alternative hypothesis

#there is a significant difference between the mean of both groups

# For bmi for pima.20.txt


M1 = mean(df.negative$bmi) # 31.28
M1

M2 = mean(df.positive$bmi) # 34.92
M2

#difference is 3.64, positive > negative

S1 = sd(df.negative$bmi) # 7.64
S1

S2 = sd(df.positive$bmi) # 4.928556
S2

E1 = S1/sqrt(10) # 2.419082
E1

E2 = S2/sqrt(10) # 1.558546
E2

t.test(bmi ~ test, data=df) #p vlaue = 0.2247

#again we accept the null hypothesis and say there is no significant difference

# for bm in pima_cleaned.txt

#conclusion we need large sample size to have more accurate understanding!


df1 = read.csv("chickwts_edited.csv",header=T)

View(df1)

summary(df1)

df1$feed = factor(df1$feed)

summary(df1)

gg = ggplot(data=df1, aes(x=weight))

gg + geom_histogram(bins=15) +
  facet_wrap(~feed) +
  xlab("Weights") +
  theme_bw(base_size = 16)

gg + geom_density() +
  facet_wrap(~feed) +
  xlab("Weights") +
  theme_bw(base_size = 16)

ggg = ggplot(data=df1, aes(x=feed,y=weight))

ggg + geom_boxplot() +
  xlab("Feed") + 
  ylab("Chicken weight in grams") +
  theme_bw(base_size=16)

#Chicken that were fed sunflower had more weight than the chickens that were fed soybean.

t.test(weight ~ feed,data=df1) # p-value = 3.582e-10

#we reject null hypothesis and accept that there is a significant difference

data(iris) 

View(iris)

plot4 = ggplot(data=iris, aes(x=Petal.Length,y=Sepal.Length))

plot4 + geom_point() + 
  facet_wrap(~Species) +
  theme_gray(base_size = 15) +
  xlab("Petal length (cm)") + 
  ylab("Sepal length (cm)") 

iris.setosa = iris[iris$Species == "setosa",]

iris.versicolor = iris[iris$Species == "versicolor",]

iris.virginica = iris[iris$Species == "virginica",]

cor.test(iris.setosa$Sepal.Length, iris.setosa$Petal.Length)

cor.test(iris.versicolor$Sepal.Length, iris.versicolor$Petal.Length)

cor.test(iris.virginica$Sepal.Length, iris.virginica$Petal.Length)

#there is significant correlation between Sepal and petal length in versicolor and virginica but not in setosa

df4 = read.csv("pcrscreen.csv", header=T)

View(df4)

summary(df4)

df4$spiro = factor(df4$spiro)

df4$tryp = factor(df4$tryp)

summary(df4)

T = table(df4$spiro,df4$tryp)
T

mosaicplot(T)

chisq.test(T)

# There is no relationship between Spiroplasma infection and trypanosomatid infection in fruit flies.
#meaning i don't think Spiroplasma protects fruit flies from trypansomatid infection.
