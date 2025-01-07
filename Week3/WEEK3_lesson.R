library(tidyverse)

pima = read.table("pima_cleaned.txt", header=T)

View(pima)

summary(pima) #test is character

pima$test = factor(pima$test) #to count no of + and - 

summary(pima)

p1 = ggplot(data=pima, aes(x=pima$glucose))

p1 + geom_histogram() +
  facet_wrap(~test) +
  xlab("Concentration of Glucose (mg/dl)") +
  theme_bw(base_size = 16)

pima.positive = pima[pima$test == "positive",]

pima.negative = pima[pima$test == "negative",]

summary(pima.negative)

summary(pima.positive)

mean(pima.positive$glucose)

mean(pima.negative$glucose)

sd(pima.positive$glucose)

sd(pima.negative$glucose)

sd(pima.positive$glucose)/sqrt(248)

sd(pima.negative$glucose)/sqrt(475)

t.test(glucose ~ test, data=pima)

#here we checked whether the glucose levels are different between both the groups. 

#Since we got extremely small p value, we reject the null hypothesis.

#We say that there is a significantly difference in glucose level of both groups.

mean(pima.positive$bmi) # 35.21653

mean(pima.negative$bmi) # 30.97495

# difference is more than 5, positive > negative 

sd(pima.positive$bmi) # 6.422097

sd(pima.negative$bmi) # 6.571093



sd(pima.positive$bmi)/sqrt(248) # 0.4078036

sd(pima.negative$bmi)/sqrt(475) # 0.3015024

t.test(bmi ~ test, data=pima) # p value = 5.826e-16

df2 = read.csv("possum.csv", header=T)

View(df2)

plot1 = ggplot(data=df2, aes(x=hdlngth,y=totlngth))

plot1 + geom_point() +
  xlab("Head length (mm)") + ylab("Total length(cm)")

plot2 = ggplot(data=df2, aes(x=hdlngth,y=skullw))

plot2 + geom_point() +
  xlab("Head length (mm)") + ylab("Skull width (mm)")

plot3 = ggplot(data=df2, aes(x=hdlngth,y=taill))

plot3 + geom_point() +
  xlab("Head length (mm)") + ylab("Tail length (cm)")


cor(df2$hdlngth, df2$skullw)

cor.test(df2$hdlngth, df2$skullw)

cor(df2[,c("hdlngth","totlngth","skullw","taill")])


df3 = read.csv("barley.csv")

View(df3)

table(df3$genotype,df3$status)

chisq.test(df3$genotype,df3$status)

#there is there is association between the genotype and the likelihood of infection


