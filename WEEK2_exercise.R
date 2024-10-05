library(tidyverse)

df = read.table("caffeine.txt", header=T)

View(df)

g1 = ggplot(data=df, aes(enzyme.ratio))

g1 + geom_histogram() +
  xlab("Enzyme ratio") +
  theme_bw()

#according to the graph, if you see the x axis there is high variability in enzyme ratios,
#moreover, data is not evenly distributed (it is not symmetrical)
#there are more count and higher peaks towards left so it is a left skewed distribution.
#there are outliers towards right end.


df1 = read.csv("pancreatic.csv", header=T)

View(df1)

g2 = ggplot(data=df1,aes(x=CA19.9,y=CA125,color=status))

g2 + geom_point()+
  xlab("CA19.9") + ylab("CA125") +
  scale_x_log10() + scale_y_log10() +
  theme_bw(base_size = 10) +
  facet_wrap(vars(status))


#according to the graph, the graph representing diease group is more scatter than control group.

#in the dieases group, the expression of both biomarkers has tremendously increased. 

#in the control group, the expression of CA125 is more than CA19.9, 

#in the dieases group, the expression of CA19.9 is sligthly more than CA125

#whereas in controlled state, the expression of both the biomarkers is lesser ,  biomarker CA125 is more expressed in control.

gg = ggplot(data=df1,aes(x=status,y=df1$CA19.9,color=status))

gg + geom_boxplot() +
  scale_y_log10() +
  theme_bw()

ggg = ggplot(data=df1,aes(x=status,y=df1$CA125,color=status))

ggg + geom_boxplot() +
  scale_y_log10() +
  theme_bw()

df2 = read.csv('plant_height.csv', header = T)

View(df2)
colnames(df2)

g3 = ggplot(data=df2,aes(x= growthform, y = height, color=growthform))

g3 + geom_boxplot() +
  scale_y_log10() +
  theme_bw(base_size = 16)

#according to the boxplot, all trees taller and their height is highly variable, whereas 
#herbs are smaller than shurb.

g4 = ggplot(data=df2,aes(x = rain, y = height, color = growthform))

g4 + geom_point() +
  scale_y_log10() +
  geom_smooth() 
  xlab("Rainfall (mm/year)") + ylab("Height (meters)") +
  theme_bw(base_size = 16)

#As rainfall increases, height increases

