
# Q. The file caffeine.txt contains the values of the urinary metabolic ratio of 5–acetylamino–6–formylamino–3–
# methyluracil to 1–methylxanthine (AFMU/1X) after oral administration of caffeine. Plot a histogram of the
# data and comment on its distribution.

library(tidyverse)

df = read.table("caffeine.txt", header=T)

View(df)

g1 = ggplot(data=df, aes(enzyme.ratio))

g1 + geom_histogram(bins = 25, fill = 'skyblue',color="black", alpha = 0.5) +
  xlab("Enzyme ratio") +
  ylab("Frequency") +
  theme_bw(base_size = 16)

#there is high variability in enzyme ratios, data is not evenly distributed (it is not symmetrical)
#there are more count and higher peaks towards left so it is a left skewed distribution.
#there are outliers towards right end.

# Q. The file pancreatic.csv contains the concentrations of 2 biomarkers, CA19-9 and CA125 (in U/ml), in control
# (healthy) and diseased (diagnosed with pancreatic cancer) individuals. Plot these data to investigate the
# relationship of each biomarker to disease state.

df1 = read.csv("pancreatic.csv", header=T)

View(df1)

g2 = ggplot(data=df1,aes(x=CA19.9,y=CA125,color=status))

g2 + geom_point()+
  xlab("CA19.9") + ylab("CA125") +
  scale_x_log10() + scale_y_log10() +
  theme_bw(base_size = 10) +
  facet_wrap(vars(status))


#data points of diease group are way more scatter than data points of control group.

#in the dieases group, the expression of both biomarkers has tremendously increased. 

#in the control group, the expression of CA125 is more than CA19.9, 

#in the dieases group, the expression of CA19.9 is sligthly more than CA125

#whereas in controlled state, the expression of both the biomarkers is lesser ,  biomarker CA125 is more expressed in control.

gg = ggplot(data=df1,aes(x=status,y=df1$CA19.9,color=status))

gg + geom_boxplot() +
  xlab("Groups") +
  ylab("Biomarker CA19.9") +
  scale_y_log10() +
  theme_bw(base_size = 16)

# higher expression of biomarker CA19.9 in dieases group compared to control group. 

ggg = ggplot(data=df1,aes(x=status,y=df1$CA125,color=status))

ggg + geom_boxplot() +
  xlab("Groups") +
  ylab("Biomarker CA125") +
  scale_y_log10() +
  theme_bw(base_size = 16)

# Q. The file plant_height.csv contains data on the heights of different plants, plus various ecological factors from
# where grow. First, produce a plot to compare the height (in metres) of different growthforms (herb, shrub or tree).
# Second, produce a plot of how the height of these different growthforms varies with rain (the column ‘rain’, in
# mm/yr). Label the axes appropriately. What does geom_smooth() do? Try adding it to your plot.

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

