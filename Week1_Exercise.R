df = read.table("spider.txt",header= T) #load the file to read

View(df) 

df1 = head(df) #select first 6 rows

View(df1)

summary(df1) #summarize the data

plot(speed.after~speed.before,data=df1,xlim=c(0,5.5),ylim=c(0,5.5))

#according to the graph, speed increasing after removing a pedipalp from male spiders. 
#it almost doubles in some cases.
#Also if you see, there are more points towards 
#the upper triangular part of the graph
#which conveys the speed does increase after removing pedipalp.
