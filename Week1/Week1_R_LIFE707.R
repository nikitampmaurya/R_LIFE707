# Q. The file spider.txt is on Canvas. The data here are from an experiment where researchers removed a
# pedipalp from male spiders. In males these are enlarged and used in mating. They hypothesised that the
# pedipalp was a sexual handicap to the male spiders They measured the speed (in cm/s) of spiders before
# and after amputation. Download this to a folder that you can access. Look at it in a text editor (eg notepad in
# Windows or TextEdit in OS X). Read it into R to create a dataframe object. View the first 6 lines. Summarise
# the data. Produce a simple plot of spider speed before versus after amputation. Is there a relationship
# between speed before and after amputation? Now set the xlim and ylim arguments to start the axes at
# zero and plot both axes on the scale. From this, or the summary of the data above, is there an effect of
# amputation on spider speed?


df = read.table("spider.txt",header= T) #load the file to read

View(df) 

head(df) #viewing first few lines 

summary(df) #summarize the data

plot(speed.after~speed.before,df,xlim=c(0,5.5),ylim=c(0,5.5))

#Since more data points are clustered in the top section of the graph, 
# we can speed does increases after removing a pedipalp from male spiders. 
