# The data in spider.txt file are from an experiment where researchers removed a pedipalp from male spiders. 
# In males these are enlarged and used in mating. 
# They hypothesised that the pedipalp was a sexual handicap to the male spiders.
# They measured the speed (in cm/s) of spiders before and after amputation. 
# Download this to a folder that you can access. Look at it in a text editor (eg notepad in Windows or TextEdit in OS X). 
# Read it into R to create a dataframe object. View the first 6 lines. 
# Summarise the data. Produce a simple plot of spider speed before versus after amputation. 
# Is there a relationship between speed before and after amputation?
# Now set the xlim and ylim arguments to start the axes at zero and plot both axes on the scale. 
# From this, or the summary of the data above,
# is there an effect of amputation on spider speed? (In later topics we’ll show you how to conduct statistical tests.)

df = read.table("spider.txt",header= T) #load the file to read

View(df) 

df1 = head(df) #select first 6 rows

View(df1)

summary(df1) #summarize the data

plot(speed.after~speed.before,data=df1,xlim=c(0,5.5),ylim=c(0,5.5))

#according to the graph, speed increases after removing a pedipalp from male spiders. 
#it almost doubles in some cases.
#Also if you see, there are more points towards 
#the upper triangular part of graph
#which conveys that the speed does increase after removing the pedipalp.
