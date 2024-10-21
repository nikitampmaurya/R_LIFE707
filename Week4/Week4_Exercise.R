
# Q1. Produce a table of estimates for the mean and variance of both sepal lengths and widths, within each species.

library(tidyverse) #Manipulating data using this library

data(iris) #loading 

View(iris) 

df = iris %>% group_by(Species) %>%
  summarise(Mean__of_Length = mean(Sepal.Length), Variance_by_Length = var(Sepal.Length),
            Mean_of_Width = mean(Sepal.Width), Variance_by_Width = var(Sepal.Width))

View(df)

#Q2. Now we can begin to summarise the data. 
#Can you produce a mean GDP for each country, averaging over years. 
#In this case we can think of “grouping” the data by country and then averaging the GDP values within each group (as we have seen before).

df1 = read_csv("indicator gapminder gdp_per_capita_ppp.csv")

View(df1)

df2 = df1 %>% 
  rename(Country = names(df1)[1]) %>%
  gather(Year,GDP,-Country) 

View(df2)

str(df2)

df2 = df2 %>% 
  mutate(Year = as.numeric(Year))

str(df2)

df2 = df2 %>%
  filter(!is.na(GDP))

summary(df2)


#A2: now to calculate mean GDP for each country

Mean_GDP_by_Country <- df2 %>%
  group_by(Country) %>%
  summarise(Mean_GDP = mean(GDP))

View(Mean_GDP_by_Country)


#Q3. Now try to produce the mean GDP for each year, averaged across country.


#A3: Calulcating the mean GDP for each year

Mean_GDP_by_Year <- df2 %>%
  group_by(Year) %>%
  summarise(Mean_GDP_by_Year = mean(GDP, na.rm = TRUE))

View(Mean_GDP_by_Year)


df3 = read_csv("indicator hiv estimated prevalence% 15-49.csv")

View(df3)

#Q.5  Load in the file “indicator hiv estimated prevalence% 15-49.csv”. 
#This file contains the estimated HIV prevalence in people of age 15–49 in different countries over time. 
#Prevalence is defined here to be the estimated number of people living with HIV per 100 population.
# Produce a tidy data set called gp_hiv using the tools in tidyverse that we introduced above.
# The dataset needs to run from 1991 onwards, and we want to end up with columns country, year and prevalence. 
# [Note that a couple of the years have no values in the data set, and by default R reads these columns in as character columns. 
# Hence when you gather() the data to create a prevalence column, all the numbers will be converted into characters.
# One way to deal with this is to convert the column back into numbers once you have filtered away all the stuff!]

df4 = df3 %>%
  rename(Country = names(df3)[1]) %>%
  gather(Year, Prevalence,-Country) %>%
  mutate(Year = as.numeric(Year)) %>%
  filter(!is.na(Prevalence)) %>%
  filter(Year > 1990)
  
str(df4)

View(df4)

summary(df4)
