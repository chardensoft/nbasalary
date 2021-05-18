library(tidyverse) #for graphs
library(ggfortify) #for some fancy graph stuff
library(car) #for analysis
library(readxl) #to read specific sheets
library(corrplot) #for correlation stuff
setwd("~/Desktop/Stats/IS 590R/nbasalary") #or wherever you have the file

#Download all the data from the excel sheet
#teams <- read_excel("NbaSalary.xlsx", sheet = "18-19 Team")
#salaries <- read_excel("NbaSalary.xlsx", sheet = "18-19 Salary")
comb <- read_excel("NbaSalary.xlsx", sheet = "Combined")
comb <- comb[c(1:6, 36:40)] #taking out data I'm not using. First 5 columns are maybe numbers we'll try to predict, 
#next 5 columns are the salary stuff that could be predictive.
practice <- comb[c(3,7:11)]

summary(practice)

plot(practice, pch = 16)
#based on this plot, it looks like one of total or avg salary will be good to keep, 
#along with potentially one of median or max salary. Min salary looks pretty useless

round(cor(practice), 2)
corrplot(cor(practice), type = "upper")
#after checking this out, I'd pick avg salary, still not sure between median and max. maybe both?

##See NBA_Salary Assumptions.R to see assumptions checking

#first model, haven't checked assumptions on it yet.
practice.lm <- lm(NRtg ~ ., data = practice)
summary(practice.lm)
practice$residuals <- practice.lm$residuals
practice$fitted.values <- practice.lm$fitted.values
#model tells us that avg salary is significant and nothing else. I was surprised by the nothing else.
#we've definitely got some multicollinearity going on, cause total salary shouldn't be negative 
#based on the graph and neither should max.
#Also, our F-test shows that our model is significant since p-value is .01369. 
#R-squared values show not a ton of explanatory power though.






