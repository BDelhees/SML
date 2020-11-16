### SML Final Project
### 01 Data import and manipulation



## Needed Packages

library(tidyverse)



## Get data
#https://github.com/vitorkrasniqi/Real_Estate_Analytics

getwd()
setwd("C:/Users/budde/OneDrive/UniLU MA/Semester 3/Supervised ML/Final Project")


df <- read.csv("training.csv", sep = ",", header = TRUE)

str(df)



## Data manipulation


# Rent is y-variable, kick out obsv. which do not have any values

df_NA <- subset(df, is.na(df$rent_full))

# No NAs in the y-variable








## Descriptive Analysis

head(df)
tail(df)

typeof(df)
str(df)
