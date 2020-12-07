############################### SML Final Project




### Data import and manipulation




## Needed Packages

library(tidyverse)
library(plyr)




## Get data


getwd()
setwd("...")


df <- read.csv("training.csv", sep = ",", header = TRUE)

str(df)

dim(df)

names(df)






### Data cleaning and manipulation




# Rent is y-variable, kick out rows which do not have any obvs.

df_NA <- subset(df, is.na(df$rent_full)) # No NAs in the y-variable

view(df_NA)



# Check NAs of the other variables

na_count <-sapply(df, function(y) sum(length(which(is.na(y)))))

na_count <- data.frame(na_count)




# Replace NAs with 0 for all binary variables



which( colnames(df)=="balcony" ) #get column number
which( colnames(df)=="cheminee" )

df[, 8:16][is.na(df[, 8:16])] <- 0 #replace NAs with 0


which( colnames(df)=="dishwasher" )
which( colnames(df)=="heating_pellets" )

df[, 19:32][is.na(df[, 19:32])] <- 0


which( colnames(df)=="kids_friendly" )

df[, 34][is.na(df[, 34])] <- 0


which( colnames(df)=="laundry" )

df[, 36][is.na(df[, 36])] <- 0


which( colnames(df)=="manlift" )
which( colnames(df)=="minergie" )

df[, 38:40][is.na(df[, 38:40])] <- 0


which( colnames(df)=="new_building" )

df[, 43][is.na(df[, 43])] <- 0


which( colnames(df)=="oldbuilding" )
which( colnames(df)=="public_transport" )

df[, 45:52][is.na(df[, 45:52])] <- 0


which( colnames(df)=="quiet" )
which( colnames(df)=="raised_groundfloor" )

df[, 55:56][is.na(df[, 55:56])] <- 0


df[, which( colnames(df)=="shared_flat" ):which( colnames(df)=="wheelchair" )][is.na(df[, which( colnames(df)=="shared_flat" ):which( colnames(df)=="wheelchair" )])] <- 0 #faster approach







# Drop useless variables without values

sum(!is.na(df$appartments))

df$appartments <- NULL

df$descr <- NULL

df$area_useable <- NULL


table(df$bright) #check frequency 


table(df$bath_tube)
df$bath_tube <- NULL


table(df$building_plot)
df$building_plot <- NULL


table(df$cabletv)


table(df$cheminee)


table(df$dishwasher)


#apply(df[c(6:12, 14:16, 18:27, 29, 31, 33:35, 38:47, 50:51, 54:64)], 2, table) #Found an easier approach


df$ceiling <- NULL

df$garden_m2 <- NULL

df$minergie <- NULL

df$pets <- NULL

df$public_transport <- NULL

df$shared_flat <- NULL

df$shopping <- NULL

df$wheelchair <- NULL

df$manlift <- NULL

df$GDENAMK <- NULL

df$GDENR <- NULL

df$address <- NULL

df$date <- NULL

df$year <- NULL

df$gardenshed <- NULL

df$oven <- NULL

df$veranda <- NULL


# Create factors out of binary variables

df <- subset(df, df$rooms < 10) #delete observation which values only occur once, muddies the water

df$rooms <- round_any(df$rooms,0.5) #round rooms (no values except .0 and .5 are allowed)

df$rooms <- as.factor(df$rooms)

df$floors <- as.factor(df$floors)

df$balcony <- as.factor(df$balcony)

df$basement <- as.factor(df$basement)

df$bath <- as.factor(df$bath)

df$bright <- as.factor(df$bright)

df$cabletv <- as.factor(df$cabletv)

df$cheminee <- as.factor(df$cheminee)

df$dishwasher <- as.factor(df$dishwasher)

df$dryer <- as.factor(df$dryer)

df$elevator <- as.factor(df$elevator)

df$floors <- as.factor(df$floors)

df$furnished <- as.factor(df$furnished)

#df$gardenshed <- as.factor(df$gardenshed) this variable produces an error regression which includes all variables

df$heating_air <- as.factor(df$heating_air)

df$heating_earth <- as.factor(df$heating_earth)

df$heating_electro <- as.factor(df$heating_electro)

df$heating_far <- as.factor(df$heating_far)

df$heating_gas <- as.factor(df$heating_gas)

df$heating_oil <- as.factor(df$heating_oil)

df$heating_pellets <- as.factor(df$heating_pellets)

df$kids_friendly <- as.factor(df$kids_friendly)

df$laundry <- as.factor(df$laundry)

df$middle_house <- as.factor(df$middle_house)

df$new_building <- as.factor(df$new_building)

df$newly_built <- as.factor(df$newly_built)

df$oldbuilding <- as.factor(df$oldbuilding)

#df$oven <- as.factor(df$oven) this variable produces an error regression which includes all variables

df$parking_indoor <- as.factor(df$parking_indoor)

df$parking_outside <- as.factor(df$parking_outside)

df$playground <- as.factor(df$playground)

df$pool <- as.factor(df$pool)

df$quiet <- as.factor(df$quiet)

df$raised_groundfloor <- as.factor(df$raised_groundfloor)

df$shower <- as.factor(df$shower)

df$sunny <- as.factor(df$sunny)

df$terrace <- as.factor(df$terrace)

df$toilets <- as.factor(df$toilets)

df$topstorage <- as.factor(df$topstorage)

#df$veranda <- as.factor(df$veranda) this variable produces an error regression which includes all variables

df$water <- as.factor(df$water)

df$month <- as.factor(df$month)

df$msregion <- as.factor(df$msregion)

df$quarter_general <- as.factor(df$quarter_general)





## Further Data manipulation


min(df$area, na.rm = T)
max(df$area, na.rm = T)
mean(df$area, na.rm = T)
view(subset(df$area, df$area < 10))
df <- subset(df, df$area >= 8) #delete observations which are less than 8 square meters


view(subset(df, df$area > 400)) #these observations appear to have wrong data entries, delete them
df <- subset(df, df$area < 400)





## Split the dataset, 80:20 approach

# set seed

set.seed(123)

train.size = dim(df)[1] * 0.8
train = sample(1:dim(df)[1], train.size)
test = -train
df.train = df[train, ]
df.test = df[test, ]




### Descriptive Analysis

head(df)
tail(df)

typeof(df)
str(df)

hist(df$rent_full, breaks = 100, xlab = "Rent in CHF", col = "red", main = "Histogram Rent")
summary(df$rent_full)


#summary continous variables

summary(df[, c("rent_full", "area", "rooms", "year_built", "Micro_rating", "Anteil_auslaend", "Avg_age", "Avg_size_household",
               "Noise_max", "anteil_efh", "apoth_pix_count_km2", "avg_anzhl_geschosse", "avg_bauperiode", "dist_to_4G",
               "dist_to_5G", "dist_to_haltst", "dist_to_highway", "dist_to_lake", "dist_to_main_stat",
               "dist_to_school_1", "dist_to_train_stat", "dist_to_river", "geb_wohnnutz_total", "max_guar_down_speed",
               "restaur_pix_count_km2", "superm_pix_count_km2", "wgh_avg_sonnenklasse_per_egid.1")])


summary <- as.data.frame(summary(df[, c("rent_full", "area", "year_built", "Micro_rating", "Anteil_auslaend", "Avg_age", "Avg_size_household",
                                        "Noise_max", "anteil_efh", "apoth_pix_count_km2", "avg_anzhl_geschosse", "avg_bauperiode", "dist_to_4G",
                                        "dist_to_5G", "dist_to_haltst", "dist_to_highway", "dist_to_lake", "dist_to_main_stat",
                                        "dist_to_school_1", "dist_to_train_stat", "dist_to_river", "geb_wohnnutz_total", "max_guar_down_speed",
                                        "restaur_pix_count_km2", "superm_pix_count_km2", "wgh_avg_sonnenklasse_per_egid.1")]))

view(summary)



## Correlation matrices for the continous variables

# SOURCE: https://www.kaggle.com/skirmer/fun-with-real-estate-data


library(corrplot)



which( colnames(df)=="rent_full" )
which( colnames(df)=="area" )
which( colnames(df)=="msregion" )
which( colnames(df)=="rooms" )
which( colnames(df)=="year_built" )
which( colnames(df)=="Micro_rating" )
which( colnames(df)=="wgh_avg_sonnenklasse_per_egid" )


correlations1 <- cor(df[, c("rent_full", "area", "year_built", "Micro_rating", "Anteil_auslaend", "Avg_age", "Avg_size_household",
                            "Noise_max", "anteil_efh", "apoth_pix_count_km2", "avg_anzhl_geschosse", "avg_bauperiode", "dist_to_4G",
                            "dist_to_5G", "dist_to_haltst", "dist_to_highway", "dist_to_lake", "dist_to_main_stat", "Micro_rating",
                            "dist_to_school_1", "dist_to_train_stat", "dist_to_river", "geb_wohnnutz_total", "max_guar_down_speed",
                            "restaur_pix_count_km2", "superm_pix_count_km2", "wgh_avg_sonnenklasse_per_egid.1")], use="complete.obs")
corrplot(correlations1, method="circle", type="lower",  sig.level = 0.01, insig = "blank")





## Tables for the binary variables
# SOURCE: https://uc-r.github.io/descriptives_categorical


# Frequencies & Proportions

library(tidyr)
library(ggplot2)
library("questionr")
library(dplyr)

table(df$balcony)
prop.table(table(df$balcony))

table(df$basement)
prop.table(table(df$basement))

table(df$bath)
prop.table(table(df$bath))

table(df$rooms)


  

#Frequencies for all binary 0-1 variables

table(stack(df, c("balcony",  "basement", "bath", "bright" ,"cabletv", "cheminee", "dishwasher",
                 "dryer", "elevator", "furnished", "gardenshed", "heating_air", "heating_earth",
                  "heating_electro", "heating_far", "heating_gas", "heating_oil", "heating_pellets",
                  "kids_friendly","laundry","middle_house","new_building","newly_built","oldbuilding",
                  "oven","parking_indoor","parking_outside","playground","pool","quiet", "raised_groundfloor",
                  "shower","sunny", "terrace", "toilets", "topstorage","veranda","water")), useNA = "always")




# Bar charts


ggplot(df, aes(x = `home_type`)) +
  geom_bar() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ylab("Count") +
  xlab("Home Type")


ggplot(df, aes(x = `rooms`)) +
  geom_bar() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ylab("Count") +
  xlab("Number of Rooms")

ggplot(df, aes(x = `KTKZ`)) +
  geom_bar() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ylab("Count") +
  xlab("Canton")



## Plot variables which correlate with rent



plot(df$area, df$rent_full,
     xlab = "Area in squared meters",
     ylab = "Rent in CHF",
     main = "Rent & Area",
     pch = ".",
     col="darkblue"
)

plot( df$rooms, df$rent_full,
     xlab = "Number of rooms",
     ylab = "Rent in CHF",
     main = "Rent & Rooms",
     pch = ".",
     col="darkblue"
)



plot(df$Micro_rating, df$rent_full, 
     xlab = "Location Microrating",
     ylab = "Rent in CHF",
     main = "Rent & Location Rating",
     pch = ".",
     col="darkblue"
)


plot(df$year_built, df$rent_full,
     xlab = "Year built",
     ylab = "Rent in CHF",
     main = "Rent & Year built",
     pch = ".",
     col="darkblue"
)


plot(df$dist_to_5G, df$rent_full, 
     xlab = "Distance to 5G in meters",
     ylab = "Rent in CHF",
     main = "Rent & Distance to 5G",
     pch = ".",
     col="darkblue"
)



plot(df$dist_to_lake, df$rent_full, 
     xlab = "Distance to lake in meters",
     ylab = "Rent in CHF",
     main = "Rent & Distance to lake",
     pch = ".",
     col="darkblue"
)


plot(df$KTKZ, df$rent_full, 
     xlab = "Distance to lake in meters",
     ylab = "Rent in CHF",
     main = "Canton",
     pch = ".",
     col="white"
)





### Models




## Regression models

# simple regression models
# Going through most of the variables which were excluded from the correlation matrices


#Rent & Area

summary(lm(rent_full~area, df.train)) #quick summary for basic linear regressions, *** significance



#Rent & Appartment type

summary(lm(rent_full~home_type, df.train)) #***



#Rent & Rooms

summary(lm(rent_full~rooms, df.train)) #mostly ***



# Multiple regression model with Rooms and area

summary(lm(rent_full~area+rooms, df.train)) #mostly ***



#Rent & month

summary(lm(rent_full~month, df.train)) #*




## Location regressions

#Rent & msregion

summary(lm(rent_full~msregion, df.train)) #** but low coefficient


#Rent & Longitude/Latitude

summary(lm(rent_full~lon+lat, df.train)) #*** but low coefficient



#Rent & Cantons

summary(lm(rent_full~KTKZ, df.train)) #mostly ***


## Time regressions

#Rent & Quarters

summary(lm(rent_full~quarter_general, df.train)) #**


#Rent & Month

summary(lm(rent_full~quarter_specific, df.train)) #Variable doesnt really make sense




## Predictions: Simple Regression 

# http://www.sthda.com/english/articles/38-regression-model-validation/157-cross-validation-essentials-in-r/
library(olsrr)
library(caret)
library(tidyverse)
library(Metrics)


model.s <- lm(rent_full ~ area, data = df.train)


p.s <- model.s %>% predict(df.test)
data.frame( R2 = R2(p.s, df.test$rent_full),
            RMSE = RMSE(p.s, df.test$rent_full),
            MAE = MAE(p.s, df.test$rent_full),
            rae = rae(p.m, df.test$rent_full))

# R2 = 0.4792411   RMSE = 493.7145   MAE =339.1548   RAE= 0.8428427


RMSE(p.s, df.test$rent_full)/mean(df.test$rent_full)

# RMSE = 0.2866804

view(p.s)

BIC(model.s) # BIC = 862224.4
AIC(model.s) # AIC = 862197.6




### Multiple Regression Models



options(max.print=1000000) #Shows the whole output in console

# Factor variables

summary(lm(rent_full~KTKZ +balcony+  basement+ bath+ bright +cabletv+ cheminee+ dishwasher+
           dryer+ elevator+ furnished+ heating_air+ heating_earth+
           heating_electro+ heating_far+ heating_gas+ heating_oil+ heating_pellets+
           kids_friendly+laundry+middle_house+new_building+newly_built+oldbuilding+
           parking_indoor+parking_outside+playground+pool+quiet+ raised_groundfloor+
           shower+sunny+ terrace+ toilets+ topstorage+water+msregion+rooms, df.train))

# *** bright, cabletv, cheminee, elevator, heating_oil, kids_friendly, newly_built, old_building
# *** parking_indoor, parking_outside, raised_groundf.trainloor, terrace, toilets

# ** balcony

# * bath, dryer, furnished, heating_oil

# adj. R2 = 0.6031




# Continous variables

summary(lm(rent_full~area+ year_built+ Micro_rating+ Anteil_auslaend+ Avg_age+ Avg_size_household+
             Noise_max+ anteil_efh+ apoth_pix_count_km2+ avg_anzhl_geschosse+ avg_bauperiode+ dist_to_4G+
             dist_to_5G+ dist_to_haltst+ dist_to_highway+ dist_to_lake+ dist_to_main_stat+ Micro_rating+
             dist_to_school_1+ dist_to_train_stat+ dist_to_river+ geb_wohnnutz_total+ max_guar_down_speed+
             restaur_pix_count_km2+ superm_pix_count_km2+ wgh_avg_sonnenklasse_per_egid.1, df.train))

# adj. R2 0.6273



# All variables
summary(lm(rent_full~ ., df))
summary(lm(rent_full~ ., df.train)) #This does not work due to some factor variables


## Manual backward selection
# Combine binary and continous regressions

summary(lm(rent_full~KTKZ+area+ year_built+ Micro_rating+ Anteil_auslaend+ Avg_age+ Avg_size_household+
             Noise_max+ anteil_efh+ apoth_pix_count_km2+ avg_anzhl_geschosse+ avg_bauperiode+ dist_to_4G+
             dist_to_5G+ dist_to_haltst+ dist_to_highway+ dist_to_lake+ dist_to_main_stat+
             dist_to_school_1+ dist_to_train_stat+ dist_to_river+ geb_wohnnutz_total+ max_guar_down_speed+
             restaur_pix_count_km2+ superm_pix_count_km2+ wgh_avg_sonnenklasse_per_egid.1++balcony+
             basement+ bath+ bright +cabletv+ cheminee+ dishwasher+dryer+ elevator+ furnished+ heating_air+
             heating_earth+heating_far+ heating_gas+ heating_oil+ heating_pellets+
             kids_friendly+laundry+middle_house+new_building+newly_built+oldbuilding+
             parking_indoor+parking_outside+playground+pool+quiet+ raised_groundfloor+
             shower+sunny+ terrace+ toilets+ topstorage+water+msregion+rooms, df.train))

BIC(lm(rent_full~KTKZ+area+ year_built+ Micro_rating+ Anteil_auslaend+ Avg_age+ Avg_size_household+
         Noise_max+ anteil_efh+ apoth_pix_count_km2+ avg_anzhl_geschosse+ avg_bauperiode+ dist_to_4G+
         dist_to_5G+ dist_to_haltst+ dist_to_highway+ dist_to_lake+ dist_to_main_stat+
         dist_to_school_1+ dist_to_train_stat+ dist_to_river+ geb_wohnnutz_total+ max_guar_down_speed+
         restaur_pix_count_km2+ superm_pix_count_km2+ wgh_avg_sonnenklasse_per_egid.1++balcony+
         basement+ bath+ bright +cabletv+ cheminee+ dishwasher+dryer+ elevator+ furnished+ heating_air+
         heating_earth+heating_far+ heating_gas+ heating_oil+ heating_pellets+
         kids_friendly+laundry+middle_house+new_building+newly_built+oldbuilding+
         parking_indoor+parking_outside+playground+pool+quiet+ raised_groundfloor+
         shower+sunny+ terrace+ toilets+ topstorage+water+msregion+rooms, df.train))

## Exclude all variables which are less than ** p-values


summary(lm(rent_full~KTKZ+area+ Micro_rating+ Anteil_auslaend+ Avg_age+ Avg_size_household+
             avg_anzhl_geschosse+ avg_bauperiode+dist_to_5G+ dist_to_haltst+ 
             dist_to_lake+ dist_to_main_stat+ dist_to_train_stat+
             superm_pix_count_km2+ cabletv+ cheminee+ elevator+newly_built+
             parking_indoor+ terrace+msregion+rooms, df.train))


## Same as above but KTKZ instead of msregion

summary(lm(rent_full~KTKZ+area+ Micro_rating+ Anteil_auslaend+ Avg_age+ Avg_size_household+
             avg_anzhl_geschosse+ avg_bauperiode+dist_to_5G+ dist_to_haltst+ 
             dist_to_lake+ dist_to_main_stat+ dist_to_train_stat+
             superm_pix_count_km2+ cabletv+ cheminee+ elevator+newly_built+
             parking_indoor+ terrace+ rooms, df.train))

# adj. R^2 = 0.6832

## Same as above but msregion instead of KTKZ, which has higher R2

summary(lm(rent_full~area+ Micro_rating+ Anteil_auslaend+ Avg_age+ Avg_size_household+
             avg_bauperiode+dist_to_5G+ dist_to_haltst+ 
             dist_to_lake+ dist_to_main_stat+ dist_to_train_stat+
             superm_pix_count_km2+ cabletv+ cheminee+ elevator+newly_built+
             parking_indoor+ terrace+ rooms+msregion, df.train))

# adj. R^2 = 0.7253 




# Check again for NAs

na_count1 <-sapply(df.train, function(y) sum(length(which(is.na(y)))))


view(na_count1)


# Following variables have NAs: year_built, wgh_avg_sonnenklasse_per_egid, Anteil_auslaend, Avg_age, Avg_size_household,
#                               anteil_efh, avg_anzhl_geschosse, avg_bauperiode, dist_to_haltst, dist_to_lake, dist_to_main_stat,
#                               dist_to_school_1, geb_wohnnutz_total, max_guar_down_speed, wgh_avg_sonnenklasse_per_egid




## Final Model, Excluded further variables 

model.f <- (lm(rent_full~area+ Micro_rating+ Anteil_auslaend+ Avg_age+ Avg_size_household+
              avg_bauperiode+dist_to_5G+ dist_to_haltst+ 
              dist_to_lake+ dist_to_main_stat+ dist_to_train_stat+
              superm_pix_count_km2+ cheminee+ elevator+newly_built+
              parking_indoor+ terrace+ rooms+msregion, df.train))

summary(model.f)

# R2 = 0.7253


## Corrected final Model: Excl. Variables which have too many NAs (produces issues with the msregion & rooms)

model.m <- lm(rent_full~area+ Micro_rating+dist_to_5G+
              superm_pix_count_km2+ cheminee+ elevator+
              newly_built+parking_indoor+msregion+rooms, df.train)

summary(model.m)


# adj. R2 = 0.7146



# Without binary variables

summary(lm(rent_full~area+ Micro_rating+dist_to_5G+
             superm_pix_count_km2+msregion+rooms, df.train))

# adj. R2 = 0.7009


### Predictions: Multiple Regresison


p.f <- predict(model.f, df.test) # This one does not work properly because of the facotor variables rooms, msregion & KTKZ



p.m <- model.m %>% predict(df.test)
data.frame( R2 = R2(p.m, df.test$rent_full),
            RMSE = RMSE(p.m, df.test$rent_full),
            MAE = MAE(p.m, df.test$rent_full),
            rae = rae(p.m, df.test$rent_full))




# R2 = 0.7109305   RMSE = 366.5258   MAE = 247.7542   RAE = 0.5464055


RMSE(p.m, df.test$rent_full)/mean(df.test$rent_full) # = 0.2125422





### Cross-validation





## 1. K-fold cross-validation Multiple Regression



# Define training control
set.seed(123) 
train.control <- trainControl(method = "cv", number = 10)

# Train the model
cv.m1 <- train(rent_full~area+ Micro_rating+dist_to_5G+
              superm_pix_count_km2+ cheminee+ elevator+
              newly_built+parking_indoor+msregion+rooms, 
              data = df, 
              method = "lm",
              trControl = train.control)
# Summarize the results
print(cv.m1)


# Output: 

# 70672 samples
# 10 predictor

# No pre-processing
# Resampling: Cross-Validated (10 fold) 
# Summary of sample sizes: 63605, 63605, 63605, 63606, 63605, 63605, ... 
# Resampling results:
  
#  RMSE      Rsquared   MAE     
# 367.241  0.7132138  247.0824

# Tuning parameter 'intercept' was held constant at a value of TRUE





## 2. Repeated K-fold Cross-validation Multiple Regression


set.seed(123)
train.control <- trainControl(method = "repeatedcv", 
                              number = 10, repeats = 5)
# Train the model
cv.m2 <- train(rent_full~area+ Micro_rating+dist_to_5G+
               superm_pix_count_km2+ cheminee+ elevator+
               newly_built+parking_indoor+msregion+rooms, 
               data = df, 
               method = "lm",
               trControl = train.control)
# Summarize the results
print(cv.m2)


# Output: 

# 70672 samples
# 10 predictor

# No pre-processing
# Resampling: Cross-Validated (10 fold, repeated 10 times) 
# Summary of sample sizes: 63605, 63605, 63605, 63606, 63605, 63605, ... 
# Resampling results:
  
#  RMSE      Rsquared   MAE     
# 367.272  0.7131887  247.0994

#Tuning parameter 'intercept' was held constant at a value of TRUE




## 3. K-fold cross-validation Univariate regression



# Define training control
set.seed(123) 
train.control <- trainControl(method = "cv", number = 10)

# Train the model
cv.s1 <- train(rent_full~area, 
               data = df, 
               method = "lm",
               trControl = train.control)
# Summarize the results
print(cv.s1)


# Output: 

# 70672 samples
# 1 predictor

# No pre-processing
# Resampling: Cross-Validated (10 fold) 
# Summary of sample sizes: 63605, 63605, 63605, 63606, 63605, 63605, ... 
# Resampling results:

#  RMSE      Rsquared   MAE     
# 495.298   0.4782797  341.4196

# Tuning parameter 'intercept' was held constant at a value of TRUE







## 4. Repeated K-fold Cross-validation for univariate Regression model


set.seed(123)
train.control <- trainControl(method = "repeatedcv", 
                              number = 10, repeats = 10)
# Train the model
cv.s2 <- train(rent_full~area, 
               data = df, 
               method = "lm",
               trControl = train.control)
# Summarize the results
print(cv.s2)




# Output: 

# No pre-processing
# Resampling: Cross-Validated (10 fold, repeated 10 times) 
# Summary of sample sizes: 63605, 63605, 63605, 63606, 63605, 63605, ... 
# Resampling results:
  
#  RMSE      Rsquared   MAE     
# 495.3149  0.4782875  341.4251

# Tuning parameter 'intercept' was held constant at a value of TRUE









### Subset FORWARD regression

# Ideally select all significant variables from the regressions above (not all variables to keep run-to a minimum)

# Most variables cause the folowing error: 

#Error in if (pvals[minp] <= penter) { : argument is of length zero
#In addition: Warning messages:
#  1: In if (pvals[minp] <= penter) { :
#      the condition has length > 1 and only the first element will be used
#  2: In min(pvals, na.rm = TRUE) :
#      no non-missing arguments to min; returning Inf

# Even when including only variables with no NAs (for multi level factor variables might be hard to distinguish)

model.x <- lm(rent_full~area+ Micro_rating+newly_built+cheminee+parking_indoor+dist_to_train_stat+cabletv+
                elevator+dishwasher+balcony+terrace+pool+month+basement+KTKZ+Noise_max, data = df.train)

k <- ols_step_forward_p(model.x, na.rm = TRUE)
plot(k)

ols_step_forward_p(model.x, details = TRUE)


p.x <- model.x %>% predict(df.test)
data.frame( R2 = R2(p.x, df.test$rent_full),
            RMSE = RMSE(p.x, df.test$rent_full),
            MAE = MAE(p.x, df.test$rent_full),
            RAE = rae(p.x, df.test$rent_full))

view(p.x)





### Subset BACKWARD regression

model.y <- lm(rent_full~area+ Micro_rating+newly_built+cheminee+parking_indoor+dist_to_train_stat+cabletv+
                elevator+dishwasher+balcony+terrace+pool+month+basement+KTKZ+Noise_max, data = df.train)

n <- ols_step_backward_p(model.y)
plot(n)

ols_step_backward_p(model.y, details = TRUE)

p.y <- model.y %>% predict(df.test)
data.frame( R2 = R2(p.y, df.test$rent_full),
            RMSE = RMSE(p.y, df.test$rent_full),
            MAE = MAE(p.y, df.test$rent_full),
            RAE = rae(p.y, df.test$rent_full))


# Compare with the forwrad regression





#### Test Models on New Testset

x.test <- read.csv("X_test.csv")



glimpse(x.test)




### Data cleaning and manipulation


# Rent is y-variable, kick out rows which do not have any obvs.

x.test_NA <- subset(x.test, is.na(x.test$rent_full)) # No NAs in the y-variable

view(x.test_NA)






# Delete ID column since it is not in the training set

#x.test$ID <- NULL


view(x.test)

# Replace NAs with 0 for all binary variables




which( colnames(x.test)=="balcony" ) #get column number
which( colnames(x.test)=="cheminee" )

x.test[, 9:17][is.na(x.test[, 9:17])] <- 0 #replace NAs with 0


which( colnames(x.test)=="dishwasher" )
which( colnames(x.test)=="heating_pellets" )

x.test[, 20:33][is.na(x.test[, 20:33])] <- 0


which( colnames(x.test)=="kids_friendly" )

x.test[, 35][is.na(x.test[, 35])] <- 0


which( colnames(x.test)=="laundry" )

x.test[, 37][is.na(x.test[, 37])] <- 0


which( colnames(x.test)=="manlift" )
which( colnames(x.test)=="minergie" )

x.test[, 39:41][is.na(x.test[, 39:41])] <- 0


which( colnames(x.test)=="new_building" )

x.test[, 44][is.na(x.test[, 44])] <- 0


which( colnames(x.test)=="oldbuilding" )
which( colnames(x.test)=="public_transport" )

x.test[, 46:53][is.na(x.test[, 46:53])] <- 0


which( colnames(x.test)=="quiet" )
which( colnames(x.test)=="raised_groundfloor" )

x.test[, 56:57][is.na(x.test[, 56:57])] <- 0


x.test[, which( colnames(x.test)=="shared_flat" ):which( colnames(x.test)=="wheelchair" )][is.na(x.test[, which( colnames(x.test)=="shared_flat" ):which( colnames(x.test)=="wheelchair" )])] <- 0 #faster approach







# Drop useless variables without values

sum(!is.na(x.test$appartments))

x.test$appartments <- NULL

x.test$descr <- NULL

x.test$area_useable <- NULL


table(x.test$bright) #check frequency 


table(x.test$bath_tube)
x.test$bath_tube <- NULL


table(x.test$building_plot)
x.test$building_plot <- NULL


table(x.test$cabletv)


table(x.test$cheminee)


table(x.test$dishwasher)


#apply(x.test[c(6:12, 14:16, 18:27, 29, 31, 33:35, 38:47, 50:51, 54:64)], 2, table) #Found an easier approach


x.test$ceiling <- NULL

x.test$garden_m2 <- NULL

x.test$minergie <- NULL

x.test$pets <- NULL

x.test$public_transport <- NULL

x.test$shared_flat <- NULL

x.test$shopping <- NULL

x.test$wheelchair <- NULL

x.test$manlift <- NULL

x.test$GDENAMK <- NULL

x.test$GDENR <- NULL

x.test$address <- NULL

x.test$date <- NULL

x.test$year <- NULL

x.test$gardenshed <- NULL

x.test$oven <- NULL

x.test$veranda <- NULL


# Create factors out of binary variables

x.test <- subset(x.test, x.test$rooms < 10) #delete observation which values only occur once, muddies the water

x.test$rooms <- round_any(x.test$rooms,0.5) #round rooms (no values except .0 and .5 are allowed)

x.test$rooms <- as.factor(x.test$rooms)

x.test$floors <- as.factor(x.test$floors)

x.test$balcony <- as.factor(x.test$balcony)

x.test$basement <- as.factor(x.test$basement)

x.test$bath <- as.factor(x.test$bath)

x.test$bright <- as.factor(x.test$bright)

x.test$cabletv <- as.factor(x.test$cabletv)

x.test$cheminee <- as.factor(x.test$cheminee)

x.test$dishwasher <- as.factor(x.test$dishwasher)

x.test$dryer <- as.factor(x.test$dryer)

x.test$elevator <- as.factor(x.test$elevator)

x.test$floors <- as.factor(x.test$floors)

x.test$furnished <- as.factor(x.test$furnished)

#x.test$gardenshed <- as.factor(x.test$gardenshed) this variable produces an error regression which includes all variables

x.test$heating_air <- as.factor(x.test$heating_air)

x.test$heating_earth <- as.factor(x.test$heating_earth)

x.test$heating_electro <- as.factor(x.test$heating_electro)

x.test$heating_far <- as.factor(x.test$heating_far)

x.test$heating_gas <- as.factor(x.test$heating_gas)

x.test$heating_oil <- as.factor(x.test$heating_oil)

x.test$heating_pellets <- as.factor(x.test$heating_pellets)

x.test$kids_friendly <- as.factor(x.test$kids_friendly)

x.test$laundry <- as.factor(x.test$laundry)

x.test$middle_house <- as.factor(x.test$middle_house)

x.test$new_building <- as.factor(x.test$new_building)

x.test$newly_built <- as.factor(x.test$newly_built)

x.test$oldbuilding <- as.factor(x.test$oldbuilding)

#x.test$oven <- as.factor(x.test$oven) this variable produces an error regression which includes all variables

x.test$parking_indoor <- as.factor(x.test$parking_indoor)

x.test$parking_outside <- as.factor(x.test$parking_outside)

x.test$playground <- as.factor(x.test$playground)

x.test$pool <- as.factor(x.test$pool)

x.test$quiet <- as.factor(x.test$quiet)

x.test$raised_groundfloor <- as.factor(x.test$raised_groundfloor)

x.test$shower <- as.factor(x.test$shower)

x.test$sunny <- as.factor(x.test$sunny)

x.test$terrace <- as.factor(x.test$terrace)

x.test$toilets <- as.factor(x.test$toilets)

x.test$topstorage <- as.factor(x.test$topstorage)

#x.test$veranda <- as.factor(x.test$veranda) this variable produces an error regression which includes all variables

x.test$water <- as.factor(x.test$water)

x.test$month <- as.factor(x.test$month)

x.test$msregion <- as.factor(x.test$msregion)

x.test$quarter_general <- as.factor(x.test$quarter_general)



# Check NAs of the other variables

na_count2 <-sapply(x.test, function(y) sum(length(which(is.na(y)))))


view(na_count2)




## Final Model Output

p.m <- model.m %>% predict(x.test)







