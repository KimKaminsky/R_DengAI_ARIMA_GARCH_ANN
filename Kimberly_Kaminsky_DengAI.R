install.packages("astsa")
# Load packages
library(caret)
library(corrplot)
library(dplyr)
library(forecast)
library(mice)
library(stringr)
library(tidyr)
library(tseries)
library(fGarch)
library(rugarch)
library(astsa)

################################################
###### Exploratory Data Analysis (EDA) #########
################################################

# Load data
Train <- read.csv("D:/Kim MSPA/Predict 413/Final Project/dengue_features_train.csv", sep = ",")
str(Train) 
head(Train, n=20)
summary(Train)

Test <- read.csv("D:/Kim MSPA/Predict 413/Final Project/dengue_features_test.csv", sep = ",")
str(Test) 
head(Test, n=20)
summary(Test)

TrLabel <- read.csv("D:/Kim MSPA/Predict 413/Final Project/dengue_labels_train.csv", sep = ",")
str(TrLabel) 
head(TrLabel)
summary(TrLabel)

# Adding the total_cases column from the TrLabel to the Train dataset
Train$total_cases <- TrLabel$total_cases
str(Train) # 'data.frame':	1456 obs. of  25 variables
head(Train)
summary(Train)

# Missing Values - Count per Variable
sapply(Train, function(Train) sum(is.na(Train)))

sapply(Test, function(Test) sum(is.na(Test)))


# ####################################################
# # Initial correlation plots for numeric variables  #
# ####################################################

# Pull out the numeric variables for analysis
nums <- sapply(Train, is.numeric)
train <- Train[ , nums]

# Nne of the values are missing more than 75% of values so will use all variables
missing_values <- train %>% summarize_each(funs(sum(is.na(.))/n()))

# correlation plot opened in a new window
X11()
corrplot(cor(train, use="complete.obs"),type="lower")


########################
# Create new variables #
########################

# Year and week of year have decent correlations with total_cases
# If they are combined into one time element will this increase the correlation?
# Create new variable week
Train$week <- 0
Train$week[Train$city == 'sj'] <- Train$weekofyear[Train$city == 'sj'] + (52 * (Train$year[Train$city == 'sj'] - 1990))
Train$week[Train$city == 'iq'] <- Train$weekofyear[Train$city == 'iq'] + (52 * (Train$year[Train$city == 'iq'] - 2000))

# Check that data looks okay
options(max.print=1000000)
check <- subset(Train, select=c(city, year, weekofyear, week_start_date, week))

# Create copy of Train before dropping variables
Train2 <- cbind(Train)

Train2$week_start_date <- NULL


#####################################
# Second look at correlation matrix #
#####################################

# Pull out the numeric variables for analysis
nums <- sapply(Train2, is.numeric)
train <- Train2[ , nums]

# correlation plot opened in a new window
# No correlation between total_cases and week - try breaking it into two datasets for each city
X11()
corrplot(cor(train, use="complete.obs"),type="lower")


###########################
# Create datasets by City #
###########################

TrainSJ <- subset(Train2, Train2$city == 'sj')
TrainIQ <- subset(Train2, Train2$city == 'iq')


TestSJ <- subset(Test, Test$city == 'sj')
TestIQ <- subset(Test, Test$city == 'iq')


##############################
# Correlation plots by city  #
##############################

# Pull out the numeric variables for analysis
nums <- sapply(TrainSJ, is.numeric)
train <- TrainSJ[ , nums]

# correlation plot opened in a new window
# No correlation between total_cases and week - try breaking it into two datasets for each city
X11()
par(oma=c(0,0,4,0))
corrplot(cor(train, use="complete.obs"),type="lower", title="Correlation Plot for San Juan", mar=c(0,0,1,0))

# Pull out the numeric variables for analysis
nums <- sapply(TrainIQ, is.numeric)
train <- TrainIQ[ , nums]

# correlation plot opened in a new window
# No correlation between total_cases and week - try breaking it into two datasets for each city
X11()
par(oma=c(0,0,4,0))
corrplot(cor(train, use="complete.obs"),type="lower", order="hclust", title="Correlation Plot for Iquito", mar=c(0,0,1,0))


#################################################
# Plot a time series to get a sense of the data #
# This shows strong seasonality for both but    #
#  the peak is different for the two cities     #
#################################################
dev.off()
SJTS <- ts(TrainSJ$total_cases, frequency = 52, start = c(1990,18), end=c(2008,17))
plot(SJTS, ylab="Total Cases Dengue", xlab="Year", main="Dengue cases in San Juan, Puerto Rico")

IQTS <- ts(TrainIQ$total_cases, frequency = 52, start = c(2000,26), end=c(2010,25))
plot(IQTS, ylab="Total Cases Dengue", xlab="Year", main="Dengue cases in Iquito, Peru")

###############################
# Analysis of  missing values #
###############################

############
# San Juan #
############

# subset of rows with missing reanalysis_air_temp_k
# half the rows with missing values are missing all variables
# Maybe use time series to impute the missing valus
miss_ratk <- subset(TrainSJ, is.na(reanalysis_air_temp_k))
miss_ratk_test <- subset(TestSJ, is.na(reanalysis_air_temp_k))

# reanalysis_air_temp_k has a strong correlation with reaanalysis_avg_temp_k
# Before imputing values do a scatterplot -- want to see that this is about the same after imputing values
plot(TrainSJ$reanalysis_air_temp_k, TrainSJ$reanalysis_avg_temp_k, main="Air Temp vs. Avg. Temp",
     xlab="Air Temp ", ylab="Average Temp ", pch=19)

# These time series shows a strong seasonal pattern that is very predictable - this can be used
AirSJTS <- ts(TrainSJ$reanalysis_air_temp_k, frequency = 52, start = c(1990,18), end=c(2008,17))
plot(AirSJTS, ylab="Air Temperature", xlab="Year", main="Air temperature in San Juan")

AAirSJTS <- ts(TrainSJ$reanalysis_avg_temp_k, frequency = 52, start = c(1990,18), end=c(2008,17))
plot(AAirSJTS, ylab="Air Temperature", xlab="Year", main="Air temperature in San Juan, Puerto Rico")

PrecipSJTS <- ts(TrainSJ$reanalysis_precip_amt_kg_per_m2, frequency = 52, start = c(1990,18), end=c(2008,17))
plot(PrecipSJTS, ylab="Precipitation amount", xlab="Year", main="Precipitation amount in San Juan, Puerto Rico")

DewPtSJTS <- ts(TrainSJ$reanalysis_dew_point_temp_k, frequency = 52, start = c(1990,18), end=c(2008,17))
plot(DewPtSJTS, ylab="Dew Point amount", xlab="Year", main="Dew Point amount in San Juan, Puerto Rico")

MaxTempSJTS <- ts(TrainSJ$reanalysis_max_air_temp_k, frequency = 52, start = c(1990,18), end=c(2008,17))
plot(MaxTempSJTS, ylab="Max Air Temp amount", xlab="Year", main="Max Air Temp amount in San Juan, Puerto Rico")

MinTempSJTS <- ts(TrainSJ$reanalysis_min_air_temp_k, frequency = 52, start = c(1990,18), end=c(2008,17))
plot(MinTempSJTS, ylab="Min Air Temp amount", xlab="Year", main="Min Air Temp amount in San Juan, Puerto Rico")

RSPHSJTS <- ts(TrainSJ$reanalysis_specific_humidity_g_per_kg, frequency = 52, start = c(1990,18), end=c(2008,17))
plot(RSPHSJTS, xlab="Year")

SATSJTS <- ts(TrainSJ$station_avg_temp_c, frequency = 52, start = c(1990,18), end=c(2008,17))
plot(SATSJTS, xlab="Year")

SMTSJTS <- ts(TrainSJ$station_max_temp_c, frequency = 52, start = c(1990,18), end=c(2008,17))
plot(SMTSJTS, xlab="Year")

SMTCSJTS <- ts(TrainSJ$station_min_temp_c, frequency = 52, start = c(1990,18), end=c(2008,17))
plot(SMTCSJTS, xlab="Year")


# These don't really look predictable from a time series perspective
RRHPSJTS <- ts(TrainSJ$reanalysis_relative_humidity_percent, frequency = 52, start = c(1990,18), end=c(2008,17))
plot(RRHPSJTS,   ylab="Relative Humidity Percent", xlab="Year", main="Relative Humidity Percent in Suan Juan")

RTkSJTS <- ts(TrainSJ$reanalysis_tdtr_k, frequency = 52, start = c(1990,18), end=c(2008,17))
plot(RTkSJTS, xlab="Year")

SATSJTS <- ts(TrainSJ$station_diur_temp_rng_c, frequency = 52, start = c(1990,18), end=c(2008,17))
plot(SATSJTS, xlab="Year")

SpmSJTS <- ts(TrainSJ$station_precip_mm, frequency = 52, start = c(1990,18), end=c(2008,17))
plot(SpmSJTS, xlab="Year")

PAmtSJTS <- ts(TrainSJ$precipitation_amt_mm, frequency = 52, start = c(1990,18), end=c(2008,17))
plot(PAmtSJTS, ylab="Precipitation amount", xlab="Year", main="Precipitation amount in San Juan, Puerto Rico")

############
# Iquitos  #
############
 
# subset of rows with missing reanalysis_air_temp_k
# half the rows with missing values are missing all variables
# Maybe use time series to impute the missing valus
miss_ratkIQ <- subset(TrainIQ, is.na(reanalysis_air_temp_k))
miss_ratkIQ_test <- subset(TestIQ, is.na(reanalysis_air_temp_k))

# reanalysis_air_temp_k has a strong correlation with reaanalysis_avg_temp_k
# Before imputing values do a scatterplot -- want to see that this is about the same after imputing values
plot(TrainIQ$reanalysis_air_temp_k, TrainIQ$reanalysis_avg_temp_k, main="Air Temp vs. Avg. Temp",
     xlab="Air Temp ", ylab="Average Temp ", pch=19)

# These time series shows a strong seasonal pattern that is very predictable - this can be used
DewPtIQTS <- ts(TrainIQ$reanalysis_dew_point_temp_k, frequency = 52, start = c(1990,18), end=c(2008,17))
plot(DewPtIQTS, ylab="Dew Point amount", xlab="Year", main="Dew Point amount in Iquitos")

RSPHIQTS <- ts(TrainIQ$reanalysis_specific_humidity_g_per_kg, frequency = 52, start = c(1990,18), end=c(2008,17))
plot(RSPHIQTS, xlab="Year")

# These don't really look predictable from a time series perspective
AirIQTS <- ts(TrainIQ$reanalysis_air_temp_k, frequency = 52, start = c(1990,18), end=c(2008,17))
plot(AirIQTS, ylab="Air Temperature", xlab="Year", main="Air temperature in Iquitos")

AAirIQTS <- ts(TrainIQ$reanalysis_avg_temp_k, frequency = 52, start = c(1990,18), end=c(2008,17))
plot(AAirIQTS, ylab="Air Temperature", xlab="Year", main="Air temperature in Iquitos")

PrecipIQTS <- ts(TrainIQ$reanalysis_precip_amt_kg_per_m2, frequency = 52, start = c(1990,18), end=c(2008,17))
plot(PrecipIQTS, ylab="Precipitation amount", xlab="Year", main="Precipitation amount in Iquitos")

MaxTempIQTS <- ts(TrainIQ$reanalysis_max_air_temp_k, frequency = 52, start = c(1990,18), end=c(2008,17))
plot(MaxTempIQTS, ylab="Max Air Temp amount", xlab="Year", main="Max Air Temp amount in Iquitos")

MinTempIQTS <- ts(TrainIQ$reanalysis_min_air_temp_k, frequency = 52, start = c(1990,18), end=c(2008,17))
plot(MinTempIQTS, ylab="Min Air Temp amount", xlab="Year", main="Min Air Temp amount in Iquitos")

SATIQTS <- ts(TrainIQ$station_avg_temp_c, frequency = 52, start = c(1990,18), end=c(2008,17))
plot(SATIQTS, xlab="Year")

SMTIQTS <- ts(TrainIQ$station_max_temp_c, frequency = 52, start = c(1990,18), end=c(2008,17))
plot(SMTIQTS, xlab="Year")

SMTCIQTS <- ts(TrainIQ$station_min_temp_c, frequency = 52, start = c(1990,18), end=c(2008,17))
plot(SMTCIQTS, xlab="Year")

RRHPIQTS <- ts(TrainIQ$reanalysis_relative_humidity_percent, frequency = 52, start = c(1990,18), end=c(2008,17))
plot(RRHPIQTS,  xlab="Year")

RTkIQTS <- ts(TrainIQ$reanalysis_tdtr_k, frequency = 52, start = c(1990,18), end=c(2008,17))
plot(RTkIQTS, xlab="Year")

SATIQTS <- ts(TrainIQ$station_diur_temp_rng_c, frequency = 52, start = c(1990,18), end=c(2008,17))
plot(SATIQTS, xlab="Year")

SpmIQTS <- ts(TrainIQ$station_precip_mm, frequency = 52, start = c(1990,18), end=c(2008,17))
plot(SpmIQTS, xlab="Year")

PAmtIQTS <- ts(TrainIQ$precipitation_amt_mm, frequency = 52, start = c(1990,18), end=c(2008,17))
plot(PAmtIQTS, ylab="Precipitation amount", xlab="Year", main="Precipitation amount in Iquitos")

#########################
# Impute missing values #
#########################

############
# San Juan #
############

# Will impute using the mean of value immediately before and after for the rows that all had 6 missing values
TrainSJ$M_reanalysis_air_temp_k <- 0
TrainSJ$M_reanalysis_air_temp_k[is.na(TrainSJ$reanalysis_air_temp_k)] <- 1
lowIndex <- as.numeric(rownames(miss_ratk)) - 1
highIndex <- as.numeric(rownames(miss_ratk)) + 1
TrainSJ$reanalysis_air_temp_k[as.numeric(rownames(miss_ratk))] <- (TrainSJ$reanalysis_air_temp_k[lowIndex] + 
                                                  TrainSJ$reanalysis_air_temp_k[highIndex]) /2

TrainSJ$M_reanalysis_avg_temp_k <- 0
TrainSJ$M_reanalysis_avg_temp_k[is.na(TrainSJ$reanalysis_avg_temp_k)] <- 1
TrainSJ$reanalysis_avg_temp_k[as.numeric(rownames(miss_ratk))] <- (TrainSJ$reanalysis_avg_temp_k[lowIndex] + 
                                                                     TrainSJ$reanalysis_avg_temp_k[highIndex]) /2

TrainSJ$M_reanalysis_precip_amt_kg_per_m2 <- 0
TrainSJ$M_reanalysis_precip_amt_kg_per_m2[is.na(TrainSJ$reanalysis_precip_amt_kg_per_m2)] <- 1
TrainSJ$reanalysis_precip_amt_kg_per_m2[as.numeric(rownames(miss_ratk))] <- (TrainSJ$reanalysis_precip_amt_kg_per_m2[lowIndex] + 
                                                                     TrainSJ$reanalysis_precip_amt_kg_per_m2[highIndex]) /2

TrainSJ$M_reanalysis_dew_point_temp_k <- 0
TrainSJ$M_reanalysis_dew_point_temp_k[is.na(TrainSJ$reanalysis_dew_point_temp_k)] <- 1
TrainSJ$reanalysis_dew_point_temp_k[as.numeric(rownames(miss_ratk))] <- (TrainSJ$reanalysis_dew_point_temp_k[lowIndex] + 
                                                                               TrainSJ$reanalysis_dew_point_temp_k[highIndex]) /2

TrainSJ$M_reanalysis_max_air_temp_k <- 0
TrainSJ$M_reanalysis_max_air_temp_k[is.na(TrainSJ$reanalysis_max_air_temp_k)] <- 1
TrainSJ$reanalysis_max_air_temp_k[as.numeric(rownames(miss_ratk))] <- (TrainSJ$reanalysis_max_air_temp_k[lowIndex] + 
                                                                           TrainSJ$reanalysis_max_air_temp_k[highIndex]) /2
TrainSJ$M_reanalysis_min_air_temp_k <- 0
TrainSJ$M_reanalysis_min_air_temp_k[is.na(TrainSJ$reanalysis_min_air_temp_k)] <- 1
TrainSJ$reanalysis_min_air_temp_k[as.numeric(rownames(miss_ratk))] <- (TrainSJ$reanalysis_min_air_temp_k[lowIndex] + 
                                                                         TrainSJ$reanalysis_min_air_temp_k[highIndex]) /2
TrainSJ$M_reanalysis_specific_humidity_g_per_kg <- 0
TrainSJ$M_reanalysis_specific_humidity_g_per_kg[is.na(TrainSJ$reanalysis_specific_humidity_g_per_kg)] <- 1
TrainSJ$reanalysis_specific_humidity_g_per_kg[as.numeric(rownames(miss_ratk))] <- (TrainSJ$reanalysis_specific_humidity_g_per_kg[lowIndex] + 
                                                                                    TrainSJ$reanalysis_specific_humidity_g_per_kg[highIndex]) /2

TrainSJ$M_station_avg_temp_c <- 0
TrainSJ$M_station_avg_temp_c[is.na(TrainSJ$station_avg_temp_c)] <- 1
TrainSJ$station_avg_temp_c[as.numeric(rownames(miss_ratk))] <- (TrainSJ$station_avg_temp_c[lowIndex] + 
                                                                 TrainSJ$station_avg_temp_c[highIndex]) /2


TrainSJ$M_station_max_temp_c <- 0
TrainSJ$M_station_max_temp_c[is.na(TrainSJ$station_max_temp_c)] <- 1
TrainSJ$station_max_temp_c[as.numeric(rownames(miss_ratk))] <- (TrainSJ$station_max_temp_c[lowIndex] + 
                                                                       TrainSJ$station_max_temp_c[highIndex]) /2
TrainSJ$M_station_min_temp_c <- 0
TrainSJ$M_station_min_temp_c[is.na(TrainSJ$station_min_temp_c)] <- 1
TrainSJ$station_min_temp_c[as.numeric(rownames(miss_ratk))] <- (TrainSJ$station_min_temp_c[lowIndex] + 
                                                                  TrainSJ$station_min_temp_c[highIndex]) /2


# Created imputed flags for the variables about to be imputed by MICE
TrainSJ$M_ndvi_ne <- 0
TrainSJ$M_ndvi_ne[is.na(TrainSJ$ndvi_ne)] <- 1
TrainSJ$M_ndvi_nw <- 0
TrainSJ$M_ndvi_nw[is.na(TrainSJ$ndvi_nw)] <- 1
TrainSJ$M_ndvi_se <- 0
TrainSJ$M_ndvi_se[is.na(TrainSJ$ndvi_se)] <- 1
TrainSJ$M_ndvi_sw <- 0
TrainSJ$M_ndvi_sw[is.na(TrainSJ$ndvi_sw)] <- 1
TrainSJ$M_precipitation_amt_mm <- 0
TrainSJ$M_precipitation_amt_mm[is.na(TrainSJ$precipitation_amt_mm)] <- 1
TrainSJ$M_reanalysis_relative_humidity_percent <- 0
TrainSJ$M_reanalysis_relative_humidity_percent[is.na(TrainSJ$reanalysis_relative_humidity_percent)] <- 1
TrainSJ$M_reanalysis_sat_precip_amt_mm <- 0
TrainSJ$M_reanalysis_sat_precip_amt_mm[is.na(TrainSJ$reanalysis_sat_precip_amt_mm)] <- 1
TrainSJ$M_reanalysis_tdtr_k <- 0
TrainSJ$M_reanalysis_tdtr_k[is.na(TrainSJ$reanalysis_tdtr_k)] <- 1
TrainSJ$M_station_diur_temp_rng_c <- 0
TrainSJ$M_station_diur_temp_rng_c[is.na(TrainSJ$station_diur_temp_rng_c)] <- 1
TrainSJ$M_station_precip_mm <- 0
TrainSJ$M_station_precip_mm[is.na(TrainSJ$station_precip_mm)] <- 1


# Use MICE package for the remaining values
imputedSJ <- mice(TrainSJ, m=5, maxit = 50, method = 'pmm', seed = 500)
# summary(imputedSJ)

imputedSJ1 <- mice::complete(imputedSJ,1)

# sapply(imputedSJ1, function(imputedSJ1) sum(is.na(imputedSJ1)))

# For some reason reanalysis_sat_precip_amt_mm is still missing - highly correlated with precipitation_amt_mm
# Looks like these are always the same so drop the reanalysis variable
imputedSJ1$reanalysis_sat_precip_amt_mm <- NULL


# Now apply same method to test data set
# Will impute using the mean of value immediately before and after for the rows that all had 2 missing values
TestSJ$M_reanalysis_air_temp_k <- 0
TestSJ$M_reanalysis_air_temp_k[is.na(TestSJ$reanalysis_air_temp_k)] <- 1
lowIndex <- as.numeric(rownames(miss_ratk_test)) - 1
highIndex <- as.numeric(rownames(miss_ratk_test)) + 1
TestSJ$reanalysis_air_temp_k[as.numeric(rownames(miss_ratk_test))] <- (TestSJ$reanalysis_air_temp_k[lowIndex] + 
                                                                     TestSJ$reanalysis_air_temp_k[highIndex]) /2

TestSJ$M_reanalysis_avg_temp_k <- 0
TestSJ$M_reanalysis_avg_temp_k[is.na(TestSJ$reanalysis_avg_temp_k)] <- 1
TestSJ$reanalysis_avg_temp_k[as.numeric(rownames(miss_ratk_test))] <- (TestSJ$reanalysis_avg_temp_k[lowIndex] + 
                                                                     TestSJ$reanalysis_avg_temp_k[highIndex]) /2

TestSJ$M_reanalysis_precip_amt_kg_per_m2 <- 0
TestSJ$M_reanalysis_precip_amt_kg_per_m2[is.na(TestSJ$reanalysis_precip_amt_kg_per_m2)] <- 1
TestSJ$reanalysis_precip_amt_kg_per_m2[as.numeric(rownames(miss_ratk_test))] <- (TestSJ$reanalysis_precip_amt_kg_per_m2[lowIndex] + 
                                                                               TestSJ$reanalysis_precip_amt_kg_per_m2[highIndex]) /2

TestSJ$M_reanalysis_dew_point_temp_k <- 0
TestSJ$M_reanalysis_dew_point_temp_k[is.na(TestSJ$reanalysis_dew_point_temp_k)] <- 1
TestSJ$reanalysis_dew_point_temp_k[as.numeric(rownames(miss_ratk_test))] <- (TestSJ$reanalysis_dew_point_temp_k[lowIndex] + 
                                                                           TestSJ$reanalysis_dew_point_temp_k[highIndex]) /2

TestSJ$M_reanalysis_max_air_temp_k <- 0
TestSJ$M_reanalysis_max_air_temp_k[is.na(TestSJ$reanalysis_max_air_temp_k)] <- 1
TestSJ$reanalysis_max_air_temp_k[as.numeric(rownames(miss_ratk_test))] <- (TestSJ$reanalysis_max_air_temp_k[lowIndex] + 
                                                                         TestSJ$reanalysis_max_air_temp_k[highIndex]) /2
TestSJ$M_reanalysis_min_air_temp_k <- 0
TestSJ$M_reanalysis_min_air_temp_k[is.na(TestSJ$reanalysis_min_air_temp_k)] <- 1
TestSJ$reanalysis_min_air_temp_k[as.numeric(rownames(miss_ratk_test))] <- (TestSJ$reanalysis_min_air_temp_k[lowIndex] + 
                                                                         TestSJ$reanalysis_min_air_temp_k[highIndex]) /2
TestSJ$M_reanalysis_specific_humidity_g_per_kg <- 0
TestSJ$M_reanalysis_specific_humidity_g_per_kg[is.na(TestSJ$reanalysis_specific_humidity_g_per_kg)] <- 1
TestSJ$reanalysis_specific_humidity_g_per_kg[as.numeric(rownames(miss_ratk_test))] <- (TestSJ$reanalysis_specific_humidity_g_per_kg[lowIndex] + 
                                                                                     TestSJ$reanalysis_specific_humidity_g_per_kg[highIndex]) /2

TestSJ$M_station_avg_temp_c <- 0
TestSJ$M_station_avg_temp_c[is.na(TestSJ$station_avg_temp_c)] <- 1
TestSJ$station_avg_temp_c[as.numeric(rownames(miss_ratk_test))] <- (TestSJ$station_avg_temp_c[lowIndex] + 
                                                                  TestSJ$station_avg_temp_c[highIndex]) /2


TestSJ$M_station_max_temp_c <- 0
TestSJ$M_station_max_temp_c[is.na(TestSJ$station_max_temp_c)] <- 1
TestSJ$station_max_temp_c[as.numeric(rownames(miss_ratk_test))] <- (TestSJ$station_max_temp_c[lowIndex] + 
                                                                  TestSJ$station_max_temp_c[highIndex]) /2
TestSJ$M_station_min_temp_c <- 0
TestSJ$M_station_min_temp_c[is.na(TestSJ$station_min_temp_c)] <- 1
TestSJ$station_min_temp_c[as.numeric(rownames(miss_ratk_test))] <- (TestSJ$station_min_temp_c[lowIndex] + 
                                                                  TestSJ$station_min_temp_c[highIndex]) /2


# Created imputed flags for the variables about to be imputed by MICE
TestSJ$M_ndvi_ne <- 0
TestSJ$M_ndvi_ne[is.na(TestSJ$ndvi_ne)] <- 1
TestSJ$M_ndvi_nw <- 0
TestSJ$M_ndvi_nw[is.na(TestSJ$ndvi_nw)] <- 1
TestSJ$M_ndvi_se <- 0
TestSJ$M_ndvi_se[is.na(TestSJ$ndvi_se)] <- 1
TestSJ$M_ndvi_sw <- 0
TestSJ$M_ndvi_sw[is.na(TestSJ$ndvi_sw)] <- 1
TestSJ$M_precipitation_amt_mm <- 0
TestSJ$M_precipitation_amt_mm[is.na(TestSJ$precipitation_amt_mm)] <- 1
TestSJ$M_reanalysis_relative_humidity_percent <- 0
TestSJ$M_reanalysis_relative_humidity_percent[is.na(TestSJ$reanalysis_relative_humidity_percent)] <- 1
TestSJ$M_reanalysis_sat_precip_amt_mm <- 0
TestSJ$M_reanalysis_sat_precip_amt_mm[is.na(TestSJ$reanalysis_sat_precip_amt_mm)] <- 1
TestSJ$M_reanalysis_tdtr_k <- 0
TestSJ$M_reanalysis_tdtr_k[is.na(TestSJ$reanalysis_tdtr_k)] <- 1
TestSJ$M_station_diur_temp_rng_c <- 0
TestSJ$M_station_diur_temp_rng_c[is.na(TestSJ$station_diur_temp_rng_c)] <- 1
TestSJ$M_station_precip_mm <- 0
TestSJ$M_station_precip_mm[is.na(TestSJ$station_precip_mm)] <- 1


# Use MICE package for the remaining values
imputedSJ_test <- mice(TestSJ, m=5, maxit = 50, method = 'pmm', seed = 500)
# summary(imputedSJ)

imputedSJT <- mice::complete(imputedSJ_test,1)

sapply(imputedSJT, function(imputedSJT) sum(is.na(imputedSJT)))

# For some reason reanalysis_sat_precip_amt_mm is still missing - highly correlated with precipitation_amt_mm
# Looks like these are always the same so drop the reanalysis variable
imputedSJT$reanalysis_sat_precip_amt_mm <- NULL


###########
# Iquitos #
###########

# Will impute using the mean of value immediately before and after for the rows that all had 6 missing values
TrainIQ$M_reanalysis_dew_point_temp_k <- 0
TrainIQ$M_reanalysis_dew_point_temp_k[is.na(TrainIQ$reanalysis_dew_point_temp_k)] <- 1
TrainIQ$reanalysis_dew_point_temp_k[as.numeric(rownames(miss_ratkIQ))] <- (TrainIQ$reanalysis_dew_point_temp_k[lowIndex] + 
                                                                           TrainIQ$reanalysis_dew_point_temp_k[highIndex]) /2

TrainIQ$M_reanalysis_specific_humidity_g_per_kg <- 0
TrainIQ$M_reanalysis_specific_humidity_g_per_kg[is.na(TrainIQ$reanalysis_specific_humidity_g_per_kg)] <- 1
TrainIQ$reanalysis_specific_humidity_g_per_kg[as.numeric(rownames(miss_ratkIQ))] <- (TrainIQ$reanalysis_specific_humidity_g_per_kg[lowIndex] + 
                                                                                     TrainIQ$reanalysis_specific_humidity_g_per_kg[highIndex]) /2

# Created imputed flags for the variables about to be imputed by MICE
TrainIQ$M_reanalysis_air_temp_k <- 0
TrainIQ$M_reanalysis_air_temp_k[is.na(TrainIQ$reanalysis_air_temp_k)] <- 1
TrainIQ$M_reanalysis_avg_temp_k <- 0
TrainIQ$M_reanalysis_avg_temp_k[is.na(TrainIQ$reanalysis_avg_temp_k)] <- 1
TrainIQ$M_reanalysis_precip_amt_kg_per_m2 <- 0
TrainIQ$M_reanalysis_precip_amt_kg_per_m2[is.na(TrainIQ$reanalysis_precip_amt_kg_per_m2)] <- 1
TrainIQ$M_reanalysis_max_air_temp_k <- 0
TrainIQ$M_reanalysis_max_air_temp_k[is.na(TrainIQ$reanalysis_max_air_temp_k)] <- 1
TrainIQ$M_reanalysis_min_air_temp_k <- 0
TrainIQ$M_reanalysis_min_air_temp_k[is.na(TrainIQ$reanalysis_min_air_temp_k)] <- 1
TrainIQ$M_station_avg_temp_c <- 0
TrainIQ$M_station_avg_temp_c[is.na(TrainIQ$station_avg_temp_c)] <- 1
TrainIQ$M_station_max_temp_c <- 0
TrainIQ$M_station_max_temp_c[is.na(TrainIQ$station_max_temp_c)] <- 1
TrainIQ$M_station_min_temp_c <- 0
TrainIQ$M_station_min_temp_c[is.na(TrainIQ$station_min_temp_c)] <- 1
TrainIQ$M_ndvi_ne <- 0
TrainIQ$M_ndvi_ne[is.na(TrainIQ$ndvi_ne)] <- 1
TrainIQ$M_ndvi_nw <- 0
TrainIQ$M_ndvi_nw[is.na(TrainIQ$ndvi_nw)] <- 1
TrainIQ$M_ndvi_se <- 0
TrainIQ$M_ndvi_se[is.na(TrainIQ$ndvi_se)] <- 1
TrainIQ$M_ndvi_sw <- 0
TrainIQ$M_ndvi_sw[is.na(TrainIQ$ndvi_sw)] <- 1
TrainIQ$M_precipitation_amt_mm <- 0
TrainIQ$M_precipitation_amt_mm[is.na(TrainIQ$precipitation_amt_mm)] <- 1
TrainIQ$M_reanalysis_relative_humidity_percent <- 0
TrainIQ$M_reanalysis_relative_humidity_percent[is.na(TrainIQ$reanalysis_relative_humidity_percent)] <- 1
TrainIQ$M_reanalysis_sat_precip_amt_mm <- 0
TrainIQ$M_reanalysis_sat_precip_amt_mm[is.na(TrainIQ$reanalysis_sat_precip_amt_mm)] <- 1
TrainIQ$M_reanalysis_tdtr_k <- 0
TrainIQ$M_reanalysis_tdtr_k[is.na(TrainIQ$reanalysis_tdtr_k)] <- 1
TrainIQ$M_station_diur_temp_rng_c <- 0
TrainIQ$M_station_diur_temp_rng_c[is.na(TrainIQ$station_diur_temp_rng_c)] <- 1
TrainIQ$M_station_precip_mm <- 0
TrainIQ$M_station_precip_mm[is.na(TrainIQ$station_precip_mm)] <- 1


# Use MICE package for the remaining values
imputedIQ <- mice(TrainIQ, m=5, maxit = 50, method = 'pmm', seed = 500)
# summary(imputedIQ)

imputedIQ1 <- mice::complete(imputedIQ,1)

# sapply(imputedIQ1, function(imputedIQ1) sum(is.na(imputedIQ1)))

# For some reason reanalysis_sat_precip_amt_mm is still missing - highly correlated with precipitation_amt_mm
# Looks like these are always the same so drop the reanalysis variable
imputedIQ1$reanalysis_sat_precip_amt_mm <- NULL

# Now do the same imputation for the test dataset
# Will impute using the mean of value immediately before and after for the rows that all had 6 missing values
TestIQ$M_reanalysis_dew_point_temp_k <- 0
TestIQ$M_reanalysis_dew_point_temp_k[is.na(TestIQ$reanalysis_dew_point_temp_k)] <- 1
TestIQ$reanalysis_dew_point_temp_k[as.numeric(rownames(miss_ratkIQ_test))] <- (TestIQ$reanalysis_dew_point_temp_k[lowIndex] + 
                                                                             TestIQ$reanalysis_dew_point_temp_k[highIndex]) /2

TestIQ$M_reanalysis_specific_humidity_g_per_kg <- 0
TestIQ$M_reanalysis_specific_humidity_g_per_kg[is.na(TestIQ$reanalysis_specific_humidity_g_per_kg)] <- 1
TestIQ$reanalysis_specific_humidity_g_per_kg[as.numeric(rownames(miss_ratkIQ_test))] <- (TestIQ$reanalysis_specific_humidity_g_per_kg[lowIndex] + 
                                                                                       TestIQ$reanalysis_specific_humidity_g_per_kg[highIndex]) /2

# Created imputed flags for the variables about to be imputed by MICE
TestIQ$M_reanalysis_air_temp_k <- 0
TestIQ$M_reanalysis_air_temp_k[is.na(TestIQ$reanalysis_air_temp_k)] <- 1
TestIQ$M_reanalysis_avg_temp_k <- 0
TestIQ$M_reanalysis_avg_temp_k[is.na(TestIQ$reanalysis_avg_temp_k)] <- 1
TestIQ$M_reanalysis_precip_amt_kg_per_m2 <- 0
TestIQ$M_reanalysis_precip_amt_kg_per_m2[is.na(TestIQ$reanalysis_precip_amt_kg_per_m2)] <- 1
TestIQ$M_reanalysis_max_air_temp_k <- 0
TestIQ$M_reanalysis_max_air_temp_k[is.na(TestIQ$reanalysis_max_air_temp_k)] <- 1
TestIQ$M_reanalysis_min_air_temp_k <- 0
TestIQ$M_reanalysis_min_air_temp_k[is.na(TestIQ$reanalysis_min_air_temp_k)] <- 1
TestIQ$M_station_avg_temp_c <- 0
TestIQ$M_station_avg_temp_c[is.na(TestIQ$station_avg_temp_c)] <- 1
TestIQ$M_station_max_temp_c <- 0
TestIQ$M_station_max_temp_c[is.na(TestIQ$station_max_temp_c)] <- 1
TestIQ$M_station_min_temp_c <- 0
TestIQ$M_station_min_temp_c[is.na(TestIQ$station_min_temp_c)] <- 1
TestIQ$M_ndvi_ne <- 0
TestIQ$M_ndvi_ne[is.na(TestIQ$ndvi_ne)] <- 1
TestIQ$M_ndvi_nw <- 0
TestIQ$M_ndvi_nw[is.na(TestIQ$ndvi_nw)] <- 1
TestIQ$M_ndvi_se <- 0
TestIQ$M_ndvi_se[is.na(TestIQ$ndvi_se)] <- 1
TestIQ$M_ndvi_sw <- 0
TestIQ$M_ndvi_sw[is.na(TestIQ$ndvi_sw)] <- 1
TestIQ$M_precipitation_amt_mm <- 0
TestIQ$M_precipitation_amt_mm[is.na(TestIQ$precipitation_amt_mm)] <- 1
TestIQ$M_reanalysis_relative_humidity_percent <- 0
TestIQ$M_reanalysis_relative_humidity_percent[is.na(TestIQ$reanalysis_relative_humidity_percent)] <- 1
TestIQ$M_reanalysis_sat_precip_amt_mm <- 0
TestIQ$M_reanalysis_sat_precip_amt_mm[is.na(TestIQ$reanalysis_sat_precip_amt_mm)] <- 1
TestIQ$M_reanalysis_tdtr_k <- 0
TestIQ$M_reanalysis_tdtr_k[is.na(TestIQ$reanalysis_tdtr_k)] <- 1
TestIQ$M_station_diur_temp_rng_c <- 0
TestIQ$M_station_diur_temp_rng_c[is.na(TestIQ$station_diur_temp_rng_c)] <- 1
TestIQ$M_station_precip_mm <- 0
TestIQ$M_station_precip_mm[is.na(TestIQ$station_precip_mm)] <- 1


# Use MICE package for the remaining values
imputedIQ_test <- mice(TestIQ, m=5, maxit = 50, method = 'pmm', seed = 500)
# summary(imputedIQ)

imputedIQT <- mice::complete(imputedIQ_test,1)

sapply(imputedIQT, function(imputedIQT) sum(is.na(imputedIQT)))

# For some reason reanalysis_sat_precip_amt_mm is still missing - highly correlated with precipitation_amt_mm
# Looks like these are always the same so drop the reanalysis variable
imputedIQT$reanalysis_sat_precip_amt_mm <- NULL


#########################################
# Correlation Matrix for Imputed Values # 
#########################################

############
# San Juan #
############

nums <- sapply(imputedSJ1, is.numeric)
train <- imputedSJ1[ , nums]

# correlation plot opened in a new window
# No correlation between total_cases and week - try breaking it into two datasets for each city
X11()
par(mar=c(0,0,0,0)+0.1)
par( ps=10)
corrplot(cor(train),type="lower")

############
# Iquitos  #
############

nums <- sapply(imputedIQ1, is.numeric)
train <- imputedIQ1[ , nums]

# correlation plot opened in a new window
# No correlation between total_cases and week - try breaking it into two datasets for each city
X11()
par(mar=c(0,0,0,0)+0.1)
par( ps=10)
corrplot(cor(train),type="lower")


#########################################
# EDA - Check for outliers & normality  # 
#########################################

############
# San Juan #
############

# Start with the response variable
hist(imputedSJ1$total_cases, col="green")

colSJ <- noquote(colnames(imputedSJ1))
length(colSJ)

getOption("device")
function() .Call("rs_createGD")

dev.off()

for (col in 2:length(colSJ)) {
  colName <- colSJ[col]
  hist(imputedSJ1[,colSJ[col]], col="blue", xlab=colName, main=colName)
}


# Get a subset of all the imputed flag variables
mFlagVars <- colSJ[str_detect(colSJ,'M_')]
tmp <- imputedSJ1 %>% dplyr::select(mFlagVars)
tmp$total_cases <- imputedSJ1$total_cases

# Now create boxplots for each of the variables against the total_cases variable
for (col in 1:length(mFlagVars)) {
  colName <- mFlagVars[col]
  boxplot(tmp$total_cases~tmp[,mFlagVars[col]],data=tmp, main=colName,
          xlab=colName, ylab="Total Cases", col = "blue", notch=TRUE)
}

############
# Iquitos  #
############

# Start with the response variable
hist(imputedIQ1$total_cases, col="green", main="Total Dengue Cases Iquitos - Log and Capped")

colSJ <- noquote(colnames(imputedIQ1))
length(colSJ)

getOption("device")
function() .Call("rs_createGD")

dev.off()

for (col in 2:length(colSJ)) {
  colName <- colSJ[col]
  hist(imputedIQ1[,colSJ[col]], col="blue", xlab=colName, main=colName)
}



# Get a subset of all the imputed flag variables
mFlagVars <- colSJ[str_detect(colSJ,'M_')]
tmp <- imputedIQ1 %>% dplyr::select(mFlagVars)
tmp$total_cases <- imputedIQ1$total_cases

# Now create boxplots for each of the variables against the total_cases variable
for (col in 1:length(mFlagVars)) {
  colName <- mFlagVars[col]
  boxplot(tmp$total_cases~tmp[,mFlagVars[col]],data=tmp, main=colName,
          xlab=colName, ylab="Total Cases", col = "blue", notch=TRUE)
}



#############################################################################
# Cap\Transform Variables to deal with outliers and deviance from normality #
#############################################################################

############
# San Juan #
############
imputedSJ1$total_cases <- log(imputedSJ1$total_cases+1)

imputedSJ1$precipitation_amt_mm <- log(imputedSJ1$precipitation_amt_mm+1)
# hist(imputedSJ1$precipitation_amt_mm, col="blue")

imputedSJ1$reanalysis_air_temp_k <- replace(imputedSJ1$reanalysis_air_temp_k, imputedSJ1$reanalysis_air_temp_k < 296, 296)
imputedSJ1$reanalysis_air_temp_k <- replace(imputedSJ1$reanalysis_air_temp_k, imputedSJ1$reanalysis_air_temp_k > 302, 302)  
  
imputedSJ1$reanalysis_dew_point_temp_k <- replace(imputedSJ1$reanalysis_dew_point_temp_k, 
                                                  imputedSJ1$reanalysis_dew_point_temp_k < 292, 292)    

imputedSJ1$reanalysis_max_air_temp_k <- replace(imputedSJ1$reanalysis_max_air_temp_k, imputedSJ1$reanalysis_max_air_temp_k < 298.9, 298.9)
imputedSJ1$reanalysis_max_air_temp_k <- replace(imputedSJ1$reanalysis_max_air_temp_k, imputedSJ1$reanalysis_max_air_temp_k > 304, 304)  

imputedSJ1$reanalysis_min_air_temp_k <- replace(imputedSJ1$reanalysis_min_air_temp_k, imputedSJ1$reanalysis_min_air_temp_k < 294, 294)
imputedSJ1$reanalysis_min_air_temp_k <- replace(imputedSJ1$reanalysis_min_air_temp_k, imputedSJ1$reanalysis_min_air_temp_k > 299.4, 299.4)  

imputedSJ1$reanalysis_precip_amt_kg_per_m2 <- log(imputedSJ1$reanalysis_precip_amt_kg_per_m2+1)

imputedSJ1$reanalysis_specific_humidity_g_per_kg <- replace(imputedSJ1$reanalysis_specific_humidity_g_per_kg, 
                                                            imputedSJ1$reanalysis_specific_humidity_g_per_kg < 12.68, 12.68)
imputedSJ1$reanalysis_specific_humidity_g_per_kg <- replace(imputedSJ1$reanalysis_specific_humidity_g_per_kg, 
                                                            imputedSJ1$reanalysis_specific_humidity_g_per_kg > 18.98, 18.98)  
imputedSJ1$reanalysis_specific_humidity_g_per_kg <- imputedSJ1$reanalysis_specific_humidity_g_per_kg^2

imputedSJ1$reanalysis_tdtr_k <- replace(imputedSJ1$reanalysis_tdtr_k, imputedSJ1$reanalysis_tdtr_k < 1.66, 1.66)
imputedSJ1$reanalysis_tdtr_k <- replace(imputedSJ1$reanalysis_tdtr_k, imputedSJ1$reanalysis_tdtr_k > 3.46, 3.46)

imputedSJ1$station_avg_temp_c <- replace(imputedSJ1$station_avg_temp_c, imputedSJ1$station_avg_temp_c < 24, 24)
imputedSJ1$station_avg_temp_c <- replace(imputedSJ1$station_avg_temp_c, imputedSJ1$station_avg_temp_c > 29.45, 29.45)

imputedSJ1$station_diur_temp_rng_c <- replace(imputedSJ1$station_diur_temp_rng_c, imputedSJ1$station_diur_temp_rng_c > 8.93, 8.93)

imputedSJ1$station_min_temp_c <- replace(imputedSJ1$station_min_temp_c, imputedSJ1$station_min_temp_c < 20, 20)
imputedSJ1$station_min_temp_c <- replace(imputedSJ1$station_min_temp_c, imputedSJ1$station_min_temp_c > 25, 25)

imputedSJ1$station_precip_mm <- log(imputedSJ1$station_precip_mm+1)

# These imputed flag variables are probably not predictive based on the box plots notches overlapping so I will delete these
imputedSJ1$M_reanalysis_air_temp_k <- NULL
imputedSJ1$M_reanalysis_avg_temp_k <- NULL
imputedSJ1$M_reanalysis_precip_amt_kg_per_m2 <- NULL
imputedSJ1$M_reanalysis_dew_point_temp_k <- NULL
imputedSJ1$M_reanalysis_max_air_temp_k <- NULL
imputedSJ1$M_reanalysis_min_air_temp_k <- NULL
imputedSJ1$M_reanalysis_specific_humidity_g_per_kg <- NULL
imputedSJ1$M_station_min_temp_c <- NULL
imputedSJ1$M_ndvi_ne <- NULL
imputedSJ1$M_ndvi_nw <- NULL
imputedSJ1$M_reanalysis_tdtr_k <- NULL
imputedSJ1$M_station_diur_temp_rng_c <- NULL
imputedSJ1$M_station_precip_mm <- NULL


# Now do this for test data
imputedSJT$precipitation_amt_mm <- log(imputedSJT$precipitation_amt_mm+1)
# hist(imputedSJT$precipitation_amt_mm, col="blue")

imputedSJT$reanalysis_air_temp_k <- replace(imputedSJT$reanalysis_air_temp_k, imputedSJT$reanalysis_air_temp_k < 296, 296)
imputedSJT$reanalysis_air_temp_k <- replace(imputedSJT$reanalysis_air_temp_k, imputedSJT$reanalysis_air_temp_k > 302, 302)  

imputedSJT$reanalysis_dew_point_temp_k <- replace(imputedSJT$reanalysis_dew_point_temp_k, 
                                                  imputedSJT$reanalysis_dew_point_temp_k < 292, 292)    

imputedSJT$reanalysis_max_air_temp_k <- replace(imputedSJT$reanalysis_max_air_temp_k, imputedSJT$reanalysis_max_air_temp_k < 298.9, 298.9)
imputedSJT$reanalysis_max_air_temp_k <- replace(imputedSJT$reanalysis_max_air_temp_k, imputedSJT$reanalysis_max_air_temp_k > 304, 304)  

imputedSJT$reanalysis_min_air_temp_k <- replace(imputedSJT$reanalysis_min_air_temp_k, imputedSJT$reanalysis_min_air_temp_k < 294, 294)
imputedSJT$reanalysis_min_air_temp_k <- replace(imputedSJT$reanalysis_min_air_temp_k, imputedSJT$reanalysis_min_air_temp_k > 299.4, 299.4)  

imputedSJT$reanalysis_precip_amt_kg_per_m2 <- log(imputedSJT$reanalysis_precip_amt_kg_per_m2+1)

imputedSJT$reanalysis_specific_humidity_g_per_kg <- replace(imputedSJT$reanalysis_specific_humidity_g_per_kg, 
                                                            imputedSJT$reanalysis_specific_humidity_g_per_kg < 12.68, 12.68)
imputedSJT$reanalysis_specific_humidity_g_per_kg <- replace(imputedSJT$reanalysis_specific_humidity_g_per_kg, 
                                                            imputedSJT$reanalysis_specific_humidity_g_per_kg > 18.98, 18.98)  
imputedSJT$reanalysis_specific_humidity_g_per_kg <- imputedSJT$reanalysis_specific_humidity_g_per_kg^2

imputedSJT$reanalysis_tdtr_k <- replace(imputedSJT$reanalysis_tdtr_k, imputedSJT$reanalysis_tdtr_k < 1.66, 1.66)
imputedSJT$reanalysis_tdtr_k <- replace(imputedSJT$reanalysis_tdtr_k, imputedSJT$reanalysis_tdtr_k > 3.46, 3.46)

imputedSJT$station_avg_temp_c <- replace(imputedSJT$station_avg_temp_c, imputedSJT$station_avg_temp_c < 24, 24)
imputedSJT$station_avg_temp_c <- replace(imputedSJT$station_avg_temp_c, imputedSJT$station_avg_temp_c > 29.45, 29.45)

imputedSJT$station_diur_temp_rng_c <- replace(imputedSJT$station_diur_temp_rng_c, imputedSJT$station_diur_temp_rng_c > 8.93, 8.93)

imputedSJT$station_min_temp_c <- replace(imputedSJT$station_min_temp_c, imputedSJT$station_min_temp_c < 20, 20)
imputedSJT$station_min_temp_c <- replace(imputedSJT$station_min_temp_c, imputedSJT$station_min_temp_c > 25, 25)

imputedSJT$station_precip_mm <- log(imputedSJT$station_precip_mm+1)

# These imputed flag variables are probably not predictive based on the box plots notches overlapping so I will delete these
imputedSJT$M_reanalysis_air_temp_k <- NULL
imputedSJT$M_reanalysis_avg_temp_k <- NULL
imputedSJT$M_reanalysis_precip_amt_kg_per_m2 <- NULL
imputedSJT$M_reanalysis_dew_point_temp_k <- NULL
imputedSJT$M_reanalysis_max_air_temp_k <- NULL
imputedSJT$M_reanalysis_min_air_temp_k <- NULL
imputedSJT$M_reanalysis_specific_humidity_g_per_kg <- NULL
imputedSJT$M_station_min_temp_c <- NULL
imputedSJT$M_ndvi_ne <- NULL
imputedSJT$M_ndvi_nw <- NULL
imputedSJT$M_reanalysis_tdtr_k <- NULL
imputedSJT$M_station_diur_temp_rng_c <- NULL
imputedSJT$M_station_precip_mm <- NULL


############
# Iquitos  #
############

imputedIQ1$total_cases <- replace(imputedIQ1$total_cases, imputedIQ1$total_cases > 45, 45)
imputedIQ1$total_cases <- log(imputedIQ1$total_cases+1)

imputedIQ1$ndvi_ne <- replace(imputedIQ1$ndvi_ne, imputedIQ1$ndvi_ne > .464, .464 )

imputedIQ1$precipitation_amt_mm <- replace(imputedIQ1$precipitation_amt_mm, imputedIQ1$precipitation_amt_mm > 157, 157 )

imputedIQ1$reanalysis_avg_temp_k <- replace(imputedIQ1$reanalysis_avg_temp_k, imputedIQ1$reanalysis_avg_temp_k < 296, 296 )

imputedIQ1$reanalysis_dew_point_temp_k <- replace(imputedIQ1$reanalysis_dew_point_temp_k, 
                                                  imputedIQ1$reanalysis_dew_point_temp_k < 293, 293 )
imputedIQ1$reanalysis_dew_point_temp_k <- imputedIQ1$reanalysis_dew_point_temp_k^5


imputedIQ1$reanalysis_min_air_temp_k <- replace(imputedIQ1$reanalysis_min_air_temp_k, 
                                                imputedIQ1$reanalysis_min_air_temp_k < 290, 290 )
imputedIQ1$reanalysis_min_air_temp_k <- replace(imputedIQ1$reanalysis_min_air_temp_k, 
                                                imputedIQ1$reanalysis_min_air_temp_k >295, 295 )
imputedIQ1$reanalysis_min_air_temp_k <- imputedIQ1$reanalysis_min_air_temp_k^2

imputedIQ1$reanalysis_precip_amt_kg_per_m2 <- log(imputedIQ1$reanalysis_precip_amt_kg_per_m2+1)

imputedIQ1$reanalysis_relative_humidity_percent <- imputedIQ1$reanalysis_relative_humidity_percent^6

imputedIQ1$reanalysis_specific_humidity_g_per_kg <- imputedIQ1$reanalysis_specific_humidity_g_per_kg^3

imputedIQ1$station_avg_temp_c <- replace(imputedIQ1$station_avg_temp_c, imputedIQ1$station_avg_temp_c < 24.45, 24.45 )

imputedIQ1$station_max_temp_c <- replace(imputedIQ1$station_max_temp_c, imputedIQ1$station_max_temp_c >37, 37 )

imputedIQ1$station_min_temp_c <- replace(imputedIQ1$station_min_temp_c, imputedIQ1$station_min_temp_c < 18.6, 18.6 )
imputedIQ1$station_min_temp_c <- replace(imputedIQ1$station_min_temp_c, imputedIQ1$station_min_temp_c > 23.6, 23.6 )

imputedIQ1$station_precip_mm <- replace(imputedIQ1$station_precip_mm, imputedIQ1$station_precip_mm > 281, 281 )
imputedIQ1$station_precip_mm <- log(imputedIQ1$station_precip_mm+1)

# These imputed flag variables are probably not predictive based on the box plots notches overlapping so I will delete these
imputedIQ1$M_station_avg_temp_c <- NULL
imputedIQ1$M_ndvi_ne <- NULL
imputedIQ1$M_ndvi_nw <- NULL
imputedIQ1$M_ndvi_se <- NULL
imputedIQ1$M_ndvi_sw <- NULL
imputedIQ1$M_reanalysis_tdtr_k <- NULL
imputedIQ1$M_station_diur_temp_rng_c <- NULL
imputedIQ1$M_station_precip_mm <- NULL

# Now the test data
imputedIQT$total_cases <- replace(imputedIQT$total_cases, imputedIQT$total_cases > 45, 45)
imputedIQT$total_cases <- log(imputedIQT$total_cases+1)

imputedIQT$ndvi_ne <- replace(imputedIQT$ndvi_ne, imputedIQT$ndvi_ne > .464, .464 )

imputedIQT$precipitation_amt_mm <- replace(imputedIQT$precipitation_amt_mm, imputedIQT$precipitation_amt_mm > 157, 157 )

imputedIQT$reanalysis_avg_temp_k <- replace(imputedIQT$reanalysis_avg_temp_k, imputedIQT$reanalysis_avg_temp_k < 296, 296 )

imputedIQT$reanalysis_dew_point_temp_k <- replace(imputedIQT$reanalysis_dew_point_temp_k, 
                                                  imputedIQT$reanalysis_dew_point_temp_k < 293, 293 )
imputedIQT$reanalysis_dew_point_temp_k <- imputedIQT$reanalysis_dew_point_temp_k^5


imputedIQT$reanalysis_min_air_temp_k <- replace(imputedIQT$reanalysis_min_air_temp_k, 
                                                imputedIQT$reanalysis_min_air_temp_k < 290, 290 )
imputedIQT$reanalysis_min_air_temp_k <- replace(imputedIQT$reanalysis_min_air_temp_k, 
                                                imputedIQT$reanalysis_min_air_temp_k >295, 295 )
imputedIQT$reanalysis_min_air_temp_k <- imputedIQT$reanalysis_min_air_temp_k^2

imputedIQT$reanalysis_precip_amt_kg_per_m2 <- log(imputedIQT$reanalysis_precip_amt_kg_per_m2+1)

imputedIQT$reanalysis_relative_humidity_percent <- imputedIQT$reanalysis_relative_humidity_percent^6

imputedIQT$reanalysis_specific_humidity_g_per_kg <- imputedIQT$reanalysis_specific_humidity_g_per_kg^3

imputedIQT$station_avg_temp_c <- replace(imputedIQT$station_avg_temp_c, imputedIQT$station_avg_temp_c < 24.45, 24.45 )

imputedIQT$station_max_temp_c <- replace(imputedIQT$station_max_temp_c, imputedIQT$station_max_temp_c >37, 37 )

imputedIQT$station_min_temp_c <- replace(imputedIQT$station_min_temp_c, imputedIQT$station_min_temp_c < 18.6, 18.6 )
imputedIQT$station_min_temp_c <- replace(imputedIQT$station_min_temp_c, imputedIQT$station_min_temp_c > 23.6, 23.6 )

imputedIQT$station_precip_mm <- replace(imputedIQT$station_precip_mm, imputedIQT$station_precip_mm > 281, 281 )
imputedIQT$station_precip_mm <- log(imputedIQT$station_precip_mm+1)

# These imputed flag variables are probably not predictive based on the box plots notches overlapping so I will delete these
imputedIQT$M_station_avg_temp_c <- NULL
imputedIQT$M_ndvi_ne <- NULL
imputedIQT$M_ndvi_nw <- NULL
imputedIQT$M_ndvi_se <- NULL
imputedIQT$M_ndvi_sw <- NULL
imputedIQT$M_reanalysis_tdtr_k <- NULL
imputedIQT$M_station_diur_temp_rng_c <- NULL
imputedIQT$M_station_precip_mm <- NULL


########################
# Create xreg datasets #
########################

# create copies of the imputed datasets
SJxreg <- cbind(imputedSJ1[1:703,])
SJxregt <- cbind(imputedSJ1[704:936,])

IQxreg <- cbind(imputedIQ1[1:390,])
IQxregt <- cbind(imputedIQ1[391:520,])



# For now keep all the variables except city and total_cases
SJxreg$city <- NULL
SJxreg$total_cases <- NULL
SJxregt$city <- NULL
SJxregt$total_cases <- NULL

IQxreg$city <- NULL
IQxreg$total_cases <- NULL
IQxregt$city <- NULL
IQxregt$total_cases <- NULL

# xregt is rank deficient -- need to find the problem and remove one of the variables
caret::findLinearCombos(SJxreg)
caret::findLinearCombos(IQxreg)

SJxreg[29] <- NULL
SJxreg[28] <- NULL
SJxreg[26] <- NULL
SJxreg[24] <- NULL
SJxreg$week <- NULL

SJxregt[29] <- NULL
SJxregt[28] <- NULL
SJxregt[26] <- NULL
SJxregt[24] <- NULL
SJxregt$week <- NULL

IQxreg[24:29] <- NULL
IQxregt[24:29] <- NULL
IQxreg[26:28] <- NULL
IQxregt[26:28] <- NULL
IQxreg$week <- NULL
IQxregt$week <- NULL

# corrplot(cor(SJxreg))


########################################################
# Write out the data files so they can be read back in #
########################################################

write.csv(imputedSJ1, file = "D:/Kim MSPA/Predict 413/Final Project/imputedSJ1.csv")
write.csv(imputedIQ1, file = "D:/Kim MSPA/Predict 413/Final Project/imputedIQ1.csv")

write.csv(SJTS, file = "D:/Kim MSPA/Predict 413/Final Project/SJTS.csv")
write.csv(IQTS, file = "D:/Kim MSPA/Predict 413/Final Project/IQTS.csv")

write.csv(SJxreg, file = "D:/Kim MSPA/Predict 413/Final Project/SJxreg.csv")
write.csv(SJxregt, file = "D:/Kim MSPA/Predict 413/Final Project/SJxregt.csv")

write.csv(IQxreg, file = "D:/Kim MSPA/Predict 413/Final Project/IQxreg.csv")
write.csv(IQxregt, file = "D:/Kim MSPA/Predict 413/Final Project/IQxregt.csv")

write.csv(imputedIQT, file = "D:/Kim MSPA/Predict 413/Final Project/imputedIQT.csv")
write.csv(imputedSJT, file = "D:/Kim MSPA/Predict 413/Final Project/imputedSJT.csv")


##########################
# Read in the data files #
##########################
imputedSJ1 <- read.csv("D:/Kim MSPA/Predict 413/Final Project/imputedSJ1.csv", sep = ",")
imputedIQ1 <- read.csv("D:/Kim MSPA/Predict 413/Final Project/imputedIQ1.csv", sep = ",")

SJxreg <- read.csv("D:/Kim MSPA/Predict 413/Final Project/SJxreg.csv", sep = ",")
SJxregt <- read.csv("D:/Kim MSPA/Predict 413/Final Project/SJxregt.csv", sep = ",")

IQxreg <- read.csv("D:/Kim MSPA/Predict 413/Final Project/IQxreg.csv", sep = ",")
IQxregt <- read.csv("D:/Kim MSPA/Predict 413/Final Project/IQxregt.csv", sep = ",")

imputedIQT <- read.csv("D:/Kim MSPA/Predict 413/Final Project/imputedIQT.csv", sep = ",")
imputedSJT <- read.csv("D:/Kim MSPA/Predict 413/Final Project/imputedSJT.csv", sep = ",")

# imputedSJ1 <- read.csv("C:/f/imputedSJ1.csv", sep = ",")
# imputedIQ1 <- read.csv("C:/f/imputedIQ1.csv", sep = ",")
# 
# SJxreg <- read.csv("C:/f/SJxreg.csv", sep = ",")
# SJxregt <- read.csv("C:/f/SJxregt.csv", sep = ",")
# 
# IQxreg <- read.csv("C:/f/IQxreg.csv", sep = ",")
# IQxregt <- read.csv("C:/f/IQxregt.csv", sep = ",")
# 
# imputedIQT <- read.csv("C:/f/imputedIQT.csv", sep = ",")
# imputedSJT <- read.csv("C:/f/imputedSJT.csv", sep = ",")

##########################
# Create train\test sets #
##########################

# Create time series for each city
SJTS <- ts(imputedSJ1$total_cases, frequency = 52, start = c(1990,18), end=c(2008,17))
IQTS <- ts(imputedIQ1$total_cases, frequency = 52, start = c(2000,26), end=c(2010,25))

# Create train/test for San Juan
trainSJ <-window(SJTS,end = c(2003,44))
testSJ <-window(SJTS,start =c(2003,45), end=c(2008,17))

# Create train/test for Iquitos
trainIQ <-window(IQTS,end = c(2007,51))
testIQ <-window(IQTS,start =c(2007,52), end=c(2010,25))

str(trainIQ)


###############################
# Auto Arima Models - no xreg #
###############################

############
# San Juan #
############
set.seed(111)
fitAA_SJ_noxreg <- auto.arima(trainSJ)
forecast_SJ_noxreg <- forecast(fitAA_SJ_noxreg,h=233)
AccuracyAA_SJ_noxreg <- accuracy(exp(forecast_SJ_noxreg$mean),exp(testSJ))
AccuracyAA_SJ_noxreg

# Try another model where we decompose the data and reseasonalize it
dSJ_train <- decompose(trainSJ, type="additive")
dSJ_test <- decompose(testSJ, type="additive")
adjSJ <- seasadj(dSJ_train)
fitd_AA_SJ_noxreg <- auto.arima(adjSJ)
forecast_RSJ_noxreg <- forecast(fitd_AA_SJ_noxreg,h=233)
SeasSJ <- ts(dSJ_test$seasonal, frequency = 52, start =c(2003,45), end=c(2008,17))
forecast_SAA_SJ_noxreg <- forecast_RSJ_noxreg$mean + SeasSJ
AccuracySAA_EXP_noxreg <- accuracy(exp(forecast_SAA_SJ_noxreg),exp(testSJ))
AccuracySAA_EXP_noxreg

# Try ensemble model  -- Decomposed model is still the best
forecast_AA_ENS_noxreg <- (forecast_SAA_SJ_noxreg + forecast_SJ_noxreg$mean)/2
Accuracy_AA_ENS_noxreg <- accuracy(exp(forecast_AA_ENS_noxreg),exp(testSJ))
Accuracy_AA_ENS_noxreg

# Don't forget to convert back to exp from log after creating final file
par(mfcol=c(1,1))
plot(exp(trainSJ),ylab="San Juan Dengue Cases", main="Model Comparison San Juan",ylim=c(min(exp(trainSJ)),max(exp(trainSJ))),
     xlim=c(1990,2008)) 
lines(exp(testSJ),col=2,lw=2)
lines(exp(forecast_RSJ_noxreg$mean),col=3,lw=2)
lines(exp(forecast_SAA_SJ_noxreg),col=4,lw=2)
lines(exp(forecast_AA_ENS_noxreg),col=5,lw=2)
legend("topleft", lty=1,col=c(2,3,4,5), lw=2,
       legend=c("Test Data","Auto-Arima","Decomposed Auto-Arima","AA Ensemble"))


############
# Iquitos  #
############
head(IQxreg)
IQxreg$X <- NULL
IQxregt$X <- NULL
fitAA_IQ_noxreg <- auto.arima(trainIQ)
forecast_IQ_noxreg  <- forecast(fitAA_IQ_noxreg,h=130)
AccuracyAA_IQ_noxreg  <- accuracy(forecast_IQ_noxreg ,testIQ)
AccuracyAA_EXP_IQ_noxreg  <- accuracy(exp(forecast_IQ_noxreg$mean),exp(testIQ))
AccuracyAA_EXP_IQ_noxreg

# Try another model where we decompose the data and reseasonalize it
dIQ_train <- decompose(trainIQ, type="multiplicative")
dIQ_test <- decompose(testIQ, type="multiplicative")
adjIQ <- seasadj(dIQ_train)
fitd_SAA_IQ_noxreg  <- auto.arima(adjIQ)
forecast_RIQ_noxreg  <- forecast(fitd_SAA_IQ_noxreg,h=130)
SeasIQ <- ts(dIQ_test$seasonal, frequency = 52, start =c(2008,1), end=c(2010,25))
forecast_SAA_IQ_noxreg  <- forecast_RIQ_noxreg$mean + SeasIQ
Accuracy_SAAIQ_noxreg <- accuracy(exp(forecast_SAA_IQ_noxreg),exp(testIQ))
Accuracy_SAAIQ_noxreg

# Try ensemble model  -- straight auto arima is still the best
forecast_AAIQ_ENS_noxreg <- (forecast_SAA_IQ_noxreg  + forecast_IQ$mean)/2
Accuracy_AAIQ_ENS_noxreg  <- accuracy(exp(forecast_AAIQ_ENS_noxreg),exp(testSJ))
Accuracy_AAIQ_ENS_noxreg

# Don't forget to convert back to exp from log after creating final file
par(mfcol=c(1,1))
plot(exp(trainIQ),ylab="Iquitos Dengue Cases", main="Model Comparison",ylim=c(min(exp(trainIQ)),max(exp(forecast_SAA_IQ))),
     xlim=c(2000,2010)) 
lines(exp(testIQ),col=2,lw=2)
lines(exp(forecast_IQ_noxreg$mean),col=3,lw=2)
lines(exp(forecast_SAA_IQ_noxreg),col=4,lw=2)
lines(exp(forecast_AAIQ_ENS_noxreg),col=5,lw=2)
legend("topleft", lty=1,col=c(2,3,4,5), lw=2,
       legend=c("Test Data","Auto-Arima","Decomposed Auto-Arima", "AA Ensemble"))




#######################################
# Final Forecast for Auto Arima Model #
#######################################

cols <- colnames(SJxregt)
sub_SJxregt <- subset(imputedSJT, select=cols)
sub_SJxregt <- rbind(SJxregt, sub_SJxregt)
forecast_AASJ_noxreg <- forecast(fitd_AA_SJ_noxreg,h=493)
SeasSJ <- ts(dSJ_train$seasonal, frequency = 52, start =c(2008,18), end=c(2013,17))
Final_AA_SJ_noxreg <- as.numeric(forecast_AASJ_noxreg$mean[234:493]) + as.numeric(SeasSJ)
Final_AA_SJ_noxreg <- round(exp(Final_AA_SJ_noxreg))

cols <- colnames(IQxregt)
sub_IQxregt <- subset(imputedIQT, select=cols)
sub_IQxregt <- rbind(IQxregt, sub_IQxregt)
forecast_AAIQ_noxreg <- forecast(fitAA_IQ_noxreg,h=286)
Final_AA_IQ_noxreg <- round(exp(as.numeric(forecast_AAIQ_noxreg$mean[131:286])))

# Create output file!!!!!
AutoArimaSJ_noxreg  <- data.frame("sj", sub_SJxregt$year[234:493], sub_SJxregt$weekofyear[234:493], Final_AA_SJ_noxreg)
colnames(AutoArimaSJ_noxreg ) <- c("city", "year", "weekofyear", "total_cases")
head(AutoArimaSJ_noxreg )

AutoArimaIQ_noxreg  <- data.frame("iq", sub_IQxregt$year[131:286], sub_IQxregt$weekofyear[131:286], Final_AA_IQ_noxreg)
colnames(AutoArimaIQ_noxreg ) <- c("city", "year", "weekofyear", "total_cases")
head(AutoArimaIQ_noxreg )

str(AutoArimaIQ_noxreg)
submitAutoArima_noxreg <- rbind(AutoArimaSJ_noxreg, AutoArimaIQ_noxreg)

# DrivenData score = 30.8389
write.csv(submitAutoArima_noxreg, "D:/Kim MSPA/Predict 413/Final Project/submitAutoArima_noxreg.csv",
          row.names=F)




#####################
# Auto Arima Models #
#####################

############
# San Juan #
############

fitAA_SJ <- auto.arima(trainSJ, xreg=SJxreg)
forecast_SJ <- forecast(fitAA_SJ,xreg=SJxregt,h=233)
AccuracyAA <- accuracy(forecast_SJ,testSJ)
AccuracyAA_EXP <- accuracy(exp(forecast_SJ$mean),exp(testSJ))


# Try another model where we decompose the data and reseasonalize it
dSJ_train <- decompose(trainSJ, type="additive")
dSJ_test <- decompose(testSJ, type="additive")
adjSJ <- seasadj(dSJ_train)
fitd_AA_SJ <- auto.arima(adjSJ, xreg=SJxreg)
forecast_RSJ <- forecast(fitd_AA_SJ,xreg=SJxregt,h=233)
SeasSJ <- ts(dSJ_test$seasonal, frequency = 52, start =c(2003,45), end=c(2008,17))
forecast_SAA_SJ <- forecast_RSJ$mean + SeasSJ
AccuracySAA_EXP <- accuracy(exp(forecast_SAA_SJ),exp(testSJ))

# Try ensemble model  -- Decomposed model is still the best
forecast_AA_ENS <- (forecast_SAA_SJ + forecast_SJ$mean)/2
Accuracy_AA_ENS <- accuracy(exp(forecast_AA_ENS),exp(testSJ))

# Don't forget to convert back to exp from log after creating final file
par(mfcol=c(1,1))
plot(exp(trainSJ),ylab="San Juan Dengue Cases", main="Model Comparison San Juan",ylim=c(min(exp(trainSJ)),max(exp(trainSJ)))) 
lines(exp(testSJ),col=2,lw=2)
lines(exp(forecast_SJ$mean),col=3,lw=2)
lines(exp(forecast_SAA_SJ),col=4,lw=2)
lines(exp(forecast_AA_ENS),col=5,lw=2)
legend("topleft", lty=1,col=c(2,3,4,5), lw=2,
       legend=c("Test Data","Auto-Arima","Decomposed Auto-Arima","AA Ensemble"))


############
# Iquitos  #
############
head(IQxreg)
IQxreg$X <- NULL
IQxregt$X <- NULL
fitAA_IQ <- auto.arima(trainIQ, xreg=IQxreg)
forecast_IQ <- forecast(fitAA_IQ,xreg=IQxregt,h=130)
AccuracyAA_IQ <- accuracy(forecast_IQ,testIQ)
AccuracyAA_EXP_IQ <- accuracy(exp(forecast_IQ$mean),exp(testIQ))

# Try another model where we decompose the data and reseasonalize it
dIQ_train <- decompose(trainIQ, type="multiplicative")
dIQ_test <- decompose(testIQ, type="multiplicative")
adjIQ <- seasadj(dIQ_train)
fitd_AA_IQ <- auto.arima(adjIQ, xreg=IQxreg)
forecast_RIQ <- forecast(fitd_AA_IQ,xreg=IQxregt,h=130)
SeasIQ <- ts(dIQ_test$seasonal, frequency = 52, start =c(2008,1), end=c(2010,25))
forecast_SAA_IQ <- forecast_RIQ$mean + SeasIQ
Accuracy_SAAIQ_EXP <- accuracy(exp(forecast_SAA_IQ),exp(testIQ))

# Try ensemble model  -- straight auto arima is still the best
forecast_AAIQ_ENS <- (forecast_SAA_IQ + forecast_IQ$mean)/2
Accuracy_AAIQ_ENS <- accuracy(exp(forecast_AAIQ_ENS),exp(testSJ))

# Don't forget to convert back to exp from log after creating final file
par(mfcol=c(1,1))
plot(exp(trainIQ),ylab="Iquitos Dengue Cases", main="Model Comparison",ylim=c(min(exp(trainIQ)),max(exp(forecast_SAA_IQ)))) 
lines(exp(testIQ),col=2,lw=2)
lines(exp(forecast_IQ$mean),col=3,lw=2)
lines(exp(forecast_SAA_IQ),col=4,lw=2)
lines(exp(forecast_AAIQ_ENS),col=5,lw=2)
legend("topleft", lty=1,col=c(2,3,4,5), lw=2,
       legend=c("Test Data","Auto-Arima","Decomposed Auto-Arima", "AA Ensemble"))


forecastAll <- c(forecast_SAA_SJ, forecast_IQ$mean)
testAll <- c(testSJ, testIQ)

AccuracyAll <- accuracy(round(exp(forecastAll)),round(exp(testAll)))


#######################################
# Final Forecast for Auto Arima Model #
#######################################

cols <- colnames(SJxregt)
sub_SJxregt <- subset(imputedSJT, select=cols)
sub_SJxregt <- rbind(SJxregt, sub_SJxregt)
forecast_AASJ <- forecast(fitd_AA_SJ,xreg=sub_SJxregt,h=493)
SeasSJ <- ts(dSJ_train$seasonal, frequency = 52, start =c(2008,18), end=c(2013,17))
Final_AA_SJ2 <- as.numeric(forecast_AASJ$mean[234:493]) + as.numeric(SeasSJ)
Final_AA_SJ2 <- round(exp(Final_AA_SJ2))

cols <- colnames(IQxregt)
sub_IQxregt <- subset(imputedIQT, select=cols)
sub_IQxregt <- rbind(IQxregt, sub_IQxregt)
forecast_AAIQ <- forecast(fitAA_IQ,xreg=sub_IQxregt,h=286)
Final_AA_IQ2 <- round(exp(as.numeric(forecast_AAIQ$mean[131:286])))

# Create output file!!!!!
AutoArimaSJ2 <- data.frame("sj", sub_SJxregt$year[234:493], sub_SJxregt$weekofyear[234:493], Final_AA_SJ2)
colnames(AutoArimaSJ2) <- c("city", "year", "weekofyear", "total_cases")
head(AutoArimaSJ2)

AutoArimaIQ2 <- data.frame("iq", sub_IQxregt$year[131:286], sub_IQxregt$weekofyear[131:286], Final_AA_IQ2)
colnames(AutoArimaIQ2) <- c("city", "year", "weekofyear", "total_cases")
head(AutoArimaIQ2)

str(AutoArimaIQ)
submitAutoArima2 <- rbind(AutoArimaSJ2, AutoArimaIQ2)

# DrivenData score = 29.7236
#write.csv(submitAutoArima2, "D:/Kim MSPA/Predict 413/Final Project/SubmitAutoArima2.csv",
#          row.names=F)



##########################################
# Analysis of residuals for above models #
# Create new model using manual method   #
##########################################

# Couldn't find a model that beat the fitd_AA_SJ
Acf(residuals(fitd_AA_SJ))
Pacf(residuals(fitd_AA_SJ))
Box.test(residuals(fitd_AA_SJ), lag=52, fitdf=4, type="Ljung")


Acf(residuals(fitAA_IQ_new))
Pacf(residuals(fitAA_IQ_new))
Box.test(residuals(fitAA_IQ_new), lag=52, fitdf=4, type="Ljung")

# This is an improvement so try using this instead
fitAA_IQ_new <- arima(trainIQ, xreg=IQxreg, order = c(12,1,0), seasonal=c(0,0,1))
forecast_IQ_new <- forecast(fitAA_IQ_new,xreg=IQxregt,h=130)
AccuracyAA_EXP_IQ_new <- accuracy(exp(forecast_IQ_new$mean),exp(testIQ))
AccuracyAA_EXP_IQ_new

# Try ensemble model with Decomposed and manual -- This is the best IQ model so far!!!!
forecast_IQ_ENS <- (forecast_SAA_IQ + forecast_IQ_new$mean)/2
Accuracy_IQ_ENS <- accuracy(exp(forecast_AAIQ_ENS),exp(testSJ))

par(mfcol=c(1,1))
plot(exp(trainIQ),ylab="Iquitos Dengue Cases", main="Model Comparison",ylim=c(min(exp(trainIQ)),max(exp(forecast_SAA_IQ)))) 
lines(exp(testIQ),col=2,lw=2)
lines(exp(forecast_IQ$mean),col=3,lw=2)
lines(exp(forecast_SAA_IQ),col=4,lw=2)
lines(exp(forecast_IQ_ENS),col=5,lw=2)
lines(exp(forecast_IQ_new$mean),col=6, lw=2)
legend("topleft", lty=1,col=c(2,3,4,5,6), lw=2,
       legend=c("Test Data","Auto-Arima","Decomposed Auto-Arima", "AA Ensemble","Manual Arima"))



# Put together forecast for Iquitos
cols <- colnames(IQxregt)
sub_IQxregt <- subset(imputedIQT, select=cols)
sub_IQxregt <- rbind(IQxregt, sub_IQxregt)
forecast_MAIQ <- forecast(fitAA_IQ_new,xreg=sub_IQxregt,h=286)
Final_MA_IQ <- round(exp(as.numeric(forecast_MAIQ$mean[131:286])))


# Create output file - decomposed auto arima for SJ and manual arima for Iquitos
ManualArimaSJ <- data.frame("sj", sub_SJxregt$year[234:493], sub_SJxregt$weekofyear[234:493], Final_AA_SJ2)
colnames(ManualArimaSJ) <- c("city", "year", "weekofyear", "total_cases")
head(ManualArimaSJ)

ManualArimaIQ <- data.frame("iq", sub_IQxregt$year[131:286], sub_IQxregt$weekofyear[131:286], Final_MA_IQ)
colnames(ManualArimaIQ) <- c("city", "year", "weekofyear", "total_cases")
head(ManualArimaIQ)

submitArimaManual <- rbind(ManualArimaSJ, ManualArimaIQ)
tail(submitArimaManual)


# DrivenData score = 27.3798
#write.csv(submitArimaManual, "D:/Kim MSPA/Predict 413/Final Project/submitArimaManual.csv",
#          row.names=F)

# Do a final forecast for decomposed AA for Iquitos 
cols <- colnames(IQxregt)
sub_IQxregt <- subset(imputedIQT, select=cols)
sub_IQxregt <- rbind(IQxregt, sub_IQxregt)
forecast_dAAIQ <- forecast(fitd_AA_IQ,xreg=sub_IQxregt,h=286)
SeasIQ <- ts(dIQ_test$seasonal, frequency = 52, start =c(2010,26), end=c(2013,25))
Final_dAA_IQ <- as.numeric(forecast_AASJ$mean[131:286]) + as.numeric(SeasIQ)
Final_dAA_IQ <- round(exp(Final_dAA_IQ))

IQ_Ensemble <- round((Final_dAA_IQ + Final_MA_IQ)/2)


# Create output file - decomposed auto arima for SJ and ensemble for Iquitos
ManualArimaSJ <- data.frame("sj", sub_SJxregt$year[234:493], sub_SJxregt$weekofyear[234:493], Final_AA_SJ2)
colnames(ManualArimaSJ) <- c("city", "year", "weekofyear", "total_cases")
head(ManualArimaSJ)

Ens_IQ <- data.frame("iq", sub_IQxregt$year[131:286], sub_IQxregt$weekofyear[131:286], IQ_Ensemble)
colnames(Ens_IQ) <- c("city", "year", "weekofyear", "total_cases")
head(Ens_IQ)

submitCombodArimaEns <- rbind(ManualArimaSJ, Ens_IQ)
tail(submitCombodArimaEns)


# DrivenData score = NEED TO SUBMIT
#write.csv(submitCombodArimaEns, "D:/Kim MSPA/Predict 413/Final Project/submitCombodArimaEns.csv",
#          row.names=F)




###############
# Garch Model #
###############

############
# San Juan #
############

# Using the seasonally adjusted data as this worked well for the Arima models

# These tests show that the series is serially uncorrelated and therefore is a candidate for a volatility model
# (the p-value is significant and thus the null hypotheses that the means are equal to zero should be rejected)
t.test(adjSJ)
Box.test(adjSJ, lag=52, type='Ljung')

# These graphs support the findings above
acf(adjSJ,lag=52)
Box.test(abs(adjSJ), lag=52, type='Ljung')

# Now specify a mean equation
y = adjSJ - mean(adjSJ)

# Check for conditional heteroscedasticity - (Ljung-Box statistic) this shows ARCH effect
Box.test(y^2, lag=52, type='Ljung')

# Fit the garch model - garch(1,1) is the best AIC 
fitGarch <- garchFit(formula=~1+garch(1,1), data =adjSJ, cond.dist="std", trace=F)
summary(fitGarch)

# Obtain volatility
vol <- volatility(fitGarch)
volK <- ts(vol, frequency=52, start=c(2007,11))

# Standardized residuals
res = residuals(fitGarch,standardize=T)
resK = ts(res, frequency=52, start=c(2007,11))

par(mfcol=c(2,1))
plot(volK, xlab='Year', ylab='Volatility', type='l')
plot(resK, xlab='Year', ylab='residuals', type='l')

acf(res,lag=52)
pacf(res, lag=52)


# Not too happy with the ACF & PACF plot and also with the failures of the Ljung-Box test - re do with an arma
fitGarchSJ <- garchFit(formula=~arma(2,40)+garch(1,1), data = adjSJ, cond.dist="std", trace=F)
summary(fitGarchSJ)

# Obtain volatility
vol <- volatility(fitGarch)
volK <- ts(vol, frequency=52, start=c(2007,11))

# Standardized residuals
res = residuals(fitGarch,standardize=T)
resK = ts(res, frequency=52, start=c(2007,11))

par(mfcol=c(2,1))
plot(volK, xlab='Year', ylab='Volatility', type='l')
plot(resK, xlab='Year', ylab='residuals', type='l')

# ACF and PACF plots for residuals
acf(res,lag=52)
pacf(res, lag=52)


predGARCH <- predict(fitGarchSJ, n.ahead=233, method="ML")
GarchPrediction <- ts(predGARCH$meanForecast, frequency = 52, start =c(2003,45), end=c(2008,17))
SeasSJ <- ts(dSJ_test$seasonal, frequency = 52, start =c(2003,45), end=c(2008,17))
forecast_GARCH_SJ <- GarchPrediction + SeasSJ
AccuracyGARCHSJ <- accuracy(exp(forecast_GARCH_SJ),exp(testSJ))



par(mfcol=c(1,1))
plot(exp(trainSJ),ylab="San Juan Dengue Cases", main="Model Comparison",ylim=c(min(exp(trainSJ)),max(exp(trainSJ)))) 
lines(exp(testSJ),col=2,lw=2)
lines(exp(forecast_SJ$mean),col=3,lw=2)
lines(exp(forecast_SAA_SJ),col=4,lw=2)
lines(exp(forecast_GARCH_SJ),col=5,lw=2)
legend("topleft", lty=1,col=c(2,3,4,5), lw=2,
       legend=c("Test Data","Auto-Arima","Decomposed Auto-Arima","Garch"))



###########
# Iquitos #
###########
t.test(trainIQ)
Box.test(trainIQ, lag=52, type='Ljung')

# These graphs support the findings above
acf(trainIQ,lag=52)
Box.test(abs(trainIQ), lag=52, type='Ljung')

# Now specify a mean equation
y = adjSJ - mean(trainIQ)

# Check for conditional heteroscedasticity - (Ljung-Box statistic) this shows ARCH effect
Box.test(y^2, lag=52, type='Ljung')

# Fit the garch model - garch(1,1) is the best AIC 
fitGarch <- garchFit(formula=~1+garch(1,1), data =trainIQ, cond.dist="std", trace=F)
summary(fitGarch)

# Obtain volatility
vol <- volatility(fitGarch)
volK <- ts(vol, frequency=52, start=c(2007,11))

# Standardized residuals
res = residuals(fitGarch,standardize=T)
resK = ts(res, frequency=52, start=c(2007,11))

par(mfcol=c(2,1))
plot(volK, xlab='Year', ylab='Volatility', type='l')
plot(resK, xlab='Year', ylab='residuals', type='l')

acf(res,lag=52)
pacf(res, lag=52)


# Not too happy with the ACF & PACF plot and also with the failures of the Ljung-Box test - re do with an arma
fitGarch <- garchFit(formula=~arma(2,17)+garch(1,1), data = adjSJ, cond.dist="std", trace=F)
summary(fitGarch)

# Obtain volatility
vol <- volatility(fitGarch)
volK <- ts(vol, frequency=52, start=c(2007,11))

# Standardized residuals
res = residuals(fitGarch,standardize=T)
resK = ts(res, frequency=52, start=c(2007,11))

par(mfcol=c(2,1))
plot(volK, xlab='Year', ylab='Volatility', type='l')
plot(resK, xlab='Year', ylab='residuals', type='l')

# ACF and PACF plots for residuals
acf(res,lag=52)
pacf(res, lag=52)


predGARCH <- predict(fitGarch, n.ahead=130, method="ML")
GarchPrediction_IQ <- ts(predGARCH$meanForecast, frequency = 52, start =c(2008,1), end=c(2010,25))
AccuracyGARCHIQ <- accuracy(exp(GarchPrediction_IQ),exp(testIQ))

#######################################
# Try with decomposing seasonal data #
#######################################

# Fit the garch model - garch(1,1) is the best AIC 
fitGarchIQ <- garchFit(formula=~1+garch(1,1), data =adjIQ, cond.dist="std", trace=F)
summary(fitGarchIQ)

# Obtain volatility
vol <- volatility(fitGarchIQ)
volK <- ts(vol, frequency=52, start=c(2007,11))

# Standardized residuals
res = residuals(fitGarchIQ,standardize=T)
resK = ts(res, frequency=52, start=c(2007,11))

par(mfcol=c(2,1))
plot(volK, xlab='Year', ylab='Volatility', type='l')
plot(resK, xlab='Year', ylab='residuals', type='l')

acf(res,lag=52)
pacf(res, lag=52)


# Not too happy with the ACF & PACF plot and also with the failures of the Ljung-Box test - re do with an arma
fitGarchIQ <- garchFit(formula=~arma(2,29)+garch(1,1), data = adjIQ, cond.dist="std", trace=F)
summary(fitGarchIQ)

# Obtain volatility
vol <- volatility(fitGarchIQ)
volK <- ts(vol, frequency=52, start=c(2007,11))

# Standardized residuals
res = residuals(fitGarchIQ,standardize=T)
resK = ts(res, frequency=52, start=c(2007,11))

par(mfcol=c(2,1))
plot(volK, xlab='Year', ylab='Volatility', type='l')
plot(resK, xlab='Year', ylab='residuals', type='l')

# ACF and PACF plots for residuals
acf(res,lag=52)
pacf(res, lag=52)


predGARCH <- predict(fitGarchIQ, n.ahead=130, method="ML")
GarchPrediction <- ts(predGARCH$meanForecast, frequency = 52,  start =c(2008,1), end=c(2010,25))
SeasIQ <- ts(dIQ_test$seasonal, frequency = 52,  start =c(2008,1), end=c(2010,25))
forecast_GARCH_IQ <- GarchPrediction + SeasIQ
AccuracyGARCHdIQ <- accuracy(exp(forecast_GARCH_IQ),exp(testIQ))


par(mfcol=c(1,1))
plot(exp(trainIQ),ylab="Iquitos Dengue Cases", main="Model Comparison",ylim=c(min(exp(trainIQ)),max(exp(forecast_SAA_IQ)))) 
lines(exp(testIQ),col=2,lw=2)
lines(exp(forecast_IQ$mean),col=3,lw=2)
lines(exp(forecast_SAA_IQ),col=4,lw=2)
lines(exp(forecast_GARCH_IQ), col=5, lw=2)
legend("topleft", lty=1,col=c(2,3,4,5), lw=2,
       legend=c("Test Data","Auto-Arima","Decomposed Auto-Arima","Garch"))



##################################
# Final Forecast for Garch Model #
##################################
SJxregt$X <- NULL
cols <- colnames(SJxregt)
sub_SJxregt <- subset(imputedSJT, select=cols)


final_GARCH_SJ <- predict(fitGarchSJ,n.ahead=493, method="ML")
GarchPred <- ts(final_GARCH_SJ$meanForecast, frequency = 52, start =c(2003,45), end=c(2013,17))
GarchPredictionSJ <- window(GarchPred,start =c(2008,18), end=c(2013,17))
SeasSJ <- ts(dSJ_train$seasonal, frequency = 52, start =c(2008,18), end=c(2013,17))
F_GARCH_SJ <- round(exp(as.numeric(GarchPredictionSJ + SeasSJ)))


# Okay I have my forecasts figured out for SJ -- now I have to put together the forecast for IQ
cols <- colnames(IQxregt)
sub_IQxregt <- subset(imputedIQT, select=cols)

final_GARCH_IQ <- predict(fitGarchIQ,n.ahead=286, method="ML")
GarchPred <- ts(final_GARCH_IQ$meanForecast, frequency = 52, start =c(2008,1), end=c(2013,26))
GarchPredictionIQ <- window(GarchPred,start =c(2010,26), end=c(2013,25))
SeasIQ <- ts(dIQ_train$seasonal, frequency = 52, start =c(2010,26), end=c(2013,25))
F_GARCH_IQ <- round(exp(as.numeric(GarchPredictionIQ + SeasIQ)))



# Create output file!!!!!
GARCH_SJ <- data.frame("sj", sub_SJxregt$year, sub_SJxregt$weekofyear, F_GARCH_SJ)
colnames(GARCH_SJ) <- c("city", "year", "weekofyear", "total_cases")
head(GARCH_SJ)


GARCH_IQ <- data.frame("iq", sub_IQxregt$year, sub_IQxregt$weekofyear, F_GARCH_IQ)
colnames(GARCH_IQ) <- c("city", "year", "weekofyear", "total_cases")
head(GARCH_IQ)

str(GARCH_SJ)
submitGarch <- rbind(GARCH_SJ, GARCH_IQ)


# DrivenData score = 26.6154
#write.csv(submitGarch, "D:/Kim MSPA/Predict 413/Final Project/submitGarch.csv",
#          row.names=F)


#################################################################################################
# Create table of best models so far - may be able to get a better score by mixing and matching #
#################################################################################################

############
# San Juan #
############

# Auto Arima
statsSJAA <- c("Auto Arima", round(AccuracyAA_EXP[,"RMSE"],4) ,  round(AccuracyAA_EXP[,"MAE"],4), round(AccuracyAA_EXP[,"MAPE"],4))
# Seasonally decomoposed Auto Arima
statsSJAA_d <- c("Seasonally Decomposed AA", round(AccuracySAA_EXP[,"RMSE"],4) , round(AccuracySAA_EXP[,"MAE"],4), round(AccuracySAA_EXP[,"MAPE"],4))
# Ensemble 
statsSJ_ENS <- c("Ensemble AA", round(Accuracy_AA_ENS[,"RMSE"], 4), round(Accuracy_AA_ENS[,"MAE"], 4), round(Accuracy_AA_ENS[,"MAPE"], 4))
# Garch
statsSJ_GARCH <- c("GARCH Seas. Decomposed", round(AccuracyGARCHSJ[,"RMSE"], 4), round(AccuracyGARCHSJ[,"MAE"], 4), round(AccuracyGARCHSJ[,"MAPE"], 4))


statsSJ <- noquote(rbind(statsSJAA,statsSJAA_d,statsSJ_ENS,statsSJ_GARCH ))
colnames(statsSJ) <- c("Method","RMSE", "MAE", "MAPE")
statsSJ


############
# Iquitos  #
############

# Auto Arima
statsIQAA <- c("Auto Arima", round(AccuracyAA_EXP_IQ[,"RMSE"],4) ,  round(AccuracyAA_EXP_IQ[,"MAE"],4), round(AccuracyAA_EXP_IQ[,"MAPE"],4))
# Seasonally decomoposed Auto Arima
statsIQAA_d <- c("Seasonally Decomposed AA", round(Accuracy_SAAIQ_EXP[,"RMSE"],4) , round(Accuracy_SAAIQ_EXP[,"MAE"],4), 
                      round(Accuracy_SAAIQ_EXP[,"MAPE"],4))
# Auto Arima Ensemble 
statsIQ_AA_ENS <- c("Ensemble AA", round(Accuracy_AAIQ_ENS[,"RMSE"], 4), round(Accuracy_AAIQ_ENS[,"MAE"], 4), 
                      round(Accuracy_AAIQ_ENS[,"MAPE"], 4))
# Manual Arima
statsIQ_MA <- c("Manual Arima", round(AccuracyAA_EXP_IQ_new [,"RMSE"], 4), round(AccuracyAA_EXP_IQ_new [,"MAE"], 4), 
                 round(AccuracyAA_EXP_IQ_new [,"MAPE"], 4))
# Ensemble - decomposed AA and manual arima
statsIQ_ENS <- c("Ensemble AA", round(Accuracy_IQ_ENS[,"RMSE"], 4), round(Accuracy_IQ_ENS[,"MAE"], 4), 
                    round(Accuracy_IQ_ENS[,"MAPE"], 4))
# Garch
statsIQ_GARCH <- c("GARCH", round(AccuracyGARCHIQ[,"RMSE"], 4), round(AccuracyGARCHIQ[,"MAE"], 4), round(AccuracyGARCHIQ[,"MAPE"], 4))
# Garch seasonally deocmposed
statsIQ_dGARCH <- c("GARCH Seas. Decomposed", round(AccuracyGARCHdIQ[,"RMSE"], 4), round(AccuracyGARCHdIQ[,"MAE"], 4), 
                    round(AccuracyGARCHdIQ[,"MAPE"], 4))


statsIQ <- noquote(rbind(statsIQAA,statsIQAA_d,statsIQ_AA_ENS,statsIQ_MA,statsIQ_ENS,statsIQ_GARCH,statsIQ_dGARCH ))
colnames(statsIQ) <- c("Method","RMSE", "MAE", "MAPE")
statsIQ


#########################################################
# Graph of models to look for other potential Ensembles #
#########################################################

############
# San Juan #
############

# reduce the window so the test portion will be largere
reducedTrainSJ <-window(SJTS,start =c(1997,50),end = c(2003,44))

# Maybe try an auto arima and garch
par(mfcol=c(1,1))
plot(exp(reducedTrainSJ),ylab="San Juan Dengue Cases", main="Model Comparison San Juan",ylim=c(min(exp(trainSJ)),max(exp(trainSJ))),
     xlim=c(1998,2010)) 
lines(exp(testSJ),col=2,lw=2)
lines(exp(forecast_SJ$mean),col=3,lw=2)
lines(exp(forecast_SAA_SJ),col=4,lw=2)
lines(exp(forecast_AA_ENS),col=5,lw=2)
lines(exp(forecast_GARCH_SJ),col=6,lw=2)
legend("topleft", lty=1,col=c(2,3,4,5,6), lw=2,
       legend=c("Test Data","Auto-Arima","Decomposed Auto-Arima","AA Ensemble","GARCH"))



############
# Iquitos  #
############

# Maybe try manual and seasonal
par(mfcol=c(1,1))
plot(exp(trainIQ),ylab="Iquitos Dengue Cases", main="Model Comparison",ylim=c(min(exp(trainIQ)),max(exp(forecast_SAA_IQ))),
     xlim=c(2000, 2013)) 
lines(exp(testIQ),col=2,lw=2)
lines(exp(forecast_IQ$mean),col=3,lw=2)
lines(exp(forecast_SAA_IQ),col=4,lw=2)
lines(exp(forecast_AAIQ_ENS),col=5,lw=2)
lines(exp(forecast_IQ_ENS),col=6,lw=2)
lines(exp(forecast_IQ_new$mean),col=7, lw=2)
lines(exp(forecast_GARCH_IQ), col=8, lw=2)
legend("topleft", lty=1,col=c(2,3,4,5,6,7,8), lw=2,
       legend=c("Test Data","Auto-Arima","Decomposed Auto-Arima", "AA Ensemble", "Ensemble Seas & Manual", "Manual", "Seasonal GARCH"))



#########################################################
# Create output file based on best models for each city #
#########################################################


# Create output file!!!!!
# San Juan is Auto arima as usual
AutoArimaSJ2 <- data.frame("sj", sub_SJxregt$year[234:493], sub_SJxregt$weekofyear[234:493], Final_AA_SJ2)
colnames(AutoArimaSJ2) <- c("city", "year", "weekofyear", "total_cases")
tail(AutoArimaSJ2)

GARCH_IQ <- data.frame("iq", sub_IQxregt$year, sub_IQxregt$weekofyear, F_GARCH_IQ)
colnames(GARCH_IQ) <- c("city", "year", "weekofyear", "total_cases")
head(GARCH_IQ)

submitAASJ_dGIQ <- rbind(AutoArimaSJ2, GARCH_IQ)

# DrivenData score = NEED TO sUBMIT
# write.csv(submitAASJ_dGIQ, "D:/Kim MSPA/Predict 413/Final Project/submitAASJ_dGIQ.csv",
#            row.names=F)


########################################
# Garch models with external variables #
#######################################


############
# San Juan #
############

SJxreg$X <- NULL

set.seed(111)
#12.96
#garch.spec <- ugarchspec(variance.model = list(model = "csGARCH",  garchOrder = c(1,1),  external.regressors = as.matrix(SJxreg)), 
#mean.model = list(armaOrder = c(1,1), include.mean = TRUE, external.regressors = as.matrix(SJxreg)), 
#distribution.model = "std")

garch.spec <- ugarchspec(variance.model = list(model = "csGARCH",  garchOrder = c(1,1),  external.regressors = as.matrix(SJxreg)), 
                           mean.model = list(armaOrder = c(1,1), include.mean = TRUE, external.regressors = as.matrix(SJxreg)), 
                           distribution.model = "std")

garchXRegSJ <- ugarchfit(data = adjSJ, spec = garch.spec)

infocriteria(garchXRegSJ)[1] 

resids <- residuals(garchXRegSJ)
plot(resids)
acf2(resids)

head(fitted(garchXRegSJ))

predGarchXreg <- ugarchforecast(garchXRegSJ, n.ahead=233, external.forecasts = list(mregfor = as.matrix(SJxregt), 
                                                                                    vregfor = as.matrix(SJxregt)))
GarchPredXreg <- ts(as.numeric(fitted(predGarchXreg)[,1]), frequency = 52, start =c(2003,45), end=c(2008,17))
SeasSJ <- ts(dSJ_test$seasonal, frequency = 52, start =c(2003,45), end=c(2008,17))
fcst_GARCH_SJ_Xreg <- GarchPredXreg + SeasSJ
fcst_GARCH_SJ_Xreg[fcst_GARCH_SJ_Xreg < 0] <- 0
AccuracyGARCH_SJ_xreg <- accuracy(round(exp(fcst_GARCH_SJ_Xreg)),exp(testSJ))
AccuracyGARCH_SJ_xreg


par(mfcol=c(1,1))
plot(exp(trainSJ),ylab="San Juan Dengue Cases", main="Model Comparison",ylim=c(min(exp(trainSJ)),max(exp(trainSJ))),
        xlim=c(1990, 2008)) 
lines(exp(testSJ),col=2,lw=2)
lines(exp(forecast_SAA_SJ),col=3,lw=2)
lines(exp(forecast_GARCH_SJ),col=4,lw=2)
lines(exp(fcst_GARCH_SJ_Xreg),col=5,lw=2)
legend("topleft", lty=1,col=c(2,3,4,5), lw=2,
       legend=c("Test Data","Decomposed Auto-Arima","Garch","Garch w/ Xreg"))



############
# Iquitos  #
############
IQxreg$X <- NULL
IQxregt$X <- NULL

IQxreg2 <- IQxreg
IQxregt2 <- IQxregt

IQxreg2$M_reanalysis_dew_point_temp_k <- NULL
IQxreg2$M_station_max_temp_c <- NULL
IQxreg2$M_station_min_temp_c <- NULL
IQxreg2$reanalysis_avg_temp_k <- NULL
IQxreg2$reanalysis_specific_humidity_g_per_kg <- NULL
IQxreg2$reanalysis_tdtr_k <- NULL
IQxreg2$reanalysis_relative_humidity_percent <- NULL
IQxreg2$ndvi_sw <- NULL
IQxreg2$ndvi_nw <- NULL
IQxreg2$ndvi_se <- NULL
IQxreg2$reanalysis_precip_amt_kg_per_m2 <- NULL
IQxreg2$reanalysis_dew_point_temp_k <- NULL

IQxregt2$M_reanalysis_dew_point_temp_k <- NULL
IQxregt2$M_station_max_temp_c <- NULL
IQxregt2$M_station_min_temp_c <- NULL
IQxregt2$reanalysis_avg_temp_k <- NULL
IQxregt2$reanalysis_specific_humidity_g_per_kg <- NULL
IQxregt2$reanalysis_tdtr_k <- NULL
IQxregt2$reanalysis_relative_humidity_percent <- NULL
IQxregt2$ndvi_sw <- NULL
IQxregt2$ndvi_nw <- NULL
IQxregt2$ndvi_se <- NULL
IQxregt2$reanalysis_precip_amt_kg_per_m2 <- NULL
IQxregt2$reanalysis_dew_point_temp_k <- NULL


corrplot(cor(IQxreg2))


set.seed(111)
# 7.217054
# garch.specIQ <- ugarchspec(variance.model = list(model = "fGARCH",  garchOrder = c(1, 2),  external.regressors = as.matrix(IQxreg2),
#submodel="AVGARCH"), 
#mean.model = list(armaOrder = c(1,1), include.mean = TRUE, external.regressors = as.matrix(IQxreg2)), 
#distribution.model = "std")

garch.specIQ <- ugarchspec(variance.model = list(model = "fGARCH",  garchOrder = c(1, 2),  external.regressors = as.matrix(IQxreg2),
                                                 submodel="AVGARCH"), 
                         mean.model = list(armaOrder = c(1,1), include.mean = TRUE, external.regressors = as.matrix(IQxreg2)), 
                         distribution.model = "std")

garchXRegIQ <- ugarchfit(data = adjIQ, spec = garch.specIQ, solver = 'hybrid')

infocriteria(garchXRegIQ)[1] 

resids <- residuals(garchXRegIQ)
plot(resids)
acf2(resids)

head(fitted(garchXRegIQ))

predGarchXregIQ <- ugarchforecast(garchXRegIQ, n.ahead=130, external.forecasts = list(mregfor = as.matrix(IQxregt2), 
                                                                                      vregfor = as.matrix(IQxregt2)))
GarchPredXregIQ <- ts(as.numeric(fitted(predGarchXregIQ)[,1]), frequency = 52, start =c(2008,1), end=c(2010,25))
SeasIQ <- ts(dIQ_test$seasonal, frequency = 52, start =c(2008,1), end=c(2010,25))
fcst_GARCH_IQ_Xreg <- GarchPredXregIQ + SeasIQ
fcst_GARCH_IQ_Xreg[fcst_GARCH_IQ_Xreg < 0] <- 0
AccuracyGARCH_IQ_xreg <- accuracy(round(exp(fcst_GARCH_IQ_Xreg)),exp(testIQ))
AccuracyGARCH_IQ_xreg


par(mfcol=c(1,1))
plot(exp(trainIQ),ylab="Iquitos Dengue Cases", main="Model Comparison",ylim=c(min(exp(trainIQ)),max(exp(forecast_SAA_IQ))),
            xlim=c(2000.5,2011)) 
lines(exp(testIQ),col=2,lw=2)
lines(exp(forecast_IQ$mean),col=3,lw=2)
lines(exp(forecast_SAA_IQ),col=4,lw=2)
lines(exp(forecast_IQ_new$mean),col=5, lw=2)
lines(exp(forecast_GARCH_IQ), col=6, lw=2)
lines(exp(fcst_GARCH_IQ_Xreg), col=7, lw=2)
legend("topleft", lty=1,col=c(2,3,4,5,6,7), lw=2,
       legend=c("Test Data","Auto-Arima","Decomposed Auto-Arima", "Manual", "Seasonal GARCH", "Garch xreg"))


############################
# Final Garch xreg models  #
############################

# San Juan
SJxregt$X <- NULL
cols <- colnames(SJxregt)
sub_SJxregt <- subset(imputedSJT, select=cols)
sub_SJxregt <- rbind(SJxregt, sub_SJxregt)


final_GARCH_SJ_xreg <- ugarchforecast(garchXRegSJ, n.ahead=493, external.forecasts = list(mregfor = as.matrix(sub_SJxregt), 
                                                                   vregfor = as.matrix(sub_SJxregt)))
GarchPred_SJXreg <- ts(as.numeric(fitted(final_GARCH_SJ_xreg)[,1]), frequency = 52, start =c(2003,45), end=c(2013,17))
GarchPredictionSJxreg <- window(GarchPred_SJXreg,start =c(2008,18), end=c(2013,17))
SeasSJ <- ts(dSJ_train$seasonal, frequency = 52, start =c(2008,18), end=c(2013,17))
F_GARCH_SJxreg <- round(exp(as.numeric(GarchPredictionSJxreg + SeasSJ)))
F_GARCH_SJxreg[F_GARCH_SJxreg < 0] <- 0


# Iquitos
cols <- colnames(IQxregt2)
sub_IQxregt <- subset(imputedIQT, select=cols)
sub_IQxregt <- rbind(IQxregt2, sub_IQxregt)


final_GARCH_IQ_xreg <- ugarchforecast(garchXRegIQ, n.ahead=286, external.forecasts = list(mregfor = as.matrix(sub_IQxregt), 
                                                                                          vregfor = as.matrix(sub_IQxregt)))
GarchPred_IQXreg <- ts(as.numeric(fitted(final_GARCH_IQ_xreg)[,1]), frequency = 52,  start =c(2008,1), end=c(2013,26))
GarchPredictionIQxreg <- window(GarchPred_IQXreg,start =c(2010,26), end=c(2013,25))
SeasIQ <- ts(dIQ_train$seasonal, frequency = 52, start =c(2010,26), end=c(2013,25))
F_GARCH_IQxreg <- round(exp(as.numeric(GarchPredictionIQxreg + SeasIQ)))
F_GARCH_IQxreg[F_GARCH_IQxreg < 0] <- 0
length(F_GARCH_IQxreg)

# Create output file!!!!!
GARCH_SJ_XREG <- data.frame("sj", sub_SJxregt$year[234:493], sub_SJxregt$weekofyear[234:493], F_GARCH_SJxreg)
colnames(GARCH_SJ_XREG) <- c("city", "year", "weekofyear", "total_cases")
head(GARCH_SJ_XREG)

GARCH_IQ_XREG <- data.frame("iq", sub_IQxregt$year[131:286], sub_IQxregt$weekofyear[131:286], F_GARCH_IQxreg)
colnames(GARCH_IQ_XREG) <- c("city", "year", "weekofyear", "total_cases")
head(GARCH_IQ_XREG)

submitGARCHXreg <- rbind(GARCH_SJ_XREG, GARCH_IQ_XREG)

# DrivenData score = 
#write.csv(submitGARCHXreg, "D:/Kim MSPA/Predict 413/Final Project/submitGARCHXreg.csv",
#           row.names=F)



############################################
# Neural Network with  external regressors #
############################################

############
# San Juan #
############

# fitNN_SJ <- nnetar(trainSJ, xreg=SJxreg, p=16, P=2)  MAE = 12.04802
set.seed(11)
fitNN_SJ <- nnetar(trainSJ, xreg= SJxreg, p=16, P=2)
predANN_SJ <- forecast(fitNN_SJ,h=233,xreg=SJxregt)
AccuracyNN_SJ <- accuracy(exp(predANN_SJ$mean),exp(testSJ))
AccuracyNN_SJ

# MAE = 10.71557
ENS_SJ <- (predANN_SJ$mean + forecast_SAA_SJ)/2
AccuracyENS_SJ <- accuracy(exp(ENS_SJ),exp(testSJ))
AccuracyENS_SJ


# Take a look at the fit to see how it can be tweaked
par(mfcol=c(1,1))
plot(exp(trainSJ),ylab="San Juan Dengue Cases", main="Model Comparison",ylim=c(min(exp(trainSJ)),max(exp(trainSJ))),
     xlim=c(1990, 2008)) 
lines(exp(testSJ),col=2,lw=2)
lines(exp(predANN_SJ$mean),col=3,lw=2)
lines(exp(forecast_SAA_SJ),col=6,lw=2)
lines(exp(ENS_SJ), col=4, lw=2)
legend("topleft", lty=1,col=c(2,3,6,4), lw=2,
       legend=c("Test Data","Auto NN","Auto Arima", "Ensemble"))


############
# Iquitos  #
############

# reset xreg files
IQxreg <- read.csv("D:/Kim MSPA/Predict 413/Final Project/IQxreg.csv", sep = ",")
IQxregt <- read.csv("D:/Kim MSPA/Predict 413/Final Project/IQxregt.csv", sep = ",")
IQxreg$X <- NULL
IQxregt$X <- NULL


# fitNN_IQ <-  nnetar(trainIQ, xreg= IQxregp=11,P=1)   MAE = 7.025544
set.seed(11)
fitNN_IQ <- nnetar(trainIQ, xreg= IQxreg, p=11, P=1) 
predANN_IQ <- forecast(fitNN_IQ,h=130,xreg=IQxregt)
AccuracyNN_IQ <- accuracy(exp(predANN_IQ$mean),exp(testIQ))
AccuracyNN_IQ

# Take a look at the fit to see how it can be tweaked
par(mfcol=c(1,1))
plot(exp(trainIQ),ylab="Iquitos Dengue Cases", main="Model Comparison",ylim=c(min(exp(trainIQ)),max(exp(trainIQ))),
     xlim=c(2000, 2010)) 
lines(exp(testIQ),col=2,lw=2)
lines(exp(predANN_IQ$mean),col=3,lw=2)
legend("topleft", lty=1,col=c(2,3), lw=2,
       legend=c("Test Data","Auto NN"))


####################################
# Final Neural Network xreg models #
####################################

# San Juan
SJxregt$X <- NULL
cols <- colnames(SJxregt)
sub_SJxregt <- subset(imputedSJT, select=cols)
sub_SJxregt <- rbind(SJxregt, sub_SJxregt)

final_NN_SJ <- forecast(fitNN_SJ, n.ahead=493, xreg=sub_SJxregt)
ts_final_NN_SJ <- ts(final_NN_SJ$mean, frequency = 52, start =c(2003,45), end=c(2013,17))
Pred_final_NN_SJ <- window(ts_final_NN_SJ,start =c(2008,18), end=c(2013,17))
F_NN_SJ <- round(as.numeric(exp(Pred_final_NN_SJ)))


# Iquitos
cols <- colnames(IQxregt)
sub_IQxregt <- subset(imputedIQT, select=cols)
sub_IQxregt <- rbind(IQxregt, sub_IQxregt)

final_NN_IQ <- forecast(fitNN_IQ, n.ahead=286,  xreg=sub_IQxregt)
ts_final_NN_IQ <- ts(final_NN_IQ$mean, frequency = 52, start =c(2008,1), end=c(2013,26))
Pred_final_NN_IQ <- window(ts_final_NN_IQ,start =c(2010,27), end=c(2013,26))
F_NN_IQ <- round(as.numeric(exp(Pred_final_NN_IQ)))



# Create output file!!!!!
NN_SJ <- data.frame("sj", sub_SJxregt$year[234:493], sub_SJxregt$weekofyear[234:493], F_NN_SJ)
colnames(NN_SJ) <- c("city", "year", "weekofyear", "total_cases")
tail(NN_SJ)

NN_IQ <- data.frame("iq", sub_IQxregt$year[131:286], sub_IQxregt$weekofyear[131:286], F_NN_IQ)
colnames(NN_IQ) <- c("city", "year", "weekofyear", "total_cases")
tail(NN_IQ)

submitNN <- rbind(NN_SJ, NN_IQ)

# DrivenData score = 30.8197
# write.csv(submitNN, "D:/Kim MSPA/Predict 413/Final Project/submitNN.csv",
#           row.names=F)

####################################
# Final Ensemble SJ and NN for IQ  #
####################################

# San Juan

F_ENS_NN_SAA_SJ <- round((Final_AA_SJ + F_NN_SJ)/2)



# Create output file!!!!!
ENS_SJ <- data.frame("sj", sub_SJxregt$year[234:493], sub_SJxregt$weekofyear[234:493], F_ENS_NN_SAA_SJ)
colnames(ENS_SJ) <- c("city", "year", "weekofyear", "total_cases")
tail(ENS_SJ)

NN_IQ <- data.frame("iq", sub_IQxregt$year[131:286], sub_IQxregt$weekofyear[131:286], F_NN_IQ)
colnames(NN_IQ) <- c("city", "year", "weekofyear", "total_cases")
tail(NN_IQ)

submitENS_NN <- rbind(ENS_SJ, NN_IQ)

# DrivenData score = 
write.csv(submitENS_NN, "D:/Kim MSPA/Predict 413/Final Project/submitENS_NN.csv",
          row.names=F)


###############################################
# Neural Network without  external regressors #
###############################################

############
# San Juan #
############

for(i in 1:40)
{
  for(j in 1:14){
    set.seed(11)
    fitNNP_SJ <- nnetar(trainSJ,p=i,P=j)
    predANNP_SJ <- forecast(fitNNP_SJ,h=233)
    AccuracyNNP_SJ <- accuracy(exp(predANNP_SJ$mean),exp(testSJ))
    if(AccuracyNNP_SJ[3] <= 13.36){
      stat <- sprintf("nnetar(p= %.2f, P= %.2f) MAE = %.2f", i, j, AccuracyNNP_SJ[3])
      print(stat)
    } # if(AccuracyNNP_SJ[3] <= 13.36){
  }# for(j in 1;14){
}

# p=17 P=13 best model from above
set.seed(11)
fitNNP_SJ <- nnetar(trainSJ,p=17,P=13)
predANNP_SJ <- forecast(fitNNP_SJ,h=233)
AccuracyNNP_SJ <- accuracy(exp(predANNP_SJ$mean),exp(testSJ))
AccuracyNNP_SJ

# Take a look at the fit to see how it can be tweaked
par(mfcol=c(1,1))
plot(exp(trainSJ),ylab="San Juan Dengue Cases", main="Model Comparison",ylim=c(min(exp(trainSJ)),max(exp(trainSJ))),
     xlim=c(1990, 2008)) 
lines(exp(testSJ),col=2,lw=2)
lines(exp(predANNP_SJ$mean),col=3,lw=2)
lines(exp(forecast_SAA_SJ),col=4,lw=2)
legend("topleft", lty=1,col=c(2,3,4), lw=2,
       legend=c("Test Data","Plain NN","Auto Arima"))


############
# Iquitos  #
############

fitNNP_IQ <- nnetar(trainIQ)

for(i in 38:60)
{
  for(j in 1:4){
    set.seed(11)
    fitNNP_IQ <- nnetar(trainIQ,p=i,P=j)
    predANNP_IQ <- forecast(fitNNP_IQ,h=130)
    AccuracyNNP_IQ <- accuracy(exp(predANNP_IQ$mean),exp(testIQ))
    if(AccuracyNNP_IQ[3] <= 7.8){
      stat <- sprintf("nnetar(p= %.2f, P= %.2f) MAE = %.2f", i, j, AccuracyNNP_IQ[3])
      print(stat)
    } # if(AccuracyNNP_SJ[3] <= 13.36){
  }# for(j in 1;14){
}

# p=36 P=4 best model from above #MAE = 6.64
set.seed(11)
fitNNP_IQ <- nnetar(trainIQ,p=36,P=4)
predANNP_IQ <- forecast(fitNNP_IQ,h=130)
AccuracyNNP_IQ <- accuracy(exp(predANNP_IQ$mean),exp(testIQ))
AccuracyNNP_IQ

# Take a look at the fit to see how it can be tweaked
par(mfcol=c(1,1))
plot(exp(trainIQ),ylab="Iquitos Dengue Cases", main="Model Comparison",ylim=c(min(exp(trainIQ)),max(exp(trainIQ))),
     xlim=c(2000, 2010)) 
lines(exp(testIQ),col=2,lw=2)
lines(exp(predANNP_IQ$mean),col=3,lw=2)
legend("topleft", lty=1,col=c(2,3), lw=2,
       legend=c("Test Data","Auto NN"))


############################################
# Final Neural Network withoug xreg models #
############################################

# San Juan
final_NNP_SJ <- forecast(fitNNP_SJ, n.ahead=493)
ts_final_NNP_SJ <- ts(final_NNP_SJ$mean, frequency = 52, start =c(2003,45), end=c(2013,17))
Pred_final_NNP_SJ <- window(ts_final_NNP_SJ,start =c(2008,18), end=c(2013,17))
F_NNP_SJ <- round(as.numeric(exp(Pred_final_NNP_SJ)))


# Iquitos
final_NNP_IQ <- forecast(fitNNP_IQ, n.ahead=286)
ts_final_NNP_IQ <- ts(final_NNP_IQ$mean, frequency = 52, start =c(2008,1), end=c(2013,26))
Pred_final_NNP_IQ <- window(ts_final_NNP_IQ,start =c(2010,27), end=c(2013,26))
F_NNP_IQ <- round(as.numeric(exp(Pred_final_NNP_IQ)))



# Create output file!!!!!
NNP_SJ <- data.frame("sj", sub_SJxregt$year[234:493], sub_SJxregt$weekofyear[234:493], F_NNP_SJ)
colnames(NNP_SJ) <- c("city", "year", "weekofyear", "total_cases")
tail(NNP_SJ)

NNP_IQ <- data.frame("iq", sub_IQxregt$year[131:286], sub_IQxregt$weekofyear[131:286], F_NNP_IQ)
colnames(NNP_IQ) <- c("city", "year", "weekofyear", "total_cases")
tail(NNP_IQ)

submitNNP <- rbind(NNP_SJ, NNP_IQ)

# DrivenData score = 
write.csv(submitNNP, "D:/Kim MSPA/Predict 413/Final Project/submitNNP.csv",
          row.names=F)



############
# San Juan #
############

# Auto Arima
statsSJAA <- c("Auto Arima w/ Climate Data", round(AccuracyAA_EXP[,"RMSE"],4) ,  round(AccuracyAA_EXP[,"MAE"],4), round(AccuracyAA_EXP[,"MAPE"],4))
# Seasonally decomoposed Auto Arima
statsSJAA_d <- c("Seasonal AA w/ Climate Data", round(AccuracySAA_EXP[,"RMSE"],4) , round(AccuracySAA_EXP[,"MAE"],4), round(AccuracySAA_EXP[,"MAPE"],4))
# Auto Arima - no external regressors
statsSJAA_SJ_noxreg <- c("Auto Arima", round(AccuracyAA_SJ_noxreg[,"RMSE"],4) , round(AccuracyAA_SJ_noxreg[,"MAE"],4), round(AccuracyAA_SJ_noxreg[,"MAPE"],4))
# Seasonally decomoposed Auto Arima - no external regressors
statsSJAA_d_noxreg <- c("Seasonal AA", round(AccuracySAA_EXP_noxreg[,"RMSE"],4) , round(AccuracySAA_EXP_noxreg[,"MAE"],4), round(AccuracySAA_EXP_noxreg[,"MAPE"],4))
# Garch
statsSJ_GARCH <- c("GARCH", round(AccuracyGARCHSJ[,"RMSE"], 4), round(AccuracyGARCHSJ[,"MAE"], 4), round(AccuracyGARCHSJ[,"MAPE"], 4))
# Garch with External Regressor
statsSJ_GARCH_xreg <- c("GARCH w/ Climate Data", round(AccuracyGARCH_SJ_xreg[,"RMSE"], 4), round(AccuracyGARCH_SJ_xreg[,"MAE"], 4), 
                        round(AccuracyGARCH_SJ_xreg[,"MAPE"], 4))
# Neural Net with External Regressor
statsSJ_NN_xreg <- c("Neural Net w/ Climate Data", round(AccuracyNN_SJ[,"RMSE"], 4), round(AccuracyNN_SJ[,"MAE"], 4), 
                        round(AccuracyNN_SJ[,"MAPE"], 4))
# Neural Net without external regressor
statsSJ_NN_noxreg <- c("Neural Net", round(AccuracyNNP_SJ[,"RMSE"], 4), round(AccuracyNNP_SJ[,"MAE"], 4), 
                     round(AccuracyNNP_SJ[,"MAPE"], 4))


statsSJ <- noquote(rbind(statsSJAA,statsSJAA_d,statsSJAA_SJ_noxreg,statsSJAA_d_noxreg,statsSJ_GARCH,statsSJ_GARCH_xreg,
                         statsSJ_NN_xreg, statsSJ_NN_noxreg))
colnames(statsSJ) <- c("Method","RMSE", "MAE", "MAPE")
rownames(statsSJ) <- NULL
statsSJ


############
# Iquitos  #
############

# Auto Arima
statsIQAA <- c("Auto Arima w/ Climate Data", round(AccuracyAA_EXP_IQ[,"RMSE"],4) ,  round(AccuracyAA_EXP_IQ[,"MAE"],4), round(AccuracyAA_EXP_IQ[,"MAPE"],4))
# Seasonally decomoposed Auto Arima
statsIQAA_d <- c("Seasonal AA w/ Climate Data", round(Accuracy_SAAIQ_EXP[,"RMSE"],4) , round(Accuracy_SAAIQ_EXP[,"MAE"],4), 
                 round(Accuracy_SAAIQ_EXP[,"MAPE"],4))
# Auto Arima - no external regressors
statsIQAA_no_xreg <- c("Auto Arima", round(AccuracyAA_EXP_IQ_noxreg[,"RMSE"],4) , round(AccuracyAA_EXP_IQ_noxreg[,"MAE"],4), 
                 round(AccuracyAA_EXP_IQ_noxreg[,"MAPE"],4))
# Auto Arima seasaonally decomposed - no external regressors
statsIQSAA_no_xreg <- c("Seasonal AA", round(Accuracy_SAAIQ_noxreg[,"RMSE"],4) , round(Accuracy_SAAIQ_noxreg[,"MAE"],4), 
                       round(Accuracy_SAAIQ_noxreg[,"MAPE"],4))
# Garch
statsIQ_dGARCH <- c("GARCH", round(AccuracyGARCHdIQ[,"RMSE"], 4), round(AccuracyGARCHdIQ[,"MAE"], 4), 
                    round(AccuracyGARCHdIQ[,"MAPE"], 4))
# Garch with external regressors
statsIQ_dGARCH_xreg <- c("GARCH w/ Climate Data", round(AccuracyGARCH_IQ_xreg[,"RMSE"], 4), round(AccuracyGARCH_IQ_xreg[,"MAE"], 4), 
                    round(AccuracyGARCH_IQ_xreg[,"MAPE"], 4))
# Neural net with external regressors
statsIQ_NN_xreg <- c("Neural Net w/ Climate Data", round(AccuracyNN_IQ[,"RMSE"], 4), round(AccuracyNN_IQ[,"MAE"], 4), 
                         round(AccuracyNN_IQ[,"MAPE"], 4))
# Neural net without external regressors
statsIQ_NN_noxreg <- c("Neural Net ", round(AccuracyNNP_IQ[,"RMSE"], 4), round(AccuracyNNP_IQ[,"MAE"], 4), 
                     round(AccuracyNNP_IQ[,"MAPE"], 4))




statsIQ <- noquote(rbind(statsIQAA,statsIQAA_d,statsIQAA_no_xreg,statsIQSAA_no_xreg,statsIQ_dGARCH,
                         statsIQ_dGARCH_xreg, statsIQ_NN_xreg,statsIQ_NN_noxreg))
colnames(statsIQ) <- c("Method","RMSE", "MAE", "MAPE")
rownames(statsIQ) <- NULL
statsIQ



#######################
# Graph of the models #
#######################

par(mfcol=c(1,1))
plot(exp(trainSJ),ylab="San Juan Dengue Cases", main="San Juan Model Comparison",ylim=c(min(exp(trainSJ)),max(exp(trainSJ))),
     xlim=c(1990, 2008)) 
lines(exp(testSJ),col=2,lw=2)
lines(exp(predANNP_SJ$mean),col=3,lw=2)
lines(exp(predANN_SJ$mean),col=4,lw=2)
lines(exp(forecast_SAA_SJ),col=5,lw=2)
lines(exp(forecast_SAA_SJ_noxreg), col=6, lw=2)
lines(exp(forecast_GARCH_SJ),col=7, lw=2)
lines(exp(fcst_GARCH_SJ_Xreg), col=8, lw=2)
legend("topleft", lty=1,col=c(2,3,4,5,6,7,8), lw=2,
       legend=c("Test Data","Neural Net","NN w/ Climate Data", "Auto Arima", "AA w/ Climate Data", 
                "Garch", "Garch w/ Climate Data"))


par(mfcol=c(1,1))
plot(exp(trainIQ),ylab="Iquitos Dengue Cases", main="Iquitos Model Comparison",ylim=c(min(exp(trainIQ)),max(exp(forecast_SAA_IQ))),
     xlim=c(2000.5,2011)) 
lines(exp(testIQ),col=2,lw=2)
lines(exp(predANNP_IQ$mean),col=3,lw=2)
lines(exp(predANN_IQ$mean),col=4,lw=2)
lines(exp(forecast_SAA_IQ),col=5,lw=2)
lines(exp(forecast_SAA_IQ_noxreg), col=6, lw=2)
lines(exp(forecast_GARCH_IQ),col=7, lw=2)
lines(exp(fcst_GARCH_IQ_Xreg), col=8, lw=2)
legend("topleft", lty=1,col=c(2,3,4,5,6,7,8), lw=2,
       legend=c("Test Data","Neural Net","NN w/ Climate Data", "Auto Arima", "AA w/ Climate Data", 
                "Garch", "Garch w/ Climate Data"))


