# Machine Learning Project - Real Estate Value Prediction
# Shih-Yuan Wang

#--------------------------------------------------------------------------------
# Packages
#--------------------------------------------------------------------------------

# install.packages("xlsx")
library(xlsx)

# install.packages("dplyr")
library(dplyr)

# For EDA
#install.packages("DataExplorer")
library(DataExplorer)

# install.packages("GGally")
library(GGally)

# install.packages("ggplot2")
library(ggplot2)

# install.packages("ggpubr")
library(ggpubr)

# install.packages("e1071")
library(e1071) 

# For waterfall charts
# install.packages("plotly")
library(plotly)

# For VIF
# install.packages("car")
library(car)

# For models
# install.packages("glmnet")
library(glmnet)

# install.packages("pls")
library(pls)

# install.packages("splines")
library(splines)

# install.packages("randomForest")
library(randomForest)

# install.packages("mgcv")
library(mgcv)

# install.packages("gbm")
library(gbm)

#--------------------------------------------------------------------------------
# 1. Access the Data Set
#--------------------------------------------------------------------------------

setwd("/Users/ginawang/Desktop/Machine Learning Using R/Final project")
# getwd()

# Import data from the UCI machine learning repository
# https://archive.ics.uci.edu/ml/datasets/Real+estate+valuation+data+set
# Real estate valuation data set 
housing <- read.xlsx2("Real estate valuation data set.xlsx", sheetIndex=1)

# Remove the first column of housing data frame
housing <- housing[, -1]

# Convert to data frame
housing <- as.data.frame(housing)

str(housing)
# Convert the data type
index <- sapply(housing, is.factor)
housing[index] <- lapply(housing[index], function(x) as.numeric(as.character(x)))

# Rename the columns
colnames(housing)[colnames(housing) == "X1.transaction.date"] <- "x1.tranDate"
colnames(housing)[colnames(housing) == "X2.house.age"] <- "x2.age"
colnames(housing)[colnames(housing) == "X3.distance.to.the.nearest.MRT.station"] <- "x3.disMRT"
colnames(housing)[colnames(housing) == "X4.number.of.convenience.stores"] <- "x4.numConv"
colnames(housing)[colnames(housing) == "X5.latitude"] <- "x5.lat"
colnames(housing)[colnames(housing) == "X6.longitude"] <- "x6.long"
colnames(housing)[colnames(housing) == "Y.house.price.of.unit.area"] <- "y.price"

str(housing)
# 'data.frame':	414 obs. of  7 variables:
# $ x1.tranDate: num  2013 2013 2014 2014 2013 ...
# $ x2.age     : num  32 19.5 13.3 13.3 5 7.1 34.5 20.3 31.7 17.9 ...
# $ x3.disMRT  : num  84.9 306.6 562 562 390.6 ...
# $ x4.numConv : num  10 9 5 5 5 3 7 6 1 3 ...
# $ x5.lat     : num  25 25 25 25 25 ...
# $ x6.long    : num  122 122 122 122 122 ...
# $ y.price    : num  37.9 42.2 47.3 54.8 43.1 32.1 40.3 46.7 18.8 22.1 ...

head(housing)
#   x1.tranDate x2.age  x3.disMRT x4.numConv   x5.lat  x6.long y.price
# 1    2012.917   32.0   84.87882         10 24.98298 121.5402    37.9
# 2    2012.917   19.5  306.59470          9 24.98034 121.5395    42.2
# 3    2013.583   13.3  561.98450          5 24.98746 121.5439    47.3
# 4    2013.500   13.3  561.98450          5 24.98746 121.5439    54.8
# 5    2012.833    5.0  390.56840          5 24.97937 121.5425    43.1
# 6    2012.667    7.1 2175.03000          3 24.96305 121.5125    32.1

# Descriptive statistics
summary(housing)
#  x1.tranDate       x2.age         x3.disMRT         x4.numConv         x5.lat         x6.long         y.price      
# Min.   :2013   Min.   : 0.000   Min.   :  23.38   Min.   : 0.000   Min.   :24.93   Min.   :121.5   Min.   :  7.60  
# 1st Qu.:2013   1st Qu.: 9.025   1st Qu.: 289.32   1st Qu.: 1.000   1st Qu.:24.96   1st Qu.:121.5   1st Qu.: 27.70  
# Median :2013   Median :16.100   Median : 492.23   Median : 4.000   Median :24.97   Median :121.5   Median : 38.45  
# Mean   :2013   Mean   :17.713   Mean   :1083.89   Mean   : 4.094   Mean   :24.97   Mean   :121.5   Mean   : 37.98  
# 3rd Qu.:2013   3rd Qu.:28.150   3rd Qu.:1454.28   3rd Qu.: 6.000   3rd Qu.:24.98   3rd Qu.:121.5   3rd Qu.: 46.60  
# Max.   :2014   Max.   :43.800   Max.   :6488.02   Max.   :10.000   Max.   :25.01   Max.   :121.6   Max.   :117.50  

#--------------------------------------------------------------------------------
# 2. Exploratory Data Analysis
#--------------------------------------------------------------------------------

# (1) To get introduced to the data set
introduce(housing)
plot_intro(housing)  # there are no missing values and all are continuous variables

# (2) The distribution of each variable
plot_histogram(housing) # view histogram of all continuous variables
# x1.tranDate   : the number of transactions had a peak in May 2013, and was higher than the average on 3 other months - November 2012, January 2013, and June 2013.
# x2.age        : house age varies from 0 to 43 years, with around 3 peaks: 13-17 years, 0-6 years, 30-36 years.
# x3.disMRT     : Most houses are located within 1 km of the nearest MRT station.
# x4.numConv    : Approximately 1 house in 6 does not have any convenience store in the living circle on foot.
#                 Half of the houses have between 1 and 6 convenience stores in the living circle on foot.
# x5.lat/x6.long: Most of the houses are located in the "East central part" of this area.
# y.price       : House price of unit varies greatly from 76,000 NTD/Ping to more than 1 million NTD/Ping.
#                 More than half of house prices of unit area are between 250,000 and 500,000 NTD/Ping.
#                 There are few houses for which the house price of unit area is more than 650,000 NTD/Ping, with one extreme value 1,175,000 NTD/Ping. 

plot_density(housing)   # View density distribution of all continuous variables
# The distribution of house price is moderately right-skewed -> check whether perform data transformation later

# (3) Relationships between variables

theme_update(plot.title = element_text(hjust = 0.5))

# x1.tranDate vs. y.price
p_x1.y <- ggplot(housing, aes(x=x1.tranDate, y=y.price)) + geom_point(color='blue') +
          geom_smooth(method = "lm", formula = y ~ x, se = TRUE, col = "red", linetype = "solid", size = 0.6) +
          ggtitle("Transaction Date VS. House Price")
# House price rises to above 600,000 after early 2013, and the outlier happened in the beginning of May 2013.
# There is no obvious trend between Transaction Date and House Price.

# x2.age vs. y.price
p_x2.y <- ggplot(housing, aes(x=x2.age, y=y.price)) + geom_point(color='blue') +
          geom_smooth(method = "lm", formula = y ~ x, se = TRUE, col = "red", linetype = "solid", size = 0.6) +
          ggtitle("House Age VS. House Price")
# There is just a weak negative relationship between House Age and House Price.

# x3.disMRT vs. y.price
p_x3.y <- ggplot(housing, aes(x=x3.disMRT, y=y.price)) + geom_point(color='blue') +
          geom_smooth(method = "lm", formula = y ~ x, se = TRUE, col = "red", linetype = "solid", size = 0.6) +
          ggtitle("Distance to The Nearest MRT VS. House Price")
# It shows a clear decreasing trend between the Distance to the Nearest MRT Station and House Price,
# so the predictor x3.disMRT has more impact on house price.

# x4.numConv vs. y.price
p_x4.y <- ggplot(housing, aes(x=x4.numConv, y=y.price)) + geom_point(color='blue') +
          geom_smooth(method = "lm", formula = y ~ x, se = TRUE, col = "red", linetype = "solid", size = 0.6) +
          ggtitle("Number of Convenience Stores VS. House Price")
# There is a stronger positive relationship between The Number of Convenience Stores and House Price.
# so the predictor x4.numConv also has more impact on house price.

ggarrange(p_x1.y, p_x2.y, p_x3.y, p_x4.y, ncol = 2, nrow = 2)

# Outlier: From these four plots, it is clear that The Distance to The Nearest MRT Station is short, so the price of the outlier may be higher.
# However, there should be some other factors needed to be researched. Here, I will just keep this outlier in this data set.

### Geographical distribution of variables ###

# x5.lat & x6.long (Location) vs. y.price
p_x56.y <- ggplot(housing, aes(x=x6.long, y=x5.lat, color = y.price)) + geom_point() + scale_color_gradient(low = "blue", high = "red") +
           ggtitle("Geographical Distribution of House Price")
# Houses located in the crowd or inner circle (dense area) have higher prices, and the lower priced houses are located in the outer region.
# The location (latitude and longitude) can be an important predictor for the house price.

# x5.lat & x6.long (Location) vs. x2.age
p_x56.x2 <- ggplot(housing, aes(x=x6.long, y=x5.lat, color = x2.age)) + geom_point() + scale_color_gradient(low = "blue", high = "red") +
           ggtitle("Geographical Distribution of House Age")
# The range of house age from 0 to 40 can be found in the dense area, so there is no strong relationship between location and house age.

# x5.lat & x6.long (Location) vs. x3.disMRT
p_x56.x3 <- ggplot(housing, aes(x=x6.long, y=x5.lat, color = x3.disMRT)) + geom_point() + scale_color_gradient(low = "blue", high = "red") +
           ggtitle("Geographical Distribution of Distance to MRT")
# The houses located in the dense area have a shorter distance to the nearest MRT station, 
# so it shows that the nearest MRT station is around the center of the dense area.

# x5.lat & x6.long (Location) vs. x4.numConv
p_x56.x4 <- ggplot(housing, aes(x=x6.long, y=x5.lat, color = x4.numConv)) + geom_point() + scale_color_gradient(low = "blue", high = "red") +
           ggtitle("Geographical Distribution of Number of Conv. Stores")
# There're more convenience stores within walking distance in the dense area.

ggarrange(p_x56.y, p_x56.x2, p_x56.x3, p_x56.x4, ncol = 2, nrow = 2)

### Correlation heatmap ###
plot_correlation(housing)
# The predictor x3.disMRT, x4.numConv, x5.lat, and x6.long have stronger relationship with the response y.price, 
# while x1.tranDate and x2.age have less impact on house price.

# The distance to the nearest MRT station, number of convenience stores, latitude, and longitude
# seem to be quite correlated to each other, which has also be known in the previous geographical distribution plots.
# Thus, I will check collinearity in linear regression model later.

# Overview of the scatter plot, density distribution, and correlation 
ggpairs(housing)

# create_report(housing)

#--------------------------------------------------------------------------------
# 3. Data Transformation
#--------------------------------------------------------------------------------

# (1) No missing values needed to be handled
plot_missing(housing)

# (2) Check skewness
skewness(housing$y.price)  # 0.5955128
# The distribution of "house price" is just moderately right-skewed. 
# -> check whether to perform a log transform to reduce skewness 

# Add log term of housing price in the initial data set
housing <- housing %>% 
  mutate(y.logPrice = log(y.price))

skewness(housing$y.logPrice)  # -0.7012889
# After transforming, it has larger skewness. 

# View the difference on QQ plot
qq_data <- housing[, c("y.price", "y.logPrice")]
plot_qq(qq_data)  
# It doesn't reduce the skewness at all, so just keep the initial response variable "price."

# Remove the logPrice column of the housing data set
housing <- housing[, -8]

# (3) Perform feature scaling to prepare for future modelling

# Scale each numerical variable except for the response variable "price"
scalehousing <- scale(housing[, 1:6]) # mean=0 and standard deviation=1

head(scalehousing)
#      x1.tranDate     x2.age  x3.disMRT x4.numConv     x5.lat    x6.long
# [1,]   -0.823725  1.2541110 -0.7915373  2.0049816  1.1240698  0.4482199
# [2,]   -0.823725  0.1568964 -0.6158665  1.6654877  0.9113415  0.4006542
# [3,]    1.540380 -0.3873220 -0.4135150  0.3075125  1.4850633  0.6873517
# [4,]    1.244867 -0.3873220 -0.4135150  0.3075125  1.4850633  0.6873517
# [5,]   -1.119238 -1.1158725 -0.5493321  0.3075125  0.8331800  0.5922203
# [6,]   -1.710265 -0.9315405  0.8645401 -0.3714751 -0.4818677 -1.3566716

# After scaling: a new data frame named cleaned_housing
cleaned_housing <- as.data.frame(cbind(scalehousing, y.price=housing[,7]))
head(cleaned_housing)
#   x1.tranDate     x2.age  x3.disMRT x4.numConv     x5.lat    x6.long y.price
# 1   -0.823725  1.2541110 -0.7915373  2.0049816  1.1240698  0.4482199    37.9
# 2   -0.823725  0.1568964 -0.6158665  1.6654877  0.9113415  0.4006542    42.2
# 3    1.540380 -0.3873220 -0.4135150  0.3075125  1.4850633  0.6873517    47.3
# 4    1.244867 -0.3873220 -0.4135150  0.3075125  1.4850633  0.6873517    54.8
# 5   -1.119238 -1.1158725 -0.5493321  0.3075125  0.8331800  0.5922203    43.1
# 6   -1.710265 -0.9315405  0.8645401 -0.3714751 -0.4818677 -1.3566716    32.1

# (4) Waterfall Charts - show the number of observations changed or droppped

# Number of observations changed

x= list("Missing value handling", "Skewness reducing", "Feature scaling")
measure= c("relative", "relative", "relative")
text= c("", "", nrow(scalehousing))
y= c(0, 0, nrow(scalehousing))
data_changed = data.frame(x=factor(x, levels=x), measure, text, y)

Waterfall_changed <- plot_ly(data_changed, type = "waterfall", measure = ~measure,
                     x = ~x, textposition = "outside", y= ~y, text = ~text,
                     connector = list(line = list(color= "rgb(63, 63, 63)"))) %>%
                     layout(title = "Number of Observations Changed",
                     xaxis = list(title = ""), yaxis = list(title = ""), 
                     autosize = TRUE, showlegend = FALSE)
Waterfall_changed

# Number of total observations

x= list("Initial data","Missing value handling", "Skewness reducing", "Feature scaling", "Cleaned data")
measure= c("relative", "relative", "relative", "relative", "total")
text= c(nrow(housing),"", "", "", nrow(cleaned_housing))
y= c(nrow(housing), 0, 0, 0, 0)
data_total = data.frame(x=factor(x, levels=x), measure, text, y)

Waterfall_total <- plot_ly(data_total, name = 'Drop', type = "waterfall", measure = ~measure,
                           x = ~x, textposition = "outside", y= ~y, text = ~text,
                           connector = list(line = list(color= "rgb(63, 63, 63)"))) %>%
                           layout(title = "Number of Total Observations",
                           xaxis = list(title = ""), yaxis = list(title = ""), 
                           autosize = TRUE, showlegend = FALSE)
Waterfall_total

# Total observationa were changed(scaled), but no observations were dropped.

#--------------------------------------------------------------------------------
# 4. Data Splitting
#--------------------------------------------------------------------------------

n <- nrow(cleaned_housing)     # 414 - number of observations 

# 80/20 split
ntrain <- round(n*0.8)      # 331 - 80% for training set 
set.seed(123)               # Set seed for reproducible results
tindex <- sample(n, ntrain) # Create a random train sample index

# Create a training set 
housing.train <- cleaned_housing[tindex,]   
glimpse(housing.train)
x.train <- housing.train[, 1:6]
y.train <- housing.train$y.price

# Create a test set 
housing.test <- cleaned_housing[-tindex,]   
glimpse(housing.test)
x.test <- housing.test[, 1:6]

#--------------------------------------------------------------------------------
# 5. Modelling
#--------------------------------------------------------------------------------

#==============================
# 5.0 Create functions and DF for models evaluation
#==============================

# MSE: Mean Squared Error 
mse <- function(predictions, actual) {
  mean((predictions - actual) ^ 2 )
}
# RMSE: Root Mean Squared Error 
rmse <- function(predictions, actual) {
  sqrt(mean((predictions - actual) ^ 2 ))
}
# MAE: Mean Absolute Error 
mae <- function(predictions, actual) {
  mean(abs(predictions - actual))
}
# MAPE: Mean Absolute Percentage Error 
mape <- function(predictions, actual) {
  mean(abs((predictions - actual)/actual))
}

# For models error summary
modelsSummary <- data.frame(Model = character(), MSE = double(), RMSE = double(), 
                            MAE = double(), MAPE = double(), Description = character())

#==============================
# 5.1 Multiple Linear Regression
#==============================

### Model 1.1 (lm1.1): use all of the predictors

lr1.1 <- lm(y.price~., data=housing.train)
summary(lr1.1)
# Residual standard error: 8.864 on 324 degrees of freedom
# Multiple R-squared:  0.5867,	Adjusted R-squared:  0.579 
# F-statistic: 76.66 on 6 and 324 DF,  p-value: < 2.2e-16

### Model 1.2 (lm1.2): use all predictors except for x6.long (the least significant variable) 

lr1.2 <- lm(y.price~.-x6.long, data=housing.train)
summary(lr1.2)
# Residual standard error: 8.854 on 325 degrees of freedom
# Multiple R-squared:  0.5863,	Adjusted R-squared:   0.58 
# F-statistic: 92.14 on 5 and 325 DF,  p-value: < 2.2e-16

# Check collinearity using vif(): variance inflation factors
vif(lr1.1)
# x1.tranDate      x2.age   x3.disMRT  x4.numConv      x5.lat     x6.long 
#    1.012077    1.011098    3.701476    1.564287    1.485543    2.597717 
vif(lr1.2)
# x1.tranDate      x2.age   x3.disMRT  x4.numConv      x5.lat 
#    1.011745    1.009241    1.829119    1.561999    1.461529 

# Variance inflation factors are all below 5, so it is acceptable.

# Predicting value on test data
housing.test$lr1.1_pred <- predict(lr1.1, housing.test)
housing.test$lr1.2_pred <- predict(lr1.2, housing.test)

### For evaluation - test error
mse1.1 <- mse(housing.test$lr1.1_pred, housing.test$y.price)
rmse1.1 <- rmse(housing.test$lr1.1_pred, housing.test$y.price)
mae1.1 <- mae(housing.test$lr1.1_pred, housing.test$y.price)
mape1.1 <- mape(housing.test$lr1.1_pred, housing.test$y.price)

mse1.2 <- mse(housing.test$lr1.2_pred, housing.test$y.price)
rmse1.2 <- rmse(housing.test$lr1.2_pred, housing.test$y.price)
mae1.2 <- mae(housing.test$lr1.2_pred, housing.test$y.price)
mape1.2 <- mape(housing.test$lr1.2_pred, housing.test$y.price)

err1.1 <- data.frame(Model = 'lr1.1', MSE = mse1.1, RMSE = rmse1.1, 
                     MAE = mae1.1, MAPE = mape1.1, Description = 'all predictors')

err1.2 <- data.frame(Model = 'lr1.2', MSE = mse1.2, RMSE = rmse1.2, 
                     MAE = mae1.2, MAPE = mape1.2, Description = 'all predictors - x6.long')
# Error summary
modelsSummary <- rbind(modelsSummary, err1.1, err1.2)
modelsSummary

#==============================
# 5.2 Ridge Regression & Lasso
#==============================

x.trainMatrix = model.matrix(y.price~., housing.train)[,-1]
x.testMatrix = model.matrix(y.price~., housing.test)[, 2:7]
grid <- 10^seq(10, -2, length=100)

### Model 2.1 (ridge2.1): Ridge Regression

ridge2.1 <- glmnet(x.trainMatrix, y.train, alpha=0, lambda=grid, thresh=1e-12)
plot(ridge2.1)

# Use cross-validation to choose the tuning parameter λ
# cv.glmnet(): cross-validation function (nfolds: default is 10)
set.seed(10)
cv.ridge <- cv.glmnet(x.trainMatrix, y.train, alpha=0)
plot(cv.ridge)
bestlam_ridge2.1 <- cv.ridge$lambda.min
bestlam_ridge2.1  # 0.8983818

### Model 2.2 (lasso2.2): Lasso

lasso2.2 <- glmnet(x.trainMatrix, y.train, lambda=grid)
plot(lasso2.2)

# Use cross-validation to choose the tuning parameter λ
set.seed(10)
cv.lasso <- cv.glmnet(x.trainMatrix, y.train, alpha=1)
plot(cv.lasso)
bestlam_lasso2.2 <- cv.lasso$lambda.min  
bestlam_lasso2.2  # 0.1498591

# Predicting value on test data
housing.test$ridge2.1_pred <- predict(ridge2.1, s=bestlam_ridge2.1, newx=x.testMatrix)[,1]
housing.test$lasso2.2_pred <- predict(lasso2.2, s=bestlam_lasso2.2, newx=x.testMatrix)[,1]

### For evaluation - test error
mse2.1 <- mse(housing.test$ridge2.1_pred, housing.test$y.price)
rmse2.1 <- rmse(housing.test$ridge2.1_pred, housing.test$y.price)
mae2.1 <- mae(housing.test$ridge2.1_pred, housing.test$y.price)
mape2.1 <- mape(housing.test$ridge2.1_pred, housing.test$y.price)

mse2.2 <- mse(housing.test$lasso2.2_pred, housing.test$y.price)
rmse2.2 <- rmse(housing.test$lasso2.2_pred, housing.test$y.price)
mae2.2 <- mae(housing.test$lasso2.2_pred, housing.test$y.price)
mape2.2 <- mape(housing.test$lasso2.2_pred, housing.test$y.price)

err2.1 <- data.frame(Model = 'ridge2.1', MSE = mse2.1, RMSE = rmse2.1, 
                     MAE = mae2.1, MAPE = mape2.1, Description = '')

err2.2 <- data.frame(Model = 'lasso2.2', MSE = mse2.2, RMSE = rmse2.2, 
                     MAE = mae2.2, MAPE = mape2.2, Description = '')

# Error summary
modelsSummary <- rbind(modelsSummary, err2.1, err2.2)
modelsSummary

#==============================
# 5.3 Principal Components Regression (PCR) & Partial Least Squares (PLS)
#==============================

### Model 3.1 (pcr3.1): Principal Components Regression

set.seed(10)
pcr3.1 <- pcr(y.price~., data=housing.train, scale=TRUE, validation="CV")
summary(pcr3.1)
validationplot(pcr3.1, val.type="MSEP")  # plot the cross-validation score
# The lowest cross-validation error occurs when M = 6 components are used.

### Model 3.2 (pls3.2): Partial Least Squares

set.seed(10)
pls3.2 <- plsr(y.price~., data=housing.train, scale=TRUE, validation="CV")
summary(pls3.2)
validationplot(pls3.2, val.type="MSEP")
# The lowest cross-validation error occurs when only M = 3 partial least squares directions are used.

# Predicting value on test data
housing.test$pcr3.1_pred <- predict(pcr3.1, x.testMatrix, ncomp=6)[1:(n-ntrain)]
housing.test$pls3.2_pred <- predict(pls3.2, x.testMatrix, ncomp=3)[1:(n-ntrain)]

### For evaluation - test error
mse3.1 <- mse(housing.test$pcr3.1_pred, housing.test$y.price)
rmse3.1 <- rmse(housing.test$pcr3.1_pred, housing.test$y.price)
mae3.1 <- mae(housing.test$pcr3.1_pred, housing.test$y.price)
mape3.1 <- mape(housing.test$pcr3.1_pred, housing.test$y.price)

mse3.2 <- mse(housing.test$pls3.2_pred, housing.test$y.price)
rmse3.2 <- rmse(housing.test$pls3.2_pred, housing.test$y.price)
mae3.2 <- mae(housing.test$pls3.2_pred, housing.test$y.price)
mape3.2 <- mape(housing.test$pls3.2_pred, housing.test$y.price)

err3.1 <- data.frame(Model = 'pcr3.1', MSE = mse3.1, RMSE = rmse3.1, 
                     MAE = mae3.1, MAPE = mape3.1, Description = '6 components')

err3.2 <- data.frame(Model = 'pls3.2', MSE = mse3.2, RMSE = rmse3.2, 
                     MAE = mae3.2, MAPE = mape3.2, Description = '3 pls directions')

# Error summary
modelsSummary <- rbind(modelsSummary, err3.1, err3.2)
modelsSummary

#==============================
# 5.4 Generalized Additive Models (GAMs) - Smoothing Splines
#==============================

# Packages "mgcv": Finds smoothing parameters by default
# In this case: use default method - Generalized cross validation criteria (GCV)

par(mfrow=c(2,3))

### Model 4.1 (gam4.1): fit a GAM using "smoothing spline" functions of all predictors

gam4.1 <- gam(y.price~s(x1.tranDate)+s(x2.age)+s(x3.disMRT)+s(x4.numConv)+
             s(x5.lat)+s(x6.long), data=housing.train)
summary(gam4.1)
plot(gam4.1, se=TRUE, col="blue")
# the functions of "x1.tranDate" and "x4.numConv" looks rather linear

### Model 4.2 (gam4.2): fit a GAM using linear functions of "x1.tranDate" and "x4.numConv"
#                       and smoothing spline functions of the other predictors

gam4.2 <- gam(y.price~x1.tranDate+s(x2.age)+s(x3.disMRT)+x4.numConv+
             s(x5.lat)+s(x6.long), data=housing.train)
summary(gam4.2)
plot(gam4.2, se=TRUE, col="blue")

### Model 4.3 (gam4.3): fit a GAM using a linear function of "x1.tranDate"
#                       and smoothing spline functions of the other predictors

gam4.3 <- gam(y.price~x1.tranDate+s(x2.age)+s(x3.disMRT)+s(x4.numConv)+
               s(x5.lat)+s(x6.long), data=housing.train)
summary(gam4.3)
plot(gam4.3, se=TRUE, col="blue")

### Model 4.4 (gam4.4): fit a GAM using a linear function of "x4.numConv"
#                       and smoothing spline functions of the other predictors

gam4.4 <- gam(y.price~s(x1.tranDate)+s(x2.age)+s(x3.disMRT)+x4.numConv+
              s(x5.lat)+s(x6.long), data=housing.train)
summary(gam4.4)
plot(gam4.4, se=TRUE, col="blue")

par(mfrow=c(1,1))

# Perform a series of ANOVA tests to determine which of these four models is the best
anova(gam4.1, gam4.2, gam4.3, gam4.4, test="F")
# a GAM with a linear function of x4.numConv is preferred among these four models
# -> choose Model 4.4 (gam4.4) 

# Predicting value on test data
housing.test$gam4.4_pred <- predict(gam4.4, newdata=housing.test)

### For evaluation - test error
mse4.4 <- mse(housing.test$gam4.4_pred, housing.test$y.price)
rmse4.4 <- rmse(housing.test$gam4.4_pred, housing.test$y.price)
mae4.4 <- mae(housing.test$gam4.4_pred, housing.test$y.price)
mape4.4 <- mape(housing.test$gam4.4_pred, housing.test$y.price)

err4.4 <- data.frame(Model = 'gam4.4', MSE = mse4.4, RMSE = rmse4.4, 
                     MAE = mae4.4, MAPE = mape4.4, Description = 'linear: x4.numConv, smoothing: others')

# Error summary
modelsSummary <- rbind(modelsSummary, err4.4)
modelsSummary

#==============================
# 5.5 Tree-based: Bagging & Random Forest, Boosting
#==============================

### Model 5.1 (bag5.1): Bagging - A special case of a random forest with m = p

set.seed(10)
# mtry=6: all 6 predictors should be considered for each split of the tree (bagging)
bag5.1 <- randomForest(y.price~., data=housing.train, mtry=6, importance=TRUE) # ntree=500
bag5.1
plot(bag5.1)  # shows the error and the number of trees

### Model 5.2 (rf5.2): Random Forests

set.seed(10)
rf5.2 <- randomForest(y.price~., data=housing.train, importance=TRUE) 
# Default: "p/3" variables (mtry=2, ntree=500)
rf5.2
plot(rf5.2)

# View the importance of each variable
# %IncMSE: mean decrease of accuracy in predictions on the out of bag samples when a given variable is excluded from the model
# IncNodePurity: a measure of the total decrease in node impurity(measured by the training RSS) that results from splits over that variable
importance(bag5.1)
importance(rf5.2)

# Plots of importance measures
varImpPlot(bag5.1)
varImpPlot(rf5.2)

### Model 5.3 (bst5.3): Boosting

set.seed(10)
bst5.3 <- gbm(y.price~., data=housing.train, distribution="gaussian", n.trees=5000, 
             interaction.depth=4, cv.folds = 10) # The default shrinkage parameter λ is 0.001
summary(bst5.3)  # the relative influence statistics
# x3.disMRT, x2.age, and x5.lat are the most important variables in this model

# Tuning a gbm model: early stopping
# -> Avoid overfitting: stop the training procedure once the performance stops improving beyond a certain number of iterations.
# Determine the optimum number of iterations
ntree_opt_cv <- gbm.perf(bst5.3, method="cv")
ntree_opt_cv  # 86

# Predicting value on test data
housing.test$bag5.1_pred <- predict(bag5.1, newdata=housing.test)
housing.test$rf5.2_pred <- predict(rf5.2, newdata=housing.test)
housing.test$bst5.3_pred <- predict(bst5.3, newdata=housing.test, n.trees=ntree_opt_cv)
par(mfrow=c(2,2))

### For evaluation - test error
mse5.1 <- mse(housing.test$bag5.1_pred, housing.test$y.price)
rmse5.1 <- rmse(housing.test$bag5.1_pred, housing.test$y.price)
mae5.1 <- mae(housing.test$bag5.1_pred, housing.test$y.price)
mape5.1 <- mape(housing.test$bag5.1_pred, housing.test$y.price)

mse5.2 <- mse(housing.test$rf5.2_pred, housing.test$y.price)
rmse5.2 <- rmse(housing.test$rf5.2_pred, housing.test$y.price)
mae5.2 <- mae(housing.test$rf5.2_pred, housing.test$y.price)
mape5.2 <- mape(housing.test$rf5.2_pred, housing.test$y.price)

mse5.3 <- mse(housing.test$bst5.3_pred, housing.test$y.price)
rmse5.3 <- rmse(housing.test$bst5.3_pred, housing.test$y.price)
mae5.3 <- mae(housing.test$bst5.3_pred, housing.test$y.price)
mape5.3 <- mape(housing.test$bst5.3_pred, housing.test$y.price)

err5.1 <- data.frame(Model = 'bag5.1', MSE = mse5.1, RMSE = rmse5.1, 
                     MAE = mae5.1, MAPE = mape5.1, Description = 'mtry=6, ntree=500')

err5.2 <- data.frame(Model = 'rf5.2', MSE = mse5.2, RMSE = rmse5.2, 
                     MAE = mae5.2, MAPE = mape5.2, Description = 'mtry=2, ntree=500')

err5.3 <- data.frame(Model = 'bst5.3', MSE = mse5.3, RMSE = rmse5.3, 
                     MAE = mae5.3, MAPE = mape5.3, Description = 'interaction.depth=4, optimal ntree=86')

# Error summary
modelsSummary <- rbind(modelsSummary, err5.1, err5.2, err5.3)
modelsSummary

#==============================
# 5.6 Support Vector Machine (SVM)
#==============================

### Model 6.1 (svm6.1): Support Vector Machine - Regression

# Tuning the model by varying values of maximum allowable error and cost parameter
set.seed(10)
svm6.1 <- tune(svm, y.price~., data=housing.train, kernel = "radial",
               ranges = list(epsilon = seq(0,1,0.1), cost = 1:50))
svm6.1
plot(svm6.1) # Map tuning results
svm6.1_best <- svm6.1$best.model  # Find out the best model

# Predicting value on test data using best model
housing.test$svm6.1_pred <- predict(svm6.1_best, newdata=housing.test)

### For evaluation - test error
mse6.1 <- mse(housing.test$svm6.1_pred, housing.test$y.price)
rmse6.1 <- rmse(housing.test$svm6.1_pred, housing.test$y.price)
mae6.1 <- mae(housing.test$svm6.1_pred, housing.test$y.price)
mape6.1 <- mape(housing.test$svm6.1_pred, housing.test$y.price)

err6.1 <- data.frame(Model = 'svm6.1', MSE = mse6.1, RMSE = rmse6.1, 
                     MAE = mae6.1, MAPE = mape6.1, Description = 'radial kernel, cost=5, epsilon=0.1')

# Error summary
modelsSummary <- rbind(modelsSummary, err6.1)
modelsSummary

#--------------------------------------------------------------------------------
# 6. Evaluation and Conclusion
#--------------------------------------------------------------------------------

# Take a look at the actual house price of test data and predicted value
head(housing.test)[7:18]

#    y.price lr1.1_pred lr1.2_pred ridge2.1_pred lasso2.2_pred pcr3.1_pred
# 5     43.1   47.24933   47.32899      46.99748      47.18470    47.24933
# 15    34.3   47.28675   47.39237      47.10749      47.00660    47.28675
# 16    50.5   38.98889   39.24219      39.36362      39.22052    38.98889
# 18    37.4   38.55001   38.28957      38.09747      38.51641    38.55001
# 20    47.7   47.71888   47.62086      47.18693      47.50773    47.71888
# 22    51.6   49.59867   49.71828      49.32855      49.30233    49.59867

#    pls3.2_pred gam4.4_pred bag5.1_pred rf5.2_pred bst5.3_pred svm6.1_pred
# 5     47.69216    49.89302    50.08994   50.35590    48.87671    54.61754
# 15    48.19191    41.24944    33.29943   35.79252    37.35752    33.78017
# 16    39.05754    44.60781    48.07467   45.81815    43.06665    36.00115
# 18    37.82729    39.28048    36.51833   36.05265    36.27422    29.63891
# 20    47.55934    52.81183    48.11222   48.70960    49.80282    51.52078
# 22    49.75387    51.11380    56.17460   55.74879    61.11446    52.48325


# Evaluation: summary of test error of all models
modelsSummary

#       Model      MSE     RMSE      MAE      MAPE                           Description
# 1     lr1.1 82.71355 9.094699 6.833200 0.2191385                        all predictors
# 2     lr1.2 82.46512 9.081031 6.842128 0.2194620              all predictors - x6.long
# 3  ridge2.1 80.40068 8.966642 6.745362 0.2117971                                      
# 4  lasso2.2 81.53137 9.029472 6.803126 0.2158941                                      
# 5    pcr3.1 82.71355 9.094699 6.833200 0.2191385                          6 components
# 6    pls3.2 84.22834 9.177600 6.883452 0.2170109                      3 pls directions
# 7    gam4.4 51.65687 7.187271 5.191150 0.1578802 linear: x4.numConv, smoothing: others
# 8    bag5.1 46.21509 6.798168 4.896822 0.1472833                     mtry=6, ntree=500
# 9     rf5.2 39.12631 6.255103 4.406327 0.1360753                     mtry=2, ntree=500
# 10   bst5.3 42.52357 6.521010 4.678538 0.1376725 interaction.depth=4, optimal ntree=86
# 11   svm6.1 44.40091 6.663401 4.825837 0.1442710    radial kernel, cost=5, epsilon=0.1

# Conclusion:
# The above data frame shows the summary of test Mean Squared Error(MSE), Root Mean Squared Error(RMSE),
# Mean Absolute Error(MAE), and Mean Absolute Percentage Error(MAPE) of all models.

# Overall, in terms of these test error measures, linear models(Linear Regression, Ridge Regression & Lasso, 
# and Principal Components Regression (PCR) & Partial Least Squares (PLS)) perform worse than non-linear
# (Generalized Additive Models (GAMs), Tree-based - Bagging & Random Forest & Boosting, and Support Vector Machine (SVM)) models for this data.

# Mean absolute percentage error of non-linear methods ranges from 13% to 15%, whereas linear methods ranges around 21%.
# And in all these non-linear modelling, the random forest model yields the best performances.
# Thus, this model helps us predict the house price of unit area in Xindian district of New Taipei City.

# For future prospects
# 1. Find out what other factors cause extreme values to determine whether to exclude some of the properties identified as outliers.
# 2. Tune models: Run randomized grid search to identify hyperparameters that increase the performances of the random forest model or other models.
# 3. Perform some clustering analysis to assess if we could identify geographical categories.
# 4. Test models with more recent data or even more features to check how the market has evolved and the need for building a new model.
 
