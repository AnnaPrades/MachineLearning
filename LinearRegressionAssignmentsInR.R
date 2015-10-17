#Linear Regression - Machine Learning, University of Washington, Week 1

##Read data
sales <- read.csv("home_data.csv")
dim(sales)
str(sales) #all the variables are either numeric or integer

##Which neighborhood (zip code) of Seattle has the highest average house sale price?
list_by_price <- sort(tapply(sales$price, sales$zipcode, mean), decreasing = TRUE)
head(list_by_price)
list_by_price[1] # It is the 98039

##Select only the houses with this zip code, and compute the average price.
most_expensive_zipcode <- subset(sales, zipcode == 98039)
mean(most_expensive_zipcode$price) #Question 1: 2160607

##Select the houses that have 'sqft_living' higher than 2000 sqft but no larger than 4000 sqft.
selection <- subset(sales, sqft_living > 2000 & sqft_living < 4000)

##What fraction of the all houses have 'sqft_living' in this range?
nrow(selection)/nrow(sales) #Question 2: 0.4215518

        
##Split data
library(caTools)
set.seed(123)
split = sample.split(sales$price, SplitRatio = 0.80)
train = subset(sales, split == TRUE)
test = subset(sales, split == FALSE)

##Build linear regression model my_features
my_features <- train[ , c("price","bedrooms","bathrooms", 'sqft_living', 'sqft_lot', 'floors', 'zipcode')]

##&Advanced features 
advanced_features = train[ , c("price",'bedrooms', 'bathrooms', 'sqft_living', 'sqft_lot', 
                               'floors','zipcode', 'condition', 'grade', 
                               'waterfront', 'view', 'sqft_above', 'sqft_basement',
                               'yr_built','yr_renovated','lat', 'long',
                               'sqft_living15','sqft_lot15')]
##Linear regression of my_features
my_features_model <- lm(price ~ ., data = my_features)
summary(my_features_model)

advanced_features_model <- lm(price ~., data=advanced_features)
summary(advanced_features_model)

##Calculating predictions
PredTest_my_features <- predict(my_features_model, newdata = test)
PredTest_advanced_features <- predict(advanced_features_model, newdata = test)

#Calculating out of sample R-squared
###The out-of-sample R^2 computes how good our regression predictions are compared to these baseline predictions.
baseline = mean(train$price)
SSE_my_features <- sum((PredTest_my_features - test$price)^2)
SSE_advanced_features <- sum((PredTest_advanced_features - test$price)^2)
SST = sum((baseline - test$price)^2)
R_my_features <- 1 - SSE_my_features/SST
R_my_features
R_advanced_features <- 1 - SSE_advanced_features/SST
R_advanced_features

#Calculatingtesting set Root Mean Squared Error
###RMSE is calculated based on SSE.This actually the standard deviation for your reponse.Good measure of accuracy.
RMSE_my_features = sqrt(SSE_my_features / nrow(test))
RMSE_advanced_features = sqrt(SSE_advanced_features / nrow(test))
RMSE_my_features
RMSE_advanced_features

(difference <- RMSE_my_features - RMSE_advanced_features) # Question 3: 44874.7


