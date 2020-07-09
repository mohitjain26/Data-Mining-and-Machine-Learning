library(readxl)
listings <- read_excel("C:/Users/jnmht/Desktop/Data Analytics/DATASET for DATA MINING/PROJECT/berlin_airbnb/listings.xlsx")
View(listings)
str(listings)
# missing values
sapply(listings,function(x) sum(is.na(x)))
# dropping columns which containg missing values(as this not important)
# drop neigbourhood column as we can have similar from neigborhood group
drops <- c("name","host_name","last_review","neighbourhood","id","host_id")
listings = listings[ , !(names(listings) %in% drops)]
summary(listings)
library(dplyr)
listings_outlier = listings
boxplot(listings_outlier$price)
listings <- listings%>%filter(price <=70)
boxplot(listings$price)
summary(listings$price)
# categorical data
listings$room_type = factor(listings$room_type)
listings$neighbourhood_group = factor(listings$neighbourhood_group)

library(caTools)
set.seed(123)
# multi linear regression
split = sample.split(listings$price, SplitRatio = 0.8)
training_set = subset(listings, split == TRUE)
test_set = subset(listings, split == FALSE)
regressor_linear = lm(price ~. , data = training_set)
summary(regressor_linear)

#################################################################################

# random forest 
library(randomForest)
drops <- c("reviews_per_month")
df = listings[ , !(names(listings) %in% drops)]
library(caTools)
set.seed(123)

split = sample.split(df$price, SplitRatio = 0.8)
training_set_review = subset(df, split == TRUE)
test_set_review = subset(df, split == FALSE)
set.seed(123)
classifier = randomForest(x = training_set_review[-5],
                          y = training_set_review$price,
                          ntree = 500)
# Predicting the Test set results
y_pred = predict(classifier, newdata = test_set_review[-5])
y_test = test_set_review[,5]
install.packages('Metrics')
library(Metrics)
RMSE(y_pred, test_set_review$price)
RMSE(y_pred, training_set_review$price)
mse(y_pred, test_set_review$price)
mse(y_pred, training_set_review$price)
library(rpart)
rsq.rpart(y_pred, test_set_review$price)
summary(classifier)
# R square
rss <- sum((y_pred - test_set_review$price) ^ 2)  ## residual sum of squares
tss <- sum((test_set_review$price - mean(test_set_review$price)) ^ 2)  ## total sum of squares
R_squared <- 1 - rss/tss #rsquare
