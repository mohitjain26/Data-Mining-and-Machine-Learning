library(readxl)
OnlineNewsPopularity <- read_excel("C:/Users/jnmht/Desktop/Data Analytics/DATASET for DATA MINING/PROJECT/Online News popularity/OnlineNewsPopularity.xlsx")
View(OnlineNewsPopularity)
# Exploratory Data Analysis and Cleaning Data:

# check if the data has any missing values:
sum(is.na(OnlineNewsPopularity))

# check outliers (use summary) 
summary(OnlineNewsPopularity)
hist(OnlineNewsPopularity$n_non_stop_unique_tokens)
boxplot(OnlineNewsPopularity$n_non_stop_words)
# remove outliers from "n_unique_tokens", "n_non_stop_words", and "n_non_stop_unique_tokens" 
OnlineNewsPopularity = OnlineNewsPopularity[!OnlineNewsPopularity$n_non_stop_words==1042,]
OnlineNewsPopularity = OnlineNewsPopularity[!OnlineNewsPopularity$n_unique_tokens==701,]
OnlineNewsPopularity = OnlineNewsPopularity[!OnlineNewsPopularity$n_non_stop_unique_tokens==650,]

# dropping useless data : url, timedelta, is_weekend
drops <- c("url","timedelta","is_weekend")
OnlineNewsPopularity = OnlineNewsPopularity[ , !(names(OnlineNewsPopularity) %in% drops)]

# factor the categrocical data
#Converting categorical values from numeric to factor - News subjects

OnlineNewsPopularity$data_channel_is_lifestyle = factor(OnlineNewsPopularity$data_channel_is_lifestyle)
OnlineNewsPopularity$data_channel_is_entertainment = factor(OnlineNewsPopularity$data_channel_is_entertainment)
OnlineNewsPopularity$data_channel_is_bus = factor(OnlineNewsPopularity$data_channel_is_bus)
OnlineNewsPopularity$data_channel_is_socmed = factor(OnlineNewsPopularity$data_channel_is_socmed)
OnlineNewsPopularity$data_channel_is_tech = factor(OnlineNewsPopularity$data_channel_is_tech)
OnlineNewsPopularity$data_channel_is_world = factor(OnlineNewsPopularity$data_channel_is_world)


#Converting categorical values from numeric to factor - Weekdays
OnlineNewsPopularity$weekday_is_monday = factor(OnlineNewsPopularity$weekday_is_monday)
OnlineNewsPopularity$weekday_is_tuesday = factor(OnlineNewsPopularity$weekday_is_tuesday)
OnlineNewsPopularity$weekday_is_wednesday = factor(OnlineNewsPopularity$weekday_is_wednesday)
OnlineNewsPopularity$weekday_is_thursday = factor(OnlineNewsPopularity$weekday_is_thursday)
OnlineNewsPopularity$weekday_is_friday = factor(OnlineNewsPopularity$weekday_is_friday)
OnlineNewsPopularity$weekday_is_saturday = factor(OnlineNewsPopularity$weekday_is_saturday)
OnlineNewsPopularity$weekday_is_sunday = factor(OnlineNewsPopularity$weekday_is_sunday)
#taking log shares to optimize model
####################################
OnlineNewsPopularity$shares <- log(OnlineNewsPopularity$shares)
# making a classifcation problem(median =7.244)
library(caTools)
summary(OnlineNewsPopularity$shares)
OnlineNewsPopularity$shares <- as.factor(ifelse(OnlineNewsPopularity$shares > 7.244,1,0))
split = sample.split(OnlineNewsPopularity$shares, SplitRatio = 0.8)
training_set = subset(OnlineNewsPopularity, split == TRUE)
test_set = subset(OnlineNewsPopularity, split == FALSE)


#fit_lmlog <- lm(shares ~ ., data = training_set)
#summary(fit_lmlog)

#Implementing KNN
###########################################
library(caret)

kNN_classifier <- train(shares~., data = training_set, method = "knn", 
              maximize = TRUE,
              trControl = trainControl(method = "cv", number = 10),
              preProcess=c("center", "scale"))
ggplot(kNN_classifier) + geom_line() + geom_smooth() + theme_light()

predictedkNN3 <- predict(kNN_classifier, newdata = test_set)
confusionMatrix(predictedkNN3, test_set$shares)

#implementing decision tree
# Classification and Regression Trees
library(rpart)
install.packages('rattle')
library(rattle)
news.cart<-rpart(shares ~.,training_set,method='class')

par(mfrow=c(1,1))
fancyRpartPlot(news.cart, digits=2, palettes = c("Purples", "Oranges"))

#predict
cart_pred<-predict(news.cart,test_set ,type="class")
cart_prob<-predict(news.cart,test_set ,type="prob")

# Confusion matrix
confusionMatrix(cart_pred, test_set$shares)


