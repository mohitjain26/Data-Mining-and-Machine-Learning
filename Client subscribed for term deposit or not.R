# Data Preprocessing

#importing dataset
library(readxl)
bank_additional_full <- read_excel("C:/Users/jnmht/Desktop/Data Analytics/DATASET for DATA MINING/PROJECT/Bank Marketing/bank-additional-full.xlsx")
View(bank_additional_full)
class(bank_additional_full)
bank_additional_full = as.numeric(bank_additional_full)
str(bank_additional_full)
summary(bank_additional_full)

# encoding categorical data
bank_additional_full$job = factor(bank_additional_full$job,
                                  levels = c('housemaid','services','admin.','blue-collar','technician','retired','management','unemployed','self-employed','unknown','entrepreneur','student'),
                                  labels = c(1, 2, 3,4,5,6,7,8,9,10,11,12))
bank_additional_full$marital = factor(bank_additional_full$marital,
                                      levels = c('divorced','married','single','unknown'),
                                      labels = c(1,2,3,4))
bank_additional_full$education = factor(bank_additional_full$education,
                                        levels = c('basic.4y','high.school','basic.6y','basic.9y','professional.course','unknown','university.degree','illiterate'),
                                        labels = c(1,2,3,4,5,6,7,8))
bank_additional_full$default = factor(bank_additional_full$default,
                                      levels = c('no','unknown','yes'),
                                      labels = c(1,2,3)) 
bank_additional_full$housing = factor(bank_additional_full$housing,
                                      levels = c('no','unknown','yes'),
                                      labels = c(1,2,3))
bank_additional_full$loan = factor(bank_additional_full$loan,
                                   levels = c('no','unknown','yes'),
                                   labels = c(1,2,3))
bank_additional_full$contact = factor(bank_additional_full$contact,
                                      levels = c('telephone','cellular'),
                                      labels = c(1,2))
bank_additional_full$month = factor(bank_additional_full$month,
                                    levels = c('may','jun','jul','aug','oct','nov','dec','mar','apr','sep'),
                                    labels = c(1,2,3,4,5,6,7,8,9,10))
bank_additional_full$day_of_week = factor(bank_additional_full$day_of_week,
                                          levels = c('mon','tue','wed','thu','fri'),
                                          labels = c(1,2,3,4,5))
bank_additional_full$y = factor(bank_additional_full$y,
                                levels = c('no','yes'),
                                labels = c(0,1))
# correlation
corrplot(correlation,method="number")
cor(as.numeric(bank_additional_full$y), as.numeric(bank_additional_full$job))
cor(as.numeric(bank_additional_full$y), bank_additional_full$age)
cor(as.numeric(bank_additional_full$y), as.numeric(bank_additional_full$marital))
cor(as.numeric(bank_additional_full$y), as.numeric(bank_additional_full$education))
cor(as.numeric(bank_additional_full$y), as.numeric(bank_additional_full$default))
cor(as.numeric(bank_additional_full$y), as.numeric(bank_additional_full$housing))
cor(as.numeric(bank_additional_full$y), as.numeric(bank_additional_full$loan))
cor(as.numeric(bank_additional_full$y), as.numeric(bank_additional_full$contact))
cor(as.numeric(bank_additional_full$y), as.numeric(bank_additional_full$month))
cor(as.numeric(bank_additional_full$y), as.numeric(bank_additional_full$day_of_week))

# looking for missing values(mssing values replaced with 'unknown')
sapply(bank_additional_full,function(x) sum(is.na(x)))

# split dataset into training and test set
install.packages("caTools")
library(caTools)
split = sample.split(bank_additional_full$y, SplitRatio = 0.8)
training_set = subset(bank_additional_full, split == TRUE)
test_set = subset(bank_additional_full, split == FALSE)
View(test_set)
# implementing logistic regression
classifier = glm(y ~ age+job+marital+education+default+housing+loan+day_of_week+duration,
                 family = binomial(link = "logit"),
                 data = training_set)

#expected output using logistic
prob_pred = predict(classifier, type = "response", newdata = test_set[-21])
summary(prob_pred)
prob_pred

y_pred = ifelse(prob_pred > 0.5, 1, 0)
p_class <- factor(y_pred, levels = levels(test_set[["y"]]))
p_class                            

str(test_set[["y"]])
str(p_class)
library(caret)
confusionMatrix(p_class,test_set$y)
# confusion matrix
cm = table(factor(p_class, levels=0:1), factor(test_set$y, levels=0:1))

# accuracy
accuracy = (cm[1,1]+cm[2,2])/(cm[1,1]+cm[1,2]+cm[2,1]+cm[2,2])

# precision
precision = cm[1,1]/(cm[1,1]+cm[1,2])
precision
#recall or sensitivity
#specificity
#########################################################
# Implementing SVM

# Fitting SVM to the Training set
install.packages('e1071')
library(e1071)
install.packages("kernlab")
library(kernlab)
classifier_svm = svm(formula = y ~ age+job+marital+education+default+housing+loan+day_of_week+duration,
                 data = training_set,
                 type = 'C-classification',
                 kernel = 'polynomial')

# Predicting the Test set results
y_pred_svm = predict(classifier_svm, newdata = test_set[-21])
p_class_svm <- factor(y_pred_svm, levels = levels(test_set[["y"]]))
# Making the Confusion Matrix
confusionMatrix(y_pred_svm,test_set$y)
