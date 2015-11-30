# Data load & EDA

require(caret)

# Set working directory
if (getwd() != "/Users/christophergraham/Documents/School/CMTH642/CMTH642_Assignments/Assignment3") {
    setwd("/Users/christophergraham/Documents/School/CMTH642/CMTH642_Assignments/Assignment3")
}

# load data

red <- read.csv('winequality-red.csv', sep = ';')
red$quality <- as.factor(red$quality)

# basic info
sum(complete.cases(red))
cor(red)
table(red$quality)
hist(red$quality)

# split off test data for final confirmation
set.seed(25678)
train_idx <- createDataPartition(y=red$quality, p=0.8, list = FALSE)
training <- red[train_idx,]
testing <- red[-train_idx,]

table(testing$quality)
# Set training parameters

fitControl <- trainControl(## 10-fold CV
    method = "repeatedcv",
    number = 10,
    ## repeated ten times
    repeats = 10,
    savePredictions = TRUE)

fit_lm <- train(quality ~ ., data=training, method='lm',
              trControl = fitControl)

fit_mn <- train(quality ~ ., data=training, method='multinom',
                 trControl = fitControl, verbose = FALSE)
fit_gbm <- train(quality ~ ., data=training, method='gbm',
                 trControl = fitControl, verbose = FALSE)
fit_rf <- train(quality ~ ., data=training, method='rf',
                trControl = fitControl)
fit_lda <- train(quality ~ ., data = training, method = 'lda',
                 trContro = fitControl)
fit_svm <- train(quality ~ ., data=training, method='svmRadial',
                 trControl = fitControl)
fit_nn <- train(quality ~ ., data=training, method='nnet',
                trControl = fitControl, verbose = FALSE)

# Attempt at SMOTE oversampling
require(DMwR)
new_data <- SMOTE(quality ~ ., data=training, perc.over = 600, perc.under = 600)

