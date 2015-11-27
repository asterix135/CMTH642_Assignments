# Data load & EDA

require(caret)

# Set working directory
if (getwd() != "/Users/christophergraham/Documents/School/CMTH642/CMTH642_Assignments/Assignment3") {
    setwd("/Users/christophergraham/Documents/School/CMTH642/CMTH642_Assignments/Assignment3")
}

# load data

red <- read.csv('winequality-red.csv', sep = ';')

# basic info
sum(complete.cases(red))
cor(red)
table(red$quality)

# split off test data for final confirmation
set.seed(259678)
valid_idx <- createDataPartition(y=red$quality, p=0.1, list = FALSE)
training <- red[-valid_idx,]
validation <- red[valid_idx,]
table(validation$quality)
table(training$quality)
