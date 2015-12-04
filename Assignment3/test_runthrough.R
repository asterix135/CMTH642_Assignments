# make sure working directory is where the data is stored

# load all libraries required in analysis
require(caret)
require(FNN)

# load functions that are used later in analysis
source('ensemble_eval.R')
source('over_under_wrapper.R')

red <- read.csv('winequality-red.csv', sep = ';')
red$quality <- as.factor(red$quality)
set.seed(25678)
train_idx <- createDataPartition(y=red$quality, p=0.8, list = FALSE)
training <- red[train_idx,]
testing <- red[-train_idx,]

