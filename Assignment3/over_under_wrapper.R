# Wrapper function
# derived from Chawla NV, Cieslak DA, Hall LO, Joshi A. Automatically 
# countering imbalance and its empirical relationship to cost. *Data Mining and 
# Knowledge Discovery*. 2008;17(2):225-52.

require(caret)

wrapper <- function(class_list, train_set, eval_fun) {
    # class list is a named vector with -1=Minority, 0 = Neutral; 1 = Majority
    # train_set is a data frame with full training data
    #       Actual values need to be in last column
    # eval_fun is the ml routine
    num_class <- length(class_list)
    
    # class_data will manage information on classes
    class_data <- data.frame(rbind(maj = class_list == 1))
    colnames(class_data) <- names(class_list)
    class_data <- rbind(class_data, min = class_list == -1)
    
    # Set all MajorList undersample values to 100%
    class_data <- rbind(class_data, under = as.numeric(class_data[1,] == 1))
    
    # Set all Minority Class Smote values to 0%    
    class_data <- rbind(class_data, smote = rep(0, ncol(class_data)))
    
    # Get base accuracy values
    class_data <- rbind(class_data, 
                        base_val = evaluate_model(eval_fun, train_set))
    
    # Run undersample loop
    if (sum(class_data['maj',]) > 0) {
        new <- undersample(class_data, train_set, eval_fun)
        class_data <- new[[1]]; train_data <- new[[2]]
    }
    # Run smote loop
    if (sum(class_data['min',]) > 0) {
        smote(class_data, train_set, eval_fun)
        class_data <- new[[1]]; train_data <- new[[2]]
    }
    
    return(list(class_data, train_set))
}

undersample <- function(class_data, train_data, eval_fun) {
    # returns list
    # element 1: updated class_data
    # element 2: updated train_data
    sample_decrement = 0.1
    increment_minimum = 0.05
    
    return(list(class_data, train_data))
}

smote <- function(class_data, train_data, eval_fun) {
    # returns list
    # element 1: updated class_data
    # element 2: updated train_data
    return(list(class_data, train_data))
}

evaluate_model <- function(eval_fun, data_set) {
    # runs prediction model & returns vector of class specificity values
    # assumes prediction model returns a list with first element as predictors
    cm <- confusionMatrix(eval_fun(data_set)[[1]], data_set[,ncol(data_set)])
    return(cm$byClass[,1])
}