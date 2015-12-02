# Wrapper function
# derived from Chawla NV, Cieslak DA, Hall LO, Joshi A. Automatically 
# countering imbalance and its empirical relationship to cost. *Data Mining and 
# Knowledge Discovery*. 2008;17(2):225-52.

require(caret)


# need to change mertric to f-measure
# (2 x precision x recall) / (recall + precision)

wrapper <- function(class_list, train_set, eval_fun, seed) {
    # class list is a named vector with -1=Minority, 0 = Neutral; 1 = Majority
    # train_set is a data frame with full training data
    #       Actual values need to be in last column
    # eval_fun is the ml routine
    num_class <- length(class_list)
    
    # class_data will manage information on classes
    class_data <- data.frame(rbind(orig = class_list))
    colnames(class_data) <- names(class_list)
    
    class_data <- rbind(class_data, maj = class_list == 1)
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
        train_data <- wrap_undersample(class_data, train_set, eval_fun, seed)
    }
    # Run smote loop
    if (sum(class_data['min',]) > 0) {
        wrap_smote(class_data, train_set, eval_fun, seed)
        class_data <- new[[1]]; train_data <- new[[2]]
    }
    
    return(list(class_data, train_set))
}

wrap_undersample <- function(class_data, train_data, eval_fun, seed) {
    # returns list
    # element 1: updated class_data
    # element 2: updated train_data
    
    # Set StopSampling Flag to False
    class_data <- rbind(class_data, stop_sampling = !class_data['maj',])
    # main loop
    while (sum(class_data['stop_sampling'] < ncol(class_data))) {
        for (i in 1:ncol(class_data)) {
            if (class_data['stop_sampling',i] == FALSE) {
                train_len = nrow(train_data)
                train_data <- undersample(train_data, 
                                          colnames(class_data)[i],
                                          class_data,
                                          eval_fun,
                                          seed)
                if (nrow(train_data) == train_len) {
                    class_data['stop_sampling',i] <- TRUE
                }
            }
            
        }
    }
    
    return(train_data)
}


undersample <- function(train_data, class_val, class_data, eval_fun, seed) {
    sample_decrement <- 0.1
    increment_minimum <- 0.05
    if (seed) {set.seed(2292)}
    other_classes <- train_data[train_data[,ncol(train_data)] != class_val,]
    curr_class <- train_data[train_data[,ncol(train_data)] == class_val,]
    curr_class <- curr_class[sample(nrow(curr_class), 
                                    floor(nrow(curr_class) * 
                                              (1-sample_derement))),]
    new_train <- rbind(other_classes, curr_class)
    new_acc <- evaluate_model(eval_fun, new_train)
    for (i in ncol(class_val)) {
        if (class_data['orig', i] != -1 & 
            ((class_data['base_val',i] - new_acc[i]) / 
             class_data['base_val',i] > increment_minimum)) {
            return(train_data)
        } else if (class_data['min', i] & 
                   (class_data['base_val', i] > new_acc[i])) {
            return(train_data)
        }
    }
    return(new_train)
}


wrap_smote <- function(class_data, train_data, eval_fun) {
    # returns new training data set
    # element 1: updated class_data
    # element 2: updated train_data

    # Set StopSampling Flag to False
    class_data <- rbind(class_data, stop_sampling = !class_data['min',])
    
    # main loop
    while (sum(class_data['stop_sampling'] < ncol(class_data))) {
        for (i in 1:ncol(class_data)) {
            if (class_data['stop_sampling',i] == FALSE) {
                train_len = nrow(train_data)
                train_data <- smote(train_data, 
                                          colnames(class_data)[i],
                                          class_data,
                                          eval_fun,
                                          seed)
                if (nrow(train_data) == train_len) {
                    class_data['stop_sampling',i] <- TRUE
                }
            }
        }
    }
    return(train_data)
}


smote <- function(foo) {
    sample_increment <- 100
    look_ahead_value <- 3
}


evaluate_model <- function(eval_fun, data_set) {
    # runs prediction model & returns vector of class specificity values
    # assumes prediction model returns a list with first element as predictors
    cm <- confusionMatrix(eval_fun(data_set)[[1]], data_set[,ncol(data_set)])
    return(cm$byClass[,1])
}