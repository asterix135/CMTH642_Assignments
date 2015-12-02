# Wrapper function
# derived from Chawla NV, Cieslak DA, Hall LO, Joshi A. Automatically 
# countering imbalance and its empirical relationship to cost. Data Mining and 
# Knowledge Discovery. 2008;17(2):225-52.

require(caret)
require(FNN)


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
    
    # Get base f values & set them as best
    class_data <- rbind(class_data, 
                        base_f = evaluate_model(eval_fun, train_set))
    class_data <- rbind(class_data, best_f = class_data['base_f',])
    
    # Run undersample loop
    if (sum(class_data['maj',]) > 0) {
        new_list <- wrap_undersample(class_data, train_set, eval_fun, seed)
        train_set <- new_list[[1]]; class_data <- new_list[[2]]
    }
    # Run smote loop
    if (sum(class_data['min',]) > 0) {
        new_list <- wrap_smote(class_data, train_set, eval_fun, seed)
        train_set <- new_list[[1]]; class_data <- new_list[[2]];
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
                updated <- undersample(train_data, 
                                       colnames(class_data)[i],
                                       class_data,
                                       eval_fun,
                                       seed)
                train_data <- updated[[1]]
                class_data <- updated[[2]]
                if (nrow(train_data) == train_len) {
                    class_data['stop_sampling',i] <- TRUE
                }
            }
        }
    }
    
    return(list(train_data, class_data))
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
            ((class_data['best_f',i] - new_acc[i]) / 
             class_data['best_f',i] > increment_minimum)) {
            return(train_data)
        } else if (class_data['min', i] & 
                   (class_data['best_f', i] > new_acc[i])) {
            return(train_data)
        } else {
            class_data['best_f',] <- pmax(as.numeric(class_data['best_f',]), 
                                          new_acc) 
        }
    }
    return(list(new_train, class_data))
}


wrap_smote <- function(class_data, train_data, eval_fun, seed) {
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
                updated <- smote_sample(train_data, 
                                       colnames(class_data)[i],
                                       class_data,
                                       eval_fun,
                                       seed)
                train_data <- updated[[1]]
                class_data <- updated[[2]]
                if (nrow(train_data) == train_len) {
                    class_data['stop_sampling',i] <- TRUE
                }
            }
        }
    }
    return(list(train_data, class_data))
}


smote_sample <- function(train_data, class_val, class_data, eval_fun, seed) {
    sample_increment <- 100
    look_ahead_value <- 3
}


smote <- function(minor_class, train_data) {
    k <- 6
    if (seed) {set.seed(2292)}
    other_classes <- train_data[train_data[,ncol(train_data)] != class_val,]
    curr_class <- train_data[train_data[,ncol(train_data)] == class_val,]
    synth_class <- data.frame(matrix)
        
}


populate <- function(os_rate, class_rep, near_nbrs, k) {
    # os_rate is an integer representation of % oversample rate (1 = 100% etc)
    # class_rep is the class member being replicated
    ## near_nbrs is a data frame of the k nearest neighbours of class_rep
    # This implementation assumes all predictor variables are continuous

    new_samples <- data.frame(matrix(NA, nrow = 0, ncol = ncol(near_nbrs)))
    colnames(new_samples) <- colnames(near_nbrs)
    while (os_rate > 0) {
        nn <- sample(1:k, 1)
        synthetic <- rep(NA, length(class_rep))
        for (i in 1:(ncol(near_nbrs) - 1)) {
            dif <- near_nbrs[nn, i] - class_rep[i]
            gap <- runif(1, 0, 1)
            synthetic[i] <- class_rep + gap * dif
        }
        synthetic[i + 1] <- class_rep[i + 1]
        new_samples <- rbind(new_samples, synthetic)
        os_rate <- os_rate - 1
    }
    return(new_samples)
}


evaluate_model <- function(eval_fun, data_set) {
    # runs prediction model & returns vector of class f values
    # assumes prediction model returns a list with first element as predictors
    # can be simplified if just using a simple model
    cm <- confusionMatrix(eval_fun(data_set)[[1]], data_set[,ncol(data_set)])
    f_vals <- (2 * cm$byClass[,1] * cm$byClass[,3]) / 
        (cm$byClass[,1] + cm$byClass[,3])
    return(f_vals)
}