smote_sample <- function(train_data, original, class_val, class_data, 
                         eval_fun, seed=FALSE) {
    increment_minimum <- 0.05
    lookup_ahead_value <- 3
    other_classes <- train_data[train_data[,ncol(train_data)] != class_val,]
    curr_class <- train_data[train_data[,ncol(train_data)] == class_val,]
    
    synth_class <- smote(curr_class, train_data, seed)
    
    # Evaluate if new model improves f-values by at least 5%
    new_f <- evaluate_model(eval_fun, rbind(train_data, synth_class), original)
    for (i in length(new_f)) {
        if (is.nan(new_f[i])) {
            new_f[i] <- 0
        }
    }
    
    print('new_f')
    print(new_f)
    
    if (mean(new_f[class_data['orig',] == -1]) < 
        (1 + increment_minimum) *
            mean(as.numeric(class_data['best_f',class_data['orig',] == -1]))) {
        if(class_data['lookup_ahead',class_val] < lookup_ahead_value) {
            class_data['lookup_ahead',class_val] <- 
                class_data['lookup_ahead', class_val] + 1
            return (list(rbind(train_data, synth_class), class_data))
        } else {
            return (list(train_data, class_data))
        }
    } else {
        return (list(rbind(train_data, synth_class), class_data))
    }
}
