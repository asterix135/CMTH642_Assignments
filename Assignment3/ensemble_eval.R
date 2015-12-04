# Function to use in final

getMode <- function(vect) {
    unq_vect <- unique(vect)
    return(unq_vect[which.max(tabulate(match(vect, unq_vect)))])
}

ensemble_eval <- function(train_set, test_set = NULL) {
    # Input - training set
    # Output - list of final predictions, intermediate models & normalizer
    # note - model is hard-coded to use quality as output variable

    evaluate_test <- function(model_list, test_set) {
        # 1. apply normalization model
        test_set <- predict(model_list[[7]], test_set)
        # 2-3 run base models and create interim result data frame
        ensemble_data <- data.frame(matrix(NA, nrow=nrow(test_set), ncol=0))
        for (i in 2:6) {
            new_pred <- predict(model_list[[i]], test_set)
            ensemble_data <- cbind(ensemble_data, new_pred)
        }
        colnames(ensemble_data) <- c('mn', 'nb', 'rf', 'gbm', 'lda')
        # 4. apply modal selection to interim data frame
        test_pred <- apply(ensemble_data, 1, function(x) getMode(x))
        ensemble_data <- cbind(final = test_pred, ensemble_data)
        
        return(ensemble_data)
    }    
    
    # Set parameters for modedl build
    fit_control <- trainControl(## 10-fold CV
        method = "repeatedcv",
        number = 10, 
        ## repeated ten times
        repeats = 10)
    
    # normalize data with returnable object
    norm_obj <- preProcess(train_set[,-12], method=c('center', 'scale'))
    train_set <- predict(norm_obj, train_set)
    
    set.seed(15954)
    mn_model <- train(quality ~ .,
                      data = train_set,
                      method = 'multinom',
                      trControl = fit_control,
                      trace = FALSE)
    mn_pred <- predict(mn_model, train_set)
    
    set.seed(15954)    
    nb_model <- train(quality ~ .,
                      data = train_set,
                      method = 'nb',
                      trControl = fit_control)
    nb_pred <- predict(nb_model, train_set)
    
    set.seed(15954)
    rf_model <- train(quality ~ .,
                      data = train_set,
                      method = 'rf',
                      trControl = fit_control)
    rf_pred <- predict(rf_model, train_set)
    
    set.seed(15954)
    gbm_model <- train(quality ~ .,
                       data = train_set,
                       method = 'gbm',
                       trControl = fit_control,
                       verbose=FALSE)
    gbm_pred <- predict(gbm_model, train_set)
    
    set.seed(15954)
    lda_model <- train(quality ~ .,
                       data = train_set,
                       method = 'lda',
                       trControl = fit_control)
    lda_pred <- predict(lda_model, train_set)
    
    ensemble_data <- data.frame(mn = mn_pred,
                                nb = nb_pred,
                                rf = rf_pred,
                                gbm = gbm_pred,
                                lda = lda_pred)
    
    final_pred <- apply(ensemble_data, 1, function(x) getMode(x))
    
    if (missing(test_set)) {
            return(cbind(final = final_pred, ensemble_data))
    } else {
        return(evaluate_test(list(final_model = final_pred,
                                  mn_model = mn_model,
                                  nb_model = nb_model,
                                  rf_model = rf_model,
                                  gbm_model = gbm_model,
                                  lda_model = lda_model,
                                  norm_obj = norm_obj),
                             test_set))
    }
}
