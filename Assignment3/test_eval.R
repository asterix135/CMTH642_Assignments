eval_fun <- function(train_set, test_set=NULL) {

    evaluate_test <- function(model_list, test_set) {
        # 1. apply normalization model
        test_set <- predict(model_list[[2]], test_set)
        # apply model
        test_pred <- predict(model_list[[1]], test_set)
        return(data.frame(final=test_pred))
    }
    
   fit_control <- trainControl(## 10-fold CV
        method = "repeatedcv",
        number = 10)
    
    norm_obj <- preProcess(train_set[,-12], method=c('center', 'scale'))
    train_set <- predict(norm_obj, train_set)
    
    mn_model <- train(quality ~ .,
                      data = train_set,
                      method = 'nb',
                      trControl = fit_control,
                      trace = FALSE)
    mn_pred <- predict(mn_model, train_set)
    
    if (missing(test_set)) {
        return(data.frame(final=mn_pred))
    } else {
        return(evaluate_test(list(mn_model,norm_obj), test_set))
    }
}


