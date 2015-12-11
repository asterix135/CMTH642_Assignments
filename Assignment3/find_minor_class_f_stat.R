find_minor_f <- function(conf_mtx) {
    f_stat <- NULL
    for (i in c(1,2,5,6)) {
        newf <-  (2 * conf_mtx$byClass[i,1] * conf_mtx$byClass[i,3]) / 
            (conf_mtx$byClass[i,1] + conf_mtx$byClass[i,3])
        if (is.nan(newf)) {newf <- 0}
        f_stat<- c(f_stat, newf)
    }
    return(mean(f_stat))
}