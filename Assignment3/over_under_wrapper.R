# Wrapper function
# derived from Chawla NV, Cieslak DA, Hall LO, Joshi A. Automatically 
# countering imbalance and its empirical relationship to cost. *Data Mining and 
# Knowledge Discovery*. 2008;17(2):225-52.


wrapper <- function(MinorList, MajorList, NumFolds) {
    # Set all MajorList undersample values to 100%
    UnderSampleList <- rep(1, times = length(MajorList))
    names(UnderSampleList) <- MajorList
    # Set all Minority Class Smote values to 0%
    SmoteList <- rep(0, times=length(MinorList))
    names(SmoteList) <- MinorList
    # 
}

undersample <- function(dunno) {
    pass
}

smote <- function(dunno) {
    pass
}