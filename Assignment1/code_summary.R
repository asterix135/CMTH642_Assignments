m_sales <- read.csv('rollingsales_manhattan.csv',
                    skip = 4, na.strings= c('', ' '), strip.white=TRUE,
                    stringsAsFactors = FALSE)

# Change sales price to number
m_sales$SALE.PRICE <- gsub('$', '', m_sales$SALE.PRICE, fixed=TRUE)
m_sales$SALE.PRICE <- as.numeric(gsub('\\,', '', m_sales$SALE.PRICE))
# Change Square Foot values to Numeric
m_sales$GROSS.SQUARE.FEET <- 
    as.numeric(gsub('\\,', '', m_sales$GROSS.SQUARE.FEET))
m_sales$LAND.SQUARE.FEET <- 
    as.numeric(gsub('\\,', '', m_sales$LAND.SQUARE.FEET))

# Change units to numeric class
m_sales$TOTAL.UNITS <- as.numeric(m_sales$TOTAL.UNITS)
m_sales$RESIDENTIAL.UNITS <- as.numeric(m_sales$RESIDENTIAL.UNITS)
m_sales$COMMERCIAL.UNITS <- as.numeric(m_sales$COMMERCIAL.UNITS)
# remove extra spaces in string fields
require(qdap)
m_sales$ADDRESS <- Trim(clean(m_sales$ADDRESS))
m_sales$NEIGHBORHOOD <- Trim(clean(m_sales$NEIGHBORHOOD))
m_sales$BUILDING.CLASS.CATEGORY <- Trim(clean(m_sales$BUILDING.CLASS.CATEGORY))
# convert date fields to date class
require(lubridate)
m_sales$SALE.DATE <- ymd(m_sales$SALE.DATE)

# Remove any record with SALE.PRICE <= $100,000
m_sales <- m_sales[m_sales$SALE.PRICE > 1e+5,]
nrow(m_sales)

table(as.factor(m_sales$TAX.CLASS.AT.PRESENT))

table(as.factor(m_sales$TAX.CLASS.AT.TIME.OF.SALE))

# Simplify Present tax class to uber-class
m_sales$TAX.CLASS.PRESENT.SIMPLE <- 
    substr(m_sales$TAX.CLASS.AT.PRESENT, 1, 1)

# delete BOROUGH, EASE.MENT, APART.MENT.NUMBER
m_sales <- subset(m_sales, select = 
                      -c(BOROUGH, EASE.MENT, APART.MENT.NUMBER))

# remove record with missing zip code
m_sales <- m_sales[m_sales$ZIP.CODE != 0, ]

# helper function to compute mode
mode <- function(vect) {
    unq_vect <- unique(vect)
    return(unq_vect[which.max(tabulate(match(vect, unq_vect)))])
}

# replace Neighbourhood NA values with mode for Zip Code
fix_nbhd <- is.na(m_sales$NEIGHBORHOOD)

for(i in 1:sum(fix_nbhd)) {
    zip <- m_sales[fix_nbhd,][i,'ZIP.CODE']
    compare_group <- m_sales[m_sales$ZIP.CODE == zip &
                                 !is.na(m_sales$NEIGHBORHOOD),
                             'NEIGHBORHOOD']
    new_neighborhood = mode(compare_group)
    m_sales[fix_nbhd,][i,'NEIGHBORHOOD'] = new_neighborhood
}

# replace YEAR.BUILT with modal value for address or overall df mean
fix_yr_blt <- m_sales$YEAR.BUILT == 0
mean_year <- as.integer(mean(m_sales[!fix_yr_blt, 
                                     'YEAR.BUILT'], na.rm = TRUE))
for(i in 1:sum(fix_yr_blt)) {
    address <- m_sales[fix_yr_blt,][i,'ADDRESS']
    compare_group <- m_sales[m_sales$ADDRESS == address & 
                                 m_sales$YEAR.BUILT != 0,
                             'YEAR.BUILT']
    if (length(compare_group) == 0) {
        new_year = mean_year
    } else {
        new_year = mode(compare_group)
    }
    m_sales[fix_yr_blt,][i,'YEAR.BUILT'] = new_year
}

# replace missing BUILDING.CLASS.AT.PRESENT
m_sales[is.na(m_sales$BUILDING.CLASS.AT.PRESENT), 
        'BUILDING.CLASS.AT.PRESENT'] <- 
    m_sales[is.na(m_sales$BUILDING.CLASS.AT.PRESENT), 
            'BUILDING.CLASS.AT.TIME.OF.SALE']

# replace mising TAX.CLASS.AT.PRESENT
m_sales[is.na(m_sales$TAX.CLASS.AT.PRESENT), 
        'TAX.CLASS.AT.PRESENT'] <- 
    m_sales[is.na(m_sales$TAX.CLASS.AT.PRESENT), 
            'TAX.CLASS.AT.TIME.OF.SALE']

# Update TAX.CLASS.PRESENT.SIMPLE
m_sales$TAX.CLASS.PRESENT.SIMPLE <- 
    as.integer(substr(m_sales$TAX.CLASS.AT.PRESENT, 1, 1))

m_sales <- subset(m_sales, select = -c(ADDRESS, LOT, BLOCK))

m_sales$TAX.CLASS.CHANGE <- m_sales$TAX.CLASS.PRESENT.SIMPLE ==
    m_sales$TAX.CLASS.AT.TIME.OF.SALE

m_sales$PROPERTY.CLASS.CHANGE <- m_sales$BUILDING.CLASS.AT.PRESENT ==
    m_sales$BUILDING.CLASS.AT.TIME.OF.SALE

m_sales$MONTH <- as.integer(format(m_sales$SALE.DATE, "%m"))

change_cols <- z <- c('ZIP.CODE', 'TAX.CLASS.AT.TIME.OF.SALE',
                      'TAX.CLASS.PRESENT.SIMPLE', 'MONTH')
for (i in change_cols) {
    m_sales[,i] <- as.factor(m_sales[,i])
}


# convert characters to factors
m_sales <- as.data.frame(unclass(m_sales))


# split data
uvr_data <- m_sales[m_sales$LAND.SQUARE.FEET>0,
                    c('SALE.PRICE', 'LAND.SQUARE.FEET')]
set.seed(6615)
train_crit <- sample(nrow(uvr_data), 
                     floor(nrow(uvr_data) * 0.8))
train_set <- uvr_data[train_crit,]
test_set <- uvr_data[-train_crit,]

# build lm model and fit to test panel
land_model <- lm(SALE.PRICE ~ LAND.SQUARE.FEET, 
                 data = train_set)
summary(land_model)

# Get confidence interval for Beta1
sum_coef <- summary(land_model)$coefficients
uvr_conf_int <- sum_coef[2,1] + c(-1,1) * 
    qt(0.975, df = land_model$df) * sum_coef[2,2]
uvr_conf_int


land_predict <- predict(land_model, interval='prediction', newdata = test_set)
errors <- land_predict[,'fit'] - test_set$SALE.PRICE

rmse_land <- sqrt(sum(errors^2/nrow(test_set)))
relative_change <- 1 - ((test_set$SALE.PRICE - abs(errors)) /
                            test_set$SALE.PRICE)
pred25_land <- sum(relative_change < 0.25)/nrow(test_set)
pred10_land <- sum(relative_change < 0.1)/nrow(test_set)

land_value_data <- data.frame(cbind(SALE.PRICE = m_sales$SALE.PRICE, 
                                    HAS.LAND.VALUE = m_sales$LAND.SQUARE.FEET!=0))
set.seed(6615)
train_crit <- sample(nrow(land_value_data), 
                     floor(nrow(land_value_data) * 0.8))
train_set2 <- land_value_data[train_crit,]
test_set2 <- land_value_data[-train_crit,]
has_land_model <- lm(SALE.PRICE ~ HAS.LAND.VALUE, data = train_set2)
has_land_predict <- predict(has_land_model, interval='prediction', 
                            newdata = test_set2)
errors2 <- has_land_predict[,'fit'] - test_set2$SALE.PRICE

rmse_has_land <- sqrt(sum(errors2^2/nrow(test_set2)))
relative_change <- 1 - ((test_set2$SALE.PRICE - abs(errors2)) /
                            test_set2$SALE.PRICE)
pred25_has_land <- sum(relative_change < 0.25)/nrow(test_set2)
pred10_has_land <- sum(relative_change < 0.1)/nrow(test_set2)

train_set3 <- m_sales[train_crit,]
test_set3 <- m_sales[-train_crit,]
land_w0_model <- lm(SALE.PRICE ~ LAND.SQUARE.FEET, data = train_set3)
land_w0_predict <- predict(land_w0_model, interval='prediction', 
                           newdata = test_set3)
errors3 <- land_w0_predict[,'fit'] - test_set3$SALE.PRICE

rmse_w0_land <- sqrt(sum(errors3^2/nrow(test_set3)))
relative_change <- 1 - ((test_set3$SALE.PRICE - abs(errors3)) /
                            test_set3$SALE.PRICE)
pred25_w0_land <- sum(relative_change < 0.25)/nrow(test_set3)
pred10_w0_land <- sum(relative_change < 0.1)/nrow(test_set3)

compare_frame <- data.frame(model_name = c('Land Area Model',
                                           'Has Land Area Model',
                                           'Land Model with Zeros'),
                            RMSE = c(rmse_land, 
                                     rmse_has_land,
                                     rmse_w0_land),
                            Pred25 = c(pred25_land, 
                                       pred25_has_land,
                                       pred25_w0_land),
                            Pred10 = c(pred10_land, 
                                       pred10_has_land,
                                       pred10_w0_land),
                            rSquare = c(summary(land_model)$r.squared,
                                        summary(has_land_model)$r.squared,
                                        summary(land_w0_model)$r.squared),
                            stringsAsFactors = FALSE)

compare_frame

m_sales$HAS.LAND.AREA <- m_sales$LAND.SQUARE.FEET != 0
m_sales$HAS.GROSS.AREA <- m_sales$GROSS.SQUARE.FEET != 0