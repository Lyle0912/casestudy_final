rm(list = ls())

# Load all library

library(tidyverse)
library(lubridate)
library(readxl)
library(dplyr) 
library(ggplot2)
library(zoo)
library(here)
library(readxl)
library(dplyr)
library(magrittr)
library(ggplot2)
library(mice)
library(quantmod)
library(tsibble)
library(stats)
library(purrr)
library(rlang)
library(forecast)
library(lmSubsets)
library(caret)
library(rpart)
library(randomForest)
library(keras)
library(neuralnet)
library(DALEX)
library(tensorflow)

## Import dataset and calculate return

tsdata <- read_excel("~/Desktop/case study/PredictorData2022.xlsx")
tsdata$yyyymm <- as.yearmon(as.character(tsdata$yyyymm), format="%Y%m")
tsdata %<>% select(-CRSP_SPvw, -CRSP_SPvwx, -csp)
vars_monthly <- names(tsdata)

# Rename the column
colname_mapping <- c(
  "time" = "yyyymm",
  "sp_index" = "Index",
  "dividend_12" = "D12",
  "earning_12" = "E12",
  "bookmarketRatio" = "b/m",
  "treasury_bill" = "tbl",
  "cor_AAA" = "AAA",
  "cor_BAA" = "BAA",
  "longterm_yeild" = "lty",
  "net_equity" = "ntis",
  "risk_free" = "Rfree",
  "inflation" = "infl",
  "long_returnrate" = "ltr",
  "cbond_return" = "corpr",
  "stock_var" = "svar"
)

filtered_dict_monthly <- colname_mapping[colname_mapping %in% colnames(tsdata)]
tsdata <- rename(tsdata, !!!filtered_dict_monthly)

# Transform variables into numeric
for(i in names(tsdata)) {
  if(class(tsdata[[i]]) == "character") {
    tsdata[[i]] <- as.numeric(tsdata[[i]])
  }
}

## Generate the excess returns series
calculate_returns <- function(input){
  input %<>% mutate(returns = as.vector(quantmod::Delt(sp_index))) 
  input %<>% mutate(excess_returns = returns - risk_free)
  input %<>% select(-returns, -risk_free, -sp_index)
  return(input)
}
tsdata %<>% calculate_returns()

# Taking lag of all predictors

lag_predictors <- function(input){
  output <- input %>%
    mutate(across(-c(time, excess_returns), lag, .names = "lag_{.col}")) %>% 
    select(time, excess_returns, starts_with("lag_")) %>% 
    slice(-1) 
  return(output)
}

tsdata %<>% lag_predictors()

excessmonth <- tsdata%>% dplyr::select(c("time", "excess_returns"))
# Taking the lag
for (i in 1:10) {
  excessmonth <- mutate(excessmonth, !!paste("er_lag_", i, sep = "") := lag(excess_returns, i))
}

#plug back to tsdata

tsdata <- merge(tsdata, excessmonth, by = "time", all.x = TRUE) # Join excessmonth with tsdata for the all predictors model
tsdata <- tsdata[, -grep("excess_returns.y", colnames(tsdata))]


#get the ts from 1927
excessmonth <- excessmonth[excessmonth$time >= zoo::as.yearmon("1927-01", format = "%Y-%m"), ]
excessmonth_nn <- excessmonth # for neural network later
tsdata <- na.omit(tsdata)
tsdata_nn <- tsdata







####### Historical model

### Split train/test set 

train_size <- 0.8  
Ftrain_index <- round(nrow(excessmonth) * train_size)
train_data_er <- excessmonth[1:Ftrain_index, ]
test_data_er <- excessmonth[(Ftrain_index + 1):nrow(excessmonth), ]
max_lag <- 10
formula_er <- as.formula(paste("excess_returns ~", paste(paste("er_lag_", 1:max_lag, sep = ""), collapse = " + ")))



## Regression Tree

rt_model <- rpart(formula_er, data = train_data_er,control = rpart.control(cp = 0.01))

# Predictions
rt_predictions <- predict(rt_model, newdata = test_data_er)

# Add the predictions to excessmonth based on time
excessmonth$predicted_excess_returns_rt <- NA  
# Match predictions to corresponding rows based on time
for (i in 1:nrow(test_data_er)) {
  time_index <- which(excessmonth$time == test_data_er$time[i])
  if (length(time_index) > 0) {
    excessmonth[time_index, "predicted_excess_returns_rt"] <- rt_predictions[i]
    
  }
}
# Plot results
ggplot(excessmonth, aes(x = time)) +
  geom_line(aes(y = excess_returns, color = "Actual")) +
  geom_line(aes(y = predicted_excess_returns_rt, color = "Predicted")) +
  labs(title = "Actual vs. Predicted Excess Returns using Regression Tree",
       x = "Time",
       y = "Excess Returns") +
  theme_bw()+
  scale_color_manual(values = c("Actual" = "black", "Predicted" = "red"))

# Calculate MSFE

rt_actual_values <- test_data_er$excess_returns
rt_rmse <- sqrt(mean((rt_predictions - rt_actual_values)^2))

cat("Mean Squared Forecast Error (MSFE):", rt_rmse, "\n")


# Calculate and plot important using DALEX
train_data_er_subset <- train_data_er[, !names(train_data_er) %in% c("excess_returns", "time")]
explainer <- explain(model = rt_model,
                     data = train_data_er_subset,
                     y = train_data_er$excess_returns,
                     label = "Regression Tree Model")

# Calculate permutation feature importance
perm_importance <- model_parts(explainer)
plot(perm_importance)




#### Random Forest

rf_model <- randomForest(
  formula_er, 
  data = train_data_er,
  ntree = 209,
  mtry = 3,
  importance = TRUE 
)


rf_predictions <- predict(rf_model, newdata = test_data_er)
rf_actual_values <- test_data_er$excess_returns
rf_msfe <- sqrt(mean((rf_predictions - rf_actual_values)^2))
cat("Mean Squared Forecast Error (MSFE):", rf_msfe, "\n")
excessmonth$predicted_excess_returns_rf <- NA

# Match predictions to corresponding rows based on time
for (i in 1:nrow(test_data_er)) {
  time_index <- which(excessmonth$time == test_data_er$time[i])
  if (length(time_index) > 0) {
    excessmonth[time_index, "predicted_excess_returns_rf"] <- rf_predictions[i]
  }
}
# Plot results
ggplot(excessmonth, aes(x = time)) +
  geom_line(aes(y = excess_returns, color = "Actual")) +
  geom_line(aes(y = predicted_excess_returns_rf, color = "Predicted"), linetype = "dashed") +
  labs(title = "Actual vs. Predicted Excess Returns using Random Forest",
       x = "Time",
       y = "Excess Returns") +
  theme_bw()+
  scale_color_manual(values = c("Actual" = "black", "Predicted" = "red"))


explainer2 <- explain(model = rf_model,
                      data = train_data_er_subset,
                      y = train_data_er$excess_returns,
                      label = "Random Forest Model")

# Calculate permutation feature importance
perm_importance2 <- variable_importance(explainer2)
print(perm_importance2)
plot(perm_importance2)





## Neural Network


# Use 'time' as an index
rownames(excessmonth_nn) <- excessmonth_nn$time

# Removes time columns 
excessnotime <- excessmonth_nn %>% select(-time)

# split again with no time involve

train_size <- 0.8  

Ftrain_index <- round(nrow(excessnotime) * train_size)
train_data_ernn <- excessnotime[1:Ftrain_index, ]
test_data_ernn <- excessnotime[(Ftrain_index + 1):nrow(excessnotime), ]

# Data Partition
ann_lagged_index <- createDataPartition(train_data_ernn$excess_returns, p = 0.8, list = FALSE)

# Split the data into train and validation sets
ann_train_dataL <- train_data_ernn[ann_lagged_index, ]
ann_validation_dataL <- train_data_ernn[-ann_lagged_index, ]
# Extract predictors and response variable for the train and validation sets
x1_train <- as.matrix(ann_train_dataL[, -which(names(ann_train_dataL) == "excess_returns")])
y1_train <- ann_train_dataL$excess_returns

x1_validation <- as.matrix(ann_validation_dataL[, -which(names(ann_validation_dataL) == "excess_returns")])
y1_validation <- ann_validation_dataL$excess_returns

# Extract predictors and response variable for the test set
x1_test <- as.matrix(test_data_ernn[, -which(names(test_data_ernn) == "excess_returns")])
y1_test <- test_data_ernn$excess_returns

# Define the model
model <- keras_model_sequential(name="sequential_10") %>%
  layer_dense(units = 96, activation = 'elu', input_shape = c(10), kernel_initializer = initializer_glorot_uniform(seed = NULL), bias_initializer = initializer_zeros(), name = 'dense_20') %>%
  layer_dropout(rate = 0.2, name = 'dropout_10') %>%
  layer_dense(units = 1, activation = 'linear', kernel_initializer = initializer_glorot_uniform(seed = NULL), bias_initializer = initializer_zeros(), name = 'dense_21')

# Compile the model
model %>% compile(
  loss = 'mean_squared_error',
  optimizer = optimizer_adam(),
  metrics = list('mean_squared_error')
)

# Fit the model on the training data
history <- model %>% fit(
  x1_train, y1_train,
  epochs = 11,
  batch_size = 32,
  validation_data = list(x1_validation, y1_validation)  
)

# Predictions on the test set
y1_test_pred <- model %>% predict(x1_test)

# Calculate MSfE for the test set predictions
msfe_test <- sqrt(mean((y1_test - y1_test_pred)^2))
msfe_test

# Plotting
plot(excessmonth_nn$time, excessmonth_nn$excess_returns, type = "l", col = "black", xlab = "Time", ylab = "Excess Returns", main = "Actual vs. Predicted Excess Returns using Neural Networks")
lines(excessmonth_nn$time[(Ftrain_index + 1):nrow(excessmonth_nn)], y1_test_pred, col = "red")

legend("topright", legend = c("Actual", "Predicted"), col = c("black", "red"), lty = 1)

# Adding predictions back to excessmonth_nn DataFrame
excessmonth_nn$predicted_returns <- NA
excessmonth_nn$predicted_returns[(Ftrain_index + 1):nrow(excessmonth_nn)] <- y1_test_pred

explainer <- explain(model = model,
                     data = x1_train,
                     y = y1_train,
                     label = "NN_lagged")
# Calculate permutation feature importance
perm_importance <- model_parts(explainer)

print(perm_importance)
plot(perm_importance)



## General model


# Split train test

Ltrain_index <- round(nrow(tsdata) * train_size)
train_data_ts <- tsdata[1:Ltrain_index, ]
test_data_ts <- tsdata[(Ltrain_index + 1):nrow(tsdata), ]

target_variable <- "excess_returns.x"

predictor_variables <- setdiff(names(tsdata), c(target_variable, "time"))

formula_all <- as.formula(paste(target_variable, "~", paste(paste(predictor_variables, collapse = " + "), collapse = " + "))) 


rt_model_all <- rpart(formula_all, data = train_data_ts, control = rpart.control(cp = 0.015))

# Predictions using the general data regression tree model
rt_predictions_all <- predict(rt_model_all, newdata = test_data_ts)


tsdata$predicted_excess_returns_rt <- NA

# Match predictions to corresponding rows based on time
for (i in 1:nrow(test_data_ts)) {
  time_index <- which(tsdata$time == test_data_ts$time[i])
  if (length(time_index) > 0) {
    tsdata[time_index, "predicted_excess_returns_rt"] <- rt_predictions_all[i]
  }
}
ggplot(tsdata, aes(x = time)) +
  geom_line(aes(y = excess_returns.x, color = "Actual")) +
  geom_line(aes(y = predicted_excess_returns_rt, color = "Predicted")) +
  labs(title = "Actual vs. Predicted Excess Returns using Regression Tree all",
       x = "Time",
       y = "Excess Returns") +
  theme_bw()+
  scale_color_manual(values = c("Actual" = "black", "Predicted" = "red"))



# calculate MSFE

rt_actual_values_all <- test_data_ts$excess_returns.x
rt_msfe_all <- sqrt(mean((rt_predictions_all - rt_actual_values_all)^2))

cat("Mean Squared Forecast Error (MSFE) for Regression Tree:", rt_msfe_all, "\n")

# Calculate feature importance
train_data_ts_subset <- train_data_ts[, !names(train_data_ts) %in% c("excess_returns.x")]

# Create an explainer object
explainer <- explain(model = rt_model_all,
                     data = train_data_ts_subset,
                     y = train_data_ts$excess_returns.x,
                     label = "Regression Tree Model")

# Calculate permutation feature importance
perm_importance <- model_parts(explainer)
print(perm_importance)
plot(perm_importance)

## Random Forest


# Building the Random Forest model for general data
rf_model_all <- randomForest(
  formula_all,
  data = train_data_ts,
  ntree = 209,
  mtry = 3,
  importance = TRUE  
)

predictions_rf_all <- predict(rf_model_all, newdata = test_data_ts)
# Calculating the RMSE (Root Mean Squared Error) for general data using general random forest model

rf_actual_values_all <- test_data_ts$excess_returns.x
rf_rmse_all <- sqrt(mean((predictions_rf_all - rf_actual_values_all)^2))
cat("Mean Squared Forecast Error (MSFE) for Random Forest:", rf_rmse_all, "\n")

tsdata$predicted_excess_returns_rf <- NA

# Match predictions to corresponding rows based on time
for (i in 1:nrow(test_data_ts)) {
  time_index <- which(tsdata$time == test_data_ts$time[i])
  if (length(time_index) > 0) {
    tsdata[time_index, "predicted_excess_returns_rf"] <- predictions_rf_all[i]
  }
}

ggplot(tsdata, aes(x = time)) +
  geom_line(aes(y = excess_returns.x, color = "Actual")) +
  geom_line(aes(y = predicted_excess_returns_rf, color = "Predicted"), linetype = "dashed") +
  labs(title = "Actual vs. Predicted Excess Returns using Random Forest all",
       x = "Time",
       y = "Excess Returns") +
  theme_bw()+
  scale_color_manual(values = c("Actual" = "black", "Predicted" = "red"))

train_data_ts_subset <- train_data_ts[, !names(train_data_ts) %in% c("excess_returns.x")]
# Create an explainer object
explainer <- explain(model = rf_model_all,
                     data = train_data_ts_subset,
                     y = train_data_ts$excess_returns.x,
                     label = "Random Forest Model")

# Calculate permutation feature importance
perm_importance <- model_parts(explainer)

print(perm_importance)
plot(perm_importance)


## Neural Networks
# Removes time columns 

tsdatanotime <- tsdata_nn %>% select(-time)

train_size <- 0.8 

Ltrain_index <- round(nrow(tsdatanotime) * train_size)
train_data_tsnn <- tsdatanotime[1:Ltrain_index, ]
test_data_tsnn <- tsdatanotime[(Ltrain_index + 1):nrow(tsdatanotime), ]

# Data Partition
ann_general_index <- createDataPartition(train_data_tsnn$excess_returns.x, p = 0.8, list = FALSE)

# Split the data into train and validation sets
ann_train_dataL <- train_data_tsnn[ann_general_index, ]
ann_validation_dataL <- train_data_tsnn[-ann_general_index, ]
# Extract predictors and response variable for the train and validation sets
x2_train <- as.matrix(ann_train_dataL[, -which(names(ann_train_dataL) == "excess_returns.x")])
y2_train <- ann_train_dataL$excess_returns.x

x2_validation <- as.matrix(ann_validation_dataL[, -which(names(ann_validation_dataL) == "excess_returns.x")])
y2_validation <- ann_validation_dataL$excess_returns.x

# Extract predictors and response variable for the test set
x2_test <- as.matrix(test_data_tsnn[, -which(names(test_data_tsnn) == "excess_returns.x")])
y2_test <- test_data_tsnn$excess_returns.x

# Define the model
full_model <-model <- keras_model_sequential() %>%
  layer_dense(units = 400, activation = 'elu', input_shape = c(22),kernel_initializer = initializer_glorot_uniform(seed = NULL)) %>%
  layer_dropout(rate = 0.3) %>%
  layer_dense(units = 20, activation = 'elu',kernel_initializer = initializer_glorot_uniform(seed = NULL)) %>%
  layer_dropout(rate = 0.25) %>%
  layer_dense(units = 1190, activation = 'elu',kernel_initializer = initializer_glorot_uniform(seed = NULL)) %>%
  layer_dense(units = 1, activation = 'linear')

# Compile the model
full_model %>% compile(
  loss = 'mean_squared_error',
  optimizer = optimizer_adam(learning_rate = 0.001),
  metrics = list('mean_squared_error')
)


# Fit the model
full_history <- full_model %>% fit(
  x2_train, y2_train,
  epochs = 35,
  batch_size = 25,
  validation_data = list(x2_validation, y2_validation)
)

# Predictions on the test set
y2_test_pred <- full_model %>% predict(x2_test)

# Calculate RMSE for the test set predictions
rmse_test_general <- sqrt(mean((y2_test - y2_test_pred)^2))
rmse_test_general

explainer_full <- explain(model = full_model,
                          data = x2_train,
                          y = y2_train,
                          label = "NN_Full")

# Calculate variable importance
vi_nn_full <- model_parts(explainer_full)

# Print variable importance
#higher the better
print(vi_nn_full)
plot(vi_nn_full)
















