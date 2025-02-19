list.files(path = "../input")
pacman::p_load(dplyr,
               here,
               purrr,
               readxl,
               magrittr,
               ggplot2,
               gridExtra,
               forecast,
               rlang,
               quantmod,
               tsibble,
               lubridate,
               zoo,
               stats,#for acf/pcf
               gridExtra,
               lmSubsets,
               xtable,
               readr,
               timeSeries,
               TSstudio,
               tseries,
               hrbrthemes,
               tidyr,
               leaps,
               tidyverse,
               randomForest,
               rpart,
               caret,
               keras,
               tensorflow,
               hrbrthemes)
options(repr.plot.width = 14, repr.plot.height = 6)

library(dplyr)
DataMonthly <- read_xlsx(path = "/kaggle/input/casestudydataset/PredictorData2022.xlsx", sheet = "Monthly")
DataQuarterly <- read_xlsx(path = "/kaggle/input/casestudydataset/PredictorData2022.xlsx", sheet = "Quarterly")
DataAnnual <- read_xlsx(path = "/kaggle/input/casestudydataset/PredictorData2022.xlsx", sheet = "Annual")

new_colnames_dict <- c(
  "Time" = "yyyymm",
  "Time" = "yyyyq",
  "Time" = "yyyy",
  "SP500_Index" = "Index",
  "Dividends_12MonthSum" = "D12",
  "Earnings_12MonthSum" = "E12",
  "BookToMarket_Ratio" = "b/m",
  "TreasuryBill_Rate" = "tbl",
  "AAA_CorporateBond_Yield" = "AAA",
  "BAA_CorporateBond_Yield" = "BAA",
  "LongTermGovBond_Yield" = "lty",
  "NetEquityExpansion" = "ntis",
  "RiskFree_Rate" = "Rfree",
  "Inflation_CPI" = "infl",
  "LongTermRateOfReturn" = "ltr",
  "CorporateBond_Return" = "corpr",
  "StockVariance" = "svar",
  "CrossSectionalPremium" = "csp",
  "ConsumptionWealthIncomeRatio" = "cay", #check these variables
  "InvestmentCapitalRatio" = "ik",
  "Dividend3YearPriceRatio" = "D3",
  "Earnings3YearPriceRatio" = "E3",
  "ConsumptionWealthIncomeRatioMonthly" = "caym",
  "PercentageEquityIssuing" = "eqis"
)


filtered_dict_monthly <- new_colnames_dict[new_colnames_dict %in% colnames(DataMonthly)]
DataMonthly <- rename(DataMonthly, !!!filtered_dict_monthly)

filtered_dict_monthly <- new_colnames_dict[new_colnames_dict %in% colnames(DataQuarterly)]
DataQuarterly <- rename(DataQuarterly, !!!filtered_dict_monthly)

filtered_dict_monthly <- new_colnames_dict[new_colnames_dict %in% colnames(DataAnnual)]
DataAnnual <- rename(DataAnnual, !!!filtered_dict_monthly)


convert_to_numeric <- function(data) {
  for (i in names(data)) {
    if (class(data[[i]]) == "character") {
      data[[i]] <- as.numeric(data[[i]])
    }
  }
  return(data)
}

DataMonthly %<>% convert_to_numeric()
DataQuarterly %<>% convert_to_numeric()
DataAnnual %<>% convert_to_numeric()


DataMonthly$Time <- zoo::as.yearmon(as.character(DataMonthly$Time), format="%Y%m")
columns_to_remove <- c("CRSP_SPvw", "CRSP_SPvwx")
DataMonthly <- DataMonthly[, !names(DataMonthly) %in% columns_to_remove]


DataQuarterly$Time <- zoo::as.yearqtr(as.character(DataQuarterly$Time), format="%Y%q")
DataQuarterly <- DataQuarterly[, !names(DataQuarterly) %in% columns_to_remove]


DataAnnual$Time <- zoo::as.yearmon(paste0(as.character(DataAnnual$Time), "-01"), format="%Y-%m")
DataAnnual <- DataAnnual[, !names(DataAnnual) %in% columns_to_remove]


#Calculate Returns
calculate_returns <- function(input){
  input %<>% mutate(returns = as.vector(quantmod::Delt(SP500_Index))) 
  input %<>% mutate(ExcessReturn = returns - RiskFree_Rate)
  input %<>% dplyr::select(-returns, -RiskFree_Rate, -SP500_Index)
  return(input)
}

lag_predictors <- function(input){
  output <- input %>%
    mutate(across(-c(Time, ExcessReturn), lag, .names = "lag_{.col}")) %>% 
    dplyr::select(Time, ExcessReturn, starts_with("lag_")) %>% 
    slice(-1) 
  return(output)
}


DataMonthly %<>% calculate_returns()
DataQuarterly %<>% calculate_returns()
DataAnnual %<>% calculate_returns()


#Filtering Data to Handle NULL values and Dropping lag_CrossSectionalPremium
DataMonthly <- subset(DataMonthly, select = -c(CrossSectionalPremium))
DataQuarterly <- subset(DataQuarterly, select = -c(CrossSectionalPremium))
DataAnnual <- subset(DataAnnual, select = -c(CrossSectionalPremium))



DataMonthly %<>% lag_predictors()
DataQuarterly %<>% lag_predictors()
DataAnnual %<>% lag_predictors()



n_lags <- 10


# Generate lagged columns using base R
for (i in 1:n_lags) {
  DataMonthly[[paste0("lag_", i)]] <- c(rep(NA, i), head(DataMonthly$ExcessReturn, -i))
  DataQuarterly[[paste0("lag_", i)]] <- c(rep(NA, i), head(DataQuarterly$ExcessReturn, -i))
  DataAnnual[[paste0("lag_", i)]] <- c(rep(NA, i), head(DataAnnual$ExcessReturn, -i))    
}



# Remove rows with NA resulting from lag creation
DataMonthly <- DataMonthly[complete.cases(DataMonthly$lag_10), ]

DataQuarterly <- DataQuarterly[complete.cases(DataQuarterly$lag_10), ]

DataAnnual <- DataAnnual[complete.cases(DataAnnual$lag_10), ]


#Filtering Data After 1927
DataMonthly <- DataMonthly[DataMonthly$Time >= zoo::as.yearmon("1927-01", format = "%Y-%m"), ]
threshold_yearqtr <- as.yearqtr("1927 Q1", format = "%Y Q%q")
threshold_date <- as.Date(as.yearmon(threshold_yearqtr))
DataQuarterly <- DataQuarterly[as.Date(DataQuarterly$Time) >= threshold_date, ]
DataAnnual <- DataAnnual[DataAnnual$Time >= zoo::as.yearmon("1927-01", format = "%Y-%m"), ]

## Drop the value that not much important and almost or more than 50% NA
DataQuarterly %<>% select(-lag_ConsumptionWealthIncomeRatio, -lag_InvestmentCapitalRatio, -lag_Dividend3YearPriceRatio, -lag_Earnings3YearPriceRatio)
DataAnnual %<>% select(-lag_ConsumptionWealthIncomeRatio, -lag_InvestmentCapitalRatio)


n_lags <- 10
selected_columns <- c("Time","ExcessReturn",paste0("lag_", 1:n_lags))
Monthly_lagged_data <- DataMonthly[selected_columns]
Quarterly_lagged_data <- DataQuarterly[selected_columns]
Annual_lagged_data <- DataAnnual[selected_columns]

# Use 'Time' as an index
#rownames(Monthly_lagged_data) <- Monthly_lagged_data$Time
#rownames(DataMonthly) <- DataMonthly$Time


# Removes Time columns 
#Monthly_lagged_data <- Monthly_lagged_data %>% select(-Time)
#DataMonthly <- DataMonthly %>% select(-Time)  
tail(DataMonthly)


plot <- function(filtered_data,n) {
  ggplot(filtered_data, aes(x = Time, y = ExcessReturn)) +
    geom_line(aes(color = "In Sample"), size = 0.6) +
    geom_line(data = tail(filtered_data, n), aes(x = Time, y = ExcessReturn, color = "Out of Sample"), size = 1) +
    labs(x = "Time", y = "Excess Returns", color = "Data") +
    ggtitle("Line Plot of Excess Returns over Time") +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14),
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "top"  # Move the legend to the top
    ) +
    scale_color_manual(values = c("In Sample" = "blue", "Out of Sample" = "red"), name = "Data")
}


plot(Monthly_lagged_data,120)


v1 <- plot(Monthly_lagged_data,120)
v2 <- plot(Quarterly_lagged_data,40)
v3 <- plot(Annual_lagged_data,10)

# Combine plots using facets for grouping
grouped_plots1 <- list(v1, v2, v3)
facet_labels1 <- c("Line Plot of Excess Returns for Monthly Frequency over Time",
                   "Line Plot of Excess Returns for Quarterly Frequency over Time",
                   "Line Plot of Excess Returns for Annual Frequency over Time")

# Facet labels based on model and data type
grouped_plots1 <- Map(function(plot, label) {
  plot + labs(title = label) +
    theme(plot.title = element_text(color = "red"))  # Set facet label color to red
}, grouped_plots1, facet_labels1)

# Arrange plots in a grid layout
g <- grid.arrange(grobs = grouped_plots1, ncol = 2)
ggsave("ExcessPlot.png", plot = g, width = 24, height = 16, units = "in", dpi = 400)


TrainDataMonthly <- DataMonthly
TrainMonthly_lagged_data <- Monthly_lagged_data

TrainDataQuarterly <- DataQuarterly
TrainQuarterly_lagged_data <- Quarterly_lagged_data

TrainDataAnnual <- DataAnnual
TrainAnnual_lagged_data <- Annual_lagged_data

# Remove the last 120 rows from the copy
TrainDataMonthly <- slice(TrainDataMonthly, 1:(nrow(TrainDataMonthly) - 120))
TrainMonthly_lagged_data <- slice(TrainMonthly_lagged_data, 1:(nrow(TrainMonthly_lagged_data) - 120))

TrainDataQuarterly <- slice(TrainDataQuarterly, 1:(nrow(TrainDataQuarterly) - 40))
TrainQuarterly_lagged_data <- slice(TrainQuarterly_lagged_data, 1:(nrow(TrainQuarterly_lagged_data) - 40))

TrainDataAnnual <- slice(TrainDataAnnual, 1:(nrow(TrainDataAnnual) - 10))
TrainAnnual_lagged_data <- slice(TrainAnnual_lagged_data, 1:(nrow(TrainAnnual_lagged_data) - 10))


# Function to fit ARIMA models and calculate information criteria

fit_arima_and_info_criteria <- function(x, p) {
  arima_model <- arima(x, order = c(p, 0, 0))
  
  loglik <- sum(log(dnorm(x - arima_model$resid)))
  n <- length(x)
  
  aic <- AIC(arima_model)
  bic <- BIC(arima_model)
  
  info_df <- data.frame(p = p, AIC = aic, BIC = bic)
  return(info_df)
}


# Fit ARIMA models for different values of p
p_max <- 10  # Set the maximum order
info_criteria_list <- lapply(1:p_max, function(p) fit_arima_and_info_criteria(TrainDataAnnual$ExcessReturn, p))
# Combine the results into a data frame
info_criteria_df <- do.call(rbind, info_criteria_list)

# Create the plot
plot <- ggplot(info_criteria_df, aes(x = p)) +
  geom_line(aes(y = AIC, color = "AIC"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = BIC, color = "BIC"), linetype = "solid", size = 0.5) +
  labs(title = "Information Criteria for AR(p) Models with Monthly Data",
       y = "Information Criterion",
       x = "Order of AR Model (p)") +
  theme_minimal() +
  theme(legend.position = "top") +
  scale_x_continuous(breaks = 1:p_max) +  # To ensure integer x-axis labels
  scale_color_manual(values = c("AIC" = "red", "BIC" = "black")) +
  guides(color = guide_legend(title = "Information Criterion")) +
  theme(
    axis.text.x = element_text(size = 14, face = "bold"),  # Adjust X-axis label size and weight
    axis.text.y = element_text(size = 14, face = "bold"),  # Adjust Y-axis label size and weight
    axis.title = element_text(size = 14, face = "bold"), # Adjust axis title (x and y) size and weight
    legend.text = element_text(size = 12, face = "bold"),  # Adjust legend text size and weight
    legend.title = element_text(size = 14, face = "bold") 
  )
plot


AR_rolling_forecast <- function(myTimeSeries, window_size, order) {
  all_predictions <- numeric(0)
  
  for (i in seq(nrow(myTimeSeries) - window_size + 1, nrow(myTimeSeries), 1)) {
    start_window <- i - window_size + 1
    end_window <- i - 1
    window_data <- myTimeSeries[start_window:end_window, ]
    
    # Fit AR model
    ar_model <- Arima(window_data$ExcessReturn, order = order, include.mean = TRUE, transform.pars = TRUE,method = c("CSS-ML", "ML", "CSS"))
    
    next_window <- myTimeSeries[end_window + 1, ]
    
    # Make predictions for the next observation
    next_window_prediction <- forecast(ar_model, h = 1)$mean
    all_predictions <- c(all_predictions, next_window_prediction)
  }
  
  actual_values <- tail(myTimeSeries, window_size)
  residuals <- actual_values$ExcessReturn - all_predictions
  rmse <- sqrt(mean(residuals^2))
  
  time_index <- seq_along(actual_values)
  plot_data <- data.frame(Time = actual_values$Time, ExcessReturn = actual_values$ExcessReturn, Predicted = all_predictions, Residuals = residuals)  
  
  plot <- ggplot(plot_data, aes(x = Time)) +
    geom_line(aes(y = ExcessReturn), color = "grey", size = 1) +
    geom_line(aes(y = Predicted), color = "blue", linetype = "solid", size = 1) +
    labs(x = "Time", y = "Excess Returns") +
    ggtitle("Actual vs Predicted Excess Returns over Time") +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14),
      axis.text.x = element_text(angle = 45, hjust = 1)
    ) +
    scale_color_manual(values = c("blue", "red"))
  
  return(list(plot = plot, rmse = rmse, residuals = residuals))
}

RT_rolling_forecast <- function(myTimeSeries, window_size,complexity) {
  all_predictions <- numeric(0)
  
  # Start the loop from the 10th year from the end
  for (i in seq(nrow(myTimeSeries) - window_size + 1, nrow(myTimeSeries), 1)) {
    start_window <- i - window_size + 1
    end_window <- i - 1
    window_data <- myTimeSeries[start_window:end_window, ]
    
    # Fit an rpart (single decision tree) model
    rt_model <- rpart(ExcessReturn ~ ., data = window_data, control = rpart.control(cp = complexity))
    
    next_window <- myTimeSeries[end_window + 1, ]
    next_window_predictors <- next_window[, -which(names(next_window) == "ExcessReturn")]
    
    # Ensure the length of 'next_window_predictors' matches the number of predictors
    next_window_prediction <- predict(rt_model, next_window_predictors)
    all_predictions <- c(all_predictions, next_window_prediction)
  }
  
  # Assuming you have the actual ExcessReturn values for comparison
  actual_values <- tail(myTimeSeries, window_size)
  residuals <- actual_values$ExcessReturn - all_predictions
  rmse <- sqrt(mean(residuals^2))
  plot_data <- data.frame(Time = actual_values$Time, ExcessReturn = actual_values$ExcessReturn, Predicted = all_predictions)  
  
  # Plot
  plot <- ggplot(plot_data, aes(x = Time)) +
    geom_line(aes(y = ExcessReturn), color = "grey", size = 1) +
    geom_line(aes(y = Predicted), color = "blue", size = 1) +
    labs(x = "Time", y = "Excess Returns") +
    ggtitle("Actual vs Predicted Excess Returns over Time") +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14),
      axis.text.x = element_text(angle = 45, hjust = 1)
    ) +
    scale_color_manual(values = c("blue", "red"))
  
  return(list(plot = plot, rmse = rmse, residuals = residuals))
}

RF_rolling_forecast <- function(myTimeSeries, window_size, tree) {
  all_predictions <- numeric(0)
  
  # Start the loop from the 10th year from the end
  for (i in seq(nrow(myTimeSeries) - window_size + 1, nrow(myTimeSeries), 1)) {
    start_window <- i - window_size + 1
    end_window <- i - 1
    window_data <- myTimeSeries[start_window:end_window, ]
    
    rf_model <- randomForest(ExcessReturn ~ ., data = window_data, ntree= tree)
    
    next_window <- myTimeSeries[end_window + 1, ]
    next_window_predictors <- next_window[, -which(names(next_window) == "ExcessReturn")]
    
    # Ensure the length of 'next_window_predictors' matches the number of predictors
    next_window_prediction <- predict(rf_model, next_window_predictors)
    all_predictions <- c(all_predictions, next_window_prediction)
  }
  
  # Assuming you have the actual ExcessReturn values for comparison
  actual_values <- tail(myTimeSeries, window_size)
  residuals <- actual_values$ExcessReturn - all_predictions
  rmse <- sqrt(mean(residuals^2))
  time_index <- seq_along(actual_values)
  plot_data <- data.frame(Time = actual_values$Time, ExcessReturn = actual_values$ExcessReturn, Predicted = all_predictions)  
  
  # Plot
  plot <- ggplot(plot_data, aes(x = Time)) +
    geom_line(aes(y = ExcessReturn), color = "grey", size = 1) +
    geom_line(aes(y = Predicted), color = "blue", size = 1) +
    labs(x = "Time", y = "Excess Returns") +
    ggtitle("Actual vs Predicted Excess Returns over Time") +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14),
      axis.text.x = element_text(angle = 45, hjust = 1)
    ) +
    scale_color_manual(values = c("blue", "red"))
  
  return(list(plot = plot, rmse = rmse, residuals = residuals))
  
}


# Function to get the time span
get_time_span <- function(data_input, model_input){
  coefficients_names <- model_input %>% coef() %>% names() %>% setdiff("(Intercept)")
  complete_cases <- which(complete.cases(data_input[c("ExcessReturn", coefficients_names)]))
  used_rows <- row.names(model_input$model)
  removed_rows <- setdiff(complete_cases, as.numeric(used_rows))
  included_time_spans <- data_input$Time[as.numeric(used_rows)]
  output <- paste(included_time_spans %>% min() %>% year() %>% as.character(), "-", included_time_spans %>% max() %>% year() %>% as.character())
  return(output)
}

# Function for forward stepwise variable selection
stepwise_function <- function(input) {
  input_name <- deparse(substitute(input))
  
  model <- step(object = lm(ExcessReturn ~ 1, data = input %>% na.omit()), 
                scope = paste("ExcessReturn ~", 
                              paste(c(names(input %>% select(-ExcessReturn, -Time))), collapse = " + ")) %>% as.formula(),
                direction = "forward", trace = FALSE, k = 2) 
  
  # Extract relevant information from the lm object
  coefficients <- coef(model)
  residuals <- model$residuals
  r_squared <- summary(model)$r.squared
  
  output <- list(
    Data = get_time_span(data_input = input, model_input = model),
    MSFE = mean(residuals^2)^0.5,
    R2 = r_squared,
    Coefficients = coefficients,
    model = model
  )
  
  #print(summary(model))
  return(output)
}


# Function for one-step-ahead forecasting using forward stepwise variable selection

forward_stepwise_rolling_forecast <- function(myTimeSeries, window_size) {
  all_predictions <- numeric(0)
  
  for (i in seq(nrow(myTimeSeries) - window_size + 1, nrow(myTimeSeries), 1)) {
    start_window <- i - window_size + 1
    end_window <- i - 1
    window_data <- myTimeSeries[start_window:end_window, ]
    
    # Fit forward stepwise model
    stepwise_model <- stepwise_function(input = window_data)
    
    # Make predictions for the next observation
    predictors <- names(stepwise_model$model$coefficients)[-1]  # Exclude intercept
    formula_str <- as.formula(paste("ExcessReturn ~", paste(predictors, collapse = " + ")))
    next_window_prediction <- predict(stepwise_model$model, newdata = myTimeSeries[i, ], type = "response", formula = formula_str)
    all_predictions <- c(all_predictions, next_window_prediction)
  }
  
  actual_values <- tail(myTimeSeries, window_size)
  residuals <- actual_values$ExcessReturn - all_predictions
  rmse <- sqrt(mean(residuals^2))
  
  time_index <- seq_along(actual_values)
  plot_data <- data.frame(Time = actual_values$Time, ExcessReturn = actual_values$ExcessReturn, Predicted = all_predictions, Residuals = residuals)
  
  plot <- ggplot(plot_data, aes(x = Time)) +
    geom_line(aes(y = ExcessReturn), color = "grey", size = 1) +
    geom_line(aes(y = Predicted), color = "blue", size = 1) +
    labs(x = "Time", y = "Excess Returns") +
    ggtitle("Actual vs Predicted Excess Returns over Time") +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14),
      axis.text.x = element_text(angle = 45, hjust = 1)
    ) +
    scale_color_manual(values = c("blue", "red"))
  
  return(list(plot = plot, rmse = rmse, residuals = residuals))
}

column_names <- c("Auto-Regressive", "Regression Tree", "Random Forest", "Forward Stepwise")
row_names <- c("Monthly Lagged", "Monthly All", "Quarterly Lagged", "Quarterly All", "Annual Lagged", "Annual All")

MSFE <- data.frame(matrix(NA, nrow = length(row_names), ncol = length(column_names)))
colnames(MSFE) <- column_names
rownames(MSFE) <- row_names
MSFE

MSFE["Monthly Lagged", "Auto-Regressive"] <- arm$rmse
MSFE["Monthly All", "Auto-Regressive"] <- arm$rmse
MSFE["Quarterly Lagged", "Auto-Regressive"] <- arq$rmse
MSFE["Quarterly All", "Auto-Regressive"] <- arq$rmse
MSFE["Annual Lagged", "Auto-Regressive"] <- ara$rmse
MSFE["Annual All", "Auto-Regressive"] <- ara$rmse
MSFE
MSFE["Monthly Lagged", "Forward Stepwise"] <- fsmf$rmse
MSFE["Monthly All", "Forward Stepwise"] <- fsmf$rmse
MSFE["Quarterly Lagged", "Forward Stepwise"] <- fsqf$rmse
MSFE["Quarterly All", "Forward Stepwise"] <- fsqf$rmse
MSFE["Annual Lagged", "Forward Stepwise"] <- fsaf$rmse
MSFE["Annual All", "Forward Stepwise"] <- fsaf$rmse
MSFE

MSFE["Monthly Lagged", "Regression Tree"] <- Mregression_laggedOUT$rmse
MSFE["Monthly All", "Regression Tree"] <- Mregression_OUT$rmse
MSFE["Quarterly Lagged", "Regression Tree"] <- Qregression_laggedOUT$rmse
MSFE["Quarterly All", "Regression Tree"] <- Qregression_OUT$rmse
MSFE["Annual Lagged", "Regression Tree"] <- Aregression_laggedOUT$rmse
MSFE["Annual All", "Regression Tree"] <- Aregression_OUT$rmse
MSFE


rfmlOUT <- RF_rolling_forecast(Monthly_lagged_data,120,60)
rfmfOUT <- RF_rolling_forecast(DataMonthly,120,40)
rfqlOUT <- RF_rolling_forecast(Quarterly_lagged_data,40,24)
rfqfOUT <- RF_rolling_forecast(DataQuarterly,40,28)
rfalOUT <- RF_rolling_forecast(Annual_lagged_data,10,11)
rfafOUT <- RF_rolling_forecast(DataAnnual,10,11)

MSFE["Monthly Lagged", "Random Forest"] <- rfmlOUT$rmse
MSFE["Monthly All", "Random Forest"] <- rfmfOUT$rmse
MSFE["Quarterly Lagged", "Random Forest"] <- rfqlOUT$rmse
MSFE["Quarterly All", "Random Forest"] <- rfqfOUT$rmse
MSFE["Annual Lagged", "Random Forest"] <- rfalOUT$rmse
MSFE["Annual All", "Random Forest"] <- rfafOUT$rmse
MSFE



column_names <- c("Auto-Regressive", "Regression Tree", "Random Forest", "Forward Stepwise")
row_names <- c("Monthly Lagged", "Monthly All", "Quarterly Lagged", "Quarterly All", "Annual Lagged", "Annual All")

TT_MSFE <- data.frame(matrix(NA, nrow = length(row_names), ncol = length(column_names)))
colnames(TT_MSFE) <- column_names
rownames(TT_MSFE) <- row_names
TT_MSFE

# Function to perform AutoRegressive modeling
traditional_ar_model <- function(dataset, order, test_rows) {
  # Extract ExcessReturn column
  excess_return <- dataset$ExcessReturn
  
  # Calculate the index to split the dataset
  split_index <- nrow(dataset) - test_rows
  
  # Split the dataset into training and testing sets
  train_data <- head(dataset, split_index)
  test_data <- tail(dataset, test_rows)
  
  # Fit AR model
  ar_order <- ar(train_data$ExcessReturn, aic = FALSE, order.max = order)
  
  # Predict ExcessReturn on the test set
  predictions <- predict(ar_order, n.ahead = test_rows)$pred
  
  # Calculate RMSE
  rmse <- sqrt(mean((predictions - test_data$ExcessReturn)^2))
  
  # Return RMSE
  return(rmse)
}

# Example usage
# Assuming 'your_dataset' is your actual dataset
order_value <- 1  # You need to set the appropriate order
test_rows_value <- 120  # Set the number of rows for testing

# Call the function
result_rmse <- traditional_ar_model(DataMonthly, order_value, test_rows_value)
cat("RMSE:", result_rmse, "\n")


TT_MSFE["Monthly Lagged", "Auto-Regressive"] <-  traditional_ar_model(DataMonthly, 1, 120)
TT_MSFE["Monthly All", "Auto-Regressive"] <- traditional_ar_model(DataMonthly, 1, 120)
TT_MSFE["Quarterly Lagged", "Auto-Regressive"] <- traditional_ar_model(DataQuarterly, 4, 40)
TT_MSFE["Quarterly All", "Auto-Regressive"] <- traditional_ar_model(DataQuarterly, 4, 40)
TT_MSFE["Annual Lagged", "Auto-Regressive"] <- traditional_ar_model(DataAnnual, 2, 10)
TT_MSFE["Annual All", "Auto-Regressive"] <- traditional_ar_model(DataAnnual, 2, 10)
TT_MSFE


forward_stepwise_forecast <- function(data, target_column, test_rows) {
  
  excluded_columns <- "Time"
  
  # Extracting the target column
  target <- data[[target_column]]
  
  # Lagged predictors
  predictors <- dplyr::select(data, -{{target_column}}, -{{excluded_columns}})
  
  # Split the data into training and testing sets
  train_data <- head(data, -test_rows)
  test_data <- tail(data, test_rows)
  
  # Function to compute BIC
  compute_bic <- function(model) {
    n <- length(target)
    k <- length(coef(model))
    resid <- residuals(model)
    bic <- n * log(sum(resid^2) / n) + k * log(n)
    return(bic)
  }
  
  # Initialize an empty model and BIC
  selected_predictors <- c()
  selected_bic <- Inf
  
  # Perform forward stepwise selection
  for (predictor in names(predictors)) {
    
    # Add the predictor to the selected set
    selected_predictors <- c(selected_predictors, predictor)
    
    # Fit a linear model with the selected predictors
    model <- lm(paste(target_column, "~", paste(selected_predictors, collapse = " + ")), data = train_data)
    
    # Compute BIC
    bic <- compute_bic(model)
    
    # Update the selected predictors if the current model has lower BIC
    if (bic < selected_bic) {
      selected_bic <- bic
    } else {
      # If adding the predictor increases BIC, remove it
      selected_predictors <- setdiff(selected_predictors, predictor)
    }
  }
  
  # Final model with selected predictors
  final_model <- lm(paste(target_column, "~", paste(selected_predictors, collapse = " + ")), data = train_data)
  
  # Make final predictions on the test set
  final_predictions <- predict(final_model, newdata = test_data)
  
  # Print selected predictors
  #cat("Selected Predictors:", selected_predictors, "\n")
  
  # Print BIC of the final model
  #cat("BIC:", selected_bic, "\n")
  
  # Calculate RMSE on the test set
  rmse <- sqrt(mean((test_data[[target_column]] - final_predictions)^2))
  #cat("RMSE:", rmse, "\n")
  
  # Return the final model, predictions, selected predictors, BIC, and RMSE
  return(rmse) 
}

TT_MSFE["Monthly Lagged", "Forward Stepwise"] <-  forward_stepwise_forecast(Monthly_lagged_data ,"ExcessReturn", 120)
TT_MSFE["Monthly All", "Forward Stepwise"] <- forward_stepwise_forecast(DataMonthly, "ExcessReturn", 120)
TT_MSFE["Quarterly Lagged", "Forward Stepwise"] <- forward_stepwise_forecast(Quarterly_lagged_data ,"ExcessReturn", 40)
TT_MSFE["Quarterly All", "Forward Stepwise"] <- forward_stepwise_forecast(DataQuarterly, "ExcessReturn", 40)
TT_MSFE["Annual Lagged", "Forward Stepwise"] <- forward_stepwise_forecast(Annual_lagged_data , "ExcessReturn", 10)
TT_MSFE["Annual All", "Forward Stepwise"] <- forward_stepwise_forecast(DataAnnual, "ExcessReturn", 10)
TT_MSFE


library(tree)

# Function for regression tree modeling with train-test split
regression_tree_model <- function(data, complexity, test_rows) {
  
  # Split the data into training and testing sets
  train_data <- head(data, -test_rows)
  test_data <- tail(data, test_rows)
  
  # Build regression tree model using the training set
  rt_model <- rpart(ExcessReturn ~ ., data = train_data, control = rpart.control(cp = complexity))
  
  
  # Make predictions on the test set
  tree_predictions <- predict(rt_model, newdata = test_data)
  
  # Calculate RMSE on the test set
  rmse <- sqrt(mean((test_data[["ExcessReturn"]] - tree_predictions)^2))
  
  # Print RMSE value
  #cat("RMSE:", rmse, "\n")
  
  # Return the fitted tree model and predictions
  return( rmse)
}
TT_MSFE["Monthly Lagged", "Regression Tree"] <- regression_tree_model(Monthly_lagged_data, 0.006, test_rows = 120)
TT_MSFE["Monthly All", "Regression Tree"] <- regression_tree_model(DataMonthly,0.01, test_rows = 120)
TT_MSFE["Quarterly Lagged", "Regression Tree"] <-  regression_tree_model(Quarterly_lagged_data,0.01, test_rows = 40)
TT_MSFE["Quarterly All", "Regression Tree"] <-  regression_tree_model(DataQuarterly,0.01, test_rows = 40)
TT_MSFE["Annual Lagged", "Regression Tree"] <-  regression_tree_model(Annual_lagged_data,0.01, test_rows = 10)
TT_MSFE["Annual All", "Regression Tree"] <-  regression_tree_model(DataAnnual,0.01, test_rows = 10)
TT_MSFE

# Function for random forest modeling with train-test split
random_forest_model <- function(data, test_rows) {
  
  # Split the data into training and testing sets
  train_data <- head(data, -test_rows)
  test_data <- tail(data, test_rows)
  
  # Build random forest model using the training set
  rf_model <- randomForest(ExcessReturn ~ ., data = train_data)
  
  # Make predictions on the test set
  rf_predictions <- predict(rf_model, newdata = test_data)
  
  # Calculate RMSE on the test set
  rmse <- sqrt(mean((test_data[["ExcessReturn"]] - rf_predictions)^2))
  
  # Print RMSE value
  #cat("RMSE:", rmse, "\n")
  
  # Return the fitted random forest model and predictions
  return(rmse)
}

TT_MSFE["Monthly Lagged", "Random Forest"]  <- random_forest_model(Monthly_lagged_data, test_rows = 120)
TT_MSFE["Monthly All", "Random Forest"] <- random_forest_model(DataMonthly, test_rows = 120)
TT_MSFE["Quarterly Lagged", "Random Forest"] <- random_forest_model(Quarterly_lagged_data, test_rows = 40)
TT_MSFE["Quarterly All", "Random Forest"]  <- random_forest_model(DataQuarterly, test_rows = 40)
TT_MSFE["Annual Lagged", "Random Forest"]  <- random_forest_model(Annual_lagged_data, test_rows = 10)
TT_MSFE["Annual All", "Random Forest"] <- random_forest_model(DataAnnual, test_rows = 10)
TT_MSFE



# Combine data for dm test


library(lubridate)
library(tidyverse)
library(Matrix)
library(tibble)

# Assuming `stepy$residuals` and `arm$residuals` are your two sets of residuals
residuals_df <- data.frame(AR_m=ar_month$residuals, Step_m = stepm$residuals, RT_lagm=tree_mlag$residuals, RT_allm=tree_mall$residuals, RF_lagm=rf_mlag$residuals,  RF_allm=rf_mall$residuals)
residuals_df_quar <- data.frame(AR_quar=ar_quar$residuals, Step_quar = stepq$residuals, RT_lagquar=tree_qlag$residuals, RT_allquar=tree_qall$residuals, RF_lagquar=rf_qlag$residuals, RF_allquar=rf_qall$residuals)
residuals_df_ann <- data.frame(AR_ann=ar_ann$residuals, Step_ann = stepy$residuals, RT_lagann=tree_ylag$residuals, RT_allann=tree_yall$residuals,  RF_lagann=rf_ylag$residuals, RF_allann=rf_yall$residuals)



dm_test <- function(error_a, error_b, hmax = 1, power = 1) {
  # Calc loss of model a and b (L-power norm) across all hours
  loss_a <- apply(abs(as.matrix(error_a))^power, 1, sum)^(1 / power)
  loss_b <- apply(abs(as.matrix(error_b))^power, 1, sum)^(1 / power)
  
  # Calculate delta
  delta <- loss_a - loss_b
  sum(delta)
  # Estimate variance of delta
  delta_var <- var(delta) / length(delta)
  
  # Calc test statistic
  statistic <- mean(delta, na.rm = TRUE) / sqrt(delta_var)
  
  # Calc p-value
  delta_length <- length(delta)
  k <- ((delta_length + 1 - 2 * hmax +
           (hmax / delta_length) * (hmax - 1)) / delta_length)^(1 / 2)
  statistic <- statistic * k
  p_value <- pt(statistic, df = delta_length - 1)
  
  # Return results
  return(list(stat = statistic, p.val = p_value))
}

# Extract model names
model_names <- colnames(residuals_df)

# Define matrices to store results
dm_p_val <- matrix(NA, nrow = length(model_names), ncol = length(model_names))
dm_t_stat <- matrix(NA, nrow = length(model_names), ncol = length(model_names))

# Loop through all models
for (mod_a in seq_along(model_names)) {
  for (mod_b in seq_along(model_names)) {
    if (mod_a != mod_b) {  # Check if they are different models
      # Run the Diebold-Mariano test
      dm <- dm_test(residuals_df[, mod_a], residuals_df[, mod_b])
      
      # Store the results in matrices
      dm_p_val[mod_a, mod_b] <- dm$p.val
      dm_t_stat[mod_a, mod_b] <- dm$stat
    }
  }
}

# Convert matrices to data frames and add row/column names
dm_p_val_df <- as.data.frame(dm_p_val, row.names = model_names)
colnames(dm_p_val_df) <- model_names
dm_t_stat_df <- as.data.frame(dm_t_stat, row.names = model_names, col.names = model_names)
colnames(dm_t_stat_df) <- model_names


# Extract model names
model_names <- rownames(dm_p_val_df)

# Create a tibble for plotting
dm_results_tibble <- expand.grid(mod_a = model_names, mod_b = model_names)
dm_results_tibble$value <- unlist(dm_p_val_df)

# Display the tibble
print(dm_results_tibble)


# Create an empty matrix to store the p-values
dm_p_val_matrix <- matrix(NA, nrow = length(model_names), ncol = length(model_names))
rownames(dm_p_val_matrix) <- model_names
colnames(dm_p_val_matrix) <- model_names

# Fill in the matrix with p-values
for (mod_a in model_names) {
  for (mod_b in model_names) {
    if (mod_a != mod_b) {
      p_val <- dm_p_val_df[mod_a, mod_b]
      dm_p_val_matrix[mod_a, mod_b] <- p_val
    }
  }
}

# Display the matrix
print(dm_p_val_matrix)


dm_results_tibble %>%
  ggplot(aes(x = mod_b, y = reorder(mod_a, desc(mod_a)), fill = value)) +
  geom_raster() +
  scale_fill_gradientn(
    name = "P_value",
    colours = c("#007900", "red"),
    na.value = "grey15",
    guide = guide_colourbar(
      title.vjust = 0.93,
      barwidth = 1,
      barheight = 18,
    ),
    breaks = seq(from = 0, to = .1, by = 0.01),
    limits = c(0, .1),
    oob = scales::squish
  ) + 
  theme(
    plot.title = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text = element_text(size = 15),  # Adjust the size as needed
    legend.text = element_text(size = 15),  # Adjust the size as needed
    legend.title = element_text(size = 15, face = "bold")  # Adjust the size and style as needed
  )


### for quarterly

## Calculate
# Extract model names
model_names <- colnames(residuals_df_quar)

# Define matrices to store results
dm_p_val <- matrix(NA, nrow = length(model_names), ncol = length(model_names))
dm_t_stat <- matrix(NA, nrow = length(model_names), ncol = length(model_names))

# Loop through all models
for (mod_a in seq_along(model_names)) {
  for (mod_b in seq_along(model_names)) {
    if (mod_a != mod_b) {  # Check if they are different models
      # Run the Diebold-Mariano test
      dm <- dm_test(residuals_df_quar[, mod_a], residuals_df_quar[, mod_b])
      
      # Store the results in matrices
      dm_p_val[mod_a, mod_b] <- dm$p.val
      dm_t_stat[mod_a, mod_b] <- dm$stat
    }
  }
}

# Convert matrices to data frames and add row/column names
dm_p_val_df <- as.data.frame(dm_p_val, row.names = model_names)
colnames(dm_p_val_df) <- model_names
dm_t_stat_df <- as.data.frame(dm_t_stat, row.names = model_names, col.names = model_names)
colnames(dm_t_stat_df) <- model_names

##tible
library(tibble)

# Extract model names
model_names <- rownames(dm_p_val_df)

# Create a tibble for plotting
dm_results_tibble <- expand.grid(mod_a = model_names, mod_b = model_names)
dm_results_tibble$value <- unlist(dm_p_val_df)

# Display the tibble
print(dm_results_tibble)


### Matrix
# Create an empty matrix to store the p-values
dm_p_val_matrix <- matrix(NA, nrow = length(model_names), ncol = length(model_names))
rownames(dm_p_val_matrix) <- model_names
colnames(dm_p_val_matrix) <- model_names

# Fill in the matrix with p-values
for (mod_a in model_names) {
  for (mod_b in model_names) {
    if (mod_a != mod_b) {
      p_val <- dm_p_val_df[mod_a, mod_b]
      dm_p_val_matrix[mod_a, mod_b] <- p_val
    }
  }
}

# Display the matrix
print(dm_p_val_matrix)


### Plot
dm_results_tibble %>%
  ggplot(aes(x = mod_b, y = reorder(mod_a, desc(mod_a)), fill = value)) +
  geom_raster() +
  scale_fill_gradientn(
    name = "P_value",
    colours = c("#007900", "red"),
    na.value = "grey15",
    guide = guide_colourbar(
      title.vjust = 0.93,
      barwidth = 1,
      barheight = 18,
    ),
    breaks = seq(from = 0, to = .1, by = 0.01),
    limits = c(0, .1),
    oob = scales::squish
  ) + 
  theme(
    plot.title = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text = element_text(size = 15),  # Adjust the size as needed
    legend.text = element_text(size = 15),  # Adjust the size as needed
    legend.title = element_text(size = 15, face = "bold")  # Adjust the size and style as needed
  )


### For annual

## Calculate
# Extract model names
model_names <- colnames(residuals_df_ann)

# Define matrices to store results
dm_p_val <- matrix(NA, nrow = length(model_names), ncol = length(model_names))
dm_t_stat <- matrix(NA, nrow = length(model_names), ncol = length(model_names))

# Loop through all models
for (mod_a in seq_along(model_names)) {
  for (mod_b in seq_along(model_names)) {
    if (mod_a != mod_b) {  # Check if they are different models
      # Run the Diebold-Mariano test
      dm <- dm_test(residuals_df_ann[, mod_a], residuals_df_ann[, mod_b])
      
      # Store the results in matrices
      dm_p_val[mod_a, mod_b] <- dm$p.val
      dm_t_stat[mod_a, mod_b] <- dm$stat
    }
  }
}

# Convert matrices to data frames and add row/column names
dm_p_val_df <- as.data.frame(dm_p_val, row.names = model_names)
colnames(dm_p_val_df) <- model_names
dm_t_stat_df <- as.data.frame(dm_t_stat, row.names = model_names, col.names = model_names)
colnames(dm_t_stat_df) <- model_names

##tible
library(tibble)

# Extract model names
model_names <- rownames(dm_p_val_df)

# Create a tibble for plotting
dm_results_tibble <- expand.grid(mod_b = model_names, mod_a = model_names)
dm_results_tibble$value <- unlist(dm_p_val_df)

# Display the tibble
print(dm_results_tibble)


### Matrix
# Create an empty matrix to store the p-values
dm_p_val_matrix <- matrix(NA, nrow = length(model_names), ncol = length(model_names))
rownames(dm_p_val_matrix) <- model_names
colnames(dm_p_val_matrix) <- model_names

# Fill in the matrix with p-values
for (mod_a in model_names) {
  for (mod_b in model_names) {
    if (mod_a != mod_b) {
      p_val <- dm_p_val_df[mod_a, mod_b]
      dm_p_val_matrix[mod_a, mod_b] <- p_val
    }
  }
}

# Display the matrix
print(dm_p_val_matrix)


### Plot
dm_results_tibble %>%
  ggplot(aes(x = mod_a, y = reorder(mod_b, desc(mod_b)), fill = value)) +
  geom_raster() +
  scale_fill_gradientn(
    name = "P_value",
    colours = c("#007900", "red"),
    na.value = "grey15",
    guide = guide_colourbar(
      title.vjust = 0.93,
      barwidth = 1,
      barheight = 18,
    ),
    breaks = seq(from = 0, to = .1, by = 0.01),
    limits = c(0, .1),
    oob = scales::squish
  ) + 
  theme(
    plot.title = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text = element_text(size = 15),  # Adjust the size as needed
    legend.text = element_text(size = 15),  # Adjust the size as needed
    legend.title = element_text(size = 15, face = "bold")  # Adjust the size and style as needed
  )











