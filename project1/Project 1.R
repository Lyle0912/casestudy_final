


# Import data set ---------------------------------------------------------
#path_data <- here::here("PredictorData2022")
data_monthly <- read_excel("~/Desktop/case study/PredictorData2022.xlsx")

data_quarterly <- read_excel("~/Desktop/case study/PredictorData2022.xlsx", sheet = "Quarterly")
data_annual <- read_excel("~/Desktop/case study/PredictorData2022.xlsx", sheet = "Annual")

# Total data set
data_monthly %>% dim()
data_quarterly %>% dim()
data_annual %>% dim()

# Remove unneccesariy columns
data_monthly %<>% select(-CRSP_SPvw, -CRSP_SPvwx, -csp)
data_quarterly %<>% select(-CRSP_SPvw, -CRSP_SPvwx, -csp)
data_annual %<>% select(-CRSP_SPvw, -CRSP_SPvwx, -csp)

vars_monthly <- names(data_monthly)
vars_quarterly <-names(data_quarterly) #Contains additional varibales: cay, ik, D3, E3
vars_annual <-names(data_annual) #Contains additional varibales: caym eqis, ik

# check for variables, that are not in monthly
vars_monthly
setdiff(vars_quarterly,vars_monthly)
setdiff(vars_annual,vars_monthly)

# Rename columns ----------------------------------------------------------
new_colnames_dict <- c(
  "time" = "yyyymm",
  "time" = "yyyyq",
  "time" = "yyyy",
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
  "stock_var" = "svar",
  "cs_premium" = "csp",
  "consumWealIncome" = "cay",
  "investmentCapital" = "ik",
  "dividend3year" = "D3",
  "earning3year" = "E3",
  "consum_wealth_monthly" = "caym",
  "equity_issuing" = "eqis"
)



#Rename columns 
filtered_dict_monthly <- new_colnames_dict[new_colnames_dict %in% colnames(data_monthly)]
data_monthly <- rename(data_monthly, !!!filtered_dict_monthly)

filtered_dict_quarterly <- new_colnames_dict[new_colnames_dict %in% colnames(data_quarterly)]
data_quarterly <- rename(data_quarterly, !!!filtered_dict_quarterly)

filtered_dict_annual <- new_colnames_dict[new_colnames_dict %in% colnames(data_annual)]
data_annual <- rename(data_annual, !!!filtered_dict_annual)




# Data wrangling ----------------------------------------------------------

# Transform time variables
data_monthly$time <- zoo::as.yearmon(as.character(data_monthly$time), format="%Y%m")
data_quarterly$time <- zoo::as.yearqtr(as.character(data_quarterly$time), format="%Y%q")
data_annual$time <- zoo::as.yearmon(paste0(as.character(data_annual$time), "-01"), format="%Y-%m")


# Transform character-variables into numeric
for(i in names(data_monthly)) {
  if(class(data_monthly[[i]]) == "character") {
    data_monthly[[i]] <- as.numeric(data_monthly[[i]])
  }
}

for(i in names(data_quarterly)) {
  if(class(data_quarterly[[i]]) == "character") {
    data_quarterly[[i]] <- as.numeric(data_quarterly[[i]])
  }
}

for(i in names(data_annual)) {
  if(class(data_annual[[i]]) == "character") {
    data_annual[[i]] <- as.numeric(data_annual[[i]])
  }
}



# time span -------------------------------------------------------------
data_monthly$time %>% min()
data_quarterly$time %>% min()
data_annual$time %>% min()


data_monthly$time %>% max()
data_quarterly$time %>% max()
data_annual$time %>% max()

data_monthly$time %>% max() - data_monthly$time %>% min()
data_quarterly$time %>% max() - data_quarterly$time %>% min()
data_annual$time %>% max() - data_annual$time %>% min()


# Calculate returns and lag predictors ------------------------------------
data_monthly %<>% calculate_returns()
data_quarterly %<>% calculate_returns()
data_annual %<>% calculate_returns()

data_monthly %<>% lag_predictors()
data_quarterly %<>% lag_predictors()
data_annual %<>% lag_predictors()

# Check NA´s --------------------------------------------------------------

# percentage NA per variable
colMeans(is.na(data_monthly))
colMeans(is.na(data_quarterly))
colMeans(is.na(data_annual))


# Explore excess returns --------------------------------------------------
'Explore time series aspects, like serial correlation and variance'

# Mean and variance
data_monthly$excess_returns %>% mean(na.rm=TRUE)
data_monthly$excess_returns %>% var(na.rm=TRUE)
data_quarterly$excess_returns %>% mean(na.rm=TRUE)
data_quarterly$excess_returns %>% var(na.rm=TRUE)
data_annual$excess_returns %>% mean(na.rm=TRUE)
data_annual$excess_returns %>% var(na.rm=TRUE)



# AR-modelling ------------------------------------------------------------
'In a real world scenario we wouldnt have the whole data set, i.e. future values,
if we f.e. foreacast a value in the year 1900 We would basically encounter a 
phenomen called leakage, since we´d forecast based on a model, thats trained on 
data, which was not available at that point in time. In practice you would opt
for something like a rolling forecast study design, where the model gets reestimated
after new data becomes available.'


# Manually code information criteria --------------------------------------
ar_monthly_aic <- fit_auto_ar(data_monthly, p_max = 1, criterion="aic")
ar_monthly_bic <- fit_auto_ar(data_monthly, p_max = 1, criterion="bic")
ar_quarterly_aic <- fit_auto_ar(data_quarterly, p_max = 7, criterion="aic")
ar_quarterly_bic <- fit_auto_ar(data_quarterly, p_max = 7, criterion="bic")
ar_annual_aic <- fit_auto_ar(data_annual, p_max = 5, criterion="aic")
ar_annual_bic <- fit_auto_ar(data_annual, p_max = 5, criterion="bic")


metrics_aic_ar_monthly <- data_monthly %>% select(time, excess_returns ) %>% mutate(fitted = ar_monthly_aic %>% fitted() %>% as.numeric()) %>% calculate_rmse_r2()
metrics_bic_ar_monthly <- data_monthly %>% select(time, excess_returns ) %>% mutate(fitted = ar_monthly_bic %>% fitted() %>% as.numeric()) %>% calculate_rmse_r2()
metrics_aic_ar_quarterly <- data_quarterly %>% select(time, excess_returns ) %>% mutate(fitted = ar_quarterly_aic %>% fitted() %>% as.numeric()) %>% calculate_rmse_r2()
metrics_bic_ar_quarterly <- data_quarterly %>% select(time, excess_returns ) %>% mutate(fitted = ar_quarterly_bic %>% fitted() %>% as.numeric()) %>% calculate_rmse_r2()
metrics_aic_ar_annual <- data_annual %>% select(time, excess_returns ) %>% mutate(fitted = ar_annual_aic %>% fitted() %>% as.numeric()) %>% calculate_rmse_r2()
metrics_bic_ar_annual<- data_annual %>% select(time, excess_returns ) %>% mutate(fitted = ar_annual_bic %>% fitted() %>% as.numeric()) %>% calculate_rmse_r2()

results_AR <- rbind(
data.frame(Data1 = paste("1000-2000"),
           monthly_RMSE = metrics_aic_ar_monthly %>% select(rmse),
           monthly_R2 = metrics_aic_ar_monthly %>% select(r_squared),
           Data2 = paste("1000-2000"),
           quartelry_RMSE = metrics_aic_ar_quarterly %>% select(rmse),
           quartelry_R2 = metrics_aic_ar_quarterly %>% select(r_squared),
           Data3 = paste("1000-2000"),
           annual_RMSE = metrics_aic_ar_annual %>% select(rmse),
           annual_R2 = metrics_aic_ar_annual %>% select(r_squared)),

data.frame(Data1 = paste("1000-2000"),
           monthly_RMSE = metrics_bic_ar_monthly %>% select(rmse),
           monthly_R2 = metrics_bic_ar_monthly %>% select(r_squared),
           Data2 = paste("1000-2000"),
           quartelry_RMSE = metrics_bic_ar_quarterly %>% select(rmse),
           quartelry_R2 = metrics_bic_ar_quarterly %>% select(r_squared),
           Data3 = paste("1000-2000"),
           annual_RMSE = metrics_bic_ar_annual %>% select(rmse),
           annual_R2 = metrics_bic_ar_annual %>% select(r_squared))
) 
row.names(results_AR) <- c("AR(p) with AIC", "AR(p) with BIC")
colnames(results_AR) <- c("Data1","RMSE_monthly", "R2_monthly", "Data2", "RMSE_quarterly", "R2_quarterly", "Data3", "RMSE_annual", "R2_annual")

results_AR

# Task f) + g) -----------------------------------------------------------------
'Set up linear predictive models using in turn each predictor alone and estimate them using OLS.
Dont fortget to lag the predictor before setting up the mode. Compute the corresponding MSFE and
compare them to the ones obtained from the AR(p) model'

'linear model using all predictors
- explain difference with models, that use one predictor at time
- its included above'

# Vector of all regressors (without target and time)
regressors_all <- union(union(names(data_annual), names(data_monthly)), names(data_quarterly)) %>% setdiff(c("time", "excess_returns"))
regressors_monthly <- names(data_monthly) %>% setdiff(c("time", "excess_returns"))
regressors_quarterly <- names(data_quarterly) %>% setdiff(c("time", "excess_returns"))
regressors_annual <- names(data_annual) %>% setdiff(c("time", "excess_returns"))

# Length
regressors_all %>% length()
regressors_monthly %>% length()
regressors_quarterly %>% length()
regressors_annual %>% length()


# Get results and combine df ----------------------------------------------
results_lm_monthly <- linear_models_univariate(input=data_monthly)
results_lm_quarterly <-linear_models_univariate(input=data_quarterly)
results_lm_annual <-linear_models_univariate(input=data_annual)

colnames(results_lm_monthly) <- c("Data1","RMSE_monthly", "R2_monthly")
colnames(results_lm_quarterly) <- c("Data2","RMSE_quarterly", "R2_quarterly")
colnames(results_lm_annual) <- c("Data3","RMSE_annual","R2_annual")

merged_12 <- merge(results_lm_monthly, results_lm_quarterly, by="row.names", all=TRUE)
rownames(merged_12) <- merged_12$Row.names
merged_12 %<>% select(-Row.names) 
final_merged <- merge(merged_12, results_lm_annual, by="row.names", all=TRUE)
rownames(final_merged) <- final_merged$Row.names
final_merged %<>% select(-Row.names) 

final_merged <- final_merged[c(regressors_all %>% sort(),"all predictors"), ]




# Add results AR(p) model -------------------------------------------------
final_merged <- rbind(final_merged, results_AR)


# -------------------------------------------------------------------------
# Task h) -----------------------------------------------------------------
'Use information criterion to select the most primising predictors when fitting
a multiple linear regression
- forecast and compare RMSE
- use lmSubsets::lmselect?'


results_lm_ic_monthly <- linear_models_aic_bic(input=data_monthly)
results_lm_ic_quarterly <- linear_models_aic_bic(input=data_quarterly)
results_lm_ic_annual <- linear_models_aic_bic(input=data_annual)

colnames(results_lm_ic_monthly) <- c("Data1", "RMSE_monthly", "R2_monthly")
colnames(results_lm_ic_quarterly) <- c("Data2","RMSE_quarterly", "R2_quarterly")
colnames(results_lm_ic_annual) <- c("Data3","RMSE_annual", "R2_annual")

complete_lm_ic <- cbind(results_lm_ic_monthly, results_lm_ic_quarterly, results_lm_ic_annual)

final_merged <- rbind(final_merged, complete_lm_ic)

# Task i) -----------------------------------------------------------------
stepwise_monthly <- stepwise_function(input=data_monthly)
stepwise_quarterly <- stepwise_function(input=data_quarterly)
stepwise_annual <- stepwise_function(input=data_annual)
colnames(stepwise_monthly) <- c("Data1","RMSE_monthly", "R2_monthly")
colnames(stepwise_quarterly) <- c("Data2","RMSE_quarterly", "R2_quarterly")
colnames(stepwise_annual) <- c("Data3","RMSE_annual", "R2_annual")
stepwise_complete <- cbind(stepwise_monthly, stepwise_quarterly, stepwise_annual)
rownames(stepwise_complete) <- c("stepwise LM")

final_merged <- rbind(final_merged, stepwise_complete)



monthly <- final_merged[,1:3]  %>% na.omit()  %>% arrange(desc(RMSE_monthly))
quarterly <- final_merged[,4:6] %>% na.omit() %>% arrange(desc(RMSE_quarterly))
annual <- final_merged[,7:9] %>% na.omit() %>% arrange(desc(RMSE_annual))


# # Create Latex tables -----------------------------------------------------
# print(xtable(monthly, digits = 4), type = "latex")
# print(xtable(quarterly, digits = 4), type = "latex")
# print(xtable(annual, digits = 4), type = "latex")

# Statistics --------------------------------------------------------------
'Calculate R2 for null model manually, since R doenst give a result'
res1 <- data_annual %>% select(excess_returns) %>% c()
y_bar <- data_annual$excess_returns %>% mean(na.rm=TRUE)
RSS <-  sum(res1$excess_returns^2)
TSS <- sum((res1$excess_returns - y_bar)^2)
1 - (RSS/TSS)



# Coefficients ------------------------------------------------------------
linear_models_univariate(input=data_monthly)
linear_models_aic_bic(input=data_monthly)
stepwise_function(input=data_monthly)

linear_models_univariate(input=data_quarterly)
linear_models_aic_bic(input=data_quarterly)
stepwise_function(input=data_quarterly)

linear_models_univariate(input=data_annual)
linear_models_aic_bic(input=data_annual)
stepwise_function(input=data_annual)

