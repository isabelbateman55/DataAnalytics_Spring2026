#Term Project Data ANalytics

rm(list=ls())
library(readr)
library("ggplot2")
library(psych)
library(cluster)
library(dendextend)
library(colorspace)
library(factoextra)
library(rpart)
library(rpart.plot)
library(class)
library(dplyr)
library(lubridate)
library(randomForest)

#d <- read_csv("~/Spring 2026/Data Analytics/Term Project/Cleaned Equity Data.csv")
d1 <- read_csv("~/Spring 2026/Data Analytics/Term Project/sample.csv")
dataset <- d1
#View(dataset)

#Random Sampling of Equity Dataset
#unique_tickers <- unique(dataset$Company)
#set.seed(123)
#sample_tickers <- sample(unique_tickers,40)
#df_sample <- dataset[dataset$Company %in% sample_tickers, ]
#write.csv(df_sample, "ticker_sample.csv", row.names = FALSE)


#Converting Sample from Daily to Monthly Data
# Ensure Date column is formatted correctly
#df_sample$Date <- as.Date(df_sample$Date)

# Create month variable
#df_sample$Month <- floor_date(df_sample$Date, "month")

# Aggregate daily data into monthly observations
#monthly_data <- df_sample %>%
 # group_by(Company, Month) %>%
  #summarise(
   # Monthly_Open = first(Open),
    #Monthly_High = max(High, na.rm = TRUE),
    #Monthly_Low = min(Low, na.rm = TRUE),
    #Monthly_Close = last(Close),
    #Total_Volume = sum(Volume, na.rm = TRUE),
    #Total_Dividends = sum(Dividends, na.rm = TRUE),
    #Stock_Splits = sum(`Stock Splits`, na.rm = TRUE),
    #.groups = "drop"
  #)

# Export monthly dataset
#write.csv(monthly_data, "monthly_equity_sample.csv", row.names = FALSE)



#MODEL FORMATION
#Model 1: Linear Regression to predict Monthly Returns from Lagged Returns, Volatility, Volume, and Range
equity_data <- dataset
names(equity_data) <- make.names(names(equity_data))

#Orders data by month
equity_data <- equity_data %>% arrange(Company, Month)

#Creates lagged variables (by company)
equity_data <- equity_data %>%
  group_by(Company) %>%
  mutate(
    Lag_Return = lag(MonthlyReturns, 1),
    Lag_Volatility = lag(Rolling.12month.volatility, 1),
    Lag_Volume = lag(Total_Volume, 1),
    Price_Range = Monthly_High - Monthly_Low,
    Lag_Range = lag(Price_Range, 1)
  ) %>%
  ungroup()

equity_data <- na.omit(equity_data)

#Performing Regression
set.seed(6)
s.train <- sample(nrow(equity_data), 0.7*nrow(equity_data))
train <- equity_data[s.train,]
test <- equity_data[-s.train,]

model1 <- lm(MonthlyReturns ~ Lag_Return + Lag_Volatility + Lag_Volume + Lag_Range,
             data=train)
summary(model1)

#Evaluating Model 1
pred <- predict(model1, newdata=test)
rmse <- function(actual, predicted){
  sqrt(mean((actual - predicted)^2))
}
rmse_value <- rmse(test$MonthlyReturns, pred)
rmse_value

mod1.res <- data.frame(model="1",RMSE = rmse_value)
results <- mod1.res

#MODEL 2: Linear Regression to predict Inflation from lagged Fed Funds, GDP, Unemployment, and M2
macro <- read_csv("~/Spring 2026/Data Analytics/Term Project/macro.csv")
names(macro)

macro <- macro %>%
  arrange(observation_date) %>%
  mutate(
    Lag_Interest = lag(RIFSPFFNA, 1),
    Lag_GDP = lag(GDPA, 1),
    Lag_Unemployment = lag(UNRATENSA, 1),
    Lag_M2 = lag(M2NS, 1)
)

macro <- na.omit(macro)

set.seed(6)
s.train <- sample(nrow(macro), 0.7*nrow(macro))
train <- macro[s.train,]
test <- macro[-s.train,]

model2 <- lm(FPCPITOTLZGUSA ~ Lag_Interest + Lag_GDP + Lag_Unemployment + Lag_M2,
             data=train)
summary(model2)

#Evaluating Model 2
pred <- predict(model2, newdata=test)
rmse2 <- rmse(test$FPCPITOTLZGUSA, pred)

mod2.res <- data.frame(model="2",RMSE=rmse2)
results <- rbind(results, mod2.res)



#Model 3:
combo <- read_csv("~/Spring 2026/Data Analytics/Term Project/combo.csv")
names(combo) <- make.names(names(combo))
combo <- combo %>%  arrange(observation_date)

combo <- combo %>%
  mutate(
    Lag_Interest = lag(RIFSPFFNA, 1),
    Lag_Inflation = lag(FPCPITOTLZGUSA, 1)
  )
combo <- na.omit(combo)

model3 <- lm(Average.Monthly.Return ~ Lag_Interest + Lag_Inflation,
             data=combo)
summary(model3)

#Evaluating Model 3 (no test/train split because of small sample)
pred <- predict(model3, newdata=combo)
rmse3 <- rmse(combo$Average.Monthly.Return, pred)

mod3.res <- data.frame(model="3",RMSE =rmse3)
results <- rbind(results, mod3.res)

#Model 4: Revised Equity Model; Random Forest prediction of Future Monthly Returns based on same Lagged Variables
set.seed(6)
s.train <- sample(nrow(equity_data), 0.7*nrow(equity_data))

train <- equity_data[s.train,]
test <- equity_data[-s.train,]
rf_model <- randomForest(MonthlyReturns ~ Lag_Return + Lag_Volatility + Lag_Volume + Lag_Range,
                         data=train,
                         ntree=100,
                         mtry=2,
                         importance=TRUE)
print(rf_model)
importance(rf_model)

#Evaluating Model 4
rf_pred <- predict(rf_model, newdata=test)
rf_rmse <- rmse(test$MonthlyReturns, rf_pred)

mod4.res <- data.frame(model="4",RMSE = rf_rmse)
results <- rbind(results, mod4.res)


#Model 5 -- Different Linear Regression on Macro Data
macro <- macro %>%
  arrange(observation_date) %>%
  mutate(
    d_GDP = GDPA - lag(GDPA),
    d_M2 = M2NS - lag(M2NS),
    d_Unemployment = UNRATENSA - lag(UNRATENSA),
    d_Interest = RIFSPFFNA - lag(RIFSPFFNA)
  )

macro <- macro %>%
  mutate(
    g_GDP = (GDPA / lag(GDPA)) - 1,
    g_M2 = (M2NS / lag(M2NS)) - 1
  )

set.seed(6)
s.train <- sample(nrow(macro), 0.7*nrow(macro))
train <- macro[s.train,]
test <- macro[-s.train,]

model5 <- lm(FPCPITOTLZGUSA ~ g_GDP + g_M2 + d_Interest + d_Unemployment,
                   data=macro)

summary(model5)

pred <- predict(model5, newdata=test)
rmse5 <- rmse(test$FPCPITOTLZGUSA, pred)

mod5.res <- data.frame(model="5",RMSE=rmse5)
results <- rbind(results, mod5.res)
results

#Performance Plots
# 1. Actual vs Predicted (Linear Model)
lin_pred <- predict(model1, newdata=test)

plot1 <- ggplot(data.frame(Actual=test$MonthlyReturns, Predicted=lin_pred),
                aes(x=Actual, y=Predicted)) +
  geom_point(alpha=0.5) +
  geom_abline(slope=1, intercept=0, color="red") +
  ggtitle("Model 1: Actual vs Predicted Returns") +
  xlab("Actual Returns") +
  ylab("Predicted Returns")

print(plot1)

# 2. Actual vs Predicted (Random Forest)
plot2 <- ggplot(data.frame(Actual=test$MonthlyReturns, Predicted=rf_pred),
                aes(x=Actual, y=Predicted)) +
  geom_point(alpha=0.5) +
  geom_abline(slope=1, intercept=0, color="red") +
  ggtitle("Model 4: Actual vs Predicted Returns") +
  xlab("Actual Returns") +
  ylab("Predicted Returns")

print(plot2)






