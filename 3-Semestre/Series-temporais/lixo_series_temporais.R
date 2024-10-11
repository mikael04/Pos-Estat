
ts_data |> diff() |> ggtsdisplay()

decompose(ts_data)

naive(df_queimadas_estado_anomes_treino)
autoplot()
autoplot(ts_data) +
  xlab("Ano/Mês") +
  ylab("Quantidade de focos")

res <- residuals(naive(ts_data))
autoplot(res) + xlab("Day") + ylab("") +
  ggtitle("Residuals from naïve method")

gghistogram(res) + ggtitle("Histogram of residuals")

ggAcf(res) + ggtitle("ACF of residuals")
ggAcf(res, type = "partial") + ggtitle("PACF of residuals")

checkresiduals(ts_data)

library(tseries)
library(forecast)
library(ggplot2)
# Convert the dataframe into a time series object
ts_data <- ts(df_queimadas_estado_anomes_treino$n, start = c(lubridate::year(min(df_queimadas_estado_anomes_treino$Ano_mes)), lubridate::month(min(df_queimadas_estado_anomes_treino$Ano_mes))), frequency = 12)

# Plotting the time series
autoplot(ts_data) +
  ggtitle("Monthly Count of Burnings") +
  xlab("Year") +
  ylab("Count")

# Check stationarity using Augmented Dickey-Fuller Test
adf_test <- adf.test(ts_data)
print(adf_test)

# Differencing the data if needed
if (adf_test$p.value > 0.05) {
  ts_data_diff <- diff(ts_data)
  print("Data differenced to achieve stationarity.")
} else {
  ts_data_diff <- ts_data
}

# Plotting ACF and PACF
autoplot(acf(ts_data_diff, plot = FALSE)) +
  ggtitle("Autocorrelation Function (ACF) Plot") +
  ggplot2::theme_minimal()
autoplot(pacf(ts_data_diff, plot = FALSE)) +
  ggtitle("Partial Autocorrelation Function (PACF) Plot") +
  ggplot2::theme_minimal()

# Fit an ARIMA model
model <- auto.arima(ts_data)
print(summary(model))

# Forecasting the next 12 months
forecast_data <- forecast(model, h = 12)
print(forecast_data)



# Plotting the forecast
autoplot(forecast_data) +
  ggtitle("ARIMA Model Forecast") +
  xlab("Year") +
  ylab("Count")

# Evaluating the model accuracy
# (Assuming a test dataset, df_queimadas_estado_anomes_teste)
ts_test <- ts(df_queimadas_estado_anomes_teste$n, start = c(lubridate::year(min(df_queimadas_estado_anomes_teste$Ano_mes)), lubridate::month(min(df_queimadas_estado_anomes_teste$Ano_mes))), frequency = 12)
accuracy(forecast_data, ts_test)
