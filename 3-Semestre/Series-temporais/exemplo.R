# Dados -------
# Definir a semente para reprodutibilidade
set.seed(123)

# Criar uma sequência de datas mensais
datas <- seq(as.Date("2010-01-01"), by = "month", length.out = 120)  # 10 anos de dados mensais

# Criar uma série com tendência e componente sazonal (sazonalidade anual, ou seja, lag 12)
tendencia <- seq(10, 20, length.out = 120)
sazonalidade <- 2 * sin(2 * pi * (1:120) / 12)  # Sazonalidade com período 12
ruido <- rnorm(120, mean = 0, sd = 1)

# Combinar para formar a série temporal
dados <- tendencia + sazonalidade + ruido

# Criar objeto ts
serie_temporal <- ts(dados, start = c(2010, 1), frequency = 12)

# Plotar a série temporal original
ggplot() +
  geom_line(aes(x = datas, y = serie_temporal), color = "black") +
  ggtitle("Série Temporal Original") +
  xlab("Ano") +
  ylab("Valor") +
  theme_minimal()

# Primeira diferença -------
# Calcular a primeira diferença com lag 10
primeira_dif_lag10 <- diff(serie_temporal, lag = 10, differences = 1)

# Criar objeto ts para a primeira diferença
primeira_dif_lag10_ts <- ts(primeira_dif_lag10, start = c(2010, 11), frequency = 12)

## Autocorrelação dos dados de focos de queimadas no estado selecionado
forecast::Acf(primeira_dif_lag10_ts, type = "correlation", main = "Autocorrelação para série exemplo", lag.max = 15)

## Autocorrelação parcial dos dados de focos de queimadas no estado selecionado
forecast::Acf(primeira_dif_lag10_ts, type = "partial", main = "Autocorrelação parcial para série exemplo", lag.max = 15)

# time_vector <- time(primeira_dif_lag10_ts)
# Assuming the ts object has a monthly frequency
start_year <- start(primeira_dif_lag10_ts)[1]
start_month <- start(primeira_dif_lag10_ts)[2]

# Create a sequence of dates
date_vector <- seq.Date(from = as.Date(paste(start_year, start_month, "01", sep = "-")),
                        by = "month",
                        length.out = length(primeira_dif_lag10_ts))

values_vector <- as.vector(primeira_dif_lag10_ts)


# Plotar a primeira diferença com lag 10
ggplot() +
  geom_line(aes(x = date_vector, y = values_vector), color = "black") +
  ggtitle("Primeira Diferença com Lag 10") +
  xlab("Ano") +
  ylab("Diferença") +
  theme_minimal()

# Segunda diferença -------

# Calcular a segunda diferença com lag 10
segunda_dif_lag10 <- diff(primeira_dif_lag10, lag = 10, differences = 1)

# Criar objeto ts para a segunda diferença
segunda_dif_lag10_ts <- ts(segunda_dif_lag10, start = c(2010, 21), frequency = 12)

# Assuming the ts object has a monthly frequency
start_year <- start(segunda_dif_lag10_ts)[1]
start_month <- start(segunda_dif_lag10_ts)[2]

# Create a sequence of dates
date_vector <- seq.Date(from = as.Date(paste(start_year, start_month, "01", sep = "-")),
                        by = "month",
                        length.out = length(segunda_dif_lag10_ts))

values_vector <- as.vector(segunda_dif_lag10_ts)

# Plotar a primeira diferença com lag 10
ggplot() +
  geom_line(aes(x = date_vector, y = values_vector), color = "black") +
  ggtitle("Segunda Diferença com Lag 10") +
  xlab("Ano") +
  ylab("Diferença") +
  theme_minimal()

