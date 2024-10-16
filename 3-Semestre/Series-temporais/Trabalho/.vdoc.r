#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
knitr::opts_chunk$set(echo = TRUE)
library(dplyr)
library(ggplot2)
options(scipen = 99999)
data_from <- "local"
# data_from <- "pendrive"
if(data_from == "local"){
  data_path <- "/mnt/Netac-Dados/Projetos/R/Pos-Estat/3-Semestre/Series-temporais/Trabalho/dados/"
}
if(data_from == "pendrive"){
  data_path <- "/media/userlm/Ventoy/Projetos/R/Pos-Estat/3-semestre/Series-temporais/Trabalho/dados/"
}
rewrite_data <- F
#
#
#
#
#
#
#
#
#
#
#
dados_2023_2024 <- data.table::fread(paste0(data_path, "focos_qmd_inpe_2023-10-01_2024-10-01_32.767240.csv"))

df_queimadas_2023_2024 <- dados_2023_2024 |> 
  dplyr::arrange(Estado, Municipio, DataHora)

## Verificando quantas informações temos cada município
df_queimadas_2023_2024_ag_mun_sat <- df_queimadas_2023_2024 |> 
  dplyr::group_by(Municipio, Satelite) |> 
  dplyr::mutate(n = n()) |> 
  dplyr::distinct(Municipio, Satelite, .keep_all = T) |>
  dplyr::select(Estado, Municipio, Satelite, n) |> 
  dplyr::ungroup()

df_top_22 <- df_queimadas_2023_2024_ag_mun_sat |> 
  # head(22)
  dplyr::slice(1:22)

dados_2014_2015 <- data.table::fread(paste0(data_path, "focos_qmd_inpe_2014-10-01_2015-10-01_41.322529.csv"))

df_queimadas_2014_2015 <- dados_2014_2015 |> 
  dplyr::arrange(Estado, Municipio, DataHora)

## Verificando quantas informações temos cada município
df_queimadas_2014_2015_ag_mun_sat <- df_queimadas_2014_2015 |> 
  dplyr::group_by(Municipio, Satelite) |> 
  dplyr::mutate(n = n()) |> 
  dplyr::distinct(Municipio, Satelite, .keep_all = T) |>
  dplyr::select(Estado, Municipio, Satelite, n)

## Observando um município
df_mun <- df_queimadas_2014_2015_ag_mun_sat |> 
  dplyr::filter(Municipio == "SÃO FÉLIX DO XINGU",
                Satelite == "NPP-375")

#
#
#
#
#
#
## Verificando a quantidade de focos de queimadas por mês e ano
format(lubridate::ymd_hms(df_queimadas_2014_2015[1,1]), "%Y-%m")

df_queimadas_2014_2015_sample <- df_queimadas_2014_2015 |> 
  dplyr::sample_n(1000) |> 
  dplyr::mutate(DataHora_ = lubridate::ymd_hms(DataHora)) |>
  dplyr::mutate(Ano_mes = format(DataHora_, "%Y-%m")) |>
  dplyr::group_by(Ano_mes) |> 
  dplyr::summarise(n = n()) |> 
  dplyr::ungroup() |> 
  dplyr::mutate(Ano_mes = as.factor(Ano_mes))

queimadas_ano_mes <- df_queimadas_2014_2015|> 
  dplyr::sample_n(1000) |> 
  dplyr::mutate(DataHora_ = lubridate::ymd_hms(DataHora)) |>
  dplyr::mutate(Ano_mes = format(DataHora_, "%Y-%m")) |>
  dplyr::filter(Ano_mes != "2015-10") |> 
  dplyr::group_by(Ano_mes) |> 
  dplyr::summarise(n = n()) |> 
  dplyr::ungroup()

queimadas_ano_mes |> 
  ggplot(aes(x = Ano_mes, y = n)) +
  geom_col(position = "dodge", fill = "#407D97") +
  labs(title = "Focos de queimadas por mês e ano",
       x = "Ano/Mês",
       y = "Quantidade de focos") +
  theme_minimal()
```
#
#
#
df_estado <- df_queimadas_2014_2015 |> 
  dplyr::group_by(Estado) |> 
  dplyr::summarise(n = n()) |> 
  dplyr::arrange(desc(n))

df_estado |>
  ggplot(aes(x = n, y = reorder(Estado, n))) +
  geom_col(fill = "#407D97") +
  labs(title = "Focos de queimadas por estado",
       x = "Estado",
       y = "Quantidade de focos") +
  theme_minimal()
#
#
#
#
#
df_estado_2023_2024 <- df_queimadas_2023_2024 |> 
  dplyr::group_by(Estado) |> 
  dplyr::summarise(n = n()) |> 
  dplyr::arrange(desc(n))

df_estado_2023_2024 |>
  ggplot(aes(x = n, y = reorder(Estado, n))) +
  geom_col(fill = "#407D97") +
  labs(title = "Focos de queimadas por estado",
       x = "Estado",
       y = "Quantidade de focos") +
  theme_minimal()
#
#
#
#
#
#
#
#
if(rewrite_data){
  # Usando dados apenas de um município
  ## Município selecionado
  estado_sel <- "MATO GROSSO"
  satelite_sel <- "NPP-375"
  ## Lendo todos os arquivos da pasta dados
  files <- list.files(paste0(data_path), full.names = T)
  ## Lendo os arquivos e selecionando apenas dados do estado e Satelite selecionados
  df_queimadas_estado <- lapply(files, function(x){
    dados <- data.table::fread(x)
    dados |> 
      dplyr::filter(Estado == estado_sel,
                    Satelite == satelite_sel)
  })
  ## Unindo todos os dados
  df_queimadas_estado <- dplyr::bind_rows(df_queimadas_estado)
  ## Removendo linhas duplicadas
  df_queimadas_estado <- df_queimadas_estado |> 
    dplyr::distinct()
  
## Verificando a quantidade de focos de queimadas por mês e ano
df_queimadas_estado_anomes <- df_queimadas_estado |> 
  dplyr::mutate(DataHora_ = lubridate::ymd_hms(DataHora)) |>
  dplyr::mutate(Ano_mes = format(DataHora_, "%Y-%m")) |>
  dplyr::group_by(Ano_mes) |> 
  dplyr::summarise(n = n()) |> 
  dplyr::ungroup() |> 
  dplyr::mutate(Ano_mes = Ano_mes)
  
  ## Salvando os dados
  saveRDS(df_queimadas_estado_anomes, paste0(data_path, "df_queimadas_ma_anomes.rds"))
}else{
  df_queimadas_estado_anomes <- readRDS(paste0(data_path, "df_queimadas_ma_anomes.rds"))
  estado_sel <- "MATO GROSSO"
  satelite_sel <- "NPP-375"
}
#
#
#
## Fazendo gráfico de linhas da série temporal
df_queimadas_estado_anomes |>
  mutate(Ano_mes = paste0(Ano_mes, "-01")) %>%       # Create "YYYY-MM-01"
  mutate(Ano_mes = lubridate::ymd(Ano_mes))   |> 
  ggplot(aes(x = Ano_mes, y = n, group = 1)) +
  geom_line() +
  labs(title = paste0 ("Focos de queimadas por mês e ano ", "no ", estado_sel),
       x = "Ano/Mês",
       y = "Quantidade de focos") +
  theme_minimal() +
  scale_x_date(date_labels = "%Y", date_breaks = "1 year")

#
#
#
## Autocorrelação dos dados de focos de queimadas no estado selecionado
forecast::Acf(df_queimadas_estado_anomes$n, type = "correlation", main = "Autocorrelação para volume de queimadas no mês", lag.max = 15)

## Autocorrelação parcial dos dados de focos de queimadas no estado selecionado
forecast::Acf(df_queimadas_estado_anomes$n, type = "partial", main = "Autocorrelação parcial para volume de queimadas no mês", lag.max = 15)
#
#
#
#
## Dividindo os dados em treino e teste
df_queimadas_estado_anomes <- df_queimadas_estado_anomes |> 
  dplyr::mutate(Ano_mes = as.Date(paste0(Ano_mes, "-01")))

df_queimadas_estado_anomes_treino <- df_queimadas_estado_anomes |>
  dplyr::filter(Ano_mes < "2024-01-01" & Ano_mes > "2015-01-01")

df_queimadas_estado_anomes_teste <- df_queimadas_estado_anomes |>
  dplyr::filter(Ano_mes >= "2024-01-01")

## Transformando os dados em série temporal
ts_data <- ts(df_queimadas_estado_anomes_treino$n, start = c(lubridate::year(min(df_queimadas_estado_anomes_treino$Ano_mes)), lubridate::month(min(df_queimadas_estado_anomes_treino$Ano_mes))), frequency = 12)

# auto.arima(ts_data, seasonal = TRUE)
# ts_data <- na.omit(ts_data)
fit_arima <- arima(ts_data, order = c(1, 0, 1))
fit_sarima <- arima(ts_data, order = c(1, 0, 0), seasonal = list(order = c(2, 1, 0), period = 12))

## Modelo ARIMA
modelo_arima <- forecast::Arima(ts_data, order = c(1, 0, 1), seasonal = c(12, 0, 1))
# forecast::ARIMA(ts_data, order = c(1, 0, 1), seasonal = c(12, 0, 1))
modelo_arima

auto.arima(ts_data)
#
#
#
#
