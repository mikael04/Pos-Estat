library(dplyr)
## Lendo do google sheets
library(googlesheets4)
## Lendo dados do bigquery
library(bigrquery)
library(dbplyr)
library(DBI)

# PDD - Google Sheets ----
## SINASC  ----
### Bigrquery ----
con <- dbConnect(
  bigrquery::bigquery(),
  project = "pdi-covid-basededados",
  dataset = "sinasc"
)
## 2010 - 2019
df_sinasc_2010_2019 <- tbl(con, "view_sinasc_2010_2019") |>
  dplyr::select(DTNASC, PESO, RACACORMAE, PARTO, ESCMAEAGR1, ESCMAE, IDADEMAE) |>
  dplyr::collect()

data.table::fwrite(df_sinasc_2010_2019, here::here("2-Semestre/Inferencia-STC853/TrabalhoFinal/dados/sinasc.csv"))

## 2003
df_sinasc_2003 <- tbl(con, "sinasc2003") |>
  dplyr::select(DTNASC, PESO, PARTO, ESCMAE, IDADEMAE) |>
  dplyr::collect()

data.table::fwrite(df_sinasc_2003, here::here("2-Semestre/Inferencia-STC853/TrabalhoFinal/dados/sinasc_2003.csv"))

