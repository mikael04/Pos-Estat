library(tidyverse)
library(tidyquant)
library(timetk)
library(sweep)
library(forecast)
library(ggplot2)

## True se os dados forem rodados do pendrive no notebook, F se rodar no PC no projeto
data_notebook <- T

if(data_notebook){
  data_path <- "/media/userlm/Ventoy/Projetos/R/Pos-Estat/"
}else{
  data_path <- ""
}

proj_path <- "3-Semestre/Series-Temporais/Trabalho/"

# Importando os dados
## PARÁ Mun: SÃO FÉLIX DO XINGU Satelite: NPP-375 N Contagens: 35732
dados_2022_2023 <- data.table::fread(paste0(data_path, proj_path, "dados/focos_qmd_inpe_2022-10-01_2023-10-01_45.900864.csv")) |>
  dplyr::filter(Satelite == "NPP-375", Municipio == "SÃO FÉLIX DO XINGU")
# dados_2023_2024 <- data.table::fread(paste0(data_path, proj_path, "dados/focos_qmd_inpe_2023-10-01_2024-10-01_32.767240.csv"))
dados_2014_2015 <- data.table::fread(paste0(data_path, proj_path, "dados/focos_qmd_inpe_2014-10-01_2015-10-01_41.322529.csv")) |>
  dplyr::filter(Satelite == "NPP-375", Municipio == "SÃO FÉLIX DO XINGU")


