
### Construindo mapas com ggplot2
library(data.table)
library(tidyverse)
library(geobr) # Pacote para acessar dados geográficos do Brasil

fread(file = "sinasc_2022.csv",
      select = c("UF", "IDADEMAE", "PESO", "CODMUNRES")) |>
  mutate(
    PESO = as.numeric(PESO),
  ) |>
  data.frame() -> sinasc


# Leitura de dados municipais para o ano de 2020
municipio <- read_municipality(year = 2020)

## Criando um mapa simples dos municípios usando ggplot2
ggplot(data = municipio) +
  geom_sf() +
  theme_minimal()


# Leitura de dados estaduais para o ano de 2020
estado <- read_state(year = 2020)

## Criando um mapa dos estados com personalizações visuais
ggplot(data = estado) +
  geom_sf(
    fill = "#2D3E50", # Cor de preenchimento dos estados
    color="#FEBF57", # Cor da borda dos estados
    size=.15, # Espessura da borda
    show.legend = FALSE
  ) +
  theme_void() # Configuração de um tema vazio para remover elementos adicionais


# Carregando dados municipais para o estado do RS no ano de 2020
muni_RS <- read_municipality(
  code_muni = "RS", # Código do estado
  year= 2020 # Ano dos dados
)

## Criando um mapa dos municípios do RS
muni_RS |>
ggplot() +
  geom_sf(
    fill="#2D3E50",
    color="#FEBF57"
  ) +
  theme_void()


muni_RS |>
  mutate(
    Santa_Maria = case_when(                  # Utiliza a função case_when para criar uma nova coluna
      name_muni == "Santa Maria" ~ "SIM",     # Se o município for "Santa Maria", atribui "SIM"
      TRUE ~ "NAO"                            # Caso contrário, atribui "NAO"
    )
  ) |>
  ggplot() +
  geom_sf(
    aes(fill = Santa_Maria)                  # Mapeia a cor de preenchimento segundo o valor da coluna "Santa_Maria"
  ) +
  theme_void()

### Juntando dataframes com a função left_join

idade <- data.frame(
  nome = c("João", "Thiago", "Pedro", "Maria", "Clara"),
  idade = c(23, 32, 27, 33, 19)
)
  
filhos <- data.frame(
  nome = c("Thiago", "Clara", "João", "Maria" , "Pedro"),
  quant_filhos = c(2, 0, 0, 1, 1)
)

left_join(idade, filhos, by = "nome") # A junção é realizada com base na coluna "nome"


### Juntando os dados do sinasc com do geobr

# Agrupando dados do dataframe 'enem' pela coluna 'uf' e calculando a média da idade das mães

enem <- read_delim("enem_2022.csv", locale = locale(decimal_mark = ","))

estado |>
  left_join(enem, by = c("abbrev_state" = "uf")) -> enem_map


# Criando um mapa com as médias de Matemática e suas Tecnologias
enem_map |>
  ggplot() +
  geom_sf(
    aes(fill = media_mt)   # Mapeando a cor de preenchimento à média de Matemática e suas Tecnologias
  ) +
  scale_fill_distiller(
    palette = "YlOrBr",   # Escolhendo a paleta de cores
    direction = 1        # Definindo a direção da paleta (-1 ou 1)
  ) +
  labs(
    title = "Nota Média de Matemática e suas Tecnologias"
  ) +
  theme_void() +
  theme(
    legend.position="bottom",                # Posicionamento da legenda na parte inferior
    legend.title = element_blank(),          # Removendo o título da legenda
    legend.key.width = unit(1,"cm"),         # Configurando a largura da chave da legenda
    legend.key.height = unit(0.2, "cm")      # Configurando a altura da chave da legenda
  )








