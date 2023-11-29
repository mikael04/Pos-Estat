
############## GGPLOT2 #####################
library(tidyverse)
library(data.table)

### Construindo um boxplot
iris |>
  ggplot(aes(y=Sepal.Length)) +
  geom_boxplot() 

## Construindo um boxplot para cada espécies
iris |>
  ggplot(aes(y=Sepal.Length, x = Species)) +
  geom_boxplot(
    width = 0.6, # Define a largura dos boxplots
    fill = "gray95", # Define a cor de preenchimento dos boxplots
    outlier.size = 2, # Define o tamanho dos outliers
    outlier.color = "blue" # Define a cor dos outliers
  ) +
  labs(
    x = "Espécies",
    y = "Comprimento da Sépala"
  ) 


## Adicionando mais informações ao gráfico

## Construindo um boxplot para cada espécies
iris |>
  ggplot(aes(y=Sepal.Length, x = Species)) +
  geom_errorbar(            # Adiciona barras de erro
    stat = "boxplot",       # Estatística usada para calcular as barras de erro
    width = 0.3             # Largura das barras de erro
  ) +
  geom_boxplot(
    width = 0.6,            # Define a largura dos boxplots
    outlier.size = 2,       # Define o tamanho dos outliers
    outlier.color = "blue", # Define a cor dos outliers
    outlier.alpha = 0.8     # Define a transparência dos outliers
  ) +
  geom_jitter(              # Adiciona dispersão para evitar sobreposição
    width = 0.15,           # Largura da dispersão
    alpha = 0.5             # Transparência dos pontos
  ) +
  labs(
    x = "Espécies",
    y = "Comprimento da Sépala"
  ) 


### Customizando gráficos
temperatura <- read_delim("temperatura.csv",  locale = locale(decimal_mark = ","))


temperatura |>
  mutate(
    data = lubridate::dmy(data) 
  ) |>
  group_by(
    data
  ) |>
  summarise(
    maximo_diario = max(temp_max, na.rm = TRUE) 
  ) |>
  mutate(
    mes = lubridate::month(data, label = TRUE, abbr = TRUE)
  ) |>
  ggplot(
    aes(y=maximo_diario, x = mes)
  ) +
  geom_boxplot() +                # Adicionando o boxplot
  geom_jitter(
    aes(color = maximo_diario),   # Adicionando os pontos com jitter
    width = 0.2
  ) +
  scale_color_gradient2(          # Configurando a escala de cores
    low = "blue",                  # Configuração da cor para temperaturas mais baixas
    mid = "white",                # Configuração da cor para a temperatura média
    high = "red",                # Configuração da cor para temperaturas mais altas
    midpoint = 30                 # Define o ponto médio para a escala de cores
  ) +
  theme_classic()




mycol1 <- c("#000080", "#0000ff", "cyan", "#e0ffff", "#ffff00", "#ff0000", "#800000") # Vetor de cores personalizado

temperatura |>
  mutate(
    data = lubridate::dmy(data)
  ) |>
  group_by(data) |>
  summarise(
    temp_max = max(temp_max, na.rm = NA)
  ) |> 
  mutate(
    mes = lubridate::month(data, label = TRUE, abbr = TRUE)
  ) |>
  ggplot(aes(y = temp_max, x = mes)) +
  geom_errorbar(
    stat = "boxplot", 
    width = 0.2
  ) +
  geom_boxplot(
    outlier.shape = NA  # Remove os pontos outliers do gráfico
  ) +
  geom_jitter(
    aes(color = temp_max), # Configura a cor dos pontos de dispersão com base na temperatura máxima
    width = 0.2,           # Largura da dispersão (jitter)
    alpha = 0.7,           # Transparência dos pontos  
    size = 2               # Tamanho dos pontos
  ) +
  scale_color_gradientn(
    name = "Temp. [ºF]",   # Título da legenda de cor
    colors = mycol1        # Vetor de cores personalizado
  ) +
  scale_x_discrete(        # Configuração do eixo x discreto
    name = "Meses"         # Título do eixo x
  ) +
  scale_y_continuous(                                     # Configuração do eixo y contínuo
    name = "Temperatura [ºC]",                            # Título do eixo y
    limits = c(10, 45),                                   # Limites do eixo y
    breaks = seq(0, 50, 10),                              # Rótulos dos intervalos no eixo y
    labels = scales::number_format(suffix = " ºC")        # Formato dos rótulos no eixo y
  ) +
  labs(
    title = "Temperatura máxima diária",
    caption = "Fonte: INMET"
  ) +
  theme_bw() +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = "gray80", linetype = 2 ),
    legend.direction = "vertical",
    legend.key.height = unit(8, units = "mm"),
    plot.title = element_text(face = "bold", color = "darkcyan", size = 25),
    axis.title.y = element_text(color = "gray20", size = 18),
    axis.title.x = element_text(color = "gray20", size = 18),
    axis.text = element_text(color = "gray20", size = 14),
    plot.caption = element_text(size = 12),
    legend.title = element_text(size = 14),
  )





