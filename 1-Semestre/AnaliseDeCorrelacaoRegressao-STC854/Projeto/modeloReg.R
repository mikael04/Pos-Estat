# Introdução

dataset_bg <- tibble::as_tibble(readxl::read_excel(here::here("1-Semestre/AnaliseDeCorrelacaoRegressao-STC854/Projeto/Dados/BGG_Data_Set.xlsx")))

skimr::skim(dataset_bg)

df_bg <- dataset_bg |>
  dplyr::select(id = ID, name = Name, year = `Year Published`, min_p = `Min Players`, max_p = `Max Players`,
                playtime = `Play Time`, min_age = `Min Age`, users_rat = `Users Rated`,
                compl = `Complexity Average`, own_users = `Owned Users`,
                score = `Rating Average`) |>
  dplyr::mutate(year = as.numeric(year))

## Limpando dados

# df_bg |>
#   dplyr::filter(is.na(year))

## Apenas uma observação sem ano, será removida


# df_bg |>
#   dplyr::filter(is.na(own_users))

## 23 jogos pouco avaliados e menos conhecidos, também serão removidos


df_bg <- df_bg |>
  dplyr::filter(!is.na(own_users) & !is.na(year)) |>
  dplyr::select(-id, -name)


## omo possuíamos poucas observações com dados faltantes, eu julguei mais prático por remover estas observações da base de dados, concluíndo que fariam pouca diferença no conjunto de dados completo e no modelo criado. No total foram removidas 23 observações.

## Correlação ----

# corrplot::corrplot(rxx)

rxx <- cor(df_bg[,1:8])

rxx_ <- cor.test(x = df_bg[, 1] |> dplyr::pull(), y = df_bg[, 2] |> dplyr::pull())


library(corrplot)
corrplot::corrplot(cor.test(df_bg[,1:8]))

## VIF ----

# vif2<- 1/(1-(rxx[1,2]^2))
# vif2

n_covariaveis <- 8

for(i in 1:n_covariaveis){
  for(j in 2:n_covariaveis){
    if(i!=j && i<j){
      print(paste0("VIF [", i, ",",j, "]"))
      print(1/(1-(rxx[i,j])))
    }
  }
}

## Primeiro modelo

modelo_completo <- lm(data = df_bg, compl ~ .)
summary(modelo_completo)

model_nulo <- lm(data = df_bg, compl ~ 1)
summary(model_nulo)

## Modelo step

modelo_nulo_step <- step(model_nulo, data = df_bg, direction = "both")
summary(modelo_nulo_step)

step(modelo_completo, direction = "both")

## Testando modelos "na mão"


summary(lm(formula = compl ~ year + min_p + max_p + playtime + min_age +
             users_rat + score + own_users, data = df_bg))

summary(lm(formula = compl ~ compl, data = df_bg))


summary(lm(formula = compl ~ year + score + own_users, data = df_bg))


# Analise dos resíduos
par(mfrow = c(2, 2))
plot(modelo_completo)
#
res <- (residuals(modelo_completo))
res

## Shapiro para apenas 5 mil observações
shapiro.test(modelo_completo$residuals[1:5000])

## Rejeitamos h0, não há indícios de que haja normalidade nos dados

## Anderson Darling para apenas 5 mil observações
library(nortest)
ad.test(res)

## Rejeitamos h0, não há indícios de que haja normalidade nos dados
library(lmtest)
bptest(modelo_completo)

dataset_bg_filtered <- df_bg |>
  dplyr::filter(own_users > 2000)


modelo_completo_filtrado <- lm(formula = compl ~ year + min_p + max_p + playtime + min_age + users_rat +
                   score + own_users, data = dataset_bg_filtered)

summary(modelo_completo_filtrado)

step(modelo_completo_filtrado, direction = "both")

modelo_filtrado_step <- lm(formula = compl ~ year + min_p + max_p + playtime + min_age +
                             users_rat + score + own_users,
   data = dataset_bg_filtered)

summary(modelo_filtrado_step)


## Shapiro para apenas 5 mil observações
shapiro.test(modelo_filtrado_step$residuals[1:5000])

## Rejeitamos h0, não há indícios de que haja normalidade nos dados
library(lmtest)
bptest(modelo_filtrado_step)

## Agora iremos analisar os pontos de influência para a base completa ----

influ <- influence.measures(modelo_completo)
influ$is.inf
summary(influ)

## Removendo pontos de influência baseados no dffit
df_sem_influ <- df_bg[influ$is.inf[,10] != TRUE,]
df_sem_influ_ <- df_bg[influ$is.inf[,10] != TRUE,]

modelo_completo_sem_influ <- lm(formula = compl ~ ., data = df_sem_influ)
summary(modelo_completo_sem_influ)

## Shapiro para apenas 5 mil observações
shapiro.test(modelo_completo_sem_influ$residuals[1:5000])

## Rejeitamos h0, não há indícios de que haja normalidade nos dados

## Anderson Darling mais de 5 mil observações

res <- (residuals(modelo_completo_sem_influ))
res
library(nortest)
ad.test(res)

## Rejeitamos h0, não há indícios de que haja normalidade nos dados
library(lmtest)
bptest(modelo_completo_sem_influ)

influ <- influence.measures(modelo_completo_sem_influ)
summary(influ)


## Agora iremos analisar os pontos de influência para a base filtrada ----

influ <- influence.measures(modelo_filtrado_step)
influ$is.inf
summary(influ)

## Removendo pontos de influência baseados no dffit
df_sem_influ <- dataset_bg_filtered[influ$is.inf[,10] != TRUE,]

modelo_filtrado_sem_influ <- lm(formula = compl ~ ., data = df_sem_influ)
summary(modelo_filtrado_sem_influ)

## Shapiro para apenas 5 mil observações
shapiro.test(modelo_filtrado_sem_influ$residuals[1:5000])

## Rejeitamos h0, não há indícios de que haja normalidade nos dados

## Anderson Darling mais de 5 mil observações

res <- (residuals(modelo_filtrado_sem_influ))
res
library(nortest)
ad.test(res)

## Rejeitamos h0, não há indícios de que haja normalidade nos dados
library(lmtest)
bptest(modelo_filtrado_sem_influ)

influ <- influence.measures(modelo_filtrado_sem_influ)
summary(influ)
