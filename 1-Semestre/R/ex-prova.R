## Lendo os dados

df <- readxl::read_excel(here::here("1-Semestre/Dados/dados_exemplo.xlsx"))

# a) ----
## Usando fórmulas aprendidas ----

## Calcular a média
media_x <- mean(df$x)
media_y <- mean(df$y)

## Somatório de x
som_x <- sum(df$x)

## Somatório de y
som_y <- sum(df$y)

## Adicionando coluna x*y
df <- df |>
  dplyr::mutate(xy = x*y)

## Calculando somatório de x*y
som_xy <- sum(df$xy)

## Calculando Sxy (covariância de xy)
Sxy <- som_xy - (som_x*som_y)/26

## Adicionando coluna x²
df$x2 <- df$x*df$x

## Calculando somatório de x²
som_x2 <- sum(df$x2)

## Calculando Sxx
Sxx <- som_x2 - (som_x)^2/26

## Adicionando coluna y²
df$y2 <- df$y*df$y

## Calculando somatório de y²
som_y2 <- sum(df$y2)

## Calculando Syy
Syy <- som_y2 - (som_y)^2/26

r_xy <- Sxy/sqrt(Sxx*Syy)

r_xy


## Bard ----

## covariância de xy
cov_xy <- cov(df$x, df$y)

## desvio padrão de x
sd_x <- sd(df$x)
## desvio padrão de y
sd_y <- sd(df$y)

## Calcular o coeficiente de correlação de Pearson
r <- cov_xy / (sd_x * sd_y)

## Imprimir o coeficiente de correlação
r

# b) ----

## Usando fórmulas aprendidas ----

## Bard ----
t <- r * sqrt(1 - r^2 / nrow(df))
t

# c) Interpretação ----
# Existe uma correlação forte positiva entre a razão de AIO3/NaOH empregada no processo produtivo e o teor de Na2O ocluído na Alumina

# d) Equação e linha de regressão ----
# ŷ = a+bx
# b = Sxy/Syy
b = Sxy/Sxx
b
# a = y(med) - b*x(med)
a = mean(df$y) - b*mean(df$x)
a

## Regressão linear
m1 <- lm(y~x, data=df)

eq <- paste0("Equação da regressão: ŷ = ", a, " + ", b, "*x")
eq

## Valor qualquer
x = 0.64

y_pred = -1.6052+3.1428*x
y_pred

# Bard ----
# Calcular o coeficiente angular e linear
b <- (sum(df$x * df$y) - sum(df$y) * sum(df$y)) / (sum(df$y^2) - (sum(df$y))^2)
a <- df$y[1] - b * df$y[1]

# Equação de regressão
eq <- paste0("y = ", b, "x + ", a)


## e) Gráfico de dispersão e regressão ----
# Gráfico
plot(df$x, df$y)
abline(m1, col="red")

# f) Coeficiente de determinação (R²) ----

## Observando pelo summary
summary(m1)

## Calculando
r_squared = b^2*Sxx/Syy
r_squared

## R: Calculando o índice de determinação de equação de regressão temos um valor considerado bom, indicando boa aproximação da relação entre as variáveis.

# g) Interpretação do coef. angular (b) ----

# Temos um coeficiente angular de 3,1428 (b) indicando que para cada 1 unidade de variação da razão AI2O3/NaOH temos uma variação no teor de Na2O de 3,1428.

# h) y_pred para x = 0,6 ----
x = 0.6
y_pred = a+b*x
y_pred

# i) Calcular o intervalo de confiança ----
## Para o nível de confiança de 95%

confint(m1)

# intervalo de confiança de  (-1,94 , -1,00)
# intervalo de confiança de b (2,20 , 3,67)


