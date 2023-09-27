## Lendo os dados

df <- readxl::read_excel("Dados/dados_exemplo.xlsx")

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


## Feito pelo Bard ----

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

## Feito pelo Bard ----
t <- r * sqrt(1 - r^2 / nrow(df))
t

# c) Interpretação ----
# Existe uma correlação forte positiva entre a razão de AIO3/NaOH empregada no processo produtivo e o teor de Na2O ocluído na Alumina

# d) ----
# ŷ = a+bx
# b = Sxy/Syy
b = Sxy/Sxx
b
# a = y(med) - b*x(med)

# Gráfico
plot(df$x, df$y)

# Bard
# Calcular o coeficiente angular e linear
b <- (sum(df$x * df$y) - sum(df$y) * sum(df$y)) / (sum(df$y^2) - (sum(df$y))^2)
a <- df$y[1] - b * df$y[1]

# Equação de regressão
eq <- paste0("y = ", b, "x + ", a)


m1 <- lm(y~x, data=df)
summary(m1)
0.4*b+a

-1.6052+3.1428*0.4


y <- 0.03 * 0.655 + 0.4
