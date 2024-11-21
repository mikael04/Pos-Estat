library(dplyr)
library(ggplot2)

# Importando os dados
dados <- openxlsx::read.xlsx("3-Semestre/CEP-ControleEstatisticodoProcesso/exemplo critérios.xlsx")

# Calculando a amplitude móvel
dados <- dados |>
  dplyr::mutate(Amplitude_Movel = abs(x - lag(x)))

# Calculando a média
media <- mean(dados$x)

# Calculando a amplitude móvel média
am_media <- mean(dados$Amplitude_Movel, na.rm = TRUE)

# Limite inferior
LIC <- media - 3 * (am_media/1.128)

# Limite superior
LSC <- media + 3 * (am_media/1.128)

# Plotando o gráfico
dados |>
  ggplot2::ggplot(aes(x = 1:nrow(dados), y = x)) +
  ggplot2::geom_line() +
  ggplot2::geom_point() +
  ggplot2::geom_hline(yintercept = media, linetype = "solid", color = "blue") +
  ggplot2::geom_hline(yintercept = c(LIC, LSC), linetype = "dashed", color = "red") +
  ggplot2::labs(title = "Gráfico de Controle de Amplitude Móvel",
                x = "Amostra",
                y = "Valor") +
  ggplot2::theme_minimal()


# Calculando o desvio padrão médio
dp <- sd(dados$x, na.rm = TRUE)

## Positivo
### Achando a faixa "A" -> 3 desvios padrões
A_p <- media + 3 * dp
### Achando a faixa "B"
B_p <- media + 2 * dp
### Achando a faixa "C"
C_p <- media + 1 * dp

## Negativo
### Achando a faixa "A" -> 3 desvios padrões
A_n <- media - 3 * dp
### Achando a faixa "B" -> 2 desvios padrões
B_n <- media - 2 * dp
### Achando a faixa "C" -> 1 desvios padrões
C_n <- media - 1 * dp

# Plotando o gráfico
dados |>
  ggplot2::ggplot(aes(x = 1:nrow(dados), y = x)) +
  ggplot2::geom_line() +
  ggplot2::geom_point() +
  ggplot2::geom_hline(yintercept = media, linetype = "solid", color = "blue") +
  ggplot2::geom_hline(yintercept = c(LSC, LIC), linetype = "solid", color = "brown") +
  ggplot2::geom_hline(yintercept = c(C_p, C_n), linetype = "dashed", color = "red") +
  ggplot2::geom_hline(yintercept = c(A_p, A_n), linetype = "dashed", color = "green") +
  ggplot2::geom_hline(yintercept = c(B_p, B_n), linetype = "dashed", color = "yellow") +
  ggplot2::labs(title = "Gráfico de Controle de Desvio Padrão Médio",
                x = "Amostra",
                y = "Valor") +
  ggplot2::theme_minimal()


# Usando a biblioteca qcc
library(qcc)
x = c(33.75, 33.05, 34, 33.81, 33.46, 34.02, 33.68, 33.27, 33.49, 33.20,
      33.62, 33.00, 33.54, 33.12, 33.84)
q1 = qcc(x, type="xbar.one")
q2 = qcc(dados$x, type="xbar.one", std.dev = "SD")


## Exemplo controle de especificação
## Uma corrente tem especificação 100 +- 10 miliamperes

LIC <- 100-10
LSC <- 100+10
media <-107
dp <- 1.5

Cp = (LSC - LIC) / (6 * dp)
# Resultado Cp -> 2.22 > 1.33, processo é capaz de atender a especificação
Cpk = min(LSC-media, media-LIC) / (3 * dp)
# Resultado Cpk -> 0.67 < 1.33, processo é incapaz de atender a especificação
