library(dplyr)

## Lendo os de emissão de CO2 nos países do G20 ----

## Lendo dados
df_h <- readxl::read_excel("dados/Trabalho Inferência Estatística (Energia).xlsx")

## Testando normalidade dos dados ----

### Emissões anuais de CO2 com base no consumo

df_h_can <- df_h |>
  dplyr::filter(`País` == "Canadá")

df_h_rein <- df_h |>
  dplyr::filter(`País` == "Reino Unido")

diferenca_emissao <- df_h_can$`Emissões anuais de CO2 com base no consumo` - df_h_rein$`Emissões anuais de CO2 com base no consumo`

shapiro.test(diferenca_emissao)
plot(density(diferenca_emissao), ylab='Densidade', xlab='Amostra X', main='')


## Canadá
shapiro.test(df_h_can$`Emissões anuais de CO2 com base no consumo`)
plot(density(df_h_can$`Emissões anuais de CO2 com base no consumo`), ylab='Densidade', xlab='Amostra X', main='')

## Reino Unido
shapiro.test(df_h_rein$`Emissões anuais de CO2 com base no consumo`)
plot(density(df_h_rein$`Emissões anuais de CO2 com base no consumo`), ylab='Densidade', xlab='Amostra X', main='')

## Ou seja, ao avaliarmos a base como um todo, na diferença dos dois países, não temos normalidade nos dados, como é possível ver no primeiro gráfico e no primeiro teste.

## Já quando separamos as bases por país, ambos os países apresentam valores de emissão de CO anuais que podem ser consideradas normais a um nível de significância de 5%, como é possível ver nos gráficos e nos testes de Shapiro-Wilk.


## Análise gráfica dos dados ----

### Emissões anuais de CO2 com base no consumo

boxplot(df_h$`Emissões anuais de CO2 com base no consumo`, ylab="Emissões anuais de CO2 com base no consumo", xlab="Ano")

library(ggplot2)
ggplot(df_h, aes(x = `País`, y = `Emissões anuais de CO2 com base no consumo`)) +
  geom_boxplot()

## Intervalo de confiança da média de emissão para o Canadá

### Canadá

## Usando a função do teste t
t.test(df_h_can$`Emissões anuais de CO2 com base no consumo`, conf.level = 0.95)$conf.int

## Usando cálculo manual
alpha<-0.05
n<-length(df_h_can$`Emissões anuais de CO2 com base no consumo`)

(LI<-mean(df_h_can$`Emissões anuais de CO2 com base no consumo`)-qt(1-alpha/2, n-1)*sd(df_h_can$`Emissões anuais de CO2 com base no consumo`)/sqrt(n))
(LS<-mean(df_h_can$`Emissões anuais de CO2 com base no consumo`)+qt(1-alpha/2, n-1)*sd(df_h_can$`Emissões anuais de CO2 com base no consumo`)/sqrt(n))

## Temos então que o intervalo de confiança para a média de emissão de CO2 anual do Canadá é de 545481780 à 578057875, com 95% de confiança.

## Estimação pontual


## Para a estimação pontual de emissão de CO2 anual para o Canadá, como possuímos uma distribuição aproximada da normal, utilizaremos a fórmula da média amostral para calcular uma distribuição normal, que é:

sum(df_h_can$`Emissões anuais de CO2 com base no consumo`)/length(df_h_can$`Emissões anuais de CO2 com base no consumo`)

## Ou seja, a estimação pontual da emissão de CO2 anual do Canadá é de 561769828.

## Teste de hipótese

## Agora vamos realizar o teste t pareado para verificar se as médias dos dois grupos são iguais.

t.test(df_h_can$`Emissões anuais de CO2 com base no consumo`, df_h_rein$`Emissões anuais de CO2 com base no consumo`, conf.level = 0.95)

## Ou seja, corroborando com o que boxplot indicou, usando o teste t pareado, com nível de significância de 5%, rejeita a hipótese nula de que as médias dos dois grupos são iguais, ou seja, existem evidências de que as médias de emissões de CO2 para o Canadá e o Reino Unido sejam diferentes.
