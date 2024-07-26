# Exemplo DIC ----
# Exemplo 1 Lista DIC

## Dados ----

rm(list=ls())
# setwd("2-Semestre/PlanejamentoDeExperimentos-STC856/Scripts/")
# dados <- read.table('data-raw/ExemploDic.txt', header=TRUE)

trat <- c(rep("A", 5), rep("B", 5), rep("C", 5))
des <- c(130, 129, 128, 126, 130,
         125, 131, 130, 129, 127,
         135, 129, 131, 128, 130)
dados <- data.frame(trat, des)
rep <- 5

## Graus de Liberdade ----

GL_trat <- length(unique(dados$trat))-1
GL_total <- nrow(dados)-1

GL_res <- GL_total - GL_trat

## Soma de quadrados ----
T_a <- sum(dados$des[dados$trat=="A"])
T_b <- sum(dados$des[dados$trat=="B"])
T_c <- sum(dados$des[dados$trat=="C"])

T_total <- sum(T_a+T_b+T_c)

(SQ_trat <- (T_a^2+T_b^2+T_c^2)/(rep) - T_total^2/(GL_total+1))

(SQ_total <- sum(dados$des^2) - T_total^2/(GL_total+1))

(SQ_res <- SQ_total - SQ_trat)

## Quadrado médio ----

(QM_trat = SQ_trat/GL_trat)

(QM_res = SQ_res/GL_res)

## F calculado ----

(F0 = QM_trat/QM_res)

## F tabelado ----

(F_tab <- qf(0.99, GL_trat, GL_res))

(F0 > F_tab)
## False

## Como é falso, rejeitamos a hipótese nula, ou seja, pelo teste


## Conferindo resultado ----

a0 <- ExpDes::crd(dados$trat, dados$des, quali = TRUE, mcomp="tukey", sigF = 0.05)

      # Exemplo DBC ----
# Exemplo 1 Lista DBC

## Dados ----

rm(list=ls())
# setwd("2-Semestre/PlanejamentoDeExperimentos-STC856/Scripts/")
# dados <- read.table('data-raw/ExemploDbc.txt', header=TRUE)
trat <- c(rep(c("A", "B", "C", "D", "E", "F"), 3))
bloco <- c(rep("I", 6), rep("II", 6), rep("III", 6))
producao <- c(2.9, 8, 5.6, 7.7, 4.3, 5.5,
              3.2, 10.6, 6.0, 8.8, 6.5, 5.1,
              2.4, 9.8, 6.7, 7.9, 5.6, 6.9)

dados <- data.frame(trat, bloco, producao)

## Graus de liberdade ----
GL_trat <- length(unique(dados$trat))-1
GL_bloc <- length(unique(dados$bloco))-1
GL_total <- length(dados$producao)-1

GL_res <- GL_total - GL_trat - GL_bloc

## Soma de quadrados ----
### Tratamentos ----
(T_t1 <- sum(dados$producao[dados$trat=="A"]))
(T_t2 <- sum(dados$producao[dados$trat=="B"]))
(T_t3 <- sum(dados$producao[dados$trat=="C"]))
(T_t4 <- sum(dados$producao[dados$trat=="D"]))
(T_t5 <- sum(dados$producao[dados$trat=="E"]))
(T_t6 <- sum(dados$producao[dados$trat=="F"]))

### blocos ----
(T_b1 <- sum(dados$producao[dados$bloco=="I"]))
(T_b2 <- sum(dados$producao[dados$bloco=="II"]))
(T_b3 <- sum(dados$producao[dados$bloco=="III"]))

(T_total <- sum(T_t1+T_t2+T_t3+T_t4))
(T_total <- sum(T_b1+T_b2+T_b3))

(SQ_trat <- (T_t1^2+T_t2^2+T_t3^2+T_t4^2+T_t5^2+T_t6^2)/(GL_bloc+1) - T_total^2/(GL_total+1))

(SQ_bloc <- (T_b1^2+T_b2^2+T_b3^2)/(GL_trat+1) - T_total^2/(GL_total+1))

(SQ_total <- sum(dados$producao^2) - T_total^2/(GL_total+1))

(SQ_res <- SQ_total - SQ_trat - SQ_bloc)

## Quadrados médios ----

(QM_trat = SQ_trat/GL_trat)

(QM_bloc = SQ_bloc/GL_bloc)

(QM_res = SQ_res/GL_res)

## F calculado ----

(F_trat = QM_trat/QM_res)

(F_bloc = QM_bloc/QM_res)

## F tabelado ----

(qf(0.95, GL_trat, GL_res))

(qf(0.95, GL_bloc, GL_res))

## CV ----

media_geral <- mean(dados$producao)

(CV <- sqrt(QM_res)/media_geral*100)

  ## Conferindo resultados ----

a0 <- ExpDes::rbd(dados$trat, dados$bloco, dados$producao, mcomp="tukey", sigT=0.05)

# DQL ----

## Dados ----
rm(list=ls())
dados <- data.frame(
  Teor_de_Gordura = rep(1:5, each=5),
  Grau_de_Acidez = rep(1:5, times=5),
  Bacilo = c("A", "E", "C", "D", "B",
             "C", "B", "E", "D", "A",
             "D", "C", "A", "B", "E",
             "E", "D", "B", "C", "A",
             "B", "A", "D", "E", "C"),
  Volume = c(450, 620, 680, 620, 780,
             750, 990, 750, 660, 830,
             750, 910, 690, 990, 760,
             650, 890, 835, 850, 875,
             750, 720, 850, 770, 890)
)

T_total <- sum(dados$Volume)

## Graus de liberdade ----
GL_trat <- length(unique(dados$Bacilo))-1
GL_lin <- length(unique(dados$Teor_de_Gordura))-1
GL_col <- length(unique(dados$Grau_de_Acidez))-1
GL_total <- nrow(dados)-1
GL_res <- GL_total - GL_trat - GL_lin - GL_col

## Soma de quadrados ----
### Tratamentos ----
T_t1 <- sum(dados$Volume[dados$Bacilo=="A"])
T_t2 <- sum(dados$Volume[dados$Bacilo=="B"])
T_t3 <- sum(dados$Volume[dados$Bacilo=="C"])
T_t4 <- sum(dados$Volume[dados$Bacilo=="D"])
T_t5 <- sum(dados$Volume[dados$Bacilo=="E"])

T_total <- sum(T_t1+T_t2+T_t3+T_t4+T_t5)

(SQ_trat <- (T_t1^2+T_t2^2+T_t3^2+T_t4^2+T_t5^2)/(GL_trat+1) - T_total^2/(GL_total+1))

### Linhas ----
T_l1 <- sum(dados$Volume[dados$Teor_de_Gordura==1])
T_l2 <- sum(dados$Volume[dados$Teor_de_Gordura==2])
T_l3 <- sum(dados$Volume[dados$Teor_de_Gordura==3])
T_l4 <- sum(dados$Volume[dados$Teor_de_Gordura==4])
T_l5 <- sum(dados$Volume[dados$Teor_de_Gordura==5])

T_total <- sum(T_l1+T_l2+T_l3+T_l4+T_l5)

(SQ_lin <- (T_l1^2+T_l2^2+T_l3^2+T_l4^2+T_l5^2)/(GL_lin+1) - T_total^2/(GL_total+1))

### Colunas ----
T_c1 <- sum(dados$Volume[dados$Grau_de_Acidez==1])
T_c2 <- sum(dados$Volume[dados$Grau_de_Acidez==2])
T_c3 <- sum(dados$Volume[dados$Grau_de_Acidez==3])
T_c4 <- sum(dados$Volume[dados$Grau_de_Acidez==4])
T_c5 <- sum(dados$Volume[dados$Grau_de_Acidez==5])

T_total <- sum(T_c1+T_c2+T_c3+T_c4+T_c5)

(SQ_col <- (T_c1^2+T_c2^2+T_c3^2+T_c4^2+T_c5^2)/(GL_col+1) - T_total^2/(GL_total+1))

### Total ----
(SQ_total <- sum(dados$Volume^2) - T_total^2/(GL_total+1))

### Residual ----
(SQ_res <- SQ_total - SQ_trat - SQ_lin - SQ_col)

## Quadrados médios ----

(QM_trat = SQ_trat/GL_trat)

(QM_lin = SQ_lin/GL_lin)

(QM_col = SQ_col/GL_col)

(QM_res = SQ_res/GL_res)

## F calculado ----

(F_trat = QM_trat/QM_res)

(F_lin = QM_lin/QM_res)

(F_col = QM_col/QM_res)

## F tabelado ----

(qf(0.95, GL_trat, GL_res))

(qf(0.95, GL_lin, GL_res))

(qf(0.95, GL_col, GL_res))

## Conferindo resultados ----

a0 <- ExpDes::latsd(dados$Bacilo, dados$Teor_de_Gordura, dados$Grau_de_Acidez, dados$Volume, mcomp="tukey", sigT=0.05)

## Croqui ----
library(ggplot2)

ggplot(dados, aes(x=Grau_de_Acidez, y=Teor_de_Gordura, label=Bacilo)) +
  geom_tile(color="black", fill="white") +
  geom_text(size=5) +
  scale_y_reverse() +
  labs(title="Delineamento Quadrado Latino",
       x="Grau de Acidez",
       y="Teor de Gordura") +
  theme_minimal() +
  theme(axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12),
        plot.title = element_text(size=14, face="bold"))

# Tukey ----

rm(list=ls())
## Dados ----
### Dados dos tratamentos e repetições
tratamentos <- c("A", "B", "C", "D", "E", "F")
repeticoes <- c("I", "II", "III", "IV")

### Valores das medições
valores <- c(15.7, 23.5, 20.5, 25.7, 15.2, 40.4,
             10.0, 28.3, 18.3, 26.7, 17.1, 43.2,
             12.2, 23.4, 20.1, 25.4, 18.2, 45.1,
             13.2, 27.6, 22.5, 27.2, 16.6, 45.0)

### Criando o dataframe
dados <- data.frame(
  Repeticao = rep(repeticoes, each = 6),
  Tratamento = rep(tratamentos, times = 4),
  Cochonilhas = valores
)

### Mostrando o dataframe
print(dados)

## Graus de liberdade ----
GL_trat <- length(unique(dados$Tratamento))-1
GL_rep <- length(unique(dados$Repeticao))-1
GL_total <- length(dados$Cochonilhas)-1

GL_res <- GL_total - GL_trat - GL_rep

## Soma de quadrados ----
### Tratamentos ----
T_t1 <- sum(dados$Cochonilhas[dados$Tratamento=="A"])
T_t2 <- sum(dados$Cochonilhas[dados$Tratamento=="B"])
T_t3 <- sum(dados$Cochonilhas[dados$Tratamento=="C"])
T_t4 <- sum(dados$Cochonilhas[dados$Tratamento=="D"])
T_t5 <- sum(dados$Cochonilhas[dados$Tratamento=="E"])
T_t6 <- sum(dados$Cochonilhas[dados$Tratamento=="F"])

T_total <- sum(T_t1+T_t2+T_t3+T_t4+T_t5+T_t6)

(SQ_trat <- (T_t1^2+T_t2^2+T_t3^2+T_t4^2+T_t5^2+T_t6^2)/(GL_rep+1) - T_total^2/(GL_total+1))

### Repetições ----
T_r1 <- sum(dados$Cochonilhas[dados$Repeticao=="I"])
T_r2 <- sum(dados$Cochonilhas[dados$Repeticao=="II"])
T_r3 <- sum(dados$Cochonilhas[dados$Repeticao=="III"])
T_r4 <- sum(dados$Cochonilhas[dados$Repeticao=="IV"])

T_total <- sum(T_r1+T_r2+T_r3+T_r4)

(SQ_rep <- (T_r1^2+T_r2^2+T_r3^2+T_r4^2)/(GL_trat+1) - T_total^2/(GL_total+1))

### Total ----
(SQ_total <- sum(dados$Cochonilhas^2) - T_total^2/(GL_total+1))

### Residual ----
(SQ_res <- SQ_total - SQ_trat - SQ_rep)

## Quadrados médios ----

(QM_trat = SQ_trat/GL_trat)

(QM_rep = SQ_rep/GL_rep)

(QM_res = SQ_res/GL_res)

## F calculado ----

(F_trat = QM_trat/QM_res)

(F_rep = QM_rep/QM_res)

## F tabelado ----

(qf(0.95, GL_trat, GL_res))

(qf(0.95, GL_rep, GL_res))

## Conferindo resultados ----

a0 <- aov(Cochonilhas ~ Tratamento + Repeticao, data = dados)
a0

a0 <- ExpDes::rbd(dados$Pneu, dados$Bloco, dados$consumo, mcomp="tukey", sigT=0.05)
a1 <- ExpDes::rbd(dados$Tratamento, dados$Repeticao, dados$Cochonilhas, mcomp="tukey", sigT=0.05)

tukey_result <- TukeyHSD(a0, "Tratamento")
print(tukey_result)

## Calculando a diferença mínima significativa ----

(dms <- qf(0.95, GL_trat, GL_res)*sqrt(QM_res/(GL_trat+1)))

# (qf(0.95, 5, 12)*sqrt(296.11/(4)))

med_1 <- mean(dados$Cochonilhas[dados$Tratamento=="A"])
med_2 <- mean(dados$Cochonilhas[dados$Tratamento=="B"])
med_3 <- mean(dados$Cochonilhas[dados$Tratamento=="C"])
med_4 <- mean(dados$Cochonilhas[dados$Tratamento=="D"])
med_5 <- mean(dados$Cochonilhas[dados$Tratamento=="E"])
med_6 <- mean(dados$Cochonilhas[dados$Tratamento=="F"])

medias <- order(c(med_1, med_2, med_3, med_4, med_5, med_6), decreasing = TRUE)

# a
med_6 - med_4

#b
med_4 - med_2
med_4 - med_3

# c
med_3 - med_5

# d
med_5 - med_1

# F - a
# D -  b
# B -  b
# C -   c
# E      d
# A       e

## Tukey exemplo de sala -----
# DQL 5x5, i = 5, j = 5
# SQ_res = 34116
## Delta = q(i, gl_res, alfa)*sqrt(QM_res/rep)
## Delta = q(5, 12, 5%) * sqrt(2.843/5)
## QM_res = SQ_res/GL_res = 34116/12 = 2,843
## q(5, 12, 5%) -> 4,51
## Delta = 107,54
q(0.95, 5, 12)

sqrt(2843/5)

delta = 107.54
ma = 604.8
mb = 509.8
mc = 469.8
md = 394
me = 346.8

ma - mb > delta # a
ma - mc > delta # para o "a", começa o "b"
mb - mc > delta # b
mb - md > delta # para o "b", começa o "c"
mc - md > delta # c
mc - me > delta # para o "c", começa o "d"
md - me > delta # "d"

# ma = 604,8  a
# mb = 509,8  ab
# mc = 469,8   b
# md = 394
# me = 346,8


### Tukey exercício 1

## Delta = qtukey(6, 15, 5%) * sqrt(QM_res/rep)
## Delta = q(6,15, 5%) * sqrt(3,7358/4)
## Delta = 4,44
qtukey(0.95, 6, 15) * sqrt(3.7358/4)

delta = 4.44

mf = 43.425
md = 26.250
mb = 25.700
mc = 20.350
me = 16.775
ma = 12.775

mf - md > delta
## TRUE, para o teste e começa o próximo teste

md - mb > delta
## FALSE, continua b
md - mc > delta
## TRUE, para o b

mb - mc > delta
## TRUE, para o c, começa o próximo

mc - me > delta
## FALSE, continua o d
mc - ma > delta
## TRUE, para o d, começa a próxima

me - ma > delta
## FALSE, continua o e

## Englobei a última letra, para o teste

## mf = 43,425    a
## md = 26,250     b
## mb = 25,700     bc
## mc = 20,350       d
## me = 16,775
## ma = 12,775

## Questão 3.6 ----
## Delta = qtukey(5, 12, 5%) * sqrt(QM_res/5)
## Delta = 4,51
# 111.72/sqrt(3068.1667/5)
# qtukey(0.95, 5, 12)

delta = 111.72

mb = 869
mc = 816
md = 788
me = 710
ma = 679

mb - mc > delta
## Falso, continua a
mb - md > delta
## Falso, continua a
mb - me > delta
## True, para o a, começa o

mc - md > delta
## Falso, continua b
mc - me > delta
## Falso, continua b
mc - ma > delta
## True, para o b, começa o c

md - me > delta
## Falso, continua c
md - ma > delta
## Falso, continua c, como englobamos o último, paramos o teste

# mb  a
# mc  ab
# md  abc
# me   bc
# ma    c
