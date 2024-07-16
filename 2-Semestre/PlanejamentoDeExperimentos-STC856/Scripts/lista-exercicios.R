# Exemplo DIC ----

## Dados ----

rm(list=ls())
setwd("2-Semestre/PlanejamentoDeExperimentos-STC856/Scripts/")
dados <- read.table('data-raw/ExemploDic.txt', header=TRUE)

## Graus de Liberdade ----

GL_trat <- length(unique(dados$medicamentos))-1
GL_total <- length(dados$pressao)-1

GL_res <- GL_total - GL_trat

## Soma de quadrados ----
T_a <- sum(dados$pressao[dados$medicamentos=="A"])
T_b <- sum(dados$pressao[dados$medicamentos=="B"])
T_c <- sum(dados$pressao[dados$medicamentos=="C"])
T_d <- sum(dados$pressao[dados$medicamentos=="D"])
T_e <- sum(dados$pressao[dados$medicamentos=="E"])
T_t <- sum(dados$pressao[dados$medicamentos=="T"])

T_total <- sum(T_a+T_b+T_c+T_d+T_e+T_t)

(SQ_trat <- (T_a^2+T_b^2+T_c^2+T_d^2+T_e^2+T_t^2)/(GL_trat+1) - T_total^2/(GL_total+1))

(SQ_total <- sum(dados$pressao^2) - T_total^2/30)

(SQ_res <- SQ_total - SQ_trat)

## Quadrado médio ----

(QM_trat = SQ_trat/GL_trat)

(QM_res = SQ_res/GL_res)

## F calculado ----

(F0 = QM_trat/QM_res)

## F tabelado ----

(qf(0.95, GL_trat, GL_res))

## Conferindo resultado ----

a0 <- ExpDes::crd(dados$medicamentos, dados$pressao, quali = TRUE, mcomp="tukey", sigF = 0.05)

# Exemplo DBC ----

## Dados ----

rm(list=ls())
dados <- read.table('data-raw/ExemploDbc.txt', header=TRUE)

## Graus de liberdade ----
GL_trat <- length(unique(dados$Pneu))-1
GL_bloc <- length(unique(dados$Bloco))-1
GL_total <- length(dados$consumo)-1

GL_res <- GL_total - GL_trat - GL_bloc

## Soma de quadrados ----
### Tratamentos ----
(T_t1 <- sum(dados$consumo[dados$Pneu=="A"]))
(T_t2 <- sum(dados$consumo[dados$Pneu=="B"]))
(T_t3 <- sum(dados$consumo[dados$Pneu=="C"]))
(T_t4 <- sum(dados$consumo[dados$Pneu=="D"]))

### Blocos ----
(T_b1 <- sum(dados$consumo[dados$Bloco=="I"]))
(T_b2 <- sum(dados$consumo[dados$Bloco=="II"]))
(T_b3 <- sum(dados$consumo[dados$Bloco=="III"]))

(T_total <- sum(T_t1+T_t2+T_t3+T_t4))
(T_total <- sum(T_b1+T_b2+T_b3))

(SQ_trat <- (T_t1^2+T_t2^2+T_t3^2+T_t4^2)/(GL_bloc+1) - T_total^2/(GL_total+1))


(SQ_bloc <- (T_b1^2+T_b2^2+T_b3^2)/(GL_trat+1) - T_total^2/(GL_total+1))

(SQ_total <- sum(dados$consumo^2) - T_total^2/(GL_total+1))

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

media_geral <- mean(dados$consumo)

(CV <- sqrt(QM_res)/media_geral*100)

## Conferindo resultados ----

a0 <- ExpDes::rbd(dados$Pneu, dados$Bloco, dados$consumo, mcomp="tukey", sigT=0.05)

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
