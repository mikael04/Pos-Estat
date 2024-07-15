############################################################################################
## Aula STC 1084  
############################################################################################
# Pacotes exigidos
#
library(ExpDes)     ## Para Anova
library(agricolae)  ## Para analisar os pressupostos 
#install.packages("agricolae")

#
## Exemplo de DIC
############################################################################################
# Leitura dos dados
dados <- read.table('ExemploDic.txt', header=TRUE)
dados
attach(dados)
str(dados)                ## mostra a estrutura (síntese) do objeto
#
#------------------------------------------------------------------------------------------
# Fazendo a análise de variância
#require(ExpDes)           # pacote necessário para realizar a ANAVA
a0 <- crd(medicamentos, pressao, quali = TRUE, mcomp="tukey", sigF = 0.05)
#
#------------------------------------------------------------------------------------------
# Análise gráfica dos resíduos
a1 <- aov(pressao~medicamentos)
anova(a1)
#
par(mfrow=c(2,2))
plot(a1)
#
#------------------------------------------------------------------------------------------
## Teste das pressuposições da análise de variância
shapiro.test(residuals(a1))
bartlett.test(residuals(a1),medicamentos)
#
#############################################################################################
### Exemplo de DIC para dados desbalanceados
#############################################################################################
rm(list=ls(all=TRUE))
#
# Leitura dos dados
dados <- read.table('ExemploDicInc.txt', header=TRUE)
dados
attach(dados)
str(dados)
#
a1 <- crd(vendedor, vendas, quali = TRUE, mcomp="tukey", sigF = 0.05)
#
# Análise gráfica dos resíduos
a1 <- aov(vendas~vendedor)
anova(a1)
#
par(mfrow=c(2,2))
plot(a1)
#
#------------------------------------------------------------------------------------------
## Teste das pressuposições da análise de variância
shapiro.test(residuals(a1))
bartlett.test(residuals(a1),vendedor)
#
#############################################################################################
### Exemplo de DBC
#############################################################################################
#
rm(list=ls(all=TRUE))
dados <- read.table('ExemploDbc.txt', header=TRUE)
dados
attach(dados)
str(dados)
#
#------------------------------------------------------------------------------------------
# Fazendo a análise de variância
a0 <- rbd(Pneu, Bloco, consumo, quali = TRUE, mcomp="lsdb", sigF = 0.05)
#------------------------------------------------------------------------------------------
# Análise gráfica dos resíduos
a1 <- aov(consumo~Pneu+Bloco)
anova(a1)
#
par(mfrow=c(2,2))
plot(a1)
#
#------------------------------------------------------------------------------------------
## Teste das pressuposições da análise de variância
shapiro.test(residuals(a1))
bartlett.test(residuals(a1),Pneu)
#
#------------------------------------------------------------------------------------------
############################################################################################
## Exemplo de DQL
############################################################################################
rm(list=ls(all=TRUE))
# Leitura dos dados
dados <- read.table('exemploDQL.txt', header=TRUE)
dados
attach(dados)
str(dados)                ## mostra a estrutura (síntese) do objeto
#
#------------------------------------------------------------------------------------------
# Fazendo a análise de variância
a0 <- latsd(trat, linha, coluna, resp, quali = TRUE, mcomp = "tukey", sigT = 0.05, sigF = 0.05)
#
#Outra forma de fazer a análise de variância
a1 <- lm(resp ~ trat+linha+coluna, data=dados)
anova(a1)
#
# Análise gráfica dos resíduos
par(mfrow=c(2,2))
plot(a1)
#
#------------------------------------------------------------------------------------------
# Teste das pressuposições da análise de variância
shapiro.test(residuals(a1))
bartlett.test(residuals(a1),trat)
#
#############################################################################################
### Testes de comparações múltiplas usando o pacote ExpDes
#############################################################################################
#
# "tukey" - test of Tukey
# "lsd" - teste t
# "lsdb" - test with Bonferroni
# "duncan" - test of Duncan
# "snk" - test of Student-Newman-Keuls
# "sk" - test of Scott-Knott
#
#############################################################################################
