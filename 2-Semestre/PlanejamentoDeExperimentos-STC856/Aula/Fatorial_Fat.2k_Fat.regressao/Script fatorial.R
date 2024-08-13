##################################################################################################
				######    AN?LISE FATORIAL       ########
##################################################################################################
#
library(ExpDes)     ## Para Anava
#
##################################### Exemplo 1: Intera??o n?o significativa  ####################
#
# dados <- read.table("Reagentes.txt",h=T)
dados <- read.table("2-Semestre/PlanejamentoDeExperimentos-STC856/Aula/Fatorial_Fat.2k_Fat.regressao/Reagentes.txt",h=T)
dados
summary(dados)
attach(dados)
#
#####################################  Estudo das intera??es   ###################################
#
#Em experimentos fatoriais ? importante verificar se existe intera??o
#entre os fatores. Inicialmente vamos fazer isto graficamente e mais a
#frente faremos um teste formal para presen?a de intera??o
#
par(mfrow=c(2,2))
with(dados,interaction.plot(Reagentes,Catalizador,Tempo,ylab="m?dias",xlab="Reagentes",
     xpd=F,lty=1,col=1:2))
with(dados,interaction.plot(Catalizador,Reagentes,Tempo,ylab="m?dias",xlab="Catalizador",
     xpd=F,lty=1,col=1:2))
#
###################################  An?lise de vari?ncia e desdobramento (DIC) ####################
#
mdic <- fat2.crd(Reagentes, Catalizador, Tempo, quali=c(TRUE,TRUE), mcomp="tukey",
fac.names=c("Reagentes","Catalizador"),sigT = 0.05,sigF = 0.05)
#
########################################   Outra forma ANOVA  ###################################
#
# OBJETIVO: Analisar os res?duos
#
m1 <- aov(Tempo~Reagentes*Catalizador,data=dados)
summary(m1)
#
######################################## An?lise dos res?duos   ###################################
# An?lise gr?fica dos res?duos
#
par(mfrow=c(2,2))
plot(m1)
#
shapiro.test(residuals(m1))
bartlett.test(residuals(m1)~Reagentes)
bartlett.test(residuals(m1)~Catalizador)
#
######################################  Exemplo 2 : Intera??o significativa #########################
#
dados <- read.table("2-Semestre/PlanejamentoDeExperimentos-STC856/Aula/Fatorial_Fat.2k_Fat.regressao/Exerciciorecipientes1.txt",h=T)
dados
summary(dados)
attach(dados)
#
#####################################  Estudo das intera??es   ###################################
#
par(mfrow=c(2,2))
with(dados,interaction.plot(Recipientes,Especies,Altura,ylab="m?dias",xlab="Recipiente",
     xpd=F,lty=1,col=1:2))
with(dados,interaction.plot(Especies,Recipientes,Altura,ylab="m?dias",xlab="Especie",
     xpd=F,lty=1,col=1:3))
#
###################################  An?lise de vari?ncia e desdobramento (DIC) ####################
#
mdic <- fat2.crd(Recipientes,Especies, Altura, quali=c(TRUE,TRUE), mcomp="tukey",
fac.names=c("Recipientes","Esp?cies"),sigT = 0.05,sigF = 0.05)
#
#
########################################   Outra forma ANOVA  ###################################
#
# OBJETIVO: Analisar os res?duos
#
m2 <- aov(Altura~Recipientes*Especies,data=dados)
summary(m2)
#
######################################## An?lise dos res?duos   ###################################
# An?lise gr?fica dos res?duos
#
par(mfrow=c(2,2))
plot(m2)
#
par(mfrow=c(2,2))
plot(Recipientes,residuals(m2))
title("Residuos vs Recipientes")
plot(Especies,residuals(m2))
title("Residuos vs Especies")
#
#
shapiro.test(residuals(m2))
bartlett.test(residuals(m2)~Recipientes)
bartlett.test(residuals(m2)~Especies)
#
#
###################################  ANOVA PARA DBC ##############################################
# Considerando o exemplo (recipientes) como um fatorial em DBC
#
mdbc <- fat2.rbd(Recipientes, Especies, Blocos, Altura ,quali=c(TRUE,TRUE), mcomp="tukey",
fac.names=c("Recipientes","Especies","Bloco"), sigT = 0.05, sigF = 0.05)
#
m3 <- aov(Altura~Blocos+Recipientes*Especies,data=dados)
summary(m3)
####################################################################################################

##############################################################################
#
#    FATORIAL COM REGRESSÃO                ####################################
##############################################################################
setwd("C:/Users/user/Desktop")
dadosfat <- read.table("fat_regressao.txt",h=T)
dadosfat
summary(dadosfat)
attach(dadosfat)
#
variedades <- as.factor(variedade)
doses <- as.numeric(dose)
#
mdic <- fat2.crd(variedades, doses, prod, quali=c(TRUE,FALSE), mcomp="tukey",
                 fac.names=c("Variedades","Doses"),sigT = 0.05,sigF = 0.05)
#
# Presupostos
m1 <- aov(prod~variedades*doses,data=dadosfat)
summary(m1)
#
# Analise grafica dos residuos
#
par(mfrow=c(2,2))
plot(m1)
#
shapiro.test(residuals(m1))
bartlett.test(residuals(m1)~variedades)
bartlett.test(residuals(m1)~doses)


