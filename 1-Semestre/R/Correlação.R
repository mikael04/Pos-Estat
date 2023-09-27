############################################################################################################################
##  Coeficiente de correlacao pearson, sperman, parcial e multiplo
#############################################################################################################################
#
# Exemplo 1.1
x <- c(5,8,10,12,15)   					## frequencia na escola
y <- c(10,30,45,50,75) 					## numero de livros particular
plot(x,y)              				  ## Plotar diagrama de dispersao
cor(x,y)                        ## Comando para calcular o coefciente de correlacao de pearson
#
cor.test(x,y,alternative = c("less"),	 ## Comando para calcular o coefciente de correlacao de pearson e teste de correlacao)
         method = c("pearson"),
         exact = NULL, conf.level = 0.95)
#
#
# Exemplo do calculo da correlacao utilizando a covariancia
cov(x,y)                                         ## Covariancia entre x e y
var(x)                                           ## Variancia de x
var(y)                                           ## Variancia de y
rxy = cov(x,y)/((var(x)*var(y))^0.5)             ## Coeficiente de correlacao de Pearson
rxy
#
# Exemplo 1.2
a <- c(7,4,2,6,1,3,8,5)					 ## classificacao dos professores
b <- c(44,72,69,70,93,82,67,80)                  ## nota no curso
cor(a,b,method = c("spearman"))                  ## Comando para calcular o coefciente de correlacao de spearman
cor.test(a,b,alternative = c("two.sided"),       ## Comando para calcular o coefciente de correlacao de spearman e teste de correlacao)
         method = c("spearman"),
         exact = NULL, conf.level = 0.95)
#
# TAREFA: Entregar o exercicio 1.4 da apostila (Nao precisa ser pelo software, mas
#para melhor aprendizado sugere-se aplicar as tecnicas).
#
####  Coeficiente de correlacao parcial entre 3 variaveis
#
# Exemplo 2.1 - tem-se peso, altura e idade de 10 meninos
#X1 - peso (Kg)
#X2 - altura (cm)
#X3 - idade(anos)
#
X1 <- c(30,32,24,30,26,35,25,23,35,31)
X2 <- c(145,150,125,157,127,140,132,107,155,145)
X3 <- c(7,10,7,11,8,10,10,6,12,9)
#
# Correla??es simples
cor(X1,X2)
cor(X1,X3)
cor(X2,X3)
#
# Correlacoes de primeira ordem
library(ppcor)						## pacote para realizar o calculo da correlacao parcial
## O comando pcor.test, calcula a correlacao parcial entre cada par de variavel, fixando a terceira
# e o p-valor do teste
pcor.test(X1,X2,X3)  				## coef. de correlacao parcial entre os grupos (1,2) vc 3
pcor.test(X2,X3,X1)					## coef. de correlacao parcial entre os grupos (2,3) vc 1
pcor.test(X1,X3,X2)					## coef. de correlacao parcial entre os grupos (1,3) vc 2
#
# Coeficiente de correlacao parcial envolvendo mais de 3 variaveis
# setwd("C:/Users/analu/Desktop/AULAS - REDE/STC - 854 - 02_2020/Correlacao")
dados <- read.table(here::here("Dados/Material-Aulas/Material aula software R-20230822/dadoscor.txt"), head=T)
attach(dados)
pcor.test(Var1,Var2,list(Var3,Var4))
pcor.test(Var1,Var3,list(Var2,Var4))
pcor.test(Var1,Var4,list(Var2,Var3))
#
# Coeficiente de correlacao multipla
dados <- data.frame(
				X1=c(30,32,24,30,26,35,25,23,35,31),
				X2=c(145,150,125,157,127,140,132,107,155,145),
				X3=c(7,10,7,11,8,10,10,6,12,9)
			)
dados
attach(dados)                                   ## funcao para tornar as colunas do data frame como objeto
cor(dados)
plot(dados)
m1 <- lm(X1~X2+X3, data=dados)
summary(m1)
#
# Coeficiente de correlacao multiplo R
R = sqrt(0.6694)
