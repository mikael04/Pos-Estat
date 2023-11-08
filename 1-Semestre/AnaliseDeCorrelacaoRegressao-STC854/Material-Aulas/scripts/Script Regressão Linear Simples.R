#
# EXEMPLO DE REGRESSÃO LINEAR SIMPLES
# 
#
# X: despesas de propaganda
# Y: vendas de um certo produto
dados<- data.frame(
				x=c(1.5,5.5,10,3,7.5,5,13,4,9,12.5,15.5),
				y=c(120,190,240,140,180,150,280,110,210,220,310)
			)
dados
attach(dados)
cor(dados)                                  ## Correlação entre os dados
plot(dados)                                 ## Gráfico de dispersão
#
m1 <- lm(y~x, data=dados)                   ## modelo de regressão
summary(m1)
#
plot(dados)						  ## Gráfico de dispersão com a reta ajustada
abline(m1,col="red")
#
anova(m1)                                   ## Teste F para a regressão
confint(m1)                                 ## Intervalo de confiança para os parâmtros a e b
confint(m1,leve=0.99)  
#
# Intervalo de confiança para a resposta média E(yi/7)
x0 = data.frame(x=7)
predict(m1,x0,interval="confidence")
#
# Intervalo de confiança para a resposta individual de y (x=7)
predict(m1,x0,interval="prediction")
#
# Análise dos resíduos
# Para exibir os Valores Ajustados e os Resíduos do ajuste, digite os comandos:
m1$residuals                                ## ou residuals(m1)
m1$fitted.values                            ## ou fitted(m1)
#
#
# Análise geral de Diagnóstico
#
plot(m1)
#
# Diagnóstico de Homocedasticidade
plot(fitted(m1),residuals(m1),xlab="Valores Ajustados",ylab="Resíduos")  
abline(h=0,col="red")
#
plot(dados$x,residuals(m1),xlab="Despesas",ylab="Resíduos")                   
abline(h=0,col="red")
#
#A distribuição dos resíduos é aleatória, o modelo linear é adequado 
#e a variância dos erros pode ser considerada constante
#
# Testes para Homocedasticidade
library(lmtest)                     ## Pacote para o teste   Breusch-Pagan
BP <- bptest(m1$residuals~dados$x)        ## Teste Breusch-Pagan
BP
#
# Para o teste de homocedasticidade, não rejeitamos a hipótese nula de que os resíduos têm variância constante
# p-valor > alfa.
#
# Diagnóstico de Normalidade
qqnorm(m1$residuals, ylab="Resíduos",xlab="Quantis teóricos",main="")
qqline(m1$residuals,col="red")
#
# Testes para Normalidade
SW <- shapiro.test(residuals(m1))   ## Teste Shapiro wilk
SW
# 
library(nortest)                    ## Pacote para o teste Andreson Darling, Lilliefors)      
AD <- ad.test(residuals(m1))        ## Teste Anderson Darling
AD
L <- lillie.test(residuals(m1))     ## Teste Lilliefors
L
P <- pearson.test(residuals(m1))    ## Teste Qui Quadrado de Pearson
P
# Para todos os testes de normalidade, não rejeitamos a hipótese nula de que os resíduos têm distribuição normal
# p-valor > alfa. Com exceção do teste Qui quadrado de Pearson.
#
# Teste para indepedência
DW <- dwtest(m1$residuals~dados$x)
DW
# Não rejeitamos a hipótese nula de que os resíduos são independentes, p-valor > alfa.

