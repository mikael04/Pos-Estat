#
# EXEMPLO DE REGRESS?O LINEAR SIMPLES
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
cor(dados)                                  ## Correla??o entre os dados
plot(dados)                                 ## Gr?fico de dispers?o
#
m1 <- lm(y~x, data=dados)                   ## modelo de regress?o
summary(m1) # Testa os coeficientes individuais (teste t)
#
plot(dados)						  ## Gr?fico de dispers?o com a reta ajustada
abline(m1,col="red")
#
anova(m1)       # Testa da regressão (teste f)  ## Teste F para a regress?o
confint(m1)                                 ## Intervalo de confian?a para os par?mtros a e b
confint(m1,leve=0.99) 
# R <- coef corr múltipla
# r <- coef corr simples
#
# Intervalo de confian?a para a resposta m?dia E(yi/7) # Para um modelo calculado
x0 = data.frame(x=7)
predict(m1,x0,interval="confidence")
pred <- predict(m1,x0,interval="confidence")
#
# Intervalo de confian?a para a resposta individual de y (x=7) # Para todos os modelos calculados na populacao
predict(m1,x0,interval="prediction")
#
# An?lise dos res?duos
## Análise dos resíduos
## - Precisam ser normais
## - Independente
## - Homocedásticos (variância constante) 
## res ~ Normal (0, var constante)
# Para exibir os Valores Ajustados e os Res?duos do ajuste, digite os comandos:
m1$residuals                                ## ou residuals(m1) resíduos
m1$fitted.values                            ## ou fitted(m1)    valores preditos
#
#
# An?lise geral de Diagn?stico
#
par(mfrow = c(2,2))
plot(m1)
#
# Diagn?stico de Homocedasticidade
plot(fitted(m1),residuals(m1),xlab="Valores Ajustados",ylab="Resíduos")  
abline(h=0,col="red")
#
plot(dados$x,residuals(m1),xlab="Despesas",ylab="Resíduos")                   
abline(h=0,col="red")
#
# A distribui??o dos res?duos ? aleat?ria, o modelo linear é adequado 
# e a vari?ncia dos erros pode ser considerada constante
#
# Testes para Homocedasticidade
library(lmtest)                     ## Pacote para o teste   Breusch-Pagan
BP <- bptest(m1$residuals~dados$x)        ## Teste Breusch-Pagan
BP
#
# Para o teste de homocedasticidade, n?o rejeitamos a hip?tese nula de que os res?duos t?m vari?ncia constante
# p-valor > alfa. -> hipótese nula de que os resíduos TEM variancia constante
#
# Diagn?stico de Normalidade
qqnorm(m1$residuals, ylab="Resíduos",xlab="Quantis teóricos",main="")
qqline(m1$residuals,col="red")
#
# Testes para Normalidade
SW <- shapiro.test(residuals(m1))   ## Teste Shapiro wilk
SW ## p-valor -> H0: os resíduos são normais, H1: os resíduos não são normais
## Nesse caso, aceitamos a hipótese nula
# 
library(nortest)                    ## Pacote para o teste Andreson Darling, Lilliefors)      
AD <- ad.test(residuals(m1))        ## Teste Anderson Darling
AD
L <- lillie.test(residuals(m1))     ## Teste Lilliefors
L
P <- pearson.test(residuals(m1))    ## Teste Qui Quadrado de Pearson
P
# Para todos os testes de normalidade, n?o rejeitamos a hip?tese nula de que os res?duos t?m distribui??o normal
# p-valor > alfa. Com exce??o do teste Qui quadrado de Pearson.
#
# Teste para indeped?ncia
DW <- dwtest(m1$residuals~dados$x)
DW
# N?o rejeitamos a hip?tese nula de que os res?duos s?o independentes, p-valor > alfa.
# H0 -> resíduos são independentes
# H1 -> resíduos não são independentes
# p-valor > 0.05, aceita-se a hipótese nula


# PRECISA TESTAR NORMALIDADE (shapiro), TESTAR VARIÂNCIA () e testar HOMOCEDASTICIDADE

