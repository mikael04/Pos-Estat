######################################################################################
#                             MODELO DE REGRESSAO LINEAR MULTIPLO
########################################################################################
# EXEMPLO 1:
#
# Y: número de itens produzidos com defeito, em certo dia
# X1: produção média por hora
# X2: tempo, em semanas, decorrido desde o último reparo na máquina

X1=c(18,16,25,12,20,35,17,25,39,20,18,29,20,16,29)
X2=c(2,3,2,3,3,2,1.5,5,1,2.5,2,3.5,5,1,1.5)
m <- c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1)

# Encontrando os coeficientes B'=(X'X)-1 X'Y
A <- cbind(m,X1,X2)
A
# df_A <- data.frame(m, X1, X2)
B <- t(A)                  # B é a transposta de X
B
# t(df_A)
C <- B%*%A                 # C é o produto X'X
C
P <- solve(C)              # inversa de X'X
P
Y <- c(12,3,11,1,13,20,2,25,26,15,1,15,11,5,7)    # vetor
D <- B%*%Y             # matriz X'Y
D
#
# Coeficientes
Coe <- P%*%D
Coe
#
#
mod1 <- lm(Y~X1+X2)          # modelando as vari?veis
summary(mod1)              # valores incividuais e testes individuais dos coeficientes
ANOVA <- anova(mod1)                # ANOVA (R2 e Rajustado)
ANOVA
Ft <- qf(0.95, 1, 13)
Ft

#####
#
FC <- sum(Y)^2/15         #Fator de Correção
FC
SQReg <- t(Coe)%*%D - FC
SQReg
SQTotal <- t(Y)%*%Y - FC
SQTotal
SQRes <- SQTotal - SQReg
SQRes

#
# Intervalo de confian?a de cada parâmetro
b0 <- Coe[1,1]
b0
b1 <- Coe[2,1]
b1
b2 <- Coe[3,1]
b2
QMres <- ANOVA[[3]][[3]]
QMres
#
#
ttab <- qt(.975,df=14-2)      #obtendo o valor do t tabelado (n-p=14-2)
ttab
#


# Teste de Hipótese para cada coeficiente
# bo
MatrizC <- P
MatrizC
#
#Teste individual para b0
c11 <- MatrizC[1,1]
c11
t0 = b0/(QMres*c11)^0.5
t0
#Teste individual para b1
c22 <- MatrizC[2,2]
c22
t0 = b1/(QMres*c22)^0.5
t0
#Teste individual para bo
c33 <- MatrizC[3,3]
c33
t0 = b2/(QMres*c33)^0.5
t0
# Intervalo de Confiança para os parâmetros da regressão
#
icb0i <- b0 - ttab*((QMres*P[[1]])^0.5)
icb0i
icb0s <- b0 + ttab*((QMres*P[1,1])^0.5)
icb0s
paste0("[ ", icb0i, ", ", icb0s, " ]")
#
icb1i <- b1 - ttab*((QMres*P[2,2])^0.5)
icb1i
icb1s <- b1 + ttab*((QMres*P[2,2])^0.5)
icb1s
paste0("[ ", icb1i, ", ", icb1s, " ]")

#
icb2i <- b2 - ttab*((QMres*P[3,3])^0.5)
icb2i
icb2s <- b2 + ttab*((QMres*P[3,3])^0.5)
icb2s
icb2s
paste0("[ ", icb2i, ", ", icb2s, " ]")
#
# Intervalo de confian?a para a resposta individual de y (1,13,3.2)
#
xo <- c(1,13,3.2)
#
yo = -14.7366 + (0.8856*13)+(2.3083*3.2)
yo
# ou
yo <- xo%*%Coe
yo
#
v	<- QMres*t(xo)%*%(P)%*%(xo)     # estimativa da vari?ncia de yo (x'0 * (x'x)^-1 * x0)
v
#
# Intervalo de confian?a
ici <- yo - ttab*(v)^0.5
ici
ics <- yo + ttab*(v)^0.5
ics
##
# Analise dos resíduos
par(mfrow = c(2, 2))
plot(mod1)
#
res <- (residuals(mod1))
res
# Resíduos em papel de Probabilidade Normal
c <- qqnorm(res,col = 2, pch = 19, datax=T)
linha <- qqline(res, datax=T)
#
# Resíduos versus valores ajustados
valorj <- mod1$fitted.values
valorj
plot(valorj, res, ylab="Residuos", xlab="Valores Ajustados",
     main="Analise Residual")
#
# Resíduos versus ordem de coleta
plot(res,pch=16,ylab="Residuals")
#
# Resíduos versus cada regressora incluída no modelo
par(mfrow=c(1,2))
plot(X1,res,pch=16,ylab="Residuals")
plot(X2,res,pch=16,ylab="Residuals")
#
# Res�duos parciais versus Xij para cada Xj no modelo
par(mfrow=c(2,2))
resparc1 <- mod3$residuals + 0.87*X1
resparc2 <- mod3$residuals + 2.424*X2
plot(X1,resparc1,pch=16,ylab="Parcial Residuals")
plot(X2,resparc2,pch=16,ylab="Parcial Residuals")
#
# Resíduos versus interações não incluídas no modelo
plot(X1*X2,res,pch=16,ylab="Residuals")
#
# Gráfico de Xi versus Xj
plot(X1,X2)
