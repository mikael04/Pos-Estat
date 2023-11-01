#######################################################################################
#                             MRLM -  Técnicas computacionais para seleção de vaiáveis
########################################################################################
# EXEMPLO Pág 33 - Apostila Giolo
# 
#
require(car)
dados<- data.frame(
				Y=c(78.5,74.3,104.3,87.6,95.9,109.2,102.7,72.5,93.1,115.9,83.8,113.3,109.4),
				X1=c(7,1,11,11,7,11,3,1,2,21,1,11,10),
                        X2=c(26,29,56,31,52,55,71,31,54,47,40,66,68),
				X3=c(6,15,8,8,6,9,17,22,18,4,23,9,8),
				X4=c(60,52,20,47,33,22,6,44,22,26,34,12,12)
			)
dados
attach(dados)
#
# Matriz de correlação simples
cor(dados)
# a) Ajuste os modelos:
completo <- lm(Y~X1+X2+X3+X4,data=dados)## modelo completo                           
completo <- lm(Y~.,data=dados)
completo
#
nulo <- lm(Y~1,data=dados)             ## modelo nulo
nulo
#

###########################################################################################
# FORWARD              ### modelo pra frente
modfor <- step(nulo, scope=list(lower=nulo, upper=completo),
               scale = 5, data=dados,direction="forward",trace=1,
               keep = NULL, steps = 1000, k = 2,test = 'F')
#
# BACKWARD              ### modelo de trás para frente			
modbac <- step(completo, scope=list(lower=nulo, upper=completo),
               scale = 17, data=dados,direction="backward",trace=1,
               keep = NULL, steps = 1000, k =2, test = 'F')
#
# STEPWISE				      ### passo a passo
modstep <- step(nulo, scope=list(lower=nulo, upper=completo),
               scale = 1, data=dados,direction="both",trace=1,
               keep = NULL, steps = 1000, k = 2,test = 'F')
#

modstep <- step(nulo, data=dados,direction="both")
#

########################################################################################
