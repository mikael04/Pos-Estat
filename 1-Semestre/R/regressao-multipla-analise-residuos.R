#######################################################################################
#                             MODELO DE REGRESSAO LINEAR MULTIPLO
########################################################################################
# EXEMPLO 1:
#
# X1: temperatura
# X2: concentra??o
# Y: produ??o do processo qu?mico
dados<- data.frame(
  X1=c(80,100,120,140,160,80,100,120,140,160,80,100,120,140,160,80,100,120,140,160),
  X2=c(10,10,10,10,10,15,15,15,15,15,20,20,20,20,20,25,25,25,25,25),
  Y=c(189,203,222,234,261,204,212,223,246,273,220,228,252,263,291,226,232,259,268,294))
dados
attach(dados)
#
# a) Ajuste os modelos:
mod1 <- lm(Y~X1)
mod1
summary(mod1)
anova(mod1)
#
mod2 <- lm(Y~X2)
mod2
summary(mod2)
anova(mod2)
#
mod3 <- lm(Y~X1+X2)
mod3
summary(mod3)
anova(mod3)
#
mod4 <- lm(Y~X2+X1)
mod4
summary(mod4)
anova(mod4)
#
X3 <- X1*X2
mod5 <- lm(Y~X1+X2+X3)
mod5
summary(mod5)
anova(mod5)
#
mod6 <- lm(Y~X2+X1+X3)
mod6
summary(mod6)
anova(mod6)
#
# b) Observando os resultados: Quadro I
#Modelos ajustados           Estimativas dos par?metros      QMres   R2    R2a
#                          bo     b1  	b2      b3
#1. Y em X1             135,60 0,870 	-        - 	      238,5 0,7383 0,7237
#2. Y em X2 		197,58   - 		2,424             707,3 0,2239 0,1808
#3. Y em X1 e X2 		93,18 0,870       2,424     -		36,5	0,9621 0,9577
#4. Y em X2 e X1 		93,18 0,870       2,424     -		36,5 	0,9621 0,9577
#5. Y em X1, X2 e X1X2 	93,34 0,877       2,472   -0,0004 	38,8  0,9621 0,9550
#6. Y em X2, X1 e X1X2 	93,34 0,877       2,472   -0,0004 	38,8  0,9621 0,9550
#Em uma an?lise preliminar, e dos resultados apresentados no Quadro I, ??? poss???vel observar que os modelos 5 e 6 (com intera??????o entre X1 e X2) apresentam QMres e R2 muito
#similares aos modelos 3 e 4. Isso indica que a intera??o n?o est? trazendo uma contribui??????o
#significativa e, portanto, n?o deve fazer sentido mant?-la no modelo. Os modelos 3 e 4
#parecem ser as melhores op??es dentre os modelos analisados, pois para ambos tem-se QMres pequeno e, ainda, R2 = 0,9621, o que significa que 96,21% da variabilidade total de Y
#estaria sendo explicada pelas regressoras X1 e X2.. Modelo indicado: Y em X1 e X2.
#
# c) Representa??o matricial
#
# d) i) Fixando x2=10 e ii) x2=25?
#E(Y|x1,x2=10)=93,18 + 0,87x1 + 2,424*10 = 117,42 + 0,87x1
#
#    i) Fixando x1=100 e ii) x1=130
#E(Y|x1=100,x2)=93,18 + 0,87*100 + 2,424x2 = 180,18 + 2,424x2
#
# e) Interpreta??o dos par?metros
#i) ??o ? o intercepto do plano com o eixo Y. N?o apresenta interpreta??????o pr???tica nesse estudo.
#ii)  ??1 = 0,87 indica um aumento esperado na produ??o do processo qu???mico de 0,87 unidades
#a cada acr?scimo de uma unidade na temperatura, mantida a concentra??????o fixa em 10 ou
#outro valor poss?vel.
#iii)  ??2 = 2,424 indica um aumento esperado na produ??o do processo qu???mico de 2,424
#unidades a cada acr?scimo de uma unidade na concentra??o, mantida a temperatura fixa em
#100 ou outro valor poss?vel.
#
# f) Estiamtiva da vari?ncia
#s2	= QMres = 36,5
#
# g) Valores preditos
pred <- cbind(Y,mod3$fitted.values)
pred
#
# h) Testa as hip?teses dos par?metros (conjuntamente)
summary(mod3)
#F-statistic: 215.9 on 2 and 17 DF, p-value: 8.234e-13
#??Conclus??o: os p-valores obtidos s?o pequenos o suficiente para que se conclua pela
#rejei??o das hip?teses nulas H0:  ??j = 0 para j = 1, 2.
#
# i) Testa as hip?teses dos par?metros (individualmente)
summary(mod3)
#
# j) Intervalo de Confian?a de 95% para os coeficientes
confint(mod3)
#Conclus?o: Como o valor zero n?o pertence a nenhum dos intervalos, h???
#evid?ncias para rejei??o de H0:  ??2j = 0 para j = 0, 1 e 2.
#
# k) Intervalo de confian?a para a resposta m?dia de y em x1=80 e x2=10
#?Para x1 = 80 e x2 = 10 tem-se: Y^ = 187,02 e V(Y^ ) =  8,76. Assim,
#I.C.95% = (187,02 - 2,11(2,96); 187,02 + 2,11(2,96))= (180,77; 193,26)
#
new <- data.frame(cbind(80,10))
predict(mod3,new,interval="confidence")
predict(mod3,interval="confidence")
#
# l) Obtenha os coeficientes de determina??o simples e parcial.
#r2Y1 = 0,7382
#r2Y2 = 0,2238
#r2Y2.1 = SQE(X2 | X1)/SQres(X1) = 3672,4 / 4293,6 = 0,855
#r2Y1.2 = SQE(X1 | X2)/SQres(X2) = 12110,4 / 12731,6 = 0,951

summary(mod1)
summary(mod2)
summary(mod3)
anova(mod3)
anova(mod1)
#Assim, ao ser adicionada a regressora X1 ao modelo que n?o cont?m nenhuma
#regressora, a SQres (que, nesse caso, ? a SQtotal) ? reduzida em 73,82% e, quando
#X2 ? adicionada ao modelo que cont?m X1, a SQres(X1) ? reduzida em 85,5%.
#Analogamente, adicionar X1 ao modelo que cont?m X2, faz com que a SQres(X2) seja
#reduzida em 95,1%.
#
# m) Obtenha os coeficientes de correla??o simples e parcial.
# basta tirar a ra?z quadrada tdo coeficiente de determina??o
#rY1 = 0,859
#rY2 = 0,473
#rY2.1 = 0,9246
#rY1.2 = 0,9752
#
# n) Obtenha os res?duos e fa?a uma an?lise gr?fica dos mesmos.
resid<- mod3$residuals
resid
# Res?duos versus preditos, sqrt{|res?duos|} versus preditos, Normal Q-Qplot
#e dist?ncia de Cook
par(mfrow=c(2,2))
plot(mod3, which=c(1:4),pch=16, add.smooth=FALSE, id.n = 0)
#
# Res?duos versus ordem de coleta
plot(mod3$residuals,pch=16,ylab="Residuals")
#
# Res?duos versus cada regressora inclu?da no modelo
par(mfrow=c(2,2))
plot(X1,mod3$residuals,pch=16,ylab="Residuals")
plot(X2,mod3$residuals,pch=16,ylab="Residuals")
#
# Res?duos parciais versus Xij para cada Xj no modelo
par(mfrow=c(2,2))
resparc1 <- mod3$residuals + 0.87*X1
resparc2 <- mod3$residuals + 2.424*X2
plot(X1,resparc1,pch=16,ylab="Parcial Residuals")
plot(X2,resparc2,pch=16,ylab="Parcial Residuals")
#
# Res?duos versus intera??es n?o inclu?das no modelo
plot(X1*X2,mod3$residuals,pch=16,ylab="Residuals")
#
# Gr?fico de Xi versus Xj
plot(X1,X2)
cor(X1,X2)
#
# o) Medidas para detec??o de multicolinearidade
X <- as.matrix(cbind(X1,X2))
X
rxx  <- cor(X)
rxx
det(rxx)
eigen(rxx)[1]
#
# VIF1 = 1 / (1 - 0) = 1 e VIF2 = 1 / (1 - 0) = 1
#det (rxx) = 1 e autovalores: ??1 = ??2 = 1
#N?o h?, portanto, problemas de multicolinearidade nesse estudo
#
# p) Obtenha o gr?fico dos dados observados e do plano ajustado.
#Obs: necess?rio baixar e instalar o pacote scatterplot3d
require(scatterplot3d)
s3d <- scatterplot3d(X1,X2,Y,type="h",highlight.3d=TRUE,
                     angle=55, scale.y =0.7, pch=16)
my.lm <- lm(Y~X1+X2)
s3d$plane3d(my.lm,col=4)
#
# q) Obtenha o gr?fico do plano ajustado.
x1 <- seq(80, 160, length=50)
x2 <- seq(10,25,length=50)
f <- function(x1,x2){r <- 93.18+0.87*x1+2.424*x2}
y <- outer(x1,x2,f)
y[is.na(y)] <- 1
par(bg = "white")


persp(x1,x2,y,theta = 30, phi = 20,expand = 0.5,col = "blue",
      ltheta = 120, shade = 0.75, ticktype = "detailed",
      xlab = "X1", ylab = "X2", zlab = "Y")
#
#
rm(list=ls(all=TRUE))
#
##################################################################################
# EXEMPLO 2: (Pag 22 apostila Giolo)
#
# X1: quantidade estocada (em unidades)
# X2: dist???ncia percorrida (em metros)
# Y: tempo requerido (min)
#a) Leitura dos dados
dados<- data.frame(
  X1=c(7,3,3,4,6,7,2,7,30,5,16,10,4,6,9,10,6,7,3,17,10,26,9,8,4),
  X2=c(560,220,340,80,150,330,110,210,1460,605,688,215,255,462,448,776,
       200,132,36,770,140,810,450,635,150),
  Y=c(16.68,11.5,12.03,14.88,13.75,18.11,8,17.83,79.24,21.50,
      40.33,21,13.5,19.75,24,29,15.35,19,9.5,35.10,17.90,52.32,18.75,19.83,10.75)
)
dados
attach(dados)
#
# b) Obten??????o da matriz rxx
X <- as.matrix(cbind(X1,X2))
rxx <- cor(X)
rxx
#
#c) Obten??????o dos VIF???s, det(rxx) e autovalores de rxx
vif1<- 1/(1-(rxx[1,2]^2))
vif1
vif2<- 1/(1-(rxx[2,1]^2))
vif2
det(rxx)
eigen(rxx)[1]
k = 1.824215/0.175785
k
#
#d) Ajuste dos modelos de regress???o: Y em X1 e X2; Y em X1 e Y em X2
mod1 <- lm(Y~X1+X2)
anova(mod1)
summary(mod1)
#
mod2 <- lm(Y~X1)
anova(mod2)
summary(mod2)
#
mod3 <- lm(Y~X2)
anova(mod3)
reg_3 <- summary(mod3)
reg_3$adj.r.squared
#
# e) Gr???ficos dos res???duos
par(mfrow=c(2,2))
plot(mod1,which=c(1:4),pch=16,add.smooth=FALSE)
## Resíduos vs ajustados ou Scale-location até 3 (homocedasticidade), Q-Q Resíduos (homocedasticidade)
par(mfrow=c(1,1))
plot(mod1$residuals,pch=16,ylab="Res???duos")
par(mfrow=c(2,2))
plot(X1,mod1$residuals,pch=16,ylab="Res???duos")
plot(X2,mod1$residuals,pch=16,ylab="Res???duos")
resparc1 <- mod1$residuals + 1.615*X1
resparc2 <- mod1$residuals + 0.014*X2
plot(X1,resparc1,pch=16,ylab="Res???duos Parciais")
plot(X2,resparc2,pch=16,ylab="Res???duos Parciais")
#
# f) Diagn???stico de influ???ncia
influ <- influence.measures(mod1)
influ
summary(influ)
#
# g) Gr???ficos do diagn???stico de influ???ncia
par(mfrow=c(3,3))
plot(influ$infmat[,1], ylab="DFBeta(0)", pch=16)
plot(influ$infmat[,2], ylab="DFBeta(1)", pch=16)
plot(influ$infmat[,3], ylab="DFBeta(2)", pch=16)
par(mfrow=c(2,2))
plot(influ$infmat[,4], ylab="DFFits", pch=16)
plot(influ$infmat[,5], ylab="CovRatio", pch=16)
plot(influ$infmat[,6], ylab="D de Cook", pch=16)
plot(influ$infmat[,7], ylab="hii", pch=16)
#
# h) Reajuste do modelo Y em X1 e X2
# h1) sem a observa??????o 9
dados9 <- dados[-9,]
attach(dados9)
mod1s9 <- lm(Y~X1+X2)
summary(mod1s9)
anova(mod1s9)
#
# h2) sem a observa??????o 22
dados22 <- dados[-22,]
attach(dados22)
mod1s22 <- lm(Y~X1+X2)
summary(mod1s22)
anova(mod1s22)
#
# h3) sem as observa??????es 9 e 22
dados9s22 <- dados9[-21,]
attach(dados9s22)
mod1s9s22 <- lm(Y~X1+X2)
summary(mod1s9s22)
anova(mod1s9s22)
#
# i) Gr???ficos dos res???duos do modelo Y em X1 e X2 sem a observa??????o 9
par(mfrow=c(2,2))
plot(mod1s9,which=c(1:4),pch=16,add.smooth=FALSE)
plot(mod1s9,which=c(1:4),pch=16,add.smooth=FALSE,id.n=0)
#
# j) Gr???ficos dos res???duos do modelo Y em X1 sem a observa??????o 9
dados9 <- dados[-9,]
attach(dados9)
mod2s9 <- lm(Y~X1)
plot(mod2s9,which=c(1:4),pch=16,add.smooth=FALSE)
#
# k) Gr???ficos: valores observados e modelo ajustado (Y em X1 e X2 sem obs 9)
# obs: necess???rio instalar no diret???rio library do R a fun??????o scatterplot3d #
attach(dados)
require(scatterplot3d)
s3d <- scatterplot3d(X1,X2,Y, type="h", highlight.3d=TRUE,
                     angle=55, scale.y=0.7, pch=16)
my.lm <- lm(Y~X1+X2)
s3d$plane3d(my.lm,col=4)
#
# k1) gr???fico somente do modelo ajustado
x1 <- seq(2,30,length=50)
x2 <- seq(30,1500,length=50)
f <- function(x1,x2){
  r <- 4.447+1.498*x1+0.0103*x2
}
y <- outer(x1,x2,f)
y[is.na(y)] <- 1
par(bg = "white")
persp(x1,x2,y, theta=30, phi=20, expand=0.5, col="blue",
      ltheta=120, shade=0.75, ticktype="detailed",
      xlab="X1", ylab="X2", zlab="Y")
#
# l) valor predito em x1=8 e x2=275
new <- data.frame(cbind(8,275))
predict(mod1s9,new,interval="confidence")
predict(mod1s9,interval="confidence")
