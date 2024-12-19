####Duas Amostras
######################Testes para duas amostras relacionadas
#### McNemar
mercado<- matrix(c(37,13,3,47),
                 nrow = 2,
                 dimnames = list("Antes" = c("A", "B"),
                                 "Depois" = c("A", "B")))
mercado
mcnemar.test(mercado, correct=TRUE)



#####Teste dos sinais
#####Exemplo da aula
mvelho<-c(18,15,19,23,12,16,18,17)
mnovo<-c(24,14,22,28,16,20,20,18)
mvelho-mnovo

### Vimos que temos o x = 1, pois a menor das duas categorias é 1

2*pbinom(1,8,0.5)

# ou podemos então aplicar um teste binomial
binom.test(1,8)



###Ou atraves de pacote
#### Instalar pacote BSDA
install.packages("BSDA") #depois de instalar pode remover essa linha
library(BSDA)
SIGN.test(mvelho,mnovo,    alternative = "two.sided",
          conf.level = 0.95)
#### Observe que o p-valor foi o mesmo

#####Teste de Wilcoxon
#####IMPORTANTE: NÃO ESQUECER DE COLOCAR PAIRED = TRUE
#### ISSO INFORMA QUE AS AMOSTRAS S?O PAREADAS (DEPENDENTES)
wilcox.test(mvelho,mnovo,paired=TRUE, alternative = "two.sided")
### Veja que a estat?stica do teste ? V = 1,5
### p-valor = 0,02471, ou seja rejeitamos H0.


#### Unilateral
#### Baseia-se no primeiro elemento na função
#### no caso abaixo testamos se mvelho<mnovo
wilcox.test(mvelho,mnovo,paired=TRUE, alternative = "less")
###p-valor = 0,01235, ou seja rejeitamos H0
