######TEste Kolmogorov para Normalidade
###### Exemplo do material
x<-c(198,254,262,272,275,278,285,287,287,292)

#### Aqui entramos com os dados, a distribuição (normal), média e desvio.
#### O aviso que será apresentado é porque tem dois valores iguais
ks.test(x,"pnorm",290,56)
#### Observe que D=0.48 (mesmo encontrado no material) e o p-valor é menor
#### que 0.05. Assim rejeitamos a hipotese de normalidade


########################### Teste de Lilliefors
#####Lembre-se que ele nao usa informações da população e sim da amostra
#####precisaremos instalar o pacote nortest
#install.packages("nortest")
library(nortest)
x1<-c(29,33,35,36,36)
lillie.test(x1)

#### Veja que agora não rejeitamos a normalidade. Porem, o teste foi feito com os
#### valores amostrais de media e desvio padrão.
mean(x1)
sd(x1)


##### Teste de iterações ou aleatoriedade

###caso de dados já codificados, pacote DescTools

#install.packages("DescTools")
library(DescTools)
ter<-c("a","a","a","a","a","b","b","a","a","a","b","a","a","b","b","b",
       "b","b","b","b","b","a","a")
ter
RunsTest(ter)

####caso de dados numéricos: Pacote randtests
#install.packages("randtests")
library(randtests)

x<-c(108,103,109,107,125,142,147,122,116,153,144,162,143,126,145,129,134,137,143,150,
     148,152,126,106,112,139,132,122,138,148,155,146,158)

runs.test(x)

ter1<-c("K","K","C","C","K","C","K","K","C","K","C","C","K","K","K",
        "C","K","K","C","K","K","C","C","K","C","K",
        "K","C","K","C","C","K","K","C","K","K","K","C","C","K")
length(ter1)
RunsTest(ter1,correct=F)
