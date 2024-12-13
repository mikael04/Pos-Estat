##### Teste de Kruskal Wallis ######

### Exemplo Vinho
### Na entrada dos dados, devemos corresponder cada valor ao
### tratamento a que ele pertence.

vinho <- data.frame(
  nota = c(5,6.7,7,6.8,8.3,9.3,8.6,9.0,9.2,8.7,7.3,8.2),
  tipo = c("tipo1","tipo1","tipo1","tipo1","tipo2","tipo2","tipo2","tipo2","tipo3","tipo3","tipo3","tipo3"))
vinho

####o Comando abaixo faz o teste sem pacotes, porem
#### não faz comparação multipla
#### devemos sempre colocar:
#### valores~tratamentos, nessa ordem.

kruskal.test(nota~tipo, data=vinho)
#### como o p-valor é 0,02, rejeitamos H0.
#### existem diferenças entre os vinhos
#### mas entre quais?
#### procedemos as comparações múltiplas

#### para isso utilizaremos o pacote dunn.test
install.packages("dunn.test")


library(dunn.test)
#### a sintaxe muda um pouco,
### mas a ordem continua sendo valores,tratamentos

dunn.test(vinho$nota,vinho$tipo, method = "bonferroni")
#### Esse comando já faz o teste e as comparações
#### observe o mesmo p-valor 0,02
#### Observe onde tem um asterisco
#### ali são as comparações significativas
#### nesse caso, Tipo 1 com Tipo 2.


#################### Teste de Friedman
### Na entrada dos dados, devemos corresponder cada valor ao
### tratamento e bloco a que ele pertence.


dados2 <- data.frame(Rendimento = c(4500,4800,5000,4900,7500,7800,7000,8000,
                                    6000,6200,6300,5900,6200,6100,6250,6100),
                     Bloco = c(rep(1:4,4)),
                     Tratamento = c(rep("XL210",4),rep("XL212",4),rep("XL214",4),rep("XL215",4)))
dados2


###a ordem é v.resposta, tratamento, bloco
friedman.test(dados2$Rendimento, dados2$Tratamento, dados2$Bloco)

#para as comparações multiplas:
#install.packages("pgirmess")
library(pgirmess)

friedmanmc(dados2$Rendimento, dados2$Tratamento, dados2$Bloco)


#Exemplo 3
dados5 <- data.frame(soja = c(3500,3400,3510,3520,3550,3600,3400,3440,
                              3610,3590,3410,3521,3540,3570,3540,3541,
                              3600,3560,3590,3590,3510,3390,3590,3580,
                              3570,3264,3500,3540,3570,3580,3480,3460,
                              3652,3254,3240,3654,3264,3524,3574,3654,
                              3670,3542,3660,3580,3590,3530,3640,3300,
                              3254,3352,3365,3354,3650,3687,3580,3584,
                              3650,3254,3650,3550,3555,3540,3580,3400,
                              2354,2352,2540,2350,2352,2654,2650,2350,
                              2354,2650,2451,2350,2540,2450,2560,2100
),
Bloco = c(rep(seq(1:16),5)),
Tratamento = c(rep("Tamaron",16),rep("Nomolt",16),rep("Fastac",16),rep("Azodrin",16),rep("Testemunha",16)))
dados5

friedman.test(dados5$soja, dados5$Tratamento, dados5$Bloco)
#para as comparações multiplas:
library(pgirmess)

friedmanmc(dados5$soja, dados5$Tratamento, dados5$Bloco)
