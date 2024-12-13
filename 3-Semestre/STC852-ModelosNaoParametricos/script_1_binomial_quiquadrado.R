########Script teste Binomial
######qui-quadrado####

################## TESTE BINOMIAL #######################

#calculo probabilidades binomial (exemplo 1)
#dbinom calcula a probabilidade ponto a ponto:
#p(x=0)+p(x=1)
dbinom(0,10,0.3)+dbinom(1,10,0.3)

#ou ainda
sum(dbinom(0:1,10,0.3))

#ou ainda podemos calcular a probabilidade acumulada até o ponto 1
#usando pbinom
pbinom(1,10,0.3)

###########Utilizando o comando direto para o teste binomial#############

##### temos que colocar a hipotese alternativa:
##### "less": unilat. a esquerda, "greater" a direita, "two.sided" bilateral
#### para o exemplo da doença da planta:
binom.test(1,10,0.3,alternative = "less")
#### observe o p-valor igual a 0.1493, o mesmo calculado manualmente.

########################################################################


###################para o caso do exemplo de grande amostras##############
#########################################################################
x<-725
n<-1500
p<-0.50
q<-0.50
zcal<-((x-n*p)/(sqrt(n*p*q)))
zcal
####Encontrando no R probabilidades utilizando a normal
pnorm(zcal)

### no caso do teste que fizemos em aula (725 masculino)
### bilateral
2*pnorm(zcal)
#####################################################################


##### Utilizando o comando do teste diretamente

binom.test(725,1500,0.5,alternative = "two.sided")

############## Teste qui-quadrado
###será reproduzido o teste qui-quadrado do material
###ajuste a distribuição poisson com lambda 1.38

###primeiramente vamos utilizar o R para calcular as probabilidades

pois<-c(0,1,2,3,4,5)

####Probabilidade Poisson no R
probs<-dpois(pois, lambda=1.38)
probs

#####Teste Qui-quadrado
esperado<-probs*70
esperado
observado<-c(25,19,10,9,4,3)

#### Observe que o teste será feito com observações
#### de que a aproximação pode estar incorreta
#### isso se deve ao fato das baixas frequencias.
chisq.test(observado,p=esperado,rescale.p=TRUE)


##### Vamos então testar somando as frequencias adjacentes:
observado<-c(25,19,10,(9+4+3))
esperado<-c(17.61,24.30,16.77,(7.71+2.66+0.73))
chisq.test(observado,p=esperado,rescale.p=TRUE)
#### Agora só verificar o p-valor
