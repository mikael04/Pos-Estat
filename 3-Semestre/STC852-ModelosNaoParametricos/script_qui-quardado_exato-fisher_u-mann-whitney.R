########### Testes para duas amostras independentes
########### Teste Qui-quadrado

# Creating the contingency table in R
data <- data.frame(
  Infarto_do_Miocardio = c("Presença", "Ausência", "Total"),
  Fumantes = c(17, 83, 100),
  Nao_fumantes = c(6, 86, 92),
  Total = c(23, 169, 192)
)

# Display the data
print(data)

dados<-read.table("3-Semestre/STC852-ModelosNaoParametricos/teste.txt",h=T)
dados
attach(dados)
Fuma
Infa

tabcont<-table(Fuma,Infa)
tabcont

teste<-chisq.test(tabcont,correct = F)
teste

## Como p-valor < 0.05, rejeitamos a hipótese nula de que as variáveis são independentes, ou seja, existe sim uma associação
## entre os fumantes e a presença de infarto do miocárdio, para o nível de significância de 5%, utilizando o teste qui-quadrado de independência.

x<-matrix(c(17,6,83,86), nrow=2)
rownames(x)<-c("Fumantes","NaoFumantes")
colnames(x)<-c("Presente", "Ausente")
x
chisq.test(x,correct=F)

###Se precisarem das frequencias esperadas
chisq.test(x,correct=F)$expected

##############Teste exato de Fisher

Raca <-
  matrix(c(3, 4, 7, 1),
         nrow = 2,
         dimnames = list(Raca = c("A", "B"),
                         Acasalamento = c("Fecundo", "Nao Fecundo")))
Raca
fisher.test(Raca, alternative = "less") #### Outras opcoes sao alternative="greater",
## Testando se "A" é menor do que a raça B (na ordem que foi escrito)
#### alternative="two.sided"

## H0: Não xiste diferença entre as raças
## H1: Existe diferença entre as raças

####observe o que aconteceria se fizessemos o qui-quadrado
chisq.test(Raca,correct=F)
###frequencias pequenas = FIsher


##############Teste U de Mann Whitney

grupo1 <-c(9.9,7.4,8.9,9.1,7.7,9.7,11.8,7.5,9.2,10,10.2,9.5,10.8,8.0,11)
grupo2 <-c(8.6,10.9,9.8,10.7,9.4,10.3,7.3,11.5,7.6,9.3,8.8,9.6)
grupo1
grupo2
###Observe que o comando é o mesmo das amostras pareadas
### exceto pelo fato de que la havia um paired=T na sintaxe
### que informava que as amostras eram pareadas.
wilcox.test(grupo1,grupo2, alternative = "two.sided", paired = FALSE)
## Ele usa a ordem dos parâmetros, nesse caso grupo 1 com grupo 2
### unilateral
wilcox.test(grupo1,grupo2, alternative = "less", paired = FALSE)
