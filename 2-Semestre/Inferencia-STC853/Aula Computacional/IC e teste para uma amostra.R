
######################### IC e Teste para uma Amostra #########################


# Passo 1: Carregar os pacotes que serao usados

if(!require(dplyr)) install.packages("dplyr") # Instalaçao do pacote caso nao esteja instalado
library(dplyr)                                # Carregamento do pacote

setwd("2-Semestre/Inferencia-STC853/Aula Computacional/")

# Passo 2: Carregar o banco de dados

# Importante: selecionar o diretorio de trabalho (working directory)
# Isso pode ser feito manualmente: Session > Set Working Directory > Choose Directory
# Ou usando a linha de codigo abaixo:
# setwd("C:/Users/ferna/Desktop")

dados <- read.csv('Banco de Dados 2.csv', sep = ';', dec = ',',
                  stringsAsFactors = T, fileEncoding = "latin1") # Carregamento do arquivo csv
View(dados)                             # Visualizacao dos dados em janela separada
glimpse(dados)                          # Visualizacao de um resumo dos dados


###########################################
###### Caso de Estimação da média #########
###########################################

# Passo 3: Verificacao da normalidade dos dados
#H0: os dados provém da distribuição normal
# H1: os dados não provém da distribuição normal
shapiro.test(dados$Altura)


# Passo 4 - Intervalo de confiança
# Amostra provém da distribuição normal e a variância populacional é desconhecida
alpha<-0.05
var_interesse<-dados$Altura
n<-length(var_interesse)

(LI<-mean(var_interesse)-qt(1-alpha/2, n-1)*sd(var_interesse)/sqrt(n))
(LS<-mean(var_interesse)+qt(1-alpha/2, n-1)*sd(var_interesse)/sqrt(n))


# Passo 5: Realizacao do teste t para uma amostra com IC
# H0: mu=167
# H1: mu!=167
t.test(dados$Altura, mu = 167, conf.level = 0.95, alternative = "two.sided")
t.test(dados$Altura, mu = 165, conf.level = 0.95, alternative = "greater")

## tc -> região de não rejeição de h0, p-valor de 0.4883 -> corrobora a rejeição de h0

# Observacao:
  # O teste bicaudal eh o default; caso deseje unicaudal, necessario incluir:
    # alternative = "greater" (maior) ou alternative = "less" (menor)
  # Exemplo: t.test(dados$Altura, mu = 167, alternative = "greater")
    # Nesse caso, o teste verificara se os dados indicam que e media eh
    # maior que a media testada

# Passo 6 (opcional): Visualizacao da distribuicao dos dados

boxplot(dados$Altura, ylab = "Altura (cm)")

###############################################
# Caso de Estimação da Proporção ##############
##############################################
# Passo 1: definir quantos uns foram observados e a estimativa da proporção

nmasculino<-sum(dados$Genero=="M")
phat<- nmasculino/n

# Passo 2 - Obter o intervalo de confiança
alpha<-0.05

(LI<-phat-qnorm(1-alpha/2)*sqrt(phat*(1-phat)/(n-1)))
(LS<-phat+qnorm(1-alpha/2)*sqrt(phat*(1-phat)/(n-1)))

# Passo 3 -  Realizacao do teste da proporção para uma amostra com IC
prop.test(nmasculino, n=n, p=0.6)

##  H0: Testando que a proporção é igual a 0.6, H1: prop != 0.6

# Proporção das pessoas que tem Ensino superior como Grau de instrução
nsuperior<-sum(dados$Grau_de_Instruçao=="Superior")
phat<- nsuperior/n

# Passo 2 - Obter o intervalo de confiança
alpha<-0.05

(LI<-phat-qnorm(1-alpha/2)*sqrt(phat*(1-phat)/(n-1)))
(LS<-phat+qnorm(1-alpha/2)*sqrt(phat*(1-phat)/(n-1)))

# Passo 3 -  Realizacao do teste da proporção para uma amostra com IC
prop.test(nsuperior, n=n, p=0.3, alternative = "greater")

## Proporção de pessoas que tem ensino superior é maior que 0.3,
## nesse caso rejeitamos a h0: De que a proporção da população é menor ou igual à 0.3
