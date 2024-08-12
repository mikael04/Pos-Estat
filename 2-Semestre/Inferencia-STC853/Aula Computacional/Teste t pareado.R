
######################### Teste t Pareado #########################


# Passo 1: Carregar os pacotes que serao usados

if(!require(dplyr)) install.packages("dplyr") # Instalacao do pacote caso nao esteja instalado
library(dplyr)                                # Carregamento do pacote
if(!require(psych)) install.packages("psych") # Instalacao do pacote caso nao esteja instalado
library(psych)                                # Carregamento do pacote

# Passo 2: Carregar o banco de dados

# Importante: selecionar o diretorio de trabalho (working directory)
# Isso pode ser feito manualmente: Session > Set Working Directory > Choose Directory
# Ou usando a linha de codigo abaixo:
# setwd("")

dados <- read.csv('2-Semestre/Inferencia-STC853/Aula Computacional/Banco de Dados 4.csv', sep = ';', dec = ',', fileEncoding = "latin1") %>%
  rename(Convulsoes_PT = Convulsões_PT, Convulsoes_S1 = Convulsões_S1,
                     Convulsoes_S6 = Convulsões_S6, Genero = Gênero)
# View(dados)                                              # Visualizacao dos dados em janela separada
glimpse(dados)                                                 # Visualizacao de um resumo dos dados

# Passo 3: Verificacao da normalidade dos dados
#H0: Os dados são normais
#H1: Os dados não são normais
dados$DiferencaPTS1 <- dados$Convulsoes_PT - dados$Convulsoes_S1

shapiro.test(dados$DiferencaPTS1)


dados$DiferencaPTS6 <- dados$Convulsoes_PT - dados$Convulsoes_S6

shapiro.test(dados$DiferencaPTS6)

dados$DiferencaS1S6 <- dados$Convulsoes_S1 - dados$Convulsoes_S6

shapiro.test(dados$DiferencaS1S6)

## Deveria ter dado normalidade, seguiremos apenas para fins didáticos,
## mas tecnicamente os próximos testes não fazem mais sentido já que não temos normalidade dos dados
## Testar a diferença (dados da convulsao antes, e convulsao depois)
## Resultado rejeita a h0: dados não são normais


# Passo 4: Realizacao do teste t pareado
#H0: As médias dos dois grupos são iguais
#H1: As médias dos dois grupos não são iguais
t.test(dados$Convulsoes_PT, dados$Convulsoes_S1, paired = TRUE)

## Rejeita h0, ou seja, as médias não são iguais (médias diferentes)

# Passo 5 (opcional): Visualizacao da distribuicao dos dados

par(mfrow=c(1,2)) # Estabeleci que quero que os graficos saiam na mesma linha
boxplot(dados$Convulsoes_PT, ylab="Quantidade de Convulsões", xlab="Pre-Tratamento")
boxplot(dados$Convulsoes_S1, ylab="Quantidade de Convulsões", xlab="1 semana de Tratamento")


# Passo 6 (opcional): Analise descritiva dos dados
summary(dados$Convulsoes_PT)
summary(dados$Convulsoes_S1)

## Outra forma: pela funcao describe do pacote Psych
describe(dados$Convulsoes_PT)
describe(dados$Convulsoes_S1)
