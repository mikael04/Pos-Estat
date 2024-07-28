
######################### Teste t para Amostras Independentes #########################


# Passo 1: Carregar os pacotes que ser?o usados

if(!require(dplyr)) install.packages("dplyr") # Instalacao do pacote caso nao esteja instalado
library(dplyr)                                # Carregamento do pacote
if(!require(RVAideMemoire)) install.packages("RVAideMemoire") # Instalacao do pacote caso nao esteja instalado
library(RVAideMemoire)                                        # Carregamento do pacote
if(!require(car)) install.packages("car") # Instalacao do pacote caso nao esteja instalado
library(car)                                # Carregamento do pacote

# Passo 2: Carregar o banco de dados

# Importante: selecionar o diretorio de trabalho (working directory)
# Isso pode ser feito manualmente: Session > Set Working Directory > Choose Directory
# Ou usando a linha de c?digo abaixo:
# setwd("C:/Users/ferna/Desktop")

dados <- read.csv('Banco de Dados 3.csv', sep = ';', dec = ',',
                  stringsAsFactors = T, fileEncoding = "latin1")  # Carregamento do arquivo csv
View(dados)                                       # Visualizacao dos dados em janela separada
glimpse(dados)                                    # Visualizacao de um resumo dos dados


# Passo 3: Verificacao da normalidade dos dados
## Shapiro por grupo (pacote RVAideMemoire)
#H0: Os dados são normais
#H1: Os dados não são normais
byf.shapiro(Nota_Biol ~ Posicao_Sala, dados)
byf.shapiro(Nota_Fis ~ Posicao_Sala, dados)
byf.shapiro(Nota_Hist ~ Posicao_Sala, dados)


# Passo 4: Verificacao da igualdade variancias
## Teste de igualdade de variâncias
x<-dados%>%
  filter(Posicao_Sala == "Fundos")
y<-dados%>%
  filter(Posicao_Sala == "Frente")

## Precisa testar se as variâncias são iguais ou diferentes, para saber qual método utilizar
### (teste c/ variancias desconhecidas e iguais, ou desconhecidas e diferentes)
#H0: As variâncias dos grupos são iguais
#H1: As variâncias dos grupos não são iguais
var.test(x$Nota_Biol,y$Nota_Biol) ## Não rejeita h0, var iguais
var.test(x$Nota_Fis,y$Nota_Fis) ## Rejeita h0, var diferentes
var.test(y$Nota_Hist,x$Nota_Hist) ## Rejeita h0, var diferentes

# Passo 5: Realizacao do teste t para amostras independentes
#H0: As médias dos grupos são iguais
#H1: As médias dos grupos não são iguais
t.test(Nota_Biol ~ Posicao_Sala, dados, var.equal=TRUE) ## Faz o teste conforme o parametro var.equal (pegando o resultado do teste anterior)
t.test(Nota_Fis ~ Posicao_Sala, dados, var.equal=FALSE)
t.test(Nota_Hist ~ Posicao_Sala, dados, var.equal=FALSE)

# Observacao:
  # O teste bicaudal eh o default; caso deseje unicaudal, necessario incluir:
    # alternative = "greater" ou alternative = "less"
  # Exemplo: t.test(Nota_Biol ~ Posicao_Sala, dados, var.equal=TRUE, alternative="greater")
    # Nesse caso, o teste verificara se a media do primeiro grupo é maior que a media do segundo
      # O R esta considerando "Frente" como primeiro grupo


# Passo 6 (opcional): Visualizacao da distribuicao dos dados

par(mfrow=c(1,3)) # Estabeleci que quero que os graficos saiam na mesma linha
boxplot(Nota_Biol ~ Posicao_Sala, data = dados, ylab="Notas de Biologia", xlab="Posição na Sala")
boxplot(Nota_Fis ~ Posicao_Sala, data = dados, ylab="Notas de Fisica", xlab="Posição na Sala")
boxplot(Nota_Hist ~ Posicao_Sala, data = dados, ylab="Notas de Historia", xlab="Posição na Sala")

