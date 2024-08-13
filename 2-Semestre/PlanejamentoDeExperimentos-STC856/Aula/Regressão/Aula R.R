############################################################################################
## Aula de Planejamento e An?lise de experimentos I  - 07/11/2014
############################################################################################
## Exemplo de DQL
############################################################################################
# Leitura dos dados
# getwd()
exemplo_reg <- read.table("2-Semestre/PlanejamentoDeExperimentos-STC856/Aula/Regressão/exemploREG.txt", header=TRUE)
# exemplo_reg
attach(exemplo_reg)
str(exemplo_reg)                ## mostra a estrutura (s?ntese) do objeto
#
#------------------------------------------------------------------------------------------
# Fazendo a an?lise de vari?ncia
require(ExpDes)           # pacote necess?rio para realizar a ANAVA
a0 <- latsd(trat, linha, coluna, resp, quali = TRUE, mcomp = "tukey", sigT = 0.05, sigF = 0.05)
#
#------------------------------------------------------------------------------------------
#
#############################################################################################
### Exemplo de REGRESS?O - DIC
#############################################################################################
rm(list=ls(all=TRUE))
#
# Leitura dos dados
exemplo_reg <- read.table('exemploREG.txt', header=TRUE)   ## exemplolista(dbc), exemploREG(dic)
exemplo_reg
attach(exemplo_reg)
str(exemplo_reg)
#
# Fazendo a an?lise de vari?ncia
a1 <- crd(trat, resp, quali = FALSE, sigF = 0.05)
a2 <- rbd(trat, bloco, resp, quali = FALSE, sigF = 0.05)
#
#------------------------------------------------------------------------------------------
#
#############################################################################################
### Testes de compara??es m?ltiplas usando o pacote ExpDes
#############################################################################################
#
# "tukey" - test of Tukey
# "lsd" - teste t
# "lsdb" - test with Bonferroni
# "duncan" - test of Duncan
# "snk" - test of Student-Newman-Keuls
# "sk" - test of Scott-Knott
#
#############################################################################################


