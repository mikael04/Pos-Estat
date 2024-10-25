# Carregar pacotes

library(qcc)
library(Devore7)
library(ggplot2)

# 1. Carregar os dados e prepará-los
data("xmp16.08")
charcoal_data <- xmp16.08

# 2. Análise básica
str(charcoal_data)
summary(charcoal_data)

# Médias e desvios-padrões
sample_means <- rowMeans(charcoal_data)
sample_sd <- apply(charcoal_data, 1, sd) # a função será aplicada às LINHAS

# 3. Gráficos de Controle

# 3.1 Gráfico X-barra (Fase I: primeiras 10 amostras)
xbar_p1 <- qcc(charcoal_data[1:10,], # Seleciona apenas as 10 primeiras amostras
               type="xbar",           # Especifica o tipo do gráfico como X-barra
               title="Gráfico X-barra para Peso do Carvão - Fase I", # Título
               xlab="Grupo Amostral", # Rótulo do eixo X
               ylab="Média Amostral") # Rótulo do eixo Y

summary(xbar_p1) # Resumo estatístico do gráfico

# 3.2 Gráfico S (Fase I)
s_p1 <- qcc(charcoal_data[1:10,],     
            type="S", # desvio-padrão
            title="Gráfico S para Peso do Carvão - Fase I",
            xlab="Grupo Amostral",     
            ylab="Desvio Padrão Amostral")

summary(s_p1)

# 3.1 Gráfico X-barra (Fase II: amostras restantes)
xbar_p2 <- qcc(charcoal_data[1:10,], type="xbar", 
               newdata=charcoal_data[11:16,],
               title="Gráfico X-barra para Peso do Carvão - Fase I & II",
               xlab="Grupo Amostral",
               ylab="Média Amostral")

summary(xbar_p2)

# 3.2 Gráfico S (Fase II)
s_p2 <- qcc(charcoal_data[1:10,], type="S", 
            newdata=charcoal_data[11:16,],
            title="Gráfico S para Peso do Carvão - Fase I & II",
            xlab="Grupo Amostral",
            ylab="Desvio Padrão Amostral")

summary(s_p2)

# 4. Gráficos de Controle Avançados

# 4.1 Gráfico CUSUM
# Sensível a pequenas mudanças persistentes no processo
cusum_chart <- cusum(sample_means,        
                     decision.interval=5,  # Intervalo de decisão h
                     se.shift=1,    # Valor de referência k para detectar mudanças
                     title="Gráfico CUSUM para Peso do Carvão",
                     xlab="Grupo Amostral",
                     ylab="Soma Cumulativa")

# 4.2 Gráfico EWMA 
# O gráfico EWMA usa médias móveis ponderadas exponencialmente
ewma_chart <- ewma(sample_means,          
                   lambda=0.2,  # Parâmetro de suavização (peso para obs. recentes)
                   nsigmas=3,   # Número de desvios padrão para limites de controle
                   title="Gráfico EWMA para Peso do Carvão",
                   xlab="Grupo Amostral",
                   ylab="Estatística EWMA")

# Extra
# 5. Gráfico de Sequência (Gráfico de Séries Temporais)
# Útil para visualizar o comportamento do processo ao longo do tempo
run_chart <- qcc(charcoal_data,          # Usa todo o conjunto de dados
                 type="xbar",            # Calcula e plota as médias
                 title="Gráfico de Sequência para Peso do Carvão",
                 xlab="Grupo Amostral",
                 ylab="Média Amostral")


### Exemplo simulado ####

# Primeiro, vamos criar um banco de dados de unidades de calçados produzidos 
# por uma empresa diariamente. Consideremos janeiro a dezembro. O número de 
# unidades deve estar entre 300-500 por dia. Por volta de maio, houve um desastre 
# natural que impactou a cidade onde esta empresa está localizada. A produção 
# caiu para 150 por dia durante mais de 8 semanas; além disso, alguns calçados 
# não apresentavam alta qualidade devido ao uso de funcionários de outros setores 
# da empresa, consequência do desastre impactando a força de trabalho.
# Vamos incorporar estas variáveis:
# semanas
# produção semanal de calçados de jan-dez
# número de funcionários no setor de produção (20; 8 durante o desastre natural)
# índice de qualidade (0-10, sendo 0 calçados impróprios para venda e 8-10 
# calçados que atendem à qualidade de venda)
# a taxa média de produção diária era de 95% entre 8-10 no índice de qualidade, 
# mas durante as 8 semanas do desastre natural, isso caiu para apenas 50%.
# Observações adicionais:
# 52 semanas no ano
# 6 dias de produção por semana
# Período do desastre: semanas 19-26 (8 semanas)
# Sistema de qualidade em duas fases (normal e impactada)
# Monitoramento contínuo da produção e qualidade


# Set seed for reproducibility
set.seed(123)

# Create basic structure
weeks <- 1:52  # 52 weeks in a year

# Function to generate daily production
generate_daily_prod <- function(base_min, base_max, n_days) {
  round(runif(n_days, base_min, base_max))
}

# Create weekly production data
weekly_production <- numeric(52)
employees <- numeric(52)
quality_index <- numeric(52)
quality_rate <- numeric(52)

# Normal periods (weeks 1-18 and 27-52)
normal_weeks <- c(1:18, 27:52)
disaster_weeks <- 19:26  # 8 weeks of disaster (May-June)

# Fill normal periods
for(w in normal_weeks) {
  daily_prod <- generate_daily_prod(300, 500, 6)  # 6 working days
  weekly_production[w] <- sum(daily_prod)
  employees[w] <- 20
  quality_index[w] <- runif(1, 8, 10)
  # 95% of production meets high quality standards
  quality_rate[w] <- 0.95
}

# Fill disaster period
for(w in disaster_weeks) {
  daily_prod <- generate_daily_prod(150, 200, 6)  # reduced production
  weekly_production[w] <- sum(daily_prod)
  employees[w] <- 8
  quality_index[w] <- runif(1, 5, 7)  # lower quality
  quality_rate[w] <- 0.50  # only 50% meets quality standards
}

# Create the dataset
shoe_production <- data.frame(
  Week = weeks,
  Month = c(rep("Jan",4), rep("Feb",4), rep("Mar",5), rep("Apr",4), 
            rep("May",4), rep("Jun",4), rep("Jul",5), rep("Aug",4), 
            rep("Sep",4), rep("Oct",5), rep("Nov",4), rep("Dec",5)),
  Production = weekly_production,
  Employees = employees,
  Quality_Index = round(quality_index, 2),
  Quality_Rate = quality_rate
)

# Add high quality production column
shoe_production$High_Quality_Production <- 
  round(shoe_production$Production * shoe_production$Quality_Rate)

# View first few rows
head(shoe_production)

# Summary statistics
summary(shoe_production)

# Load required packages
library(qcc)
library(ggplot2)

# Gráfico X-barra (Fase I: primeiras 18 semanas)
xbar_quality <- qcc(shoe_production[1:18,], # Seleciona apenas as 10 primeiras amostras
               type="xbar",           # Especifica o tipo do gráfico como X-barra
               title="Gráfico X-barra para produção de calçados", # Título
               xlab="Grupo Amostral", # Rótulo do eixo X
               ylab="Média Amostral") # Rótulo do eixo Y


# Gráfico X-barra (Fase II: semanas 19-26)
xbar_quality2 <- qcc(shoe_production[1:18,], type="xbar", 
               newdata=shoe_production[19:26,],
               title="Gráfico X-barra para produção de calçados - Fase I & II",
               xlab="Grupo Amostral",
               ylab="Média Amostral")

# 1. EWMA Chart for Quality Index
ewma_quality <- ewma(shoe_production$Quality_Index,
                     lambda = 0.2,      # Standard weight for detecting small shifts
                     xlab = "Week",
                     ylab = "EWMA of Quality Index",
                     title = "EWMA Chart of Shoe Quality Index")

# 2. CUSUM Chart for Weekly High Quality Production
cusum_production <- cusum(shoe_production$High_Quality_Production,
                          decision.interval = 5,
                          se.shift = 1,
                          xlab = "Week",
                          ylab = "CUSUM of High Quality Production",
                          title = "CUSUM Chart of High Quality Production")

# 3. Individual Chart for Quality Rate
xbar_quality <- qcc(shoe_production$Quality_Rate,
                    type = "xbar.one",
                    title = "Individual Chart of Quality Rate")

# Generate summary statistics by month
monthly_stats <- aggregate(shoe_production[,c("Production", "Quality_Rate", 
                                              "High_Quality_Production")],
                           by = list(Month = shoe_production$Month),
                           FUN = mean)

print(monthly_stats)



