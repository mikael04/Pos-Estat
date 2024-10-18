dados_02 <- openxlsx::read.xlsx("exemplo cusum e EWMA.xlsx", sheet = "GCR") |>
  janitor::clean_names()

colnames(dados_02)

# Fit the linear regression model
model <- lm(pureza_y_percent ~ nivel_de_hidrocarbonetos_x_percent, data = dados_02)

# Summary of the model (optional)
summary(model)

