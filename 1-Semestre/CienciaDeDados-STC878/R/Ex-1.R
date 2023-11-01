library(dplyr)

medalhas <- data.frame(
  pais = c("Chile", "Peru", "Cuba", "Colômbia", "México", "Canadá", "Brasil", "EUA"),
  ouro = c(6, 7, 11, 14, 35, 35, 37, 72),
  prata = c(19, 2, 6, 20, 22, 32, 47, 45),
  bronze = c(13, 13, 7, 14, 32, 38, 39, 54)
)

glimpse(medalhas)
View(medalhas)

medalhas |>
  dplyr::arrange(desc(ouro), desc(prata))
