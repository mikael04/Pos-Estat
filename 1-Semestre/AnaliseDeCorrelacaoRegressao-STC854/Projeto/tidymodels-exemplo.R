library(tidymodels)

data(ames)

set.seed(4595)
data_split <- initial_split(ames, strata = "Sale_Price", prop = 0.75)

ames_train <- training(data_split)
ames_test  <- testing(data_split)
