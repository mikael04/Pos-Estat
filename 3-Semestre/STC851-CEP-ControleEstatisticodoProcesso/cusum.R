
### Aula de CEP -- 18 de outubro de 2024


############## Exercício 1 ################

install.packages("qcc")
library(qcc)

# Create the dataframe
data <- data.frame(
  xi = c(9.45, 7.99, 9.29, 11.66, 12.16, 10.18, 8.04, 11.46, 9.20, 10.34,
         9.03, 11.47, 10.51, 9.40, 10.08, 9.37, 10.62, 10.31, 8.52, 10.84,
         10.90, 9.33, 12.29, 11.50, 10.60, 11.08, 10.38, 11.62, 11.31, 10.52)
)

# Display the first few rows of the dataframe
print(head(data))

# Display summary statistics
summary(data)

# If you want to save this dataframe to a CSV file, you can use:
# write.csv(df, "sample_data.csv", row.names = FALSE)


# Function to calculate CUSUM Sh and Sl values
calculate_cusum <- function(data, target, k) {
  n <- length(data)
  Sh <- numeric(n)
  Sl <- numeric(n)

  
  for (i in 1:n) {
    if (i == 1) {
      Sh[i] <- max(0, data$xi[i] - target - k)
      Sl[i] <- max(0, target - k - data$xi[i])
    } else {
      Sh[i] <- max(0, Sh[i-1] + data$xi[i] - target - k)
      Sl[i] <- max(0, target - k - data$xi[i] + Sl[i-1])
    }
  }
  
  return(list(Sh = Sh, Sl = Sl))
}


# Summary statistics
summary(data)

target <- 10
k <- 0.5

result <- calculate_cusum(data$xi, target, k)


# Print first few values of Sh and Sl for debugging
print("First few Sh values:")
print(head(result$Sh))
print("First few Sl values:")
print(head(result$Sl))


# Print results
print(result$Sh)
print(result$Sl)

# Plot CUSUM chart
plot(result$Sh, type = "l", col = "red", ylim = range(c(result$Sh, result$Sl)), 
     xlab = "Sample", ylab = "CUSUM", main = "CUSUM Chart")
lines(result$Sl, col = "blue")
legend("topleft", legend = c("Sh", "Sl"), col = c("red", "blue"), lty = 1)


# Plot CUSUM chart with improved visibility
plot(result$Sh, type = "l", col = "red", 
     ylim = range(c(result$Sh, result$Sl, 0)), # Ensure 0 is included in the range
     xlab = "Sample", ylab = "CUSUM", main = "CUSUM Chart")
lines(result$Sl, col = "blue")
abline(h = 0, col = "gray", lty = 2) # Add a horizontal line at y = 0
legend("topleft", legend = c("Sh", "Sl"), col = c("red", "blue"), lty = 1)





############################ Exercício 3 ###############

# Gráfico de controle de regressão

data2 <- data.frame(
  X = c(0.99, 1.02, 1.15, 1.29, 1.46, 1.36, 0.87, 1.23, 1.55, 1.4, 1.19, 1.15, 0.98, 1.01, 1.11, 1.2, 1.26, 1.32, 1.43, 0.95),
  Y = c(90.01, 89.05, 91.43, 93.74, 96.73, 94.45, 87.57, 91.77, 99.42, 93.65, 93.54, 92.52, 90.56, 89.54, 89.85, 90.39, 93.25, 93.41, 94.98, 87.33)
)

# View the first few rows of the dataframe
head(data2)

# Run linear regression
model <- lm(Y ~ X, data = data2)

# Display the summary of the model
summary(model)


# Install and load necessary packages
if (!require(qcc)) install.packages("qcc")
library(qcc)
library(ggplot2)

# Assuming 'data' is the dataframe we created earlier
# and 'model' is the linear regression model we fitted

# Calculate predicted values and residuals
data2$predicted <- predict(model)
data2$residuals <- residuals(model)

# Calculate control limits
sd_residuals <- sd(data2$residuals)
ucl <- data2$predicted + 3 * sd_residuals
lcl <- data2$predicted - 3 * sd_residuals

# Identify points outside control limits
out_of_control <- data2[data2$Y > ucl | data2$Y < lcl, ]
print("Points outside control limits:")
print(out_of_control)

##### Calculando a mão

# Assuming 'data' is your dataframe and 'model' is your linear regression model

# Calculate required components
n <- nrow(data2)
x_mean <- mean(data2$X)
y_mean <- mean(data2$Y)
Sxx <- sum((data2$X - x_mean)^2)
s <- sqrt(sum(residuals(model)^2) / (n - 2))

# Calculate predicted values
data2$y_hat <- predict(model)

# Calculate UCL and LCL
data2$UCL <- data2$y_hat + 3 * s * sqrt(1 + 1/n + ((data2$X - x_mean)^2 / Sxx))
data2$LCL <- data2$y_hat - 3 * s * sqrt(1 + 1/n + ((data2$X - x_mean)^2 / Sxx))

# View the results
head(data2[c("X", "Y", "y_hat", "UCL", "LCL")])

# Plot the results
library(ggplot2)

ggplot(data2, aes(x = X)) +
  geom_point(aes(y = Y)) +
  geom_line(aes(y = y_hat), color = "blue") +
  geom_line(aes(y = UCL), color = "red", linetype = "dashed") +
  geom_line(aes(y = LCL), color = "red", linetype = "dashed") +
  labs(title = "Regression Control Chart",
       x = "Hydrocarbon Level (X)",
       y = "Purity (Y)") +
  theme_minimal()