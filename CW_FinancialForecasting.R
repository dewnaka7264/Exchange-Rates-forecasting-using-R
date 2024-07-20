if (!require(readxl)) install.packages("readxl")
install.packages("Metrics")

# Package imports
library(neuralnet)
library(Metrics)

# Read the ExchangeUSD.xlsx file
exchange_data <- read_excel("ExchangeUSD.xlsx")
nrow(exchange_data) # View the number of rows in data just after the loading


# Extract the USD/EUR exchange rates column
exchange_rates <- exchange_data$`USD/EUR`

# Data transformation (if necessary)
# For example, converting date columns to appropriate format, handling missing values, etc.

#  Normalization  
# Min max normalization
normalize <- function(x)
{
  a <- (x-min(x))/(max(x)-min(x))
  return(a)
}
#  Denormalization
unnormalize <- function(x, min=0, max=1) { 
  return( (max - min)*x + min )
}

# Generate I/O matrix 

# Extract the USD/EUR exchange rates column (3rd column)
exchange_rates <- exchange_data$`USD/EUR`

# Function to generate autoregressive input vectors up to lag level
generate_input_vectors <- function(data, lag_level) {
  input_vectors <- matrix(NA, nrow = length(data) - lag_level, ncol = lag_level)
  
  for (i in 1:(length(data) - lag_level)) {
    input_vectors[i, ] <- data[(i + lag_level - 1):(i)]
  }
  
  return(input_vectors)
}

# Determine the number of rows for training and testing sets
total_rows <- nrow(exchange_data)
train_rows <- round(0.8 * total_rows)

# Split the data into training and testing sets
train_data <- exchange_data[1:train_rows, ]
test_data <- exchange_data[(train_rows + 1):total_rows, ]

train_exchange_rate = train_data$`USD/EUR`
test_exchange_rate = test_data$`USD/EUR`


# Define the lag level
lag_level <- 4

# Generate autoregressive input vectors
train_input_vectors <- generate_input_vectors(train_exchange_rate, lag_level)
test_input_vectors <- generate_input_vectors(test_exchange_rate, lag_level)

# Define the output variable (next day exchange rate)
train_output_variables <- train_exchange_rate[(lag_level + 1):length(train_exchange_rate)]
test_output_variables <- test_exchange_rate[(lag_level + 1):length(test_exchange_rate)]

# Combine input and output variables to form the I/O matrix
train_data_matrix <- cbind(train_input_vectors, train_output_variables)
test_data_matrix <- cbind(test_input_vectors, test_output_variables)

print(head(train_data_matrix))
print(test_data_matrix)

train_rates_df <- as.data.frame(train_data_matrix)
test_rates_df <- as.data.frame(test_data_matrix)

exchange_rate_min <- min(train_rates_df)
exchange_rate_max <- max(train_rates_df)

names(train_rates_df)[names(train_rates_df) == "train_output_variables"] <- "output"
names(test_rates_df)[names(test_rates_df) == "test_output_variables"] <- "output"

train_rates_df_without_pred = train_rates_df[, -ncol(train_rates_df)]
test_rates_df_without_pred = test_rates_df[, -ncol(test_rates_df)]


scaled_train_rates_df = as.data.frame(lapply(train_rates_df, normalize))
print(scaled_train_rates_df)
scaled_test_rates_df = as.data.frame(lapply(test_rates_df, normalize))

names(scaled_train_rates_df)[names(scaled_train_rates_df) == "train_rates_df$output"] <- "output"
names(test_rates_df)[names(test_rates_df) == "test_output_variables"] <- "output"

# Print the first few rows of the I/O matrix without scalling
head(train_rates_df)



#Neural Network 

PredictExchangeRate <- function(hidden_vector,epochs=1, act_fct="logistic", isLinear=TRUE){
  # Define the neural network architecture
  model <- neuralnet(output ~ ., 
                     data = scaled_train_rates_df, 
                     act.fct = act_fct,
                     rep=epochs,
                     hidden = hidden_vector, 
                     linear.output = isLinear)
  
  # Plot the neural network
  plot(model)
  
  
  if (nrow(scaled_test_rates_df) == 0) return;
  # Predict using the neural network
  predictions <- predict(model, newdata = as.data.frame(scaled_test_rates_df[,-ncol(scaled_test_rates_df)]))
  
  # Denormalize the predicted exchange rates
  predicted_exchange_rates = unnormalize(predictions, exchange_rate_min, exchange_rate_max)
  final_results <-cbind(test_rates_df$output,predicted_exchange_rates)
  colnames(final_results) <- c("Actual value", "NN_Result")
  head(final_results)
  
  # Retrieve weight values
  # print(paste("Weight values : ", model$weights))
  
  # Calculate RMSE
  rmse <- sqrt(mean((predicted_exchange_rates - test_rates_df$output)^2))
  print(paste("RMSE  : ", rmse))
  
  # Calculate Mean Absolute Error
  mae <- mean(abs(predicted_exchange_rates - test_rates_df$output))
  print(paste("MAE   : ", mae))
  
  # Calculate Mean Absolute Percentage Error
  mape <- mean(abs((predicted_exchange_rates - test_rates_df$output))/test_rates_df$output) * 100
  print(paste("MAPE  : ", mape))
  
  # Calculate Symmetric Mean Absolute Percentage Error
  smape <- 100 * mean(2 * abs(predicted_exchange_rates - test_rates_df$output) / 
                        (abs(predicted_exchange_rates) + abs(test_rates_df$output)))
  print(paste("sMAPE : ", smape))
  
  # calculate deviation and accuracy
  deviation <- (test_rates_df$output - predicted_exchange_rates) / test_rates_df$output 
  deviation
  accuracy <- 1 - abs(mean(deviation))
  accuracy
  cat("Model Accuracy:",accuracy)
  
  # Neural Network end

  
  
  par(mfrow=c(1,1))
  plot(test_rates_df$output, predicted_exchange_rates,
       col='red',
       main='Actual vs Predicted (USD/EUR)',
       pch=18,cex=0.7,
       xlab = "Actual exchange rate",
       ylab = "Predicted exchange rate")
  abline(a=0, b=1, col='blue')
  
  x = 1:nrow(test_rates_df)
  plot(x, test_rates_df$output, col = "red", type = "l", lwd=2,
       main = "Exhange rate (USD/EUR) prediction",
       xlab = "No. of days",
       ylab = "Exchange rate")
  lines(x, predicted_exchange_rates, col = "blue", lwd=2)
  legend("topright",  legend = c("Actual rate", "Predicted rate"), 
         fill = c("red", "blue"), col = 2:3,  adj = c(0, 0.6))
  grid()
}


PredictExchangeRate(c(5),act_fct = "logistic", isLinear = TRUE, epochs = 5)
 PredictExchangeRate(c(4,6),act_fct = "logistic", isLinear = TRUE, epochs = 5)
 PredictExchangeRate(c(4),act_fct = "tanh", isLinear = FALSE, epochs = 5)
PredictExchangeRate(c(3,6),act_fct = "tanh", isLinear = FALSE, epochs = 5)


warnings()

