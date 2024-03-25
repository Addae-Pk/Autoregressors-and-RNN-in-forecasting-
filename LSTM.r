#############MANDATORY INSTALLATIONS#########
##Installing Keras & Tensorflow in R ##
install.packages("remotes")
remotes::install_github("rstudio/tensorflow")
reticulate::install_python()
library(tensorflow)
install_tensorflow(envname = "r-tensorflow")
install.packages("keras")
library(keras)
install_keras()
library(tensorflow)
tf$constant("Hello TensorFlow!")

#Packages for RNN Architecture needs 
library(ggplot2)
library(dplyr)
library(moments)
library(EnvStats)
library(tsoutliers)
library(mFilter)
library(tseries)
library(forecast)
library(urca)
library(vars)
library(systemfit)
library(corrplot)
library(MASS)
library(tidyverse)
library(reticulate)
library(tensorflow)
library(keras) 
library(plyr)



data <- read.csv("/file.csv")
colnames(data) <- c('dld_gdp', 'ld_sp', 'dld_uss')
ggplot(data, aes(x = 1:nrow(data), y = `dld_gdp`), main="Preprocessing : Checking STL components") + geom_line()
glimpse(data)
head(data)
dates <- seq (as.Date("2000-01-01"), by="quarter", length.out=91)
rownames(data) <-dates

#Split : train and test set 
train_size <- 79
trainset <- data[1:train_size, ]
testset <- data[(train_size + 1):nrow(data), ]

#Function to reshape data for LSTM layer
reshape_data <- function(data, step) {
  num_samples <- length(data)
  new_length <- floor(num_samples / step) * step
  data <- head(data, new_length)
  
  dim(data) <- c(length(data) / step, step, 1)
  array(data, dim = c(length(data) / step, step, 1))
}

#parameters 
step <- 12
batch_size <- 1

#Reshape input data for LSTM layer
trainset_x <- lapply(trainset, function(col) reshape_data(col, step))
validation_x <- lapply(testset, function(col) reshape_data(col, step))

lstm_input_train <- array(unlist(trainset_x), dim = c(dim(trainset_x[[1]])[1], dim(trainset_x[[1]])[2], length(trainset_x)))
lstm_input_validation <- array(unlist(validation_x), dim = c(dim(validation_x[[1]])[1], dim(validation_x[[1]])[2], length(validation_x)))

#necessary for output data
trainset_y <- reshape_data(trainset$dld_gdp, step)
testset_y <- reshape_data(testset$dld_gdp, step)

#LSTM model
model <- keras_model_sequential() %>%
  layer_lstm(units = 2, input_shape = c(step, 3), return_sequences = TRUE) %>%
  layer_activation('tanh')%>%
  layer_dense(1)

#To check model configuration
model

model %>%  compile(optimizer='adam', loss='mse',
                   metrics=('accuracy'))
#Training: we fit the model
fit <- model %>% fit(
  x = list(lstm_input_train),
  y = trainset_y,
  batch_size = batch_size,
  epochs = 150,
  validation_data = list(list(lstm_input_validation), testset_y)
)
plot(fit)

##Forecast post-covid (2020 - 2023)
predictions <- predict(model, lstm_input_validation)
predictions <- predictions[,,1]
actual_values <- as.vector(testset_y[,,1])
comparaison_df<- data.frame(Actual=actual_values, Predicted = predictions)
dates1 <- seq (as.Date("2019-10-01"), by="quarter", length.out=12)
rownames(comparaison_df) <-dates1
comparaison_df
data
#Plotting
p <- ggplot(data, aes(x = 1:nrow(data), y = dld_gdp)) +
  geom_line(color = "black") +
  labs(title = "Actual vs. Forecast", x = "dates", y = "dld_gdp")
p + geom_line(data = comparaison_df, aes(x = (nrow(data) - 11):nrow(data), y = Predicted), linetype = "dashed", color = "red")


##evaluation
mse <-mean((comparaison_df$Actual - comparaison_df$Predicted))^2
cat("Mean squared Error:", mse, "\n")
sqrt(mse)
accuracy(comparaison_df$Actual, comparaison_df$Predicted)

#evaluating the forecast
for_RNN <- c(
  -3.434973e-03, -2.243469e-03, -2.125453e-03,
  -1.105850e-03, -8.662818e-04, -5.397657e-04,
  -1.984928e-04, -3.410811e-04, 4.687407e-04,
  7.448029e-04, 3.647525e-06, -2.988791e-04
)
accuracy(for_RNN, real_values_gdp)

gdp_forRNN <- as.numeric(l_gdp[80]) + cumsum(ld_gdp[81]+cumsum(for_RNN))

plot(l_gdp, type = 'l', main = 'Real vs. Forecasted log GDP', xlab = 'Time')
lines(82:93, gdp_forRNN, col = 'red')

accuracy(l_gdp[82:93], gdp_forRNN)

plot(gdp_dld, type = 'l', main = 'Forecasted vs. Real dld_gdp', xlab = 'Time')
lines(80:91, for_RNN, col = "red")

print(for_RNN)
print(gdp_forRNN)

####COMPARISON
plot(gdp_dld[80:91], type = 'l', ylim = c(-0.021, 0.015), xlab = 'Time', main = 'Plot of dld_gdp')
lines(forModelVECM[['fcst']]$dld_gdp[,1], col = "red")
lines(forModelVAR[['fcst']]$dld_gdp[,1], col = "blue")
lines(1:12, forecastGDP$mean, col = 'green')
lines(1:12, predicted_list, col = 'purple')
legend("bottomleft", legend = c("Real Value", "ARIMA", "VAR", "VECM"), col = c("black", "green", "blue", "red"), lty = 1, cex = 0.4)

plot(l_gdp[82:93], type = 'l', main = 'Plot of log GDP', xlab = 'Time', ylim = c(9.9, 10.25))
lines(gdp_forVECM, col = 'red')
lines(gdp_forVAR, col = 'blue')
lines(gdp_for, col = 'green')
lines(1:12, gdp_forRNN, col = 'purple')
legend("topleft", legend = c("Real Value", "ARIMA", "VAR", "VECM", "RNN"), col = c("black", "green", "blue", "red", "purple"), lty = 1, cex = 0.4)


###Hypertuning of parameters
#Customized grid search 
create_lstm_model <- function(units, batch_size, trainset_x, trainset_y, validation_x, validation_y) {
  model <- keras_model_sequential() %>%
    layer_lstm(units = units, input_shape = c(step, 3), return_sequences = TRUE) %>%
    layer_dense(1)
  model %>% compile(optimizer = 'adam', loss = 'mae')
  history <- model %>% fit(
    x = list(trainset_x),
    y = trainset_y,
    batch_size = batch_size,
    epochs = 10,
    validation_data = list(list(validation_x), validation_y),
    verbose = 0
  )
  return(list(model = model, history = history))
}
#Grid search function
grid_search <- function(units_grid, batch_size_grid, trainset_x, trainset_y, validation_x, validation_y) {
  results <- data.frame(units=numeric(), batch_size=numeric(), val_loss=numeric())
  
  for (units in units_grid) {
    for (batch_size in batch_size_grid) {
      cat("Units:", units, "Batch Size:", batch_size, "\n")
      model_results <- create_lstm_model(units, batch_size, trainset_x, trainset_y, validation_x, validation_y)
      val_loss <- tail(model_results$history$val_loss, 1)
      
      result <- data.frame(units = units, batch_size = batch_size, val_loss = val_loss)
      results <- rbind(results, result)
    }
  }
  return(results)
}
#Defining grids
units_grid <- c(2, 4, 8)
batch_size_grid <- c(5, 10, 20)

grid_results <- grid_search(units_grid, batch_size_grid, lstm_input_train, trainset_y, lstm_input_validation, testset_y)
print(grid_results)
best_params <- grid_results %>% filter(val_loss == min(val_loss))

print("Best Hyperparameters:")
print(best_params)
l_gdp <- log(gdp)
ld_gdp <- diff(l_gdp)
dld_gdp <- diff(ld_gdp)
