library(readxl)
library(Metrics)
library(neuralnet)
library(grid)
library(MASS)
#Loading the data set
ExchangeUSD<- read_excel("ExchangeUSD (2).xlsx")
str(ExchangeUSD)
exchange_rate <- ExchangeUSD [[3]]

#Creating Time-delayed variables (AR Models)

#AR(1)
lag_1 <- c(NA,exchange_rate[1:(length(exchange_rate)-1)])

#AR(2)
lag_2 <- c(NA,NA,exchange_rate[1:(length(exchange_rate)-2)])

#AR(3)
lag_3 <- c(NA,NA,NA,exchange_rate[1:(length(exchange_rate)-3)])

#AR(4)
lag_4 <- c(NA,NA,NA,NA,exchange_rate[1:(length(exchange_rate)-4)])


#Constructing Input/Output Matrix   

# Combine the lagged variables into data frames for each AR model
data_ar1 <- data.frame(lag_1)
data_ar2 <- data.frame(lag_1, lag_2)
data_ar3 <- data.frame(lag_1, lag_2, lag_3)
data_ar4 <- data.frame(lag_1, lag_2, lag_3, lag_4)

# Ensure the data frames are without any NA values to match the output correctly
data_ar1 <- na.omit(data_ar1)
data_ar2 <- na.omit(data_ar2)
data_ar3 <- na.omit(data_ar3)
data_ar4 <- na.omit(data_ar4)


# Create output vectors for each AR model
output_ar1 <- exchange_rate[(1 + 1):length(exchange_rate)]
output_ar2 <- exchange_rate[(2 + 1):length(exchange_rate)]
output_ar3 <- exchange_rate[(3 + 1):length(exchange_rate)]
output_ar4 <- exchange_rate[(4 + 1):length(exchange_rate)]

# Ensure the output vectors are correctly sized to match the input data frames
output_ar1 <- output_ar1[1:nrow(data_ar1)]
output_ar2 <- output_ar2[1:nrow(data_ar2)]
output_ar3 <- output_ar3[1:nrow(data_ar3)]
output_ar4 <- output_ar4[1:nrow(data_ar4)]



# Combine into final datasets
final_dataset_ar1 <- data.frame(data_ar1, Output = output_ar1)
final_dataset_ar2 <- data.frame(data_ar2, Output = output_ar2)
final_dataset_ar3 <- data.frame(data_ar3, Output = output_ar3)
final_dataset_ar4 <- data.frame(data_ar4, Output = output_ar4)

# Get last index for AR(1) dataset
last_index_ar1 <- nrow(final_dataset_ar1)


# Get last index for AR(2) dataset
last_index_ar2 <- nrow(final_dataset_ar2)


# Get last index for AR(3) dataset
last_index_ar3 <- nrow(final_dataset_ar3)


# Get last index for AR(4) dataset
last_index_ar4 <- nrow(final_dataset_ar4)



#Normalizing function
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}


# later on we’ll apply the reverse of normalization to the network output
Reverse_normalization <- function(x, min, max) {
  return ((max - min) * x + min)
}
#Before normalizing 

#AR1
train_data_ar1_a <-final_dataset_ar1[1:400,]
test_data_ar1_a <-final_dataset_ar1[400:last_index_ar1,]
min_val_ar1 <-min(train_data_ar1_a)
max_val_ar1 <-max(train_data_ar1_a)



#AR2
train_data_ar2_a <-final_dataset_ar2[1:400,]
test_data_ar2_a <-final_dataset_ar2[400:last_index_ar2,]
min_val_ar2 <-min(train_data_ar2_a)
max_val_ar2 <-max(train_data_ar2_a)

#A3
train_data_ar3_a <-final_dataset_ar3[1:400,]
test_data_ar3_a <-final_dataset_ar3[400:last_index_ar3,]
min_val_ar3 <-min(train_data_ar3_a)
max_val_ar3 <-max(train_data_ar3_a)


#A4
train_data_ar4_a <-final_dataset_ar4[1:400,]
test_data_ar4_a <-final_dataset_ar4[400:last_index_ar4,]
min_val_ar4 <-min(train_data_ar4_a)
max_val_ar4 <-max(train_data_ar4_a)



#Applying Normalization for each column in I/O matrices
#Final Data set ar1
final_dataset_ar1$lag_1 <- normalize(final_dataset_ar1$lag_1)
final_dataset_ar1$Output <-normalize(final_dataset_ar1$Output)

#Final Data set ar2
final_dataset_ar2$lag_1 <-normalize(final_dataset_ar2$lag_1)
final_dataset_ar2$lag_2 <-normalize(final_dataset_ar2$lag_2)
final_dataset_ar2$Output<-normalize(final_dataset_ar2$Output)

#Final Data set ar3
final_dataset_ar3$lag_1 <-normalize(final_dataset_ar3$lag_3)
final_dataset_ar3$lag_2 <- normalize(final_dataset_ar3$lag_2)
final_dataset_ar3$lag_3 <- normalize(final_dataset_ar3$lag_3)
final_dataset_ar3$Output <- normalize(final_dataset_ar3$Output)

# Normalize AR(4) Dataset
final_dataset_ar4$lag_1 <- normalize(final_dataset_ar4$lag_1)
final_dataset_ar4$lag_2 <- normalize(final_dataset_ar4$lag_2)
final_dataset_ar4$lag_3 <- normalize(final_dataset_ar4$lag_3)
final_dataset_ar4$lag_4 <- normalize(final_dataset_ar4$lag_4)
final_dataset_ar4$Output <- normalize(final_dataset_ar4$Output)

summary(final_dataset_ar1)
summary(final_dataset_ar2)
summary(final_dataset_ar3)
summary(final_dataset_ar4)


#Spliting normalized train data and test data for each data set
#AR1
train_data_ar1 <-final_dataset_ar1[1:400,]
test_data_ar1 <-final_dataset_ar1[400:last_index_ar1,]




#AR2
train_data_ar2 <-final_dataset_ar2[1:400,]
test_data_ar2 <-final_dataset_ar2[400:last_index_ar2,]


#A3
train_data_ar3 <-final_dataset_ar3[1:400,]
test_data_ar3 <-final_dataset_ar3[400:last_index_ar3,]



#A4
train_data_ar4 <-final_dataset_ar4[1:400,]
test_data_ar4 <-final_dataset_ar4[400:last_index_ar4,]





#For AR(1) with 1 hidden layer and 10 Neurons 
nn_ar1_1 <- neuralnet(Output~lag_1,hidden = 10,data=train_data_ar1,linear.output = TRUE)
plot(nn_ar1_1)
nn_ar1_1_results <-compute(nn_ar1_1,test_data_ar1)
predicted_nn_ar1_1_results <-nn_ar1_1_results$net.result
predicted_nn_ar1_1_results_original <- Reverse_normalization(predicted_nn_ar1_1_results,min_val_ar1,max_val_ar1)
original_nn_ar1_1 <- exchange_rate[401:500]
final_nn_ar1_1 <- cbind(original_nn_ar1_1, predicted_nn_ar1_1_results_original)
colnames(final_nn_ar1_1) <- c("Actual", "Predicted")
head(final_nn_ar1_1)

rmse_ar1_1 <- rmse(final_nn_ar1_1[, "Actual"], final_nn_ar1_1[, "Predicted"])
print(rmse_ar1_1)

mae_ar1_1 <- mae(final_nn_ar1_1[, "Actual"], final_nn_ar1_1[, "Predicted"])
print(mae_ar1_1)

mape_ar1_1 <- mape(final_nn_ar1_1[, "Actual"], final_nn_ar1_1[, "Predicted"])
print(mape_ar1_1)

smape_ar1_1 <- smape(final_nn_ar1_1[, "Actual"], final_nn_ar1_1[, "Predicted"])
print(smape_ar1_1)


#For AR(1) with 2 hidden layers and 10 neurons each
nn_ar1_2 <-neuralnet(Output~lag_1,hidden = c(10,10),data=train_data_ar1,linear.output = FALSE)
plot(nn_ar1_2)
nn_ar1_2_results <-compute(nn_ar1_2,test_data_ar1)
predicted_nn_ar1_2_results <- nn_ar1_2_results$net.result
predicted_nn_ar1_2_results_original<-Reverse_normalization(predicted_nn_ar1_2_results,min_val_ar1,max_val_ar1)
original_nn_ar1_2 <- exchange_rate[401:500]
final_nn_ar1_2 <- cbind(original_nn_ar1_2,predicted_nn_ar1_2_results_original)
colnames(final_nn_ar1_2) <- c("Actual", "Predicted")
head(final_nn_ar1_2)
rmse_ar1_2 <- rmse(final_nn_ar1_2[, "Actual"], final_nn_ar1_2[, "Predicted"])
print(rmse_ar1_2)

mae_ar1_2 <- mae(final_nn_ar1_2[, "Actual"], final_nn_ar1_2[, "Predicted"])
print(mae_ar1_2)

mape_ar1_2 <- mape(final_nn_ar1_2[, "Actual"], final_nn_ar1_2[, "Predicted"])
print(mape_ar1_2)

smape_ar1_2 <- smape(final_nn_ar1_2[, "Actual"], final_nn_ar1_2[, "Predicted"])
print(smape_ar1_2)

#For AR(1) with 2 hidden layers and 15 neurons each 
nn_ar1_3 <- neuralnet(Output ~ lag_1,hidden = c(15,15),data=train_data_ar1,linear.output = TRUE)
plot(nn_ar1_3)
nn_ar1_3_results <-compute(nn_ar1_3,test_data_ar1)
predicted_nn_ar1_3_results <- nn_ar1_3_results$net.result
predicted_nn_ar1_3_results_original<-Reverse_normalization(predicted_nn_ar1_3_results,min_val_ar1,max_val_ar1)
original_nn_ar1_3 <- exchange_rate[401:500]
final_nn_ar1_3 <- cbind(original_nn_ar1_3,predicted_nn_ar1_3_results_original)
colnames(final_nn_ar1_3) <- c("Actual", "Predicted")
head(final_nn_ar1_3)
rmse_ar1_3 <- rmse(final_nn_ar1_3[, "Actual"], final_nn_ar1_3[, "Predicted"])
print(rmse_ar1_3)

mae_ar1_3 <- mae(final_nn_ar1_3[, "Actual"], final_nn_ar1_3[, "Predicted"])
print(mae_ar1_3)

mape_ar1_3 <- mape(final_nn_ar1_3[, "Actual"], final_nn_ar1_3[, "Predicted"])
print(mape_ar1_3)

smape_ar1_3<- smape(final_nn_ar1_3[, "Actual"], final_nn_ar1_3[, "Predicted"])
print(smape_ar1_3)

#For AR(2) with 1 hidden layer and 10 neurons
nn_ar2_1 <- neuralnet(Output ~ lag_1+lag_2,hidden =10,data=train_data_ar2,linear.output = FALSE)
plot(nn_ar2_1)
nn_ar2_1_results <- compute(nn_ar2_1,test_data_ar2)
predicted_nn_ar2_1_results <- nn_ar2_1_results$net.result
predicted_nn_ar2_1_results_original<-Reverse_normalization(predicted_nn_ar2_1_results,min_val_ar2,max_val_ar2)
original_nn_ar2_1 <- exchange_rate[402:500]
final_nn_ar2_1 <- cbind(original_nn_ar2_1,predicted_nn_ar2_1_results_original)
colnames(final_nn_ar2_1) <- c("Actual", "Predicted")
head(final_nn_ar2_1)
rmse_ar2_1 <- rmse(final_nn_ar2_1[, "Actual"], final_nn_ar2_1[, "Predicted"])
print(rmse_ar2_1)

mae_ar2_1 <- mae(final_nn_ar2_1[, "Actual"], final_nn_ar2_1[, "Predicted"])
print(mae_ar2_1)

mape_ar2_1 <- mape(final_nn_ar2_1[, "Actual"], final_nn_ar2_1[, "Predicted"])
print(mape_ar2_1)

smape_ar2_1<- smape(final_nn_ar2_1[, "Actual"], final_nn_ar2_1[, "Predicted"])
print(smape_ar2_1)

#For AR(2) with 2 hidden layer and 10 neurons each
nn_ar2_2 <- neuralnet(Output ~ lag_1+lag_2,hidden =c(10,10),data=train_data_ar2,linear.output = TRUE)
plot(nn_ar2_2)
nn_ar2_2_results <- compute(nn_ar2_2,test_data_ar2)
predicted_nn_ar2_2_results <- nn_ar2_2_results$net.result
predicted_nn_ar2_2_results_original<-Reverse_normalization(predicted_nn_ar2_2_results,min_val_ar2,max_val_ar2)
original_nn_ar2_2 <- exchange_rate[402:500]
final_nn_ar2_2 <- cbind(original_nn_ar2_2,predicted_nn_ar2_2_results_original)
colnames(final_nn_ar2_2) <- c("Actual", "Predicted")
head(final_nn_ar2_2)
rmse_ar2_2 <- rmse(final_nn_ar2_2[, "Actual"], final_nn_ar2_2[, "Predicted"])
print(rmse_ar2_2)

mae_ar2_2 <- mae(final_nn_ar2_2[, "Actual"], final_nn_ar2_2[, "Predicted"])
print(mae_ar2_2)

mape_ar2_2 <- mape(final_nn_ar2_2[, "Actual"], final_nn_ar2_2[, "Predicted"])
print(mape_ar2_2)

smape_ar2_2<- smape(final_nn_ar2_2[, "Actual"], final_nn_ar2_2[, "Predicted"])
print(smape_ar2_2)

#For A(2) with 2 hidden layer and 15 neurons each 
nn_ar2_3 <- neuralnet(Output ~ lag_1+lag_2,hidden =c(15,15),data=train_data_ar2,linear.output = TRUE)
plot(nn_ar2_3)
nn_ar2_3_results <- compute(nn_ar2_3,test_data_ar2)
predicted_nn_ar2_3_results <- nn_ar2_3_results$net.result
predicted_nn_ar2_3_results_original<-Reverse_normalization(predicted_nn_ar2_3_results,min_val_ar2,max_val_ar2)
original_nn_ar2_3 <- exchange_rate[402:500]
final_nn_ar2_3 <- cbind(original_nn_ar2_3,predicted_nn_ar2_3_results_original)
colnames(final_nn_ar2_3) <- c("Actual", "Predicted")
head(final_nn_ar2_3)
rmse_ar2_3 <- rmse(final_nn_ar2_3[, "Actual"], final_nn_ar2_3[, "Predicted"])
print(rmse_ar2_3)

mae_ar2_3 <- mae(final_nn_ar2_3[, "Actual"], final_nn_ar2_3[, "Predicted"])
print(mae_ar2_3)

mape_ar2_3 <- mape(final_nn_ar2_3[, "Actual"], final_nn_ar2_3[, "Predicted"])
print(mape_ar2_3)

smape_ar2_3<- smape(final_nn_ar2_3[, "Actual"], final_nn_ar2_3[, "Predicted"])
print(smape_ar2_3)


#For A(3) with 1 hidden layer and 10 neurons
nn_ar3_1 <- neuralnet(Output ~ lag_1+lag_2+lag_3,hidden =10,data=train_data_ar3,linear.output = TRUE)
plot(nn_ar3_1)
nn_ar3_1_results <- compute(nn_ar3_1,test_data_ar3)
predicted_nn_ar3_1_results <- nn_ar3_1_results$net.result
predicted_nn_ar3_1_results_original<-Reverse_normalization(predicted_nn_ar3_1_results,min_val_ar3,max_val_ar3)
original_nn_ar3_1 <- exchange_rate[403:500]
final_nn_ar3_1 <- cbind(original_nn_ar3_1,predicted_nn_ar3_1_results_original)
colnames(final_nn_ar3_1) <- c("Actual", "Predicted")
head(final_nn_ar3_1)
rmse_ar3_1 <- rmse(final_nn_ar3_1[, "Actual"], final_nn_ar3_1[, "Predicted"])
print(rmse_ar3_1)

mae_ar3_1 <- mae(final_nn_ar3_1[, "Actual"], final_nn_ar3_1[, "Predicted"])
print(mae_ar3_1)

mape_ar3_1 <- mape(final_nn_ar3_1[, "Actual"], final_nn_ar3_1[, "Predicted"])
print(mape_ar3_1)

smape_ar3_1<- smape(final_nn_ar3_1[, "Actual"], final_nn_ar3_1[, "Predicted"])
print(smape_ar3_1)

#For A(3)  with 2 hidden layer and 4 and 5 neurons
nn_ar3_2 <- neuralnet(Output ~ lag_1+lag_2+lag_3,hidden =c(4,5),data=train_data_ar3,linear.output = FALSE)
plot(nn_ar3_2)
nn_ar3_2_results <- compute(nn_ar3_2,test_data_ar3)
predicted_nn_ar3_2_results <- nn_ar3_2_results$net.result
predicted_nn_ar3_2_results_original<-Reverse_normalization(predicted_nn_ar3_2_results,min_val_ar3,max_val_ar3)
original_nn_ar3_2 <- exchange_rate[403:500]
final_nn_ar3_2 <- cbind(original_nn_ar3_2,predicted_nn_ar3_2_results_original)
colnames(final_nn_ar3_2) <- c("Actual", "Predicted")
head(final_nn_ar3_2)
rmse_ar3_2 <- rmse(final_nn_ar3_2[, "Actual"], final_nn_ar3_2[, "Predicted"])
print(rmse_ar3_2)

mae_ar3_2 <- mae(final_nn_ar3_2[, "Actual"], final_nn_ar3_2[, "Predicted"])
print(mae_ar3_2)

mape_ar3_2 <- mape(final_nn_ar3_2[, "Actual"], final_nn_ar3_2[, "Predicted"])
print(mape_ar3_2)

smape_ar3_2<- smape(final_nn_ar3_2[, "Actual"], final_nn_ar3_2[, "Predicted"])
print(smape_ar3_2)

#For A(3) with 2 hidden layers 15 and 15
nn_ar3_3 <- neuralnet(Output ~ lag_1+lag_2+lag_3,hidden =c(15,15),data=train_data_ar3,linear.output = TRUE)
plot(nn_ar3_3)
nn_ar3_3_results <-compute(nn_ar3_3,test_data_ar3)
predicted_nn_ar3_3_results <- nn_ar3_3_results$net.result
predicted_nn_ar3_3_results_original<-Reverse_normalization(predicted_nn_ar3_3_results,min_val_ar3,max_val_ar3)
original_nn_ar3_3 <- exchange_rate[403:500]
final_nn_ar3_3 <- cbind(original_nn_ar3_3,predicted_nn_ar3_3_results_original)
colnames(final_nn_ar3_3) <- c("Actual", "Predicted")
head(final_nn_ar3_3)
rmse_ar3_3 <- rmse(final_nn_ar3_3[, "Actual"], final_nn_ar3_3[, "Predicted"])
print(rmse_ar3_3)

mae_ar3_3 <- mae(final_nn_ar3_3[, "Actual"], final_nn_ar3_3[, "Predicted"])
print(mae_ar3_3)

mape_ar3_3 <- mape(final_nn_ar3_3[, "Actual"], final_nn_ar3_3[, "Predicted"])
print(mape_ar3_3)

smape_ar3_3<- smape(final_nn_ar3_3[, "Actual"], final_nn_ar3_3[, "Predicted"])
print(smape_ar3_3)

#For A(4) with 1 hidden layer with 10 neurons
nn_ar4_1 <- neuralnet(Output ~ lag_1+lag_2+lag_3+lag_4,hidden =10,data=train_data_ar4,linear.output = TRUE)
plot(nn_ar4_1)
nn_ar4_1_results <-compute(nn_ar4_1,test_data_ar4)
predicted_nn_ar4_1_results <- nn_ar4_1_results$net.result
predicted_nn_ar4_1_results_original<-Reverse_normalization(predicted_nn_ar4_1_results,min_val_ar4,max_val_ar4)
original_nn_ar4_1 <- exchange_rate[404:500]
final_nn_ar4_1 <- cbind(original_nn_ar4_1,predicted_nn_ar4_1_results_original)
colnames(final_nn_ar4_1) <- c("Actual", "Predicted")
head(final_nn_ar4_1)
rmse_ar4_1 <- rmse(final_nn_ar4_1[, "Actual"], final_nn_ar4_1[, "Predicted"])
print(rmse_ar4_1)

mae_ar4_1 <- mae(final_nn_ar4_1[, "Actual"], final_nn_ar4_1[, "Predicted"])
print(mae_ar4_1)

mape_ar4_1 <- mape(final_nn_ar4_1[, "Actual"], final_nn_ar4_1[, "Predicted"])
print(mape_ar4_1)

smape_ar4_1<- smape(final_nn_ar4_1[, "Actual"], final_nn_ar4_1[, "Predicted"])
print(smape_ar4_1)

#For A(4) with 2 hidden layers with 4 and 5
nn_ar4_2 <- neuralnet(Output ~ lag_1+lag_2+lag_3+lag_4,hidden =c(4,5),data=train_data_ar4,linear.output = TRUE)
plot(nn_ar4_2)
nn_ar4_2_results <-compute(nn_ar4_2,test_data_ar4)
predicted_nn_ar4_2_results <- nn_ar4_2_results$net.result
predicted_nn_ar4_2_results_original<-Reverse_normalization(predicted_nn_ar4_2_results,min_val_ar4,max_val_ar4)
original_nn_ar4_2 <- exchange_rate[404:500]
final_nn_ar4_2 <- cbind(original_nn_ar4_2,predicted_nn_ar4_2_results_original)
colnames(final_nn_ar4_2) <- c("Actual", "Predicted")
head(final_nn_ar4_2)
rmse_ar4_2 <- rmse(final_nn_ar4_2[, "Actual"], final_nn_ar4_2[, "Predicted"])
print(rmse_ar4_2)

mae_ar4_2 <- mae(final_nn_ar4_2[, "Actual"], final_nn_ar4_2[, "Predicted"])
print(mae_ar4_2)

mape_ar4_2 <- mape(final_nn_ar4_2[, "Actual"], final_nn_ar4_2[, "Predicted"])
print(mape_ar4_2)

smape_ar4_2<- smape(final_nn_ar4_2[, "Actual"], final_nn_ar4_2[, "Predicted"])
print(smape_ar4_2)

#For A(4) with 2 hidden layers with 15 and 15 
nn_ar4_3 <- neuralnet(Output ~ lag_1+lag_2+lag_3+lag_4,hidden =c(15,15),data=train_data_ar4,linear.output = TRUE)
plot(nn_ar4_3)
nn_ar4_3_results <-compute(nn_ar4_3,test_data_ar4)
predicted_nn_ar4_3_results <- nn_ar4_3_results$net.result
predicted_nn_ar4_3_results_original<-Reverse_normalization(predicted_nn_ar4_3_results,min_val_ar4,max_val_ar4)
original_nn_ar4_3 <- exchange_rate[404:500]
final_nn_ar4_3 <- cbind(original_nn_ar4_3,predicted_nn_ar4_3_results_original)
colnames(final_nn_ar4_3) <- c("Actual", "Predicted")
head(final_nn_ar4_3)
rmse_ar4_3 <- rmse(final_nn_ar4_3[, "Actual"], final_nn_ar4_3[, "Predicted"])
print(rmse_ar4_3)

mae_ar4_3 <- mae(final_nn_ar4_3[, "Actual"], final_nn_ar4_3[, "Predicted"])
print(mae_ar4_3)

mape_ar4_3 <- mape(final_nn_ar4_3[, "Actual"], final_nn_ar4_3[, "Predicted"])
print(mape_ar4_3)

smape_ar4_3<- smape(final_nn_ar4_3[, "Actual"], final_nn_ar4_3[, "Predicted"])
print(smape_ar4_3)


#For A(2) with 3 hidden layers 4,4,5

nn_ar2_4 <- neuralnet(Output ~ lag_1+lag_2,hidden =c(4,4,5),data=train_data_ar2,linear.output = FALSE)
plot(nn_ar2_4)
nn_ar2_4_results <- compute(nn_ar2_4,test_data_ar2)
predicted_nn_ar2_4_results <- nn_ar2_4_results$net.result
predicted_nn_ar2_4_results_original<-Reverse_normalization(predicted_nn_ar2_4_results,min_val_ar2,max_val_ar2)
original_nn_ar2_4 <- exchange_rate[402:500]
final_nn_ar2_4 <- cbind(original_nn_ar2_4,predicted_nn_ar2_4_results_original)
colnames(final_nn_ar2_4) <- c("Actual", "Predicted")
head(final_nn_ar2_4)
rmse_ar2_4 <- rmse(final_nn_ar2_4[, "Actual"], final_nn_ar2_4[, "Predicted"])
print(rmse_ar2_4)

mae_ar2_4 <- mae(final_nn_ar2_4[, "Actual"], final_nn_ar2_4[, "Predicted"])
print(mae_ar2_4)

mape_ar2_4 <- mape(final_nn_ar2_4[, "Actual"], final_nn_ar2_4[, "Predicted"])
print(mape_ar2_4)

smape_ar2_4<- smape(final_nn_ar2_4[, "Actual"], final_nn_ar2_4[, "Predicted"])
print(smape_ar2_4)

#For A(1) with hidden layers 4,4,5
nn_ar1_4 <- neuralnet(Output~lag_1,hidden = c(4,4,5),data=train_data_ar1,linear.output = TRUE)
plot(nn_ar1_4)
nn_ar1_4_results <-compute(nn_ar1_4,test_data_ar1)
predicted_nn_ar1_4_results <-nn_ar1_4_results$net.result
predicted_nn_ar1_4_results_original <- Reverse_normalization(predicted_nn_ar1_4_results,min_val_ar1,max_val_ar1)
original_nn_ar1_4 <- exchange_rate[401:500]
final_nn_ar1_4 <- cbind(original_nn_ar1_4, predicted_nn_ar1_4_results_original)
colnames(final_nn_ar1_4) <- c("Actual", "Predicted")
head(final_nn_ar1_4)

rmse_ar1_4 <- rmse(final_nn_ar1_4[, "Actual"], final_nn_ar1_4[, "Predicted"])
print(rmse_ar1_4)

mae_ar1_4 <- mae(final_nn_ar1_4[, "Actual"], final_nn_ar1_4[, "Predicted"])
print(mae_ar1_4)

mape_ar1_4 <- mape(final_nn_ar1_1[, "Actual"], final_nn_ar1_4[, "Predicted"])
print(mape_ar1_4)

smape_ar1_4 <- smape(final_nn_ar1_4[, "Actual"], final_nn_ar1_4[, "Predicted"])
print(smape_ar1_4)

#For A(4) with hidden layers 1,2
nn_ar4_4 <- neuralnet(Output ~ lag_1+lag_2+lag_3+lag_4,hidden =c(1,2),data=train_data_ar4,linear.output = TRUE)
plot(nn_ar4_4)
nn_ar4_4_results <-compute(nn_ar4_4,test_data_ar4)
predicted_nn_ar4_4_results <- nn_ar4_3_results$net.result
predicted_nn_ar4_4_results_original<-Reverse_normalization(predicted_nn_ar4_4_results,min_val_ar4,max_val_ar4)
original_nn_ar4_4 <- exchange_rate[404:500]
final_nn_ar4_4 <- cbind(original_nn_ar4_4,predicted_nn_ar4_4_results_original)
colnames(final_nn_ar4_4) <- c("Actual", "Predicted")
head(final_nn_ar4_4)
rmse_ar4_4 <- rmse(final_nn_ar4_4[, "Actual"], final_nn_ar4_4[, "Predicted"])
print(rmse_ar4_4)

mae_ar4_4 <- mae(final_nn_ar4_4[, "Actual"], final_nn_ar4_4[, "Predicted"])
print(mae_ar4_4)

mape_ar4_4 <- mape(final_nn_ar4_4[, "Actual"], final_nn_ar4_4[, "Predicted"])
print(mape_ar4_4)

smape_ar4_4<- smape(final_nn_ar4_4[, "Actual"], final_nn_ar4_4[, "Predicted"])
print(smape_ar4_4)


#Model Comparishion
rmse_all <- c(rmse_ar1_1,rmse_ar1_2,rmse_ar1_3,rmse_ar2_1,rmse_ar2_2,rmse_ar2_3,rmse_ar3_1,rmse_ar3_2,rmse_ar3_3,rmse_ar4_1,rmse_ar4_2,rmse_ar4_3,rmse_ar2_4,rmse_ar1_4,rmse_ar4_4)
mae_all <- c(mae_ar1_1,mae_ar1_2,mae_ar1_3,mae_ar2_1,mae_ar2_2,mae_ar2_3,mae_ar3_1,mae_ar3_2,mae_ar3_3,mae_ar4_1,mae_ar4_2,mae_ar4_3,mae_ar2_4,mae_ar1_4,mae_ar4_4)
mape_all <- c(mape_ar1_1,mape_ar1_2,mape_ar1_3,mape_ar2_1,mape_ar2_2,mape_ar2_3,mape_ar3_1,mape_ar3_2,mape_ar3_3,mape_ar4_1,mape_ar4_2,mape_ar4_3,mape_ar2_4,mape_ar1_4,mape_ar4_4)
smape_all <- c(smape_ar1_1,smape_ar1_2,smape_ar1_3,smape_ar2_1,smape_ar2_3,smape_ar2_3,smape_ar3_1,smape_ar3_2,smape_ar3_3,smape_ar4_1,smape_ar4_2,smape_ar4_3,smape_ar2_4,smape_ar1_4,smape_ar4_4)


#creating comparision table

model_comparison_NARX = data.frame(
  Model = c("Model 1", "Model 2", "Model 3", "Model 4", "Model 5",
            "Model 6", "Model 7", "Model 8", "Model 9", "Model 10",
            "Model 11", "Model 12", "Model 13", "Model 14", "Model 15"),
  RMSE = rmse_all,
  MAE = mae_all,
  MAPE = mape_all,
  SMAPE = smape_all
)

# Print the data frame to view it
print(model_comparison_NARX)                               


#Model 15 is the best 

# Plotting the actual vs predicted values
plot(final_nn_ar4_4[, "Actual"], type = 'l', col = 'red', lwd = 2, ylim = range(final_nn_ar4_4), 
     xlab = "Observation", ylab = "Exchange Rate", main = "Comparison of Actual and Predicted Values")
lines(final_nn_ar4_4[, "Predicted"], col = 'blue', lwd = 2)
legend("topright", legend = c("Actual", "Predicted"), col = c("red", "blue"), lty = 1, lwd = 2)
