install.packages("readr")

library(readr)

source("http://blogs.fiveelementanalytics.com/RCode/min_max_normalization.R")

mnist_raw <- read_csv("https://pjreddie.com/media/files/mnist_train.csv", col_names = FALSE)
mnist_test <- read_csv("Path../mnist_test.csv", col_names = FALSE)

View(mnist_raw)
summary(mnist_test)
summary(mnist_raw)

### SECTION 1: 
#normalize  columns on a scale from 0 to 1, then convert them to data frame, except for the first column
mnist_raw[,-1] = as.data.frame(lapply(mnist_raw[,-1], min_max_normal))
mnist_test[,-1] = as.data.frame(lapply(mnist_test[,-1], min_max_normal))

View(mnist_raw)
# Clean data, replace missing value by 0
mnist_raw <- replace(mnist_raw, is.na(mnist_raw), 0)
mnist_test <- replace(mnist_test, is.na(mnist_test), 0)

dim(mnist_raw)

### SECTION 2
#Add new columns from "zero" to "nine". if the result of comparison is "True", convert it into
#numeric value "1", otherwise into "0"
mnist_raw[, "zero"] = as.numeric(mnist_raw[,"X1"] == 0)
mnist_raw[, "one"] = as.numeric(mnist_raw[,"X1"] == 1)
mnist_raw[, "two"] = as.numeric(mnist_raw[,"X1"] == 2)
mnist_raw[, "three"] = as.numeric(mnist_raw[,"X1"] == 3)
mnist_raw[, "four"] = as.numeric(mnist_raw[,"X1"] == 4)
mnist_raw[, "five"] = as.numeric(mnist_raw[,"X1"] == 5)
mnist_raw[, "six"] = as.numeric(mnist_raw[,"X1"] == 6)
mnist_raw[, "seven"] = as.numeric(mnist_raw[,"X1"] == 7)
mnist_raw[, "eight"] = as.numeric(mnist_raw[,"X1"] == 8)
mnist_raw[, "nine"] = as.numeric(mnist_raw[,"X1"] == 9)

mnist_test[, "zero"] = as.numeric(mnist_test[,"X1"] == 0)
mnist_test[, "one"] = as.numeric(mnist_test[,"X1"] == 1)
mnist_test[, "two"] = as.numeric(mnist_test[,"X1"] == 2)
mnist_test[, "three"] = as.numeric(mnist_test[,"X1"] == 3)
mnist_test[, "four"] = as.numeric(mnist_test[,"X1"] == 4)
mnist_test[, "five"] = as.numeric(mnist_test[,"X1"] == 5)
mnist_test[, "six"] = as.numeric(mnist_test[,"X1"] == 6)
mnist_test[, "seven"] = as.numeric(mnist_test[,"X1"] == 7)
mnist_test[, "eight"] = as.numeric(mnist_test[,"X1"] == 8)
mnist_test[, "nine"] = as.numeric(mnist_test[,"X1"] == 9)

View(mnist_raw)

### SECTION 3
#Create a column named "actual" that is the same as "X1"
mnist_raw$actual = mnist_raw[,1]
mnist_test$actual = mnist_test[,1]



#Exclude the first column and change the data into data frame
mnist_raw = as.data.frame(mnist_raw[,-1])
mnist_test = as.data.frame(mnist_test[,-1])

View(mnist_test)

###SECTION 4
#Create a formula(frm) with 'zero'-''nine' as dependent variables, and all the columns
#from mnist_raw as independent variables.
frm = 'zero + one + two + three + four + five + six + seven + eight + nine ~ '
for (i in 1:784) {
  
  frm = paste(frm, names(mnist_raw)[i],sep="+")
  
}
frm

#### SECTION 5
#Build up a training data 
mnist_raw_train8 = mnist_raw[1:60000,]

ncol(mnist_raw_train)

View(mnist_raw_train)

### SECTION 6 
input = ncol(mnist_raw) -10 
output = 10
neuron_estimate_start  =  (input+output)/3
neuron_estimate


#### SECTION 7  

install.packages("neuralnet")
library(neuralnet)

#Train the neural network by specifying the fomular, data(exclude actual number), 



##Optimize the code by hidden layers and nodes

# Print the total time taken to execute the line
execution_time8_500 <- system.time({nn8_500 = neuralnet(frm, data=mnist_raw_train8[,-795], 
                                                        hidden=c(500, 200),
                                                        linear.output=F)})
execution_time8_550 <- system.time({nn8_550 = neuralnet(frm, data=mnist_raw_train8[,-795], 
                                                        hidden=c(550, 150), 
                                                        linear.output=F)})
execution_time8_600 <- system.time({nn8_600 = neuralnet(frm, data=mnist_raw_train8[,-795], 
                                                        hidden=c(600, 150), 
                                                        linear.output=F)})
execution_time8_650 <- system.time({nn8_650 = neuralnet(frm, data=mnist_raw_train8[,-795], 
                                                        hidden=c(650, 100), 
                                                        linear.output=F)})
execution_time8_700 <- system.time({nn8_700 = neuralnet(frm, data=mnist_raw_train8[,-795], 
                                                        hidden=c(400, 200, 100),
                                                        linear.output=F)})


print(execution_time8_500)
print(execution_time8_550)
print(execution_time8_600)
print(execution_time8_650)
print(execution_time8_700)

#generate predictions from a neural network model nn using a subset of the mnist_raw_train data
#"covariate = "  define the input data
#"$net.result" extracts these predictions
predict_num8_500 = compute(nn8_500, covariate = mnist_raw_train8[,-(785:795)])$net.result
predict_num8_550 = compute(nn8_550, covariate = mnist_raw_train8[,-(785:795)])$net.result
predict_num8_600 = compute(nn8_600, covariate = mnist_raw_train8[,-(785:795)])$net.result
predict_num8_650 = compute(nn8_650, covariate = mnist_raw_train8[,-(785:795)])$net.result
predict_num8_700 = compute(nn8_700, covariate = mnist_raw_train8[,-(785:795)])$net.result

#Test error rate
error8_500 = 1 - mean((apply(predict_num8_500, 1, which.max) - 1) == mnist_raw[ , "actual"])
error8_550 = 1 - mean((apply(predict_num8_550, 1, which.max) - 1) == mnist_raw[ , "actual"])
error8_600 = 1 - mean((apply(predict_num8_600, 1, which.max) - 1) == mnist_raw[ , "actual"])
error8_650 = 1 - mean((apply(predict_num8_650, 1, which.max) - 1) == mnist_raw[ , "actual"])
error8_700 = 1 - mean((apply(predict_num8_700, 1, which.max) - 1) == mnist_raw[ , "actual"])

print(1-error8_500)
print(error8_550)
print(error8_600)
print(error8_650)
print(error8_700)

#Apply to test set
predict_test_500 = compute(nn8_500, covariate = mnist_test[,-(785:795)])$net.result
predict_test_550 = compute(nn8_550, covariate = mnist_test[,-(785:795)])$net.result
predict_test_600 = compute(nn8_600, covariate = mnist_test[,-(785:795)])$net.result
predict_test_650 = compute(nn8_650, covariate = mnist_test[,-(785:795)])$net.result
predict_test_700 = compute(nn8_700, covariate = mnist_test[,-(785:795)])$net.result


# Calculate the error rate for the test set
error_test_500 = 1 - mean((apply(predict_test_500, 1, which.max) - 1) == mnist_test[ , "actual"])
error_test_550 = 1 - mean((apply(predict_test_550, 1, which.max) - 1) == mnist_test[ , "actual"])
error_test_600 = 1 - mean((apply(predict_test_600, 1, which.max) - 1) == mnist_test[ , "actual"])
error_test_650 = 1 - mean((apply(predict_test_650, 1, which.max) - 1) == mnist_test[ , "actual"])
error_test_700 = 1 - mean((apply(predict_test_700, 1, which.max) - 1) == mnist_test[ , "actual"])

# Print the error rate
print(error_test_500)
print(error_test_550)
print(error_test_600)
print(error_test_650)
print(error_test_700)



library(ggplot2)

# Prepare the data
training_data <- data.frame(
  Nodes = c("500, 200", "550, 150", "600, 150", "650, 100", "400, 200, 100"),
  ExecutionTime = c(11490.3, 8920.09, 13714.91, 11532.36, 6991.92),
  ErrorRate = c(0.12, 0.16, 0.15, 0.17, 0.13)
)

test_data <- data.frame(
  Nodes = c("500, 200", "550, 150", "600, 150", "650, 100", "400, 200, 100"),
  ExecutionTime = c(11490.3, 8920.09, 13714.91, 11532.36, 6991.92),
  ErrorRate = c(4.05, 3.96, 4.28, 4.36, 4.66)
)

# Plotting the training error rates
ggplot(training_data, aes(x = Nodes, y = ErrorRate, group = 1)) +
  geom_line() +
  geom_point() +
  theme_minimal() +
  ggtitle("Training Error Rate by Nodes") +
  xlab("Nodes") +
  ylab("% Error")

# Plotting the test error rates
ggplot(test_data, aes(x = Nodes, y = ErrorRate, group = 1)) +
  geom_line() +
  geom_point() +
  theme_minimal() +
  ggtitle("Test Error Rate by Nodes") +
  xlab("Nodes") +
  ylab("% Error")




##Conduct KNN
library(class)
library(readr)

mnist_raw <- read_csv("https://pjreddie.com/media/files/mnist_train.csv", col_names = FALSE)
mnist_test <- read_csv("E:/Learning/Hofstra/BAN 270 Data mining/Final Exam/mnist_test.csv", col_names = FALSE)

View(mnist_raw)
summary(mnist_test)
summary(mnist_raw)

#normalize  columns on a scale from 0 to 1, then convert them to data frame, except for the first column
mnist_raw[,-1] = as.data.frame(lapply(mnist_raw[,-1], min_max_normal))
mnist_test[,-1] = as.data.frame(lapply(mnist_test[,-1], min_max_normal))

View(mnist_raw)
# Clean data, replace missing value by 0
mnist_raw <- replace(mnist_raw, is.na(mnist_raw), 0)
mnist_test <- replace(mnist_test, is.na(mnist_test), 0)

#Create a column named "actual" that is the same as "X1"
mnist_raw$actual = mnist_raw[,1]
mnist_test$actual = mnist_test[,1]

#Exclude the first column and change the data into data frame
mnist_raw = as.data.frame(mnist_raw[,-1])
mnist_test = as.data.frame(mnist_test[,-1])


# Train the KNN model
train_class <- unlist(mnist_raw$actual)
test_class <- unlist(mnist_test$actual)

knn_pred_train <- knn(train = mnist_raw[,-ncol(mnist_raw)],
                      test = mnist_raw[,-ncol(mnist_raw)],
                      cl = unlist(mnist_raw$actual), k = 3, prob = TRUE)

execution_time <- system.time({
  knn_pred_test <- knn(train = mnist_raw[,-ncol(mnist_raw)],
                       test = mnist_test[,-ncol(mnist_test)],
                       cl = unlist(mnist_raw$actual), k = 3, prob = TRUE)
})

# Print the execution time
print(execution_time)

# Calculate error rates
train_error_rate <- sum(knn_pred_train != train_class) / length(train_class)
test_error_rate <- sum(knn_pred_test != test_class) / length(test_class)

# Print error rates
print(paste("Training Error Rate:", train_error_rate))
print(paste("Test Error Rate:", test_error_rate))