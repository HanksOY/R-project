##Data cleaning##
library(dplyr)

# Read  datasets
employee = read.csv("Path.../employee_survey_data_clean.csv")
general = read.csv("Path.../general_data.csv")
manager = read.csv("Path.../manager_survey_data.csv")

#Merge datasets
merge_data <- employee %>% inner_join(general, by = "EmployeeID") %>% inner_join(manager, by = "EmployeeID")

#Convert to numeric
merge_data[, c(2:5, 9:10, 12, 14, 17:18, 20:29)] <- lapply(merge_data[, c(2:5, 9:10, 12, 14, 17:18, 20:29)], as.numeric)

View(merge_data)

summary(merge_data)

#NA to mean convertion function
NA_mean <- function(x) {
  if(is.numeric(x)) {
    x[is.na(x)] <- round(mean(x, na.rm = TRUE))
  }
  return(x)
}

merge_data_NA_remove <- data.frame(sapply(merge_data, NA_mean))
View(merge_data_NA_remove)


#Check data
dim(merge_data)
sum(is.na(merge_data_NA_remove))

merge_data_NA_remove[, c(2:5, 9:10, 12, 14, 17:18, 20:29)] <- lapply(merge_data_NA_remove[, c(2:5, 9:10, 12, 14, 17:18, 20:29)], as.numeric)
summary(merge_data_NA_remove)

#Save as excel file
install.packages("openxlsx")
library(openxlsx)

write.xlsx(merge_data_NA_remove, file = "E:/Learning/Hofstra/BAN 270 Data mining/Mid term exam/merge_data_NA_remove.xlsx")

install.packages("readxl")
library(readxl)

merge_data_NA_remove <- read_excel("E:/Learning/Hofstra/BAN 270 Data mining/Mid term exam/merge_data_NA_remove.xlsx")

View(merge_data_NA_remove)
summary(merge_data_NA_remove)

##Check discrimination##
#Plot MonthlyIncome by Gender
#mean_income_gender <- merge_data_NA_remove %>% group_by(Gender) %>% 
summarize(mean_income_gender = mean(MonthlyIncome) )

ggplot(merge_data_NA_remove, aes(x = Gender, y = MonthlyIncome, fill = Gender)) + 
  geom_bar(stat = "summary", fun = mean) + labs("Mean Monthly Income by Gender",x = "Gender",y = "Mean Monthly Income") +
  theme_minimal()

#Plot Education by Gender
ggplot(merge_data_NA_remove, aes(x = Gender, y= Education, fill = Gender)) + 
  geom_bar(stat = "summary", fun = mean) + labs("Education by Gender",x = "Gender",y = "Mean Education") +
  theme_minimal()

#Plot EmployeeCount by Gender
ggplot(merge_data_NA_remove, aes(x = Gender, y= EmployeeCount, fill = Gender)) + 
  geom_bar(stat = "summary", fun = sum) + labs("Count by Gender",x = "Gender",y = "Quantity") +
  theme_minimal()

#Plot JobLevel by Gender
ggplot(merge_data_NA_remove, aes(x = Gender, y= JobLevel, fill = Gender)) + 
  geom_bar(stat = "summary", fun = mean) + labs("JobLevel by Gender",x = "Gender",y = "Job Level") +
  theme_minimal()

#Plot PercentSalaryHike by Gender
ggplot(merge_data_NA_remove, aes(x = Gender, y= PercentSalaryHike, fill = Gender)) + 
  geom_bar(stat = "summary", fun = mean) + labs("PercentSalaryHike by Gender",x = "Gender",y = "Percent of Salary Hike") +
  theme_minimal()

#Plot StockOptionLevel by Gender
ggplot(merge_data_NA_remove, aes(x = Gender, y= StockOptionLevel, fill = Gender)) + 
  geom_bar(stat = "summary", fun = mean) + labs("StockOptionLevel by Gender",x = "Gender",y = "Stock Option Level") +
  theme_minimal()

#Plot YearsSinceLastPromotion by Gender
ggplot(merge_data_NA_remove, aes(x = Gender, y= YearsSinceLastPromotion, fill = Gender)) + 
  geom_bar(stat = "summary", fun = mean) + labs("YearsSinceLastPromotion by Gender",x = "Gender",y = "Years Since Last Promotion") +
  theme_minimal()

#Plot PerformanceRating by Gender
ggplot(merge_data_NA_remove, aes(x = Gender, y= PerformanceRating, fill = Gender)) + 
  geom_bar(stat = "summary", fun = mean) + labs("Performance Rating by Gender",x = "Gender",y = "Performance Rating") +
  theme_minimal()



##Find the factors of attrition##
#Convert Attrintion to numeric value
merge_data_NA_remove <- merge_data_NA_remove %>% mutate(Attrition_numeric = as.numeric(Attrition == "Yes"))
table(merge_data_NA_remove$Attrition_numeric)

#Load libraries
library(caret)
set.seed(3456)

##Method 1
#Try get correlations between JobSatisfaction and other vars
# List of other variables to test against JobSatisfaction
factor_vars <- c("EnvironmentSatisfaction", "WorkLifeBalance",  "Age", "DistanceFromHome", "Education",
                 "JobLevel", "NumCompaniesWorked", "PercentSalaryHike", "StockOptionLevel", "TotalWorkingYears",
                 "TrainingTimesLastYear", "YearsAtCompany", "YearsSinceLastPromotion", "YearsWithCurrManager",
                 "JobInvolvement", "MonthlyIncome", "PerformanceRating")


# Calculate correlations
correlations <- sapply(merge_data_NA_remove[factor_vars], function(x) cor(x, merge_data_NA_remove$JobSatisfaction, use = "complete.obs"))
correlations


# Test significance of correlations
cor_tests <- lapply(merge_data_NA_remove[factor_vars], function(x) cor.test(x, merge_data_NA_remove$JobSatisfaction, method = "pearson"))

# Extract p-values
cor_p_values <- sapply(cor_tests, function(x) x$p.value)

cor_p_values[cor_p_values < 0.05]


##Method 2
# Partition the data
trainIndex <- createDataPartition(merge_data_NA_remove$Attrition_numeric, p = .8, 
                                  list = FALSE, times = 1)

# Create training and testing sets
trainingSet <- merge_data_NA_remove[trainIndex, ]
testingSet <- merge_data_NA_remove[-trainIndex, ]

# Train the logistic regression model on the training set
lr_attrition_train <- glm(Attrition_numeric ~ EnvironmentSatisfaction +	JobSatisfaction +	
                            WorkLifeBalance	+ Age + Department	+ PercentSalaryHike	+ JobRole	+ 
                            MaritalStatus	+ MonthlyIncome	+ NumCompaniesWorked	+ StockOptionLevel +
                            TotalWorkingYears	+ TrainingTimesLastYear	+ YearsSinceLastPromotion	+ 
                            YearsWithCurrManager	+ JobInvolvement	+ PerformanceRating, data = trainingSet, 
                          family=binomial(link="logit"), control = glm.control(maxit = 50))

# Use stepwise regression to simplify the model
stepwise_model <- step(lr_attrition_train, direction="both")

# Predict the testing set using the stepwise model
testingSet$predicted_prob <- predict(stepwise_model, newdata=testingSet, type = "response")

# Calculate binary predictions and residuals
testingSet$predicted_class <- ifelse(testingSet$predicted_prob > 0.5, 1, 0)
testingSet$residuals <- testingSet$Attrition_numeric - testingSet$predicted_prob

# Convert both predicted classes and actual classes to factors with levels 0 and 1
testingSet$predicted_class <- factor(testingSet$predicted_class, levels = c(0, 1))
testingSet$Attrition_numeric <- factor(testingSet$Attrition_numeric, levels = c(0, 1))

# Create the confusion matrix
conf_matrix <- confusionMatrix(data = testingSet$predicted_class, reference = testingSet$Attrition_numeric)

# Output model performance
print(conf_matrix)

# histogram of residuals for testing set
hist_res_test <- ggplot(testingSet, aes(x=residuals)) + geom_histogram() + ggtitle("Histogram of residuals (test)")
hist_res_test

#Model validation by computing accuracy
accuracy <- conf_matrix$overall['Accuracy']
print(accuracy)

#Apply model to whole data
lr_attrition <- glm(Attrition_numeric ~ EnvironmentSatisfaction +	JobSatisfaction +	
                      WorkLifeBalance	+ Age + Department	+ PercentSalaryHike	+ JobRole	+ 
                      MaritalStatus	+ MonthlyIncome	+ NumCompaniesWorked	+ StockOptionLevel +
                      TotalWorkingYears	+ TrainingTimesLastYear	+ YearsSinceLastPromotion	+ 
                      YearsWithCurrManager	+ JobInvolvement	+ PerformanceRating,  data = merge_data_NA_remove, 
                    family=binomial(link="logit"), control = glm.control(maxit = 50))

lm_SW_attrition <- step(lr_attrition, direction="both")

summary(lm_SW_attrition)

# Predict the Attrition using the stepwise model
merge_data_NA_remove$predicted_prob <- predict(lm_SW_attrition, newdata=merge_data_NA_remove, type = "response")
merge_data_NA_remove$predicted_raw <- predict(lm_SW_attrition, newdata=merge_data_NA_remove)

# Calculate binary predictions and residuals
merge_data_NA_remove$predicted_class <- ifelse(merge_data_NA_remove$predicted_prob > 0.5, 1, 0)
merge_data_NA_remove$residuals <- merge_data_NA_remove$Attrition_numeric - merge_data_NA_remove$predicted_prob

# Convert both predicted classes and actual classes to factors with levels 0 and 1
merge_data_NA_remove$predicted_class <- factor(merge_data_NA_remove$predicted_class, levels = c(0, 1))
merge_data_NA_remove$Attrition_numeric <- factor(merge_data_NA_remove$Attrition_numeric, levels = c(0, 1))

# Create the confusion matrix
conf_matrix_whole <- confusionMatrix(data = merge_data_NA_remove$predicted_class, reference = merge_data_NA_remove$Attrition_numeric)


# Output model performance
print(conf_matrix_whole)

# histogram of residuals 
hist_res_whole <- ggplot(merge_data_NA_remove, aes(x=residuals)) + geom_histogram() + ggtitle("Histogram of residuals")
hist_res_whole

# Scatter plot  
ggplot(merge_data_NA_remove, aes(x=predicted_raw, y=residuals) )+ geom_point()

#Model validation by computing accuracy
accuracy_whole <- conf_matrix_whole$overall['Accuracy']
print(accuracy_whole)



##Method 3
#Run Regression Model including types of cars
lr_satisfaction <- lm(JobSatisfaction ~ EnvironmentSatisfaction +	WorkLifeBalance	+ Age + Department	+
                        PercentSalaryHike	+ JobRole	+ MaritalStatus	+ MonthlyIncome	+ NumCompaniesWorked	+ 
                        StockOptionLevel +TotalWorkingYears	+ TrainingTimesLastYear	+ YearsSinceLastPromotion	+ 
                        YearsWithCurrManager	+ JobInvolvement	+ PerformanceRating, data = trainingSet)
summary(lr_satisfaction)

#Check multicollineararity
library(car)
vif(lr_satisfaction)

#Test Regression Assumptions – 1: Mean of Residuals is zero
mean(lr_satisfaction$residuals)

#Test for Regression Assumptions – 2: Residuals are normally distributed
install.packages("nortest")
library(nortest)
residuals_lr_satisfaction <- resid(lr_satisfaction)
shapiro.test(residuals_lr_satisfaction)

#Test for Regression Assumptions – 3: Residuals have constant variance
install.packages("lmtest")
library(lmtest)
bptest(lr_satisfaction)

#Test for Regression Assumptions – 4: Independence of Residuals
durbinWatsonTest(lr_satisfaction)