data <- read.csv("C:\\Users\\sujit\\Downloads\\walmart (2).csv")
library(dplyr)

descriptive_stats <- summary(data)
descriptive_stats

missing_values <- colSums(is.na(data))
list(descriptive_stats = descriptive_stats, missing_values = missing_values)


dimensions <- dim(data)

cat("Number of Rows:", dimensions[1], "\n")
cat("Number of Columns:", dimensions[2], "\n")



data_types <- sapply(data, class)
print(data_types)

head(data,15)

data$CPI <- as.numeric(data$CPI)
data$Weekly_Sales <- as.numeric(data$Weekly_Sales)
data$Temperature <- as.numeric(data$Temperature)
data$Fuel_Price <- as.numeric(data$Fuel_Price)


hist(data$Weekly_Sales, main='Weekly_Sales', xlab='Values', col='blue', density=30, border='black')

# Plot histogram for CPI
hist(data$CPI, main='CPI', xlab='Values', col='blue', density=30, border='black')

# Plot histogram for Temperature
hist(data$Temperature, main='Temperature', xlab='Values', col='blue', density=30, border='black')

# Plot histogram for Fuel_Price
hist(data$Fuel_Price, main='Fuel_Price', xlab='Values', col='blue', density=30, border='black')

# Plot histogram for Unemployment
hist(data$Unemployment, main='Unemployment', xlab='Values', col='blue', density=30, border='black')



#Counting the holidays and non holidays
Holidays_count <- table(data$Holiday_Flag)
Holidays_count


Holiday_percentage <- paste(round(prop.table(Holidays_count) * 100, 1), "%", sep = "")

# Create a pie chart for Holiday count
pie(Holidays_count, labels = Holiday_percentage, main = "Holiday Analysis")

pno <- 1
par(mfrow=c(3,2), mar=c(4, 4, 2, 1))

columns <- c("Weekly_Sales", "Temperature", "Fuel_Price", "CPI", "Unemployment")

for (i in columns) {
  if (pno <= 5) {
    boxplot(data[[i]], main = paste("Boxplot of", i), col="skyblue", border="black", notch=TRUE)
    pno <- pno + 1
  }
}

# Reset the plotting parameters
par(mfrow=c(1, 1), mar=c(5, 4, 4, 2) + 0.1)



#Outlier treatment in this case would be done using the Quantile method
treat_outliers <- function(data, column_name) {
  # Extract the specified column
  column_data <- data[[column_name]]
  
  # Calculate the IQR
  q <- quantile(column_data, c(0.25, 0.75))
  iqr <- q[2] - q[1]
  
  # Define upper and lower bounds
  uw <- q[2] + 1.5 * iqr
  lw <- q[1] - 1.5 * iqr
  
  # Replace outliers with upper or lower bound
  data[[column_name]] <- ifelse(data[[column_name]] > uw, uw,
                                ifelse(data[[column_name]] < lw, lw, data[[column_name]]))
  
  return(data)
}

# Remvoing outlier for column "Weekly_Sales" , "Unemployment" and "Temperature"
data <- treat_outliers(data, "Weekly_Sales")
data <- treat_outliers(data, "Temperature")
data <- treat_outliers(data, "Unemployment")

#Boxplot after removing the outliers
boxplot(data["Weekly_Sales"], main = paste("Boxplot of Weekly_Sales"), col="skyblue", border="black", notch=TRUE)
boxplot(data["Temperature"], main = paste("Boxplot of Temperature"), col="skyblue", border="black", notch=TRUE)
boxplot(data["Unemployment"], main = paste("Boxplot of Unemployment"), col="skyblue", border="black", notch=TRUE)

data$month <- as.numeric(format(as.Date(data$Date), "%m"))
data$weekday <- weekdays(as.Date(data$Date))

head(data,10)


#plotting sales month wise
barplot(table(data$month), horiz = TRUE, main = "Bar Plot of Month Counts", xlab = "Counts", ylab = "Month")

#plotting sales weekdays wise
barplot(table(data$weekday), horiz = TRUE, main = "Bar Plot of weekdays Counts", xlab = "Counts", ylab = "weekdays")

#plotting sales Store wise
barplot(table(data$Store), horiz = TRUE, main = "Bar Plot of Store Counts", xlab = "Counts", ylab = "Store")

data <- data[, !names(data) %in% "Temperature"]


#Data Modeling
X <- data[, !names(data) %in% c("Weekly_Sales")]
Y <- data$Weekly_Sales
#Using sampling to split train and test datasetand Assuming your feature matrix is named "X" and target variable is named "Y"
set.seed(42) 

#trainig to 80% and testing to 20%
indices_train <- sample(1:nrow(X), size = 0.8 * nrow(X), replace = FALSE)
indices_test <- setdiff(1:nrow(X), indices_train)
# Create training sets
X_train <- X[indices_train, ]
Y_train <- Y[indices_train]

# Create testing sets
X_test <- X[indices_test, ]
Y_test <- Y[indices_test]


# Applying Decision Tree
library(rpart)
# Combine X_train and Y_train into a data frame
train_data <- data.frame(X_train, Y_train)
test_data <- data.frame(X_test, Y_test)
# Build a decision tree model ----------------
tree_model <- rpart(Y_train ~ ., data = train_data, method = "anova")
dt_pred <- predict(tree_model, newdata = data.frame(X_test))

#PLOTTING GRAPH FOR DECISION TREE
# Create a data frame for plotting
plot_data_dt <- data.frame(Y_test = Y_test, DT_Pred = dt_pred)

# Load ggplot2 library
library(ggplot2)

# Create a scatter plot
ggplot(plot_data_dt, aes(x = Y_test, y = DT_Pred)) +
  geom_point(alpha = 0.7, color = "green", size = 3) +
  labs(title = "Decision Tree Regression: Actual vs. Predicted",
       x = "Actual (Y_test)",
       y = "Predicted (DT_Pred)") +
  geom_smooth(method = "lm", se = FALSE, linetype = "dashed", color = "gray") +
  theme_minimal()

#RMSE for Decision Tree
library(Metrics)
dt_rmse_value <- rmse(Y_test, dt_pred)
print(paste("RMSE for Decision Tree:", dt_rmse_value))
# Calculate R-squared score
dt_r2_score <- cor(dt_pred, Y_test)^2
print(paste("R2 score for Decision Tree:", dt_r2_score))


#Applying KNN regressor
install.packages("caret")
library(caret)
knn_model <- train(Y_train ~ ., data = train_data, method = "knn", trControl = trainControl(method = "cv"))
knn_pred <- predict(knn_model, newdata = data.frame(X_test))
#RMSE for KNN Regressor
library(Metrics)
knn_rmse_value <- rmse(Y_test, knn_pred)
print(paste("RMSE for KNN regressor:", knn_rmse_value))
# Calculate R-squared score
knn_r2_score <- cor(knn_pred, Y_test)^2
print(paste("R2 score for KNN:", knn_r2_score))

#PLOTING FOR KNN REGRESSOR
# Create a data frame for plotting
plot_data_knn <- data.frame(Y_test = Y_test, KNN_Pred = knn_pred)

# Load ggplot2 library
library(ggplot2)

# Create a scatter plot with jitter
ggplot(plot_data_knn, aes(x = Y_test, y = KNN_Pred)) +
  geom_point(alpha = 0.7, color = "purple", size = 3, position = position_jitter(width = 0.2, height = 0.2)) +
  labs(title = "k-NN Regression: Actual vs. Predicted",
       x = "Actual (Y_test)",
       y = "Predicted (KNN_Pred)") +
  geom_smooth(method = "lm", se = FALSE, linetype = "dashed", color = "gray") +
  theme_minimal()


#Applying Lasso Regression
library(glmnet)
lasso_model <- cv.glmnet(as.matrix(X_train), Y_train, alpha = 1)  # alpha = 1 specifies Lasso regression
lasso_pred <- predict(lasso_model, s = lasso_model$lambda.min, newx = as.matrix(X_test))

#RMSE for Lasso Regression
library(Metrics)
lasso_rmse_value <- rmse(Y_test, lasso_pred)
print(paste("RMSE for Lasso regression:", lasso_rmse_value))
# Calculate R-squared score
lasso_r2_score <- cor(lasso_pred, Y_test)^2
print(paste("R2 score for Lasso:", lasso_r2_score))

# Load ggplot2 library
library(ggplot2)
result_df = data.frame(y_test=Y_test, lasso_pred)

#PLOTTING FOR LASSO REGRESSION
# Create a scatter plot with jittered points
library(ggplot2)

# Create a scatter plot without jitter
ggplot(result_df, aes(x = y_test, y = lasso_pred)) +
  geom_point(alpha = 0.7, color = "blue", size = 0.5) +
  labs(title = "Lasso Regression: Actual vs. Predicted",
       x = "Actual (y_test)",
       y = "Predicted (lasso_pred)") +
  geom_smooth(method = "lm", se = FALSE, linetype = "dashed", color = "gray") +
  theme_minimal()

#From R2 score of the above three model we can conclude that KNN hsa the best
# R2 score i.e of 0.90 Thus KNN turns out to be the best model

# 5 fold cross validation
train_control <- trainControl(method = "cv", 
                              number = 5) 

# training the model by assigning Weekly_Sales column 

model <- train(Weekly_Sales ~., data = data,  
               method = "knn", 
               trControl = train_control)
print(model)

# Create a data frame for plotting
plot_knn_5fold <- data.frame(Y_test = Y_test, KNN_Pred = knn_pred)

#PLOTTING 5 FOLD CROSS VALIDATION
# Load ggplot2 library
library(ggplot2)

# Create a scatter plot with jitter
ggplot(plot_knn_5fold, aes(x = Y_test, y = KNN_Pred)) +
  geom_point(alpha = 0.7, color = "orange", size = 3, position = position_jitter(width = 0.2, height = 0.2)) +
  labs(title = "k-NN with 5 FOLD Cross validation: Actual vs. Predicted",
       x = "Actual (Y_test)",
       y = "Predicted (KNN_Pred)") +
  geom_smooth(method = "lm", se = FALSE, linetype = "dashed", color = "gray") +
  theme_minimal()