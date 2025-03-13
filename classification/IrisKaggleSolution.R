# Attempts
# Random forest, accuracy = .8
# change tuneLength to 10, accuracy = .815
# Specify a Grid for Tuning, accuracy =.8
# use optimism boot
#setwd("~/August2024_thesis/Kaggle G2F")
#setwd("~/Fall24/Spatial Analytics/Kaggle G2F/Kaggle G2F")
# Load necessary libraries
library(dplyr)
library(caret)
library(readr)

data <- read.csv("G2F_data.csv")
data$year <- as.factor(data$year)
data$Win_YN <- as.factor(data$Win_YN)
data$Hybrid <- as.factor(data$Hybrid)
data$Env <- NULL
data$field_location <- as.factor(data$field_location)

# Display the first few rows to confirm changes
head(data)
str(data)
# Split the data into training and testing sets
set.seed(123)  # For reproducibility
train_index <- createDataPartition(data$Win_YN, p = 0.7, list = FALSE)
train_data <- data[train_index, ]
test_data <- data[-train_index, ]

# logistic regression
# Train a logistic regression model using caret
log_model <- train(Win_YN ~ ., data = data, method = "glm", family = "binomial", 
                   trControl = trainControl(method = "cv", number = 5), tuneLength = 3)
predictions <- predict(log_model, newdata = test_data)
# accuracy
conf_matrix <- confusionMatrix(predictions, test_data$Win_YN)
conf_matrix$overall["Accuracy"] #.8303
# save log_model as a file
save(log_model, file = "log_model.RData")

# Decision Tree
# Train a decision tree model using caret
dt_model <- train(Win_YN ~ ., data = data, method = "rpart", 
                  trControl = trainControl(method = "cv", number = 5), tuneLength = 3)
# Make predictions on the training data
predictions <- predict(dt_model, newdata = test_data)
# Evaluate the model performance
conf_matrix <- confusionMatrix(predictions, test_data$Win_YN)
conf_matrix$overall["Accuracy"] #.719
# save dt_model as a file
save(dt_model, file = "dt_model.RData")

# Random Forest
rf_model <- train(Win_YN ~ ., data = data, method = "rf", 
                  trControl = trainControl(method = "cv", number = 5), tuneLength = 3)
# Make predictions on the training data
predictions <- predict(rf_model, newdata = test_data)
# Evaluate the model performance
conf_matrix <- confusionMatrix(predictions, test_data$Win_YN)
conf_matrix$overall["Accuracy"] # 1
# save rf_model as a file
save(rf_model, file = "rf_model.RData")


# Random Forest setting ntree = 500
rf_model_2 <- train(Win_YN ~ ., data = data, method = "rf", 
                  trControl = trainControl(method = "cv", number = 5), 
                  tuneLength = 3, ntree = 500)
# Make predictions on the training data
predictions <- predict(rf_model_2, newdata = test_data)
# Evaluate the model performance
conf_matrix <- confusionMatrix(predictions, test_data$Win_YN)
conf_matrix$overall["Accuracy"] # 1
# save rf_model_2 as a file
save(rf_model_2, file = "rf_model_2.RData")

# Make submission file
kaggle_prediction <- read_csv("kaggle_prediction.csv")
# change data format like before
kaggle_prediction$year <- as.factor(kaggle_prediction$year)
kaggle_prediction$Hybrid <- as.factor(kaggle_prediction$Hybrid)
kaggle_prediction$Env <- NULL
kaggle_prediction$field_location <- as.factor(kaggle_prediction$field_location)

# log_model
kaggle_predictions <- predict(log_model, newdata = kaggle_prediction)
submission <- data.frame(ID = 1:length(kaggle_predictions), Win_YN = kaggle_predictions)

# Display the first few rows of the output file
head(submission)

# Save the output to a CSV file
write_csv(submission, "log_model_prediction.csv") #.828

# dt_model
kaggle_predictions <- predict(dt_model, newdata = kaggle_prediction)
submission <- data.frame(ID = 1:length(kaggle_predictions), Win_YN = kaggle_predictions)
write_csv(submission, "dt_model_prediction.csv") #.684

# rf_model
kaggle_predictions <- predict(rf_model, newdata = kaggle_prediction)
submission <- data.frame(ID = 1:length(kaggle_predictions), Win_YN = kaggle_predictions)
write_csv(submission, "rf_model_prediction.csv") # .836

# rf_model_2
kaggle_predictions <- predict(rf_model_2, newdata = kaggle_prediction)
submission <- data.frame(ID = 1:length(kaggle_predictions), Win_YN = kaggle_predictions)
write_csv(submission, "rf_model_2_prediction.csv") #.844

# Load necessary libraries
library(ggplot2)
library(caret)

# Visualization for rf_model_2
# -------------------------------
# Visualization 1: Confusion Matrix Heatmap
# -------------------------------
# Convert the confusion matrix table to a data frame
conf_df <- as.data.frame(conf_matrix$table)

# Create the confusion matrix heatmap
conf_matrix_plot <- ggplot(data = conf_df, aes(x = Reference, y = Prediction, fill = Freq)) +
  geom_tile(color = "black") +
  geom_text(aes(label = Freq), size = 6, color = "white") +
  scale_fill_gradient(low = "skyblue", high = "navy") +
  labs(title = "Confusion Matrix", x = "Actual Class", y = "Predicted Class") +
  theme_minimal()

# Display the plot
print(conf_matrix_plot)

# -------------------------------
# Visualization 2: Variable Importance Plot
# -------------------------------
# Calculate variable importance from the Random Forest model
var_imp <- varImp(rf_model_2, scale = FALSE)
# Plot the variable importance
plot(var_imp, main = "Variable Importance")

# -------------------------------
# Calculate variable importance from the Random Forest model
var_imp <- varImp(rf_model_2, scale = FALSE)

# Convert the importance into a data frame
var_imp_df <- data.frame(Variable = rownames(var_imp$importance), 
                         Importance = var_imp$importance$Overall)

# Sort the variables by importance in descending order and select the top 10
top10_vars <- var_imp_df[order(var_imp_df$Importance, decreasing = TRUE), ][1:10, ]

# Plot the top 10 most important variables using ggplot2
library(ggplot2)
ggplot(top10_vars, aes(x = reorder(Variable, Importance), y = Importance)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(title = "Top 10 Most Important Variables", 
       x = "Variables", 
       y = "Importance") +
  theme_minimal()



