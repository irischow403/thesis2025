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

# Adjust Cross-Validation Method (accuracy = 81.7%)
trControl = trainControl(method = "repeatedcv", number = 5, repeats = 3)
#rf_model <- train(Win_YN ~ ., data = data, method = "rf", 
#                  trControl = trControl, tuneLength = 3)

# Display the model results
print(rf_model)

# Make predictions on the training data
predictions <- predict(rf_model, newdata = test_data)

# Evaluate the model performance
#conf_matrix <- confusionMatrix(predictions, data$Win_YN)
#print(conf_matrix)


# Load new dataset for Kaggle prediction
kaggle_prediction <- read_csv("kaggle_prediction.csv")

# Make predictions on the new Kaggle dataset
kaggle_predictions <- predict(rf_model, newdata = kaggle_prediction)

# Add predictions to the Kaggle dataset
kaggle_prediction$hybrid_binomial <- kaggle_predictions

# Create the output file with ID and hybrid_binomial columns
kaggle_output <- kaggle_prediction %>% 
  mutate(ID = row_number()) %>% 
  select(ID, hybrid_binomial)

# Save the output to a CSV file
write_csv(kaggle_output, "kaggle_prediction_output.csv")

# Display the first few rows of the output file
head(kaggle_output)

# split the data according to hybrid

sapply(lapply(data, unique), length)

# make data list

data_list <- split(ml_df, ml_df$Hybrid)

# standardise per hybrid

for(i in 1:length(data_list)){
  data_table <- as_tibble(data_list[[i]])
  data_table$hybrid_binomial
  data_table$hybrid_binomial <- as.numeric(data_table$hybrid_binomial)
  data_table$hybrid_binomial  <- scale(data_table$hybrid_binomial , center = FALSE, scale = max(data_table$hybrid_binomial ))
  
}

# making feature importance plot
# Extract feature importance
feature_importance <- importance(rf_model)

# Convert feature importance to a data frame
feature_importance_df <- as.data.frame(feature_importance)



