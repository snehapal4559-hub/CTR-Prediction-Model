# Load necessary libraries
library(caret)
library(pROC)
library(ggplot2)

# Sample Dataset (Use your dataset)
data <- read.csv("your_data.csv")  # Replace with actual dataset

# Set seed for reproducibility
set.seed(123)

# Split data into 80% train and 20% test
trainIndex <- createDataPartition(data$Clicked_on_Ad, p = 0.8, list = FALSE)
train_data <- data[trainIndex, ]
test_data <- data[-trainIndex, ]

# Train Logistic Regression Model
logit_model <- glm(Clicked_on_Ad ~ ., data = train_data, family = binomial)

# Predict probabilities on test data
test_pred_probs <- predict(logit_model, newdata = test_data, type = "response")

# Convert probabilities to binary (Threshold = 0.5)
test_pred <- ifelse(test_pred_probs > 0.5, 1, 0)

# Confusion Matrix
conf_matrix <- confusionMatrix(as.factor(test_pred), as.factor(test_data$Clicked_on_Ad))
print(conf_matrix)

# Extract performance metrics
TP <- conf_matrix$table[2,2]
TN <- conf_matrix$table[1,1]
FP <- conf_matrix$table[1,2]
FN <- conf_matrix$table[2,1]

accuracy <- (TP + TN) / (TP + TN + FP + FN)
precision <- TP / (TP + FP)
recall <- TP / (TP + FN)
f1_score <- 2 * ((precision * recall) / (precision + recall))

# Print Performance Metrics
cat("Accuracy:", accuracy, "\n")
cat("Precision:", precision, "\n")
cat("Recall (TPR):", recall, "\n")
cat("F1 Score:", f1_score, "\n")

# Plot ROC Curve
roc_curve <- roc(test_data$Clicked_on_Ad, test_pred_probs)
plot(roc_curve, col = "blue", main = "ROC Curve for Logistic Regression")

# Compute AUC
auc_value <- auc(roc_curve)
cat("AUC Score:", auc_value, "\n")

# Visualizing Confusion Matrix with ggplot2
df_results <- data.frame(Actual = test_data$Clicked_on_Ad, Predicted = test_pred)
ggplot(df_results, aes(x = factor(Actual), fill = factor(Predicted))) +
  geom_bar(position = "dodge") +
  labs(title = "Actual vs. Predicted Clicks", x = "Actual", y = "Count", fill = "Predicted")
