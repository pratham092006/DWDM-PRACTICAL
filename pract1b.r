# Load libraries
library(rpart)
library(rpart.plot)
library(caret)

# Read dataset
bank <- read.csv("bank_tree.xlsx - Sheet1.csv", stringsAsFactors = TRUE)

# View dataset
head(bank)
str(bank)
summary(bank)

# Convert all character columns to factors
bank[] <- lapply(bank, function(x) {
  if(is.character(x))
    as.factor(x)
  else
    x
})

# Check structure again
str(bank)

# Split dataset (92% training)
set.seed(123)

sample_index <- sample(1:nrow(bank),
                       size = 0.92 * nrow(bank))

train_data <- bank[sample_index, ]
test_data  <- bank[-sample_index, ]   # Proper test set

summary(train_data)
summary(test_data)

# Predictor and Target (optional)
Test_x <- test_data[, -17]
Test_y <- test_data[, 17]

# Build Decision Tree
tree_model <- rpart(
  y ~ .,
  data = train_data,
  method = "class"
)

# Model Summary
tree_model
print(tree_model)

# Plot Tree
rpart.plot(tree_model)

# Prediction
prediction <- predict(tree_model,
                      test_data,
                      type = "class")

prediction

# Confusion Matrix
confusionMatrix(prediction, test_data$y)

# Prune Tree
pruned_tree <- prune(tree_model, cp = 0.01)

# Plot Pruned Tree
rpart.plot(pruned_tree)

# Grow a Larger Tree
tree_model <- rpart(
  y ~ .,
  data = train_data,
  method = "class",
  control = rpart.control(
    cp = 0.0001,
    minsplit = 3
  )
)

# Plot Larger Tree
rpart.plot(tree_model)

# Predictions Again
prediction <- predict(tree_model,
                      test_data,
                      type = "class")

confusionMatrix(prediction, test_data$y)