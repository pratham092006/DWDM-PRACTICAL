library(rpart)
library(rpart.plot)
library(caret)

data("iris")

head(iris)
str(iris)
summary(iris)

set.seed(123)

sample_index <- sample(1:nrow(iris), size = 0.92 * nrow(iris))

train_data <- iris[sample_index, ]
test_data <- iris[sample_index, ]      # (Use iris[-sample_index, ] if you want a proper test set)

summary(train_data)
summary(test_data)

Test_x <- test_data[, 1:4]
Test_y <- test_data[, 1:5]

tree_modal <- rpart(Species ~ ., data = train_data, method = "class")

tree_modal
print(tree_modal)

rpart.plot(tree_modal)

prediction <- predict(tree_modal, test_data, type = "class")

prediction

confusionMatrix(prediction,test_data$Species)

 pruned_tree<-prune(tree_modal,cp=0.01)


> pruned_tree<-prune(tree_modal,cp=0.01)
> tree_modal=rpart(Species~.,data=train_data,method="class",control=rpart.control(cp=0.0001,minsplit = 3))
> rpart.plot(tree_modal)