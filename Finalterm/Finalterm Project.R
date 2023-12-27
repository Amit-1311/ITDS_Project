mydata <- read.csv("D:/Cervical cancer.csv",header = TRUE,sep = ",")
mydata


names(mydata)


colSums(is.na(mydata))


all_numeric <- sapply(mydata, is.numeric)
print(all_numeric)


mydata$Result <- factor(mydata$Result, levels = c("Positive","Negetive"),labels = c("0","1"))
mydata


str(mydata)


summary(mydata)

all_numeric <- mydata[, sapply(mydata, is.numeric)]
correlation_matrix <- cor(all_numeric)
dim(correlation_matrix)
correlation_matrix


significant_attributes <- names(which(abs(correlation_matrix[1, ]) > 0.2))
significant_attributes


install.packages("e1071")
install.packages("caret")
install.packages("naivebayes")

library("e1071")
library("caret")
library("naivebayes")

naive_bayes_model <- naiveBayes(mydata[, -ncol(mydata)], mydata[, ncol(mydata)])
naive_bayes_model


set.seed(123)

predictors <- colnames(mydata)[colnames(mydata) != "ca_cervix"]
target <- "ca_cervix"

split_index <- sample(1:nrow(mydata), 0.7 * nrow(mydata))
train_mydata <- mydata[split_index, ] 
test_mydata <- mydata[-split_index, ]

nb_model <- naiveBayes(train_mydata[, predictors], train_mydata$ca_cervix)

unseen_instance <- test_mydata[1, predictors, drop = FALSE]

prediction <- predict(nb_model, unseen_instance)

cat("Original Class:", test_mydata[1, "ca_cervix"], "\n")
cat("Predicted Class:", prediction, "\n")

predictions <- predict(nb_model, test_mydata[, predictors])

conf_matrix <- table(predictions, test_mydata$ca_cervix)
print("Confusion Matrix:")
print(conf_matrix)

accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
cat("Accuracy:", round(accuracy, 2), "\n")


set.seed(123)
library("caret")

cv_results <- train(
  x = train_mydata[, predictors],
  y = train_mydata[, target],
  method = "rf",
  trControl = trainControl(
    method = "cv",
    number = 10
  )
)
print(cv_results)


precision <- conf_matrix[2, 2] / sum(conf_matrix[, 2])
recall <- conf_matrix[2, 2] / sum(conf_matrix[2, ])
f_measure <- 2 * (precision * recall) / (precision + recall)

cat("Precision:", round(precision, 2), "\n")
cat("Recall:", round(recall, 2), "\n")
cat("F_measure:", round(f1_score, 2), "\n")