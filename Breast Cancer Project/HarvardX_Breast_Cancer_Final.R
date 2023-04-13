

#Introduction

#Downloading the dataset
temp <- tempfile(fileext = ".csv")
download.file("https://archive.ics.uci.edu/ml/machine-learning-databases/breast-cancer-wisconsin/wdbc.data", temp, mode = "wb")
colnames <- c("id","diagnosis","radius_mean","texture_mean","perimeter_mean","area_mean","smoothness_mean","compactness_mean","concavity_mean","concave points_mean","symmetry_mean","fractal_dimension_mean","radius_se","texture_se","perimeter_se","area_se","smoothness_se","compactness_se","concavity_se","concave.points_se","symmetry_se","fractal_dimension_se","radius_worst","texture_worst","perimeter_worst","area_worst","smoothness_worst","compactness_worst","concavity_worst","concave.points_worst","symmetry_worst","fractal_dimension_worst")
bcwdt <- read.csv(temp, col.names = colnames)
bcwdt

options(digits = 5)

#Investigating the wisconsin breast cancer dataset
if(!require(dplyr)) install.packages("dplyr")
library(dplyr)
glimpse(bcwdt)
#first 5 rows of first seven variables
bcwdt[,1:7] %>% head() %>% knitr::kable()
#Investigating for duplicates and NAs
sum(duplicated(bcwdt))
sum(is.na(bcwdt))

#prop.table of cancer diagnosis
bcwdt_m <- bcwdt %>%
  mutate(diagnosis = case_when(diagnosis == "B" ~ "Benign",
                               diagnosis == "M" ~ "Malignant"))
prop.table(table(bcwdt_m$diagnosis)) %>% knitr::kable()
#barplot of the count of benign and malignant tumors
select(bcwdt, -id) %>%
  mutate(diagnosis = case_when(diagnosis == "B" ~ "Benign",
                              diagnosis == "M" ~ "Malignant")) %>% 
  ggplot(aes(diagnosis)) +
  geom_bar()


#Method and Results
#mutating the dataset diagnosis variable
bcwdt_m <- bcwdt %>% 
  mutate(diagnosis = case_when(diagnosis == "B" ~ 0,
                               diagnosis == "M" ~ 1)) %>%
  select(-id)
bcwdt_m$diagnosis <- as.factor(bcwdt_m$diagnosis)
set.seed(1, sample.kind = "Rounding") # if using R 3.5 or earlier, remove the sample.kind argument
test_index <- createDataPartition(as.factor(bcwdt_m$diagnosis), times = 1, p = 0.2, list = FALSE)
test_set <- bcwdt_m[test_index, ]
train_set <- bcwdt_m[-test_index, ]


#linear regression for prediction
lm_fit <- lm(diagnosis ~ ., train_set)
p_hat <- predict(lm_fit, test_set)
y_hat <- ifelse(p_hat > 0.5, "1", "0") %>% factor()
test_set$diagnosis <- as.factor(test_set$diagnosis)
#confusion matrix of the lm method
cm_lm <- confusionMatrix(y_hat, test_set$diagnosis)
#extracting accuracy, sensitivity and specificity
accuracy_lm <- cm_lm$overall[["Accuracy"]]
sensitivity_lm <- cm_lm$byClass[["Sensitivity"]]
specificity_lm <- cm_lm$byClass[["Specificity"]]
accuracy_lm
sensitivity_lm
specificity_lm
#table for results
results_table <- data.frame(Method = "Logistic Regression", 
                            Accuracy = accuracy_lm, 
                            Sensitivity = sensitivity_lm,
                            Specificity = specificity_lm)
results_table


#logistic regression for prediction
glm_fit <- glm(diagnosis ~ ., train_set, family = "binomial")
p_hat_logit <- predict(glm_fit, test_set, type = "response")
y_hat_logit <- ifelse(p_hat_logit > 0.5, 1, 0) %>% factor()
test_set$diagnosis <- as.factor(test_set$diagnosis)
#confusion matrix of the lm method
cm_glm <- confusionMatrix(y_hat_logit, test_set$diagnosis)
#extracting accuracy, sensitivity and specificity
accuracy_glm <- cm_glm$overall[["Accuracy"]]
sensitivity_glm <- cm_glm$byClass[["Sensitivity"]]
specificity_glm <- cm_glm$byClass[["Specificity"]]
accuracy_glm
sensitivity_glm
specificity_glm
#table for results
results_table <- data.frame(Method = "Logistic Regression", 
                            Accuracy = accuracy_glm, 
                            Sensitivity = sensitivity_glm,
                            Specificity = specificity_glm)
results_table


#bootstrap classification (decision) tree
set.seed(1, sample.kind = "Rounding")
train_rpart<- train(diagnosis ~ ., 
                    method = "rpart", 
                    data = train_set, 
                    tuneGrid = data.frame(cp = seq(0, 0.1, len = 1000)))
train_rpart$bestTune
#plotting the decision tree cp x accuracy
plot(train_rpart)
ggplot(train_rpart, aes(cp, Accuracy)) +
  geom_line() +
  annotate("point", train_rpart$results$cp[62], train_rpart$results$Accuracy[62], size=10, shape=21, fill="transparent", col = 'red')

y_hat <- predict(train_rpart, test_set)
cm_ct <- confusionMatrix(y_hat, test_set$diagnosis)
accuracy_ct <- cm_ct$overall[["Accuracy"]]
sensitivity_ct <-cm_ct$byClass[["Sensitivity"]]
specificity_ct <-cm_ct$byClass[["Specificity"]]

results_table <- rbind(results_table, 
                       data.frame(Method = "classification (decision) tree", 
                                  Accuracy = accuracy_ct,
                                  Sensitivity  = sensitivity_ct,
                                  Specificity = specificity_ct))
results_table


#k nearest neighbors
if(!require(purr)) install.packages("purrr")
ks <- seq(1, 10)
library(purrr)
accuracy <- map_df(ks, function(k){
  fit <- knn3(diagnosis ~ ., data = train_set, k = k)
  
  y_hat <- predict(fit, train_set, type = "class")
  cm_train <- confusionMatrix(data = y_hat, reference = train_set$diagnosis)
  train_error <- cm_train$overall["Accuracy"]
  
  y_hat <- predict(fit, test_set, type = "class")
  cm_test <- confusionMatrix(data = y_hat, reference = test_set$diagnosis)
  test_error <- cm_test$overall["Accuracy"]
  
  tibble(train = train_error, test = test_error)
  
})

#pick the k that maximizes accuracy using the estimates built on the test data
accuracy_df <- data.frame(ks, accuracy)
ggplot(error.df,aes(ks, test)) + 
  geom_point() + 
  geom_line(lty = "dotted", color = "red") +
  annotate("point", ks[which.max(accuracy$test)], max(accuracy$test), size=10, shape=21, fill="transparent", col = "red")

#k that maximizes accuracy and max accuracy
ks[which.max(accuracy$test)]
max(accuracy$test)

#applying the best k to the model to get the results
knn_fit <-knn3(diagnosis ~., train_set, k = ks[which.max(accuracy$test)])
y_hat_knn <- predict(knn_fit, test_set, type = "class")
cm_knn <- confusionMatrix(y_hat_knn, test_set$diagnosis)
accuracy_knn <- cm_knn$overall[["Accuracy"]]
sensitivity_knn <- cm_knn$byClass[["Sensitivity"]]
specificity_knn <- cm_knn$byClass[["Specificity"]]
accuracy_knn
sensitivity_knn
specificity_knn

results_table <- rbind(results_table, 
                      data.frame(Method = "K nearest neighbors", 
                                 Accuracy = accuracy_knn,
                                 Sensitivity  = sensitivity_knn,
                                 Specificity = specificity_knn))
results_table


#bootstrap knn
library(caret)
fit <- train(diagnosis ~ .,  method = "knn", 
             tuneGrid = data.frame(k = seq(1, 30)), 
             data = bcwdt_m)
ggplot(fit)

##Conclusion

##References


