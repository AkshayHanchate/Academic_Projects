#Akshay Hanchate

library(ISLR)
library(ggplot2)
library(caret)


data("College")
head(College)

#EDA
View(College)

str(College)

summary(College)

#lets check the missing data 
colSums(is.na(College))
#we dont have any missing values 


#lets convert the private values to 1 and 0 respectively as this will be the predicatble variable 
College$Private =  as.factor(ifelse(College$Private == "Yes", 1, 0))


#Scatter plot of getting relation between application and acceptance. 
ggplot(College, aes(x = Apps, y = Accept, color = Private)) +
  geom_point() +
  labs(title = "Scatterplot of Apps vs. Accept", x = "Applications", y = "Acceptances")

#Histogram to find students from Outstates. 
ggplot(College, aes(x = Enroll, fill = Private)) +
  geom_histogram(binwidth = 70, position = "dodge") +
  labs(title = "Histogram of Outstate", x = "Outstate Students")

ggplot(College, aes(x = Private, y = Accept, fill = Private)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Acceptance Ratio by University Type",
       x = "Type of University",
       y = "Acceptance Ratio") +
  theme_minimal()

#Feature Engineering 
set.seed(123)
train_dataset =  createDataPartition(College$Private, p = 0.7, list = FALSE, times = 1)
trainmodel =  College[ train_dataset,]
testmodel =  College[-train_dataset,]

#lets check the dimensions of the sets
dim(trainmodel)
dim(testmodel)

#lets fit the model with all the varaibles first. 
model1 =  glm(Private ~ ., data = trainmodel, family = "binomial")
summary(model1)

#lets take stepwise selection to check if lesser variables give us a good fit 
#will be using stepwise selection for both 
modelstepwise =  step(model1, direction = "both", trace = 0)
summary(modelstepwise)

#lets try one more model with lesser variables to analyze how its affects our accuracy
model2 = glm(Private ~ F.Undergrad + Outstate + PhD, data = trainmodel, family = "binomial")
summary(model2)



#lets check the confusion matrix 
train_predict =  predict(modelstepwise, newdata = trainmodel, type = "response")
confusion_matrix =  table(train_predict > 0.5, trainmodel$Private)
confusion_matrix


#lets check the same with testmodel 
test_predict =  predict(modelstepwise, newdata = testmodel, type = "response")
confusion_matrix_test =  table(test_predict > 0.5, testmodel$Private)
confusion_matrix_test


#checking on train model 
install.packages("pROC")
library(pROC)

ROC =  roc(trainmodel$Private, train_predict)

plot(ROC, main = "ROC Curve", col = "blue", lwd = 2)


#AUC curve
AUC =  auc(ROC)
legend("bottomright", legend = paste("AUC =", round(AUC, 2)), col = "blue", lty = 1, cex = 0.8)

cat("AUC:", AUC, "\n")

#checking on test model

ROC_test =  roc(testmodel$Private, test_predict)
plot(ROC, main = "ROC Curve", col = "blue", lwd = 2)


AUC_test =  auc(ROC_test)
legend("bottomright", legend = paste("AUC =", round(AUC_test, 2)), col = "blue", lty = 1, cex = 0.8)

cat("AUC:", AUC_test, "\n")

#our test auc of similar to train model 














