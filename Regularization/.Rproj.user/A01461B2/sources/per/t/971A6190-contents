#Akshay Hanchate

library(ISLR)
library(ggplot2)
library(caret)
library(glmnet)
library(Metrics)

#Splitting the dataset into train and test

data("College")
head(College)

#EDA
View(College)

str(College)

summary(College)

#lets check the missing data 
colSums(is.na(College))
#we dont have any missing values


#Feature Engineering 
set.seed(123)
traindataset =  createDataPartition(College$Grad.Rate, p = 0.7, list = FALSE, times = 1)
train_model =  College[ traindataset,]
test_model =  College[-traindataset,]

#lets check the dimensions of the sets
dim(train_model)
dim(test_model)



train_x_model = model.matrix(Grad.Rate ~.,train_model)[,-1]
test_x_model = model.matrix(Grad.Rate ~.,test_model)[,-1]


train_y_model = train_model$Grad.Rate
test_y_model = test_model$Grad.Rate


#Finding the values of Lambda using cross validation
set.seed(123)
cv.ridge = cv.glmnet(train_x_model,train_y_model, alpha = 0)
cv.ridge

plot(cv.ridge)


#to find lambda min and lambda ise
log(cv.ridge$lambda.min)
log(cv.ridge$lambda.1se)

cv.ridge$lambda.min

#create a ridge regresstion model using lambda min
model_min = glmnet(train_x_model,train_y_model, alpha = 0, lambda= cv.ridge$lambda.min)

model_min

coef(model_min)

#fitting ridge regression model with lambda.1se

model_1se = glmnet(train_x_model,train_y_model, alpha = 0, lambda= cv.ridge$lambda.1se)
model_1se

coef(model_1se)


#lets try to check with ols model with no regularization 
ols = lm(Grad.Rate ~., data=train_model )

coef(ols)


#Lets Calculate the RSME for our ridge regression train model 
preds.train = predict(model_1se, newx=train_x_model)
train.rmse = rmse(train_y_model, preds.train)

#lets check the same for test model 
preds.test = predict(model_1se, newx = test_x_model)
test.rmse = rmse(test_y_model,preds.test)


#comparining both the values 
train.rmse
test.rmse




#Performing the same for Lasso Regression 
#Finding the values of Lambda using cross validation
set.seed(123)
cv.lasso = cv.glmnet(train_x_model,train_y_model, nfolds = 10)
plot(cv.lasso)


#to find lambda min and lambda ise
log(cv.lasso$lambda.min)
log(cv.lasso$lambda.1se)

cv.lasso$lambda.min

#create a lasso regresstion model using lambda min
model_min = glmnet(train_x_model,train_y_model, alpha = 1, lambda= cv.lasso$lambda.min)

model_min

coef(model_min)

#fitting ridge regression model with lambda.1se

model_1se = glmnet(train_x_model,train_y_model, alpha = 1, lambda= cv.lasso$lambda.1se)
model_1se

coef(model_1se)


#lets try to check with ols model with no regularization 
ols = lm(Grad.Rate ~., data=train_model )

coef(ols)


#Lets Calculate the RSME for our ridge regression train model 
preds.train = predict(model_1se, newx=train_x_model)
train.rmse = rmse(train_y_model, preds.train)

#lets check the same for test model 
preds.test = predict(model_1se, newx = test_x_model)
test.rmse = rmse(test_y_model,preds.test)


#comparining both the values 
train.rmse
test.rmse


#trying the stepwise selection 

#lets fit the model with all the varaibles first. 
model1 =  lm(Grad.Rate ~ ., data = train_model)
summary(model1)


#Lets use the stepwise selection for both 
modelstepwise =  step(model1, direction = "both", trace = 0)
summary(modelstepwise)























