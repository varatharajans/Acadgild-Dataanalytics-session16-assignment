# Acadgild-Dataanalytics-session16-assignment
DATA ANALYTICS WITH R, EXCEL AND TABLEAU SESSION 16 ASSIGNMENT 

Session 16 – Assignment

1. Use the below given data set 
Data Set 
2. Perform the below given activities: 
a. Predict the no of comments in next H hrs 
Note:- 
1. Use LASSO, Elastic Net and Ridge and other regression techniques that are covered in the module 
2. Report the training accuracy and test accuracy 
3. compare with linear models and report the accuracy 
4. create a graph displaying the accuracy of all models 

Attribute Information:
(39  - This describes the H hrs, for which we have the target variable/ comments received.
, 54 -Target Variable - Decimal  Target  The no of comments in next H hrs(H is given in Feature no 39).

39 
H Local 
ï¿¼Decimal(0-23) Encoding 
Other feature 
This describes the H hrs, for which we have the target variable/ comments received. 

54 
Target Variable 
Decimal 
Target 
The no of comments in next H hrs(H is given in Feature no 39).

Prediction Accuracy

A good learner is the one which has good prediction accuracy; in other words, which has the smallest prediction error.
Let us try to understand the prediction problem intuitively. Consider the simple case of fitting a linear regression model to the observed data. A model is a good fit, if it provides a high R2 value. 

1. Use LASSO, Elastic Net and Ridge and other regression techniques that are covered in the module
library(tidyverse)
library(caret)
library(glmnet)
# Load the data
setwd("~/Dataset/Dataset/Training")

Features_Variant_1 <- read.csv("C:/users/seshan/Documents/Dataset/Dataset/Training/Features_Variant_1.csv")
View(Features_Variant_1)
Features.data <- na.omit(Features_Variant_1)
# Split the data into training and test set
set.seed(123)
training.samples <- Features$X0.19 %>%
  createDataPartition(p = 0.8, list = FALSE)
train.data  <- Features_Variant_1[training.samples, ]
test.data <- Features_Variant_1[-training.samples, ]
# Predictor variables
x <- model.matrix(X0.19~., train.data)[,-1]
# Outcome variable
y <- train.data$X0.19
glmnet(x, y, alpha = 1, lambda = NULL)
# Find the best lambda using cross-validation
set.seed(123) 
cv <- cv.glmnet(x, y, alpha = 0)
# Display the best lambda value
cv$lambda.min
plot(cv$lambda.min)
# Fit the final model on the training data
model <- glmnet(x, y, alpha = 0, lambda = cv$lambda.min)
plot(model)
# Display regression coefficients
coef(model)

# Make predictions on the test data
x.test <- model.matrix(X0.19 ~., test.data)[,-1]
predictions <- model %>% predict(x.test) %>% as.vector()
# Model performance metrics
data.frame(
  RMSE = RMSE(predictions, test.data$X0.19),
  Rsquare = R2(predictions, test.data$X0.19)
)

#Computing lasso regression
# Find the best lambda using cross-validation
set.seed(123) 
cv <- cv.glmnet(x, y, alpha = 1)
# Display the best lambda value
cv$lambda.min
# Fit the final model on the training data
model <- glmnet(x, y, alpha = 1, lambda = cv$lambda.min)
# Dsiplay regression coefficients
coef(model)
# Make predictions on the test data
x.test <- model.matrix(X0.19 ~., test.data)[,-1]
predictions <- model %>% predict(x.test) %>% as.vector()
# Model performance metrics
data.frame(
  RMSE = RMSE(predictions, test.data$X0.19),
  Rsquare = R2(predictions, test.data$X0.19))
#Computing elastic net regession
# Build the model using the training set
set.seed(123)
model <- train(X0.19  ~., data = train.data, method = "glmnet",
  trControl = trainControl("cv", number = 10),
  tuneLength = 10
)
# Best tuning parameter
model$bestTun
plot(model$bestTun)
# Coefficient of the final model. You need
# to specify the best lambda
coef(model$finalModel, model$bestTune$lambda)
# Make predictions on the test data
x.test <- model.matrix(X0.19 ~., test.data)[,-1]
predictions <- model %>% predict(x.test)
# Model performance metrics
data.frame(
  RMSE = RMSE(predictions, test.data$X0.19),
  Rsquare = R2(predictions, test.data$X0.19)
)
#Comparing the different models
#Using caret package
#Setup a grid range of lambda values:
  lambda <- 10^seq(-3, 3, length = 100)
#Compute ridge regression
  # Build the model
  set.seed(123)
  ridge <- train(
    X0.19 ~., data = train.data, method = "glmnet",
    trControl = trainControl("cv", number = 10),
    tuneGrid = expand.grid(alpha = 0, lambda = lambda)
  )
  # Model coefficients
  coef(ridge$finalModel, ridge$bestTune$lambda)
  # Make predictions
  predictions <- ridge %>% predict(test.data)
  plot(predictions)
  # Model prediction performance
  data.frame(
    RMSE = RMSE(predictions, test.data$X0.19),
    Rsquare = R2(predictions, test.data$X0.19)
  )
  
  #Compute lasso regression
  # Build the model
  set.seed(123)
  lasso <- train(
    X0.19 ~., data = train.data, method = "glmnet",
    trControl = trainControl("cv", number = 10),
    tuneGrid = expand.grid(alpha = 1, lambda = lambda)
  )
  plot( lasso)
  # Model coefficients
  coef(lasso$finalModel, lasso$bestTune$lambda)
    # Make predictions
  predictions <- lasso %>% predict(test.data)
  # Model prediction performance
  data.frame(
    RMSE = RMSE(predictions, test.data$X0.19),
    Rsquare = R2(predictions, test.data$X0.19)
  )
  #Elastic net regression
  # Build the model
  set.seed(123)
  elastic <- train(
    X0.19 ~., data = train.data, method = "glmnet",
    trControl = trainControl("cv", number = 10),
    tuneLength = 10
  )
  # Model coefficients
  coef(elastic$finalModel, elastic$bestTune$lambda)
  
  # Make predictions
  predictions <- elastic %>% predict(test.data)
  plot( predictions)
  # Model prediction performance
  data.frame(
    RMSE = RMSE(predictions, test.data$X0.19),
    Rsquare = R2(predictions, test.data$X0.19)
  )
  #Comparing models performance:
  models <- list(ridge = ridge, lasso = lasso, elastic = elastic)
  resamples(models) %>% summary( metric = "RMSE")
 
  
  #k-fold Cross Validation
  # load the library
  library(caret)
  
  # define training control
  train_control <- trainControl(method="cv", number=10)
  # fix the parameters of the algorithm
  grid <- expand.grid(.fL=c(0), .usekernel=c(FALSE))
  # train the model
  model <- train(X0.19~., data=Features_Variant_1, trControl=train_control, method="nb", tuneGrid=grid)
  # summarize results
  print(model)
  
  # load the library
  library(caret)
  
  # define training control
  train_control <- trainControl(method="repeatedcv", number=10, repeats=3)
  # train the model
  model <- train(X0.19~., data=Features_Variant_1, trControl=train_control, method="nb")
  # summarize results
  print(model)
  
  #create a graph displaying the accuracy of all models
  plot(model)
  plot(varImp(ridge$finalModel))
  plot(cv)
  plot(ridge)
  hist(Features$X0.19,col = "green")
  hist(Features$X24,col = "red")
  hist(Features$X11.291044776119403,col = 'yellow')
  fit = glmnet(x, y)
  plot(fit)
  cvfit = cv.glmnet(x, y)
  plot(cvfit)
  tfit=glmnet(x,y,lower=-.7,upper=.5)
  plot(tfit)



