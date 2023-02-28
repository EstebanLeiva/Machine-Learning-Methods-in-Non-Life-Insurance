library(mvtnorm)
library(mlr3)
library(mlr3learners )
library(mlr3tuning)
library(mlr3mbo)
library(glmnet)
library(mlr3verse)

#Generate data

generate_data <- function(n,p,s,rho,m,sigma){
  z_1 <- rnorm(p*n,0,(1-rho)^(1/2))
  z_0 <- rnorm(n,0,rho^(1/2))
  X <- 2.5*atan(z_1+z_0)/pi
  dim(X) <- c(n,p)
  Y <- m(X) + rnorm(n)
  dat <- data.frame(Y = Y, X=X)
  return(list(dat=dat,Y=Y,X=X))
}



#Model 1
n = 1000
p = 100
rho = 0.3
s = 5
m <- function(x){ rowSums(x[,1:s])}

dat <- generate_data(n,p,s,rho,m,sigma)
Y <- dat$Y
X <- dat$X
dat <- dat$dat
task_model1 = as_task_regr(dat, target = "Y")

#Linear Regression
learner_linear_regression_lm_1 = lrn("regr.lm")

#Ridge Regression
learner_ridge_1 = lrn("regr.glmnet",
                      s= to_tune(0, 1),
                      alpha=0)
instance = tune(
  method = tnr("mbo"),
  task = task_model1,
  learner = learner_ridge_1,
  resampling = rsmp("cv", folds = 5),
  measures = msr("regr.mse"),
  terminator = trm("evals",n_evals=10)
)

learner_ridge_1_tuned = lrn("regr.glmnet")  
learner_ridge_1_tuned$param_set$values = instance$result_learner_param_vals

#Lasso Regression
learner_lasso_1 = lrn("regr.glmnet",
                      s= to_tune(0, 1),
                      alpha=1)
instance = tune(
  method = tnr("mbo"),
  task = task_model1,
  learner = learner_lasso_1,
  resampling = rsmp("cv", folds = 5),
  measures = msr("regr.mse"),
  terminator = trm("evals",n_evals=10)
)

learner_lasso_1_tuned = lrn("regr.glmnet")  
learner_lasso_1_tuned$param_set$values = instance$result_learner_param_vals

#Elastic net

learner_elastic_net_1 = lrn("regr.glmnet",
                            s= to_tune(0, 1),
                            alpha=to_tune(0, 1))

instance = tune(
  method = tnr("mbo"),
  task = task_model1,
  learner = learner_elastic_net_1,
  resampling = rsmp("cv", folds = 5),
  measures = msr("regr.mse"),
  terminator = trm("evals",n_evals=10)
)

learner_elastic_1_tuned = lrn("regr.glmnet")  
learner_elastic_1_tuned$param_set$values = instance$result_learner_param_vals

#Training of the models and results on new data sets of model 1
learner_linear_regression_lm_1$train(task_model1)
learner_ridge_1_tuned$train(task_model1)
learner_lasso_1_tuned$train(task_model1)
learner_elastic_1_tuned$train(task_model1)

#### Simulate test data


my_test_function <- function(s){
  dat_test <- generate_data(n,p,s,rho,m,sigma)
  Y_test <- dat_test$Y
  X_test <- data.frame(X=dat_test$X)
  dat_test <- dat_test$dat
  
  predictions <- learner_lasso_1_tuned$predict_newdata(dat_test)
  lasso_mse <- mean((predictions$response-Y_test)^2)
  
  predictions <- learner_ridge_1_tuned$predict_newdata(dat_test)
  ridge_mse <- mean((predictions$response-Y_test)^2)
  
  predictions <- learner_elastic_1_tuned$predict_newdata(dat_test)
  elasticnet_mse <- mean((predictions$response-Y_test)^2)
  
  
  #all(attr(terms(lr), "term.labels")== colnames(X_test)) #sanity check
  predictions<- predict(learner_linear_regression_lm_1, X_test)
  lr_mse <- mean((predictions-Y_test)^2)
  
  return(c(lasso_mse,ridge_mse,elasticnet_mse,lr_mse))
  
}

my_res <- t(sapply(1:100, my_test_function))



colnames(my_res)=c("lasso_mse","ridge_mse","elasticnet_mse","lr_mse")
colMeans(my_res)
apply(my_res, 2, sd)


#Model 2
n = 1000
p = 100
rho = 0.3

z_1 <- rnorm(p*n,0,(1-rho)^(1/2))
z_0 <- rnorm(n,0,rho^(1/2))
model2 <- 2.5*atan(z_1+z_0)/pi
dim(model2) <- c(n,p)

#Model 3
n = 1000
p = 100
rho = 0.3

z_1 <- rnorm(p*n,0,(1-rho)^(1/2))
z_0 <- rnorm(n,0,rho^(1/2))
model3 <- 2.5*atan(z_1+z_0)/pi
dim(model3) <- c(n,p)

#Model 4
n = 1000
p = 100
rho = 0.3

z_1 <- rnorm(p*n,0,(1-rho)^(1/2))
z_0 <- rnorm(n,0,rho^(1/2))
model4 <- 2.5*atan(z_1+z_0)/pi
dim(model4) <- c(n,p)
