library("mlr3")

data("mtcars")

mtcars_subset = subset(mtcars, select = c("mpg", "cyl", "disp"))
task_mtcars = as_task_regr(mtcars_subset, target = "mpg", id="cars")
task_mtcars

library("mlr3viz")
autoplot(task_mtcars, type = "pairs")

#Divide the training and test data 66% train, 33% test
splits = partition(task_mtcars)

library("mlr3verse")

learner = lrn("regr.xgboost",
              eta = to_tune(0, 1),
              nrounds = to_tune(10, 5000),
              max_depth = to_tune(1, 20),
              colsample_bytree = to_tune(0.1, 1),
              subsample = to_tune(0.1, 1)
              )

instance = tune(
  method = tnr("random_search"),
  task = task_mtcars,
  learner = learner,
  resampling = rsmp("cv", folds = 5),
  measures = msr("regr.mse"),
  terminator = trm("evals",n_evals=10)
)

instance$result

learner_fitted = lrn("regr.xgboost",
                     eta = 0.04967612,
                     nrounds = 2636 ,
                     max_depth = 5,
                     colsample_bytree = 0.6740981,
                     subsample = 0.6372794
                      )

learner_fitted$train(task_mtcars,splits$train)

predictions = learner_fitted$predict(task_mtcars, splits$test)
predictions



