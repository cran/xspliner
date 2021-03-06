## ----message = FALSE-----------------------------------------------------
library(xgboost)
library(xspliner)
library(breakDown)
HR <- HR_data

str(HR_data)
model_matrix_train <- model.matrix(left ~ . -1, HR)
data_train <- xgb.DMatrix(model_matrix_train, label = HR$left)
param <- list(max_depth = 2, objective = "binary:logistic")

HR_xgb_model <- xgb.train(param, data_train, nrounds = 50)
model_xs <- xspline(HR_xgb_model, lhs = "left", response = "left", predictors = colnames(HR)[-7],
                 data = HR, form = "additive", family = "binomial", link = "logit",
                 bare = c("number_project", "time_spend_company", "Work_accident", "promotion_last_5years"),
                 xs_opts = list(effect = list(train = model_matrix_train)),
                 xf_opts = list(transition = list(alter = "never"))) 
summary(model_xs)

## ----message = FALSE-----------------------------------------------------
library(DALEX)
library(caret)
library(xspliner)
data(apartments)

set.seed(123)
variable <- "construction.year"
regr_rf <- train(m2.price ~ ., data = apartments, method = "rf", ntree = 100)
model_xs <- xspline(regr_rf, data = apartments, bare = c("floor", "no.rooms"),
                    xs_opts = list(transition = list(bs = "ps", fx = FALSE, k = 20, m = -1)))
plot_variable_transition(model_xs, "surface")

