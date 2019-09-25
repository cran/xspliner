## ----message = FALSE-----------------------------------------------------
library(pdp)
data(boston)
str(boston)
data(boston)
set.seed(123)

library(randomForest)
boston.rf <- randomForest(cmedv ~ lstat + ptratio + age, data = boston)

## ----message = FALSE-----------------------------------------------------
library(xspliner)
model_xs <- xspline(cmedv ~ xs(lstat) + xs(ptratio) + age, model = boston.rf)

## ----eval = FALSE--------------------------------------------------------
#  plot(model_xs, "lstat")

## ------------------------------------------------------------------------
plot_variable_transition(model_xs, "lstat")

## ------------------------------------------------------------------------
plot_variable_transition(model_xs, "lstat", data = boston, plot_data = TRUE)

## ------------------------------------------------------------------------
plot_variable_transition(model_xs, "lstat", plot_deriv = TRUE)

## ------------------------------------------------------------------------
plot_variable_transition(model_xs, "ptratio", plot_response = FALSE, plot_deriv = TRUE)

## ----eval = FALSE--------------------------------------------------------
#  plot_model_comparison(model_xs, model = boston.rf, data = boston)

## ------------------------------------------------------------------------
plot(model_xs, model = boston.rf, data = boston)

## ------------------------------------------------------------------------
iris_data <- droplevels(iris[iris$Species != "setosa", ])

library(e1071) 
library(xspliner)
model_svm <- svm(Species ~  Sepal.Length + Sepal.Width + Petal.Length + Petal.Width, 
                 data = iris_data, probability = TRUE)

model_xs <- xspline(Species ~  xs(Sepal.Length) + xs(Sepal.Width) + xs(Petal.Length) + xs(Petal.Width),
                    model = model_svm)

## ------------------------------------------------------------------------
prob_svm <- function(object, newdata) attr(predict(object, newdata = newdata, probability = TRUE), "probabilities")[, 2]
prob_xs <- function(object, newdata) predict(object, newdata = newdata, type = "response")

## ------------------------------------------------------------------------
plot_model_comparison(model_xs, model = model_svm, data = iris_data,
     prediction_funs = list(prob_xs, prob_svm)
)  

## ------------------------------------------------------------------------
plot_model_comparison(model_xs, model = model_svm, data = iris_data,
     prediction_funs = list(prob_xs, prob_svm),
     sort_by = "svm"
)  

## ------------------------------------------------------------------------
class_svm <- function(object, newdata) predict(object, newdata = newdata)
response_levels <- levels(iris_data$Species)
class_xs <- function(object, newdata) {
  probs <- predict(object, newdata = newdata, type = "response")
  factor(ifelse(probs > 0.5, response_levels[2], response_levels[1]), levels = response_levels)
}

## ------------------------------------------------------------------------
plot_model_comparison(model_xs, model = model_svm, data = iris_data,
     prediction_funs = list(class_xs, class_svm)
)  

## ------------------------------------------------------------------------
plot_model_comparison(model_xs, model = model_svm, data = iris_data,
     prediction_funs = list(class_xs, class_svm),
     sort_by = "svm"
)  

## ----message = FALSE-----------------------------------------------------
library(mgcv)

data(airquality)
ozone <- subset(na.omit(airquality),
                select = c("Ozone", "Solar.R", "Wind", "Temp"))
set.seed(123)

model_rf <- randomForest(Ozone ~ ., data = ozone)
model_xs <- xspline(Ozone ~ xs(Solar.R) + xs(Wind) + xs(Temp), model_rf, data = ozone)
model_glm <- glm(Ozone ~ ., data = ozone)
model_gam <- mgcv::gam(Ozone ~ s(Solar.R) + s(Wind) + s(Temp), data = ozone)

plot_model_comparison(model_xs, 
     model = model_rf, 
     data = ozone, 
     compare_with = list(glm = model_glm, gam = model_gam),
     sort_by = "xspliner")

## ----fig.width = 9.5-----------------------------------------------------
plot_variable_transition(model_xs)

## ----fig.width = 9.5-----------------------------------------------------
plot_variable_transition(model_xs, n_plots = 2)

## ----fig.width = 9.5-----------------------------------------------------
plot_variable_transition(model_xs, c("Wind", "Temp"))

