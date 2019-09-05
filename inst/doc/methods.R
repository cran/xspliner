## ----message = FALSE-----------------------------------------------------
library(xspliner)
library(randomForest)
library(magrittr)

rf_iris <- randomForest(Petal.Width ~  Sepal.Length + Petal.Length + Species, data = iris)
model_xs <- xspline(Petal.Width ~ 
  Sepal.Length + 
  xs(Petal.Length, effect = list(grid.resolution = 100), transition = list(bs = "cr")) + 
  xf(Species, transition = list(stat = "loglikelihood", value = -300)),
  model = rf_iris)
newdata <- data.frame(
  Sepal.Length = 10, 
  Petal.Length = 2, 
  Species = factor("virginica", levels = levels(iris$Species)))
predict(model_xs, newdata = newdata)

## ------------------------------------------------------------------------
print(model_xs)

## ------------------------------------------------------------------------
print(model_xs, "Petal.Length")

## ------------------------------------------------------------------------
print(model_xs, "Species")

## ------------------------------------------------------------------------
transition(model_xs, predictor = "Petal.Length", type = "data") %>% 
  head

transition(model_xs, predictor = "Species", type = "data") %>% 
  head

## ------------------------------------------------------------------------
transition(model_xs, predictor = "Petal.Length", type = "base")

transition(model_xs, predictor = "Species", type = "base")

## ------------------------------------------------------------------------
petal_length_xs <- transition(model_xs, predictor = "Petal.Length", type = "function")
x <- seq(1, 7, length.out = 50)
plot(x, petal_length_xs(x))

## ------------------------------------------------------------------------
species_xf <- transition(model_xs, predictor = "Species", type = "function")
species_xf(c("setosa", "versicolor", "virginica"))

## ------------------------------------------------------------------------
summary(model_xs)

## ------------------------------------------------------------------------
summary(model_xs, "Petal.Length")

## ------------------------------------------------------------------------
summary(model_xs, "Species")

## ------------------------------------------------------------------------
library(xspliner)
library(e1071)
set.seed(1)
data <- ISLR::Default
default.svm <- svm(default ~ ., data = data, probability = TRUE)
default.xs <- xspline(default ~ student + xs(balance) + xs(income), model = default.svm)

## ------------------------------------------------------------------------
prob_svm <- function(object, newdata) attr(predict(object, newdata = newdata, probability = TRUE), "probabilities")[, 2]
prob_xs <- function(object, newdata) predict(object, newdata = newdata, type = "response")

## ------------------------------------------------------------------------
summary(default.xs, model = default.svm, newdata = data, prediction_funs = list(prob_xs, prob_svm))

## ------------------------------------------------------------------------
response_svm <- function(object, newdata) predict(object, newdata = newdata)
response_xs <- function(object, newdata) {
  y_levels <- levels(newdata[[environment(object)$response]])
  factor(y_levels[(predict.glm(object, newdata = newdata, type = "link") > 0) + 1], levels = y_levels)
}

## ------------------------------------------------------------------------
summary(default.xs, model = default.svm, newdata = data, prediction_funs = list(response_xs, response_svm))

