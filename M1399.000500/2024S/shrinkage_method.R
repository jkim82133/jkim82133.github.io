## ----setup, echo = FALSE-------------------------------------------------------------
knitr::opts_chunk$set(error = TRUE)


## ----chunk1--------------------------------------------------------------------------
library(ISLR2)
names(Hitters)
dim(Hitters)
sum(is.na(Hitters$Salary))


## ----chunk2--------------------------------------------------------------------------
Hitters <- na.omit(Hitters)
dim(Hitters)
sum(is.na(Hitters))


## ----chunk3--------------------------------------------------------------------------
library(leaps)
regfit.full <- regsubsets(Salary ~ ., Hitters)
summary(regfit.full)


## ----chunk4--------------------------------------------------------------------------
regfit.full <- regsubsets(Salary ~ ., data = Hitters,
    nvmax = 19)
reg.summary <- summary(regfit.full)


## ----chunk5--------------------------------------------------------------------------
names(reg.summary)


## ----chunk6--------------------------------------------------------------------------
reg.summary$rsq


## ----chunk7--------------------------------------------------------------------------
par(mfrow = c(2, 2))
plot(reg.summary$rss, xlab = "Number of Variables",
    ylab = "RSS", type = "l")
plot(reg.summary$adjr2, xlab = "Number of Variables",
    ylab = "Adjusted RSq", type = "l")


## ----chunk8--------------------------------------------------------------------------
which.max(reg.summary$adjr2)
plot(reg.summary$adjr2, xlab = "Number of Variables",
    ylab = "Adjusted RSq", type = "l")
points(11, reg.summary$adjr2[11], col = "red", cex = 2, 
    pch = 20)


## ----chunk9--------------------------------------------------------------------------
plot(reg.summary$cp, xlab = "Number of Variables",
    ylab = "Cp", type = "l")
which.min(reg.summary$cp)
points(10, reg.summary$cp[10], col = "red", cex = 2,
    pch = 20)
which.min(reg.summary$bic)
plot(reg.summary$bic, xlab = "Number of Variables",
    ylab = "BIC", type = "l")
points(6, reg.summary$bic[6], col = "red", cex = 2,
    pch = 20)


## ----chunk10-------------------------------------------------------------------------
plot(regfit.full, scale = "r2")
plot(regfit.full, scale = "adjr2")
plot(regfit.full, scale = "Cp")
plot(regfit.full, scale = "bic")


## ----chunk11-------------------------------------------------------------------------
coef(regfit.full, 6)


## ----chunk12-------------------------------------------------------------------------
regfit.fwd <- regsubsets(Salary ~ ., data = Hitters,
    nvmax = 19, method = "forward")
summary(regfit.fwd)
regfit.bwd <- regsubsets(Salary ~ ., data = Hitters,
    nvmax = 19, method = "backward")
summary(regfit.bwd)


## ----chunk13-------------------------------------------------------------------------
coef(regfit.full, 7)
coef(regfit.fwd, 7)
coef(regfit.bwd, 7)


## ----chunk14-------------------------------------------------------------------------
set.seed(1)
train <- sample(c(TRUE, FALSE), nrow(Hitters),
    replace = TRUE)
test <- (!train)


## ----chunk15-------------------------------------------------------------------------
regfit.best <- regsubsets(Salary ~ .,
    data = Hitters[train, ], nvmax = 19)


## ----chunk16-------------------------------------------------------------------------
test.mat <- model.matrix(Salary ~ ., data = Hitters[test, ])


## ----chunk17-------------------------------------------------------------------------
val.errors <- rep(NA, 19)
for (i in 1:19) {
 coefi <- coef(regfit.best, id = i)
 pred <- test.mat[, names(coefi)] %*% coefi
 val.errors[i] <- mean((Hitters$Salary[test] - pred)^2)
}


## ----chunk18-------------------------------------------------------------------------
val.errors
which.min(val.errors)
coef(regfit.best, 7)


## ----chunk19-------------------------------------------------------------------------
 predict.regsubsets <- function(object, newdata, id, ...) {
  form <- as.formula(object$call[[2]])
  mat <- model.matrix(form, newdata)
  coefi <- coef(object, id = id)
  xvars <- names(coefi)
  mat[, xvars] %*% coefi
 }


## ----chunk20-------------------------------------------------------------------------
regfit.best <- regsubsets(Salary ~ ., data = Hitters,
    nvmax = 19)
coef(regfit.best, 7)


## ----chunk21-------------------------------------------------------------------------
k <- 10
n <- nrow(Hitters)
set.seed(1)
folds <- sample(rep(1:k, length = n))
cv.errors <- matrix(NA, k, 19,
    dimnames = list(NULL, paste(1:19)))


## ----chunk22-------------------------------------------------------------------------
for (j in 1:k) {
  best.fit <- regsubsets(Salary ~ .,
       data = Hitters[folds != j, ],
       nvmax = 19)
  for (i in 1:19) {
    pred <- predict(best.fit, Hitters[folds == j, ], id = i)
    cv.errors[j, i] <-
         mean((Hitters$Salary[folds == j] - pred)^2)
   }
 }


## ----chunk23-------------------------------------------------------------------------
mean.cv.errors <- apply(cv.errors, 2, mean)
mean.cv.errors
par(mfrow = c(1, 1))
plot(mean.cv.errors, type = "b")


## ----chunk24-------------------------------------------------------------------------
reg.best <- regsubsets(Salary ~ ., data = Hitters,
    nvmax = 19)
coef(reg.best, 10)


## ----chunk625------------------------------------------------------------------------
x <- model.matrix(Salary ~ ., Hitters)[, -1]
y <- Hitters$Salary


## ----chunk626------------------------------------------------------------------------
library(glmnet)
grid <- 10^seq(10, -2, length = 100)
ridge.mod <- glmnet(x, y, alpha = 0, lambda = grid)


## ----chunk627------------------------------------------------------------------------
dim(coef(ridge.mod))


## ----chunk628------------------------------------------------------------------------
ridge.mod$lambda[50]
coef(ridge.mod)[, 50]
sqrt(sum(coef(ridge.mod)[-1, 50]^2))


## ----chunk629------------------------------------------------------------------------
ridge.mod$lambda[60]
coef(ridge.mod)[, 60]
sqrt(sum(coef(ridge.mod)[-1, 60]^2))


## ----chunk630------------------------------------------------------------------------
predict(ridge.mod, s = 50, type = "coefficients")[1:20, ]


## ----chunk631------------------------------------------------------------------------
set.seed(1)
train <- sample(1:nrow(x), nrow(x) / 2)
test <- (-train)
y.test <- y[test]


## ----chunk632------------------------------------------------------------------------
ridge.mod <- glmnet(x[train, ], y[train], alpha = 0,
    lambda = grid, thresh = 1e-12)
ridge.pred <- predict(ridge.mod, s = 4, newx = x[test, ])
mean((ridge.pred - y.test)^2)


## ----chunk633------------------------------------------------------------------------
mean((mean(y[train]) - y.test)^2)


## ----chunk634------------------------------------------------------------------------
ridge.pred <- predict(ridge.mod, s = 1e10, newx = x[test, ])
mean((ridge.pred - y.test)^2)


## ----chunk635------------------------------------------------------------------------
ridge.pred <- predict(ridge.mod, s = 0, newx = x[test, ],
    exact = T, x = x[train, ], y = y[train])
mean((ridge.pred - y.test)^2)
lm(y ~ x, subset = train)
predict(ridge.mod, s = 0, exact = T, type = "coefficients",
    x = x[train, ], y = y[train])[1:20, ]


## ----chunk636------------------------------------------------------------------------
set.seed(1)
cv.out <- cv.glmnet(x[train, ], y[train], alpha = 0)
plot(cv.out)
bestlam <- cv.out$lambda.min
bestlam


## ----chunk637------------------------------------------------------------------------
ridge.pred <- predict(ridge.mod, s = bestlam,
    newx = x[test, ])
mean((ridge.pred - y.test)^2)


## ----chunk638------------------------------------------------------------------------
out <- glmnet(x, y, alpha = 0)
predict(out, type = "coefficients", s = bestlam)[1:20, ]


## ----chunk639------------------------------------------------------------------------
lasso.mod <- glmnet(x[train, ], y[train], alpha = 1,
    lambda = grid)
plot(lasso.mod)


## ----chunk640------------------------------------------------------------------------
set.seed(1)
cv.out <- cv.glmnet(x[train, ], y[train], alpha = 1)
plot(cv.out)
bestlam <- cv.out$lambda.min
lasso.pred <- predict(lasso.mod, s = bestlam,
    newx = x[test, ])
mean((lasso.pred - y.test)^2)


## ----chunk641------------------------------------------------------------------------
out <- glmnet(x, y, alpha = 1, lambda = grid)
lasso.coef <- predict(out, type = "coefficients",
    s = bestlam)[1:20, ]
lasso.coef
lasso.coef[lasso.coef != 0]

