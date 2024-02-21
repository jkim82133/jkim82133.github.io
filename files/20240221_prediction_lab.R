## ----chunk31-----------------------------------------------------------------------------------------------------
# library(MASS)
library(ISLR2)


## ----chunk32-----------------------------------------------------------------------------------------------------
head(Boston)


## ----chunk33, error=TRUE-----------------------------------------------------------------------------------------
lm.fit <- lm(medv ~ lstat)


## ----chunk34-----------------------------------------------------------------------------------------------------
lm.fit <- lm(medv ~ lstat, data = Boston)
attach(Boston)
lm.fit <- lm(medv ~ lstat)


## ----chunk35-----------------------------------------------------------------------------------------------------
lm.fit
summary(lm.fit)


## ----chunk36-----------------------------------------------------------------------------------------------------
names(lm.fit)
coef(lm.fit)


## ----chunk37-----------------------------------------------------------------------------------------------------
confint(lm.fit)


## ----chunk38-----------------------------------------------------------------------------------------------------
predict(lm.fit, data.frame(lstat = (c(5, 10, 15))),
    interval = "confidence")
predict(lm.fit, data.frame(lstat = (c(5, 10, 15))),
    interval = "prediction")


## ----chunk39-----------------------------------------------------------------------------------------------------
plot(lstat, medv)
abline(lm.fit)


## ----chunk310----------------------------------------------------------------------------------------------------
plot(lstat, medv)
abline(lm.fit, lwd = 3)
abline(lm.fit, lwd = 3, col = "red")
plot(lstat, medv, col = "red")
plot(lstat, medv, pch = 20)
plot(lstat, medv, pch = "+")
plot(1:20, 1:20, pch = 1:20)


## ----chunk314----------------------------------------------------------------------------------------------------
lm.fit <- lm(medv ~ lstat + age, data = Boston)
summary(lm.fit)


## ----chunk315----------------------------------------------------------------------------------------------------
lm.fit <- lm(medv ~ ., data = Boston)
summary(lm.fit)


## ----chunk317----------------------------------------------------------------------------------------------------
lm.fit1 <- lm(medv ~ . - age, data = Boston)
summary(lm.fit1)


## ----chunk318----------------------------------------------------------------------------------------------------
lm.fit1 <- update(lm.fit, ~ . - age)


## ----chunk325----------------------------------------------------------------------------------------------------
head(Carseats)


## ----chunk326----------------------------------------------------------------------------------------------------
lm.fit <- lm(Sales ~ ., data = Carseats)
summary(lm.fit)


## ----chunk327----------------------------------------------------------------------------------------------------
attach(Carseats)
contrasts(ShelveLoc)


## ----chunk311----------------------------------------------------------------------------------------------------
par(mfrow = c(2, 2))
lm.fit <- lm(medv ~ lstat + age, data = Boston)
plot(lm.fit)


## ----chunk312----------------------------------------------------------------------------------------------------
plot(predict(lm.fit), residuals(lm.fit))
plot(predict(lm.fit), rstudent(lm.fit))


## ----chunk313----------------------------------------------------------------------------------------------------
plot(hatvalues(lm.fit))
which.max(hatvalues(lm.fit))


## ----chunk61-----------------------------------------------------------------------------------------------------
library(ISLR2)
names(Hitters)
dim(Hitters)
sum(is.na(Hitters$Salary))


## ----chunk62-----------------------------------------------------------------------------------------------------
Hitters <- na.omit(Hitters)
dim(Hitters)
sum(is.na(Hitters))


## ----chunk63-----------------------------------------------------------------------------------------------------
library(leaps)
regfit.full <- regsubsets(Salary ~ ., Hitters)
summary(regfit.full)


## ----chunk64-----------------------------------------------------------------------------------------------------
regfit.full <- regsubsets(Salary ~ ., data = Hitters,
    nvmax = 19)
reg.summary <- summary(regfit.full)


## ----chunk65-----------------------------------------------------------------------------------------------------
names(reg.summary)


## ----chunk66-----------------------------------------------------------------------------------------------------
reg.summary$rsq


## ----chunk67-----------------------------------------------------------------------------------------------------
par(mfrow = c(2, 2))
plot(reg.summary$rss, xlab = "Number of Variables",
    ylab = "RSS", type = "l")
plot(reg.summary$adjr2, xlab = "Number of Variables",
    ylab = "Adjusted RSq", type = "l")


## ----chunk68-----------------------------------------------------------------------------------------------------
which.max(reg.summary$adjr2)
plot(reg.summary$adjr2, xlab = "Number of Variables",
    ylab = "Adjusted RSq", type = "l")
points(11, reg.summary$adjr2[11], col = "red", cex = 2, 
    pch = 20)


## ----chunk69-----------------------------------------------------------------------------------------------------
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


## ----chunk610----------------------------------------------------------------------------------------------------
plot(regfit.full, scale = "r2")
plot(regfit.full, scale = "adjr2")
plot(regfit.full, scale = "Cp")
plot(regfit.full, scale = "bic")


## ----chunk611----------------------------------------------------------------------------------------------------
coef(regfit.full, 6)


## ----chunk612----------------------------------------------------------------------------------------------------
regfit.fwd <- regsubsets(Salary ~ ., data = Hitters,
    nvmax = 19, method = "forward")
summary(regfit.fwd)
regfit.bwd <- regsubsets(Salary ~ ., data = Hitters,
    nvmax = 19, method = "backward")
summary(regfit.bwd)


## ----chunk613----------------------------------------------------------------------------------------------------
coef(regfit.full, 7)
coef(regfit.fwd, 7)
coef(regfit.bwd, 7)


## ----chunk621----------------------------------------------------------------------------------------------------
k <- 10
n <- nrow(Hitters)
set.seed(1)
folds <- sample(rep(1:k, length = n))
cv.errors <- matrix(NA, k, 19,
    dimnames = list(NULL, paste(1:19)))


## ----chunk619----------------------------------------------------------------------------------------------------
 predict.regsubsets <- function(object, newdata, id, ...) {
  form <- as.formula(object$call[[2]])
  mat <- model.matrix(form, newdata)
  coefi <- coef(object, id = id)
  xvars <- names(coefi)
  mat[, xvars] %*% coefi
 }


## ----chunk622----------------------------------------------------------------------------------------------------
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


## ----chunk623----------------------------------------------------------------------------------------------------
mean.cv.errors <- apply(cv.errors, 2, mean)
mean.cv.errors
par(mfrow = c(1, 1))
plot(mean.cv.errors, type = "b")


## ----chunk624----------------------------------------------------------------------------------------------------
reg.best <- regsubsets(Salary ~ ., data = Hitters,
    nvmax = 19)
coef(reg.best, 10)


## ----chunk625----------------------------------------------------------------------------------------------------
x <- model.matrix(Salary ~ ., Hitters)[, -1]
y <- Hitters$Salary


## ----chunk626----------------------------------------------------------------------------------------------------
library(glmnet)
grid <- 10^seq(10, -2, length = 100)
ridge.mod <- glmnet(x, y, alpha = 0, lambda = grid)


## ----chunk627----------------------------------------------------------------------------------------------------
dim(coef(ridge.mod))


## ----chunk628----------------------------------------------------------------------------------------------------
ridge.mod$lambda[50]
coef(ridge.mod)[, 50]
sqrt(sum(coef(ridge.mod)[-1, 50]^2))


## ----chunk629----------------------------------------------------------------------------------------------------
ridge.mod$lambda[60]
coef(ridge.mod)[, 60]
sqrt(sum(coef(ridge.mod)[-1, 60]^2))


## ----chunk630----------------------------------------------------------------------------------------------------
predict(ridge.mod, s = 50, type = "coefficients")[1:20, ]


## ----chunk631----------------------------------------------------------------------------------------------------
set.seed(1)
train <- sample(1:nrow(x), nrow(x) / 2)
test <- (-train)
y.test <- y[test]


## ----chunk632----------------------------------------------------------------------------------------------------
ridge.mod <- glmnet(x[train, ], y[train], alpha = 0,
    lambda = grid, thresh = 1e-12)
ridge.pred <- predict(ridge.mod, s = 4, newx = x[test, ])
mean((ridge.pred - y.test)^2)


## ----chunk633----------------------------------------------------------------------------------------------------
mean((mean(y[train]) - y.test)^2)


## ----chunk634----------------------------------------------------------------------------------------------------
ridge.pred <- predict(ridge.mod, s = 1e10, newx = x[test, ])
mean((ridge.pred - y.test)^2)


## ----chunk635----------------------------------------------------------------------------------------------------
ridge.pred <- predict(ridge.mod, s = 0, newx = x[test, ],
    exact = T, x = x[train, ], y = y[train])
mean((ridge.pred - y.test)^2)
lm(y ~ x, subset = train)
predict(ridge.mod, s = 0, exact = T, type = "coefficients",
    x = x[train, ], y = y[train])[1:20, ]


## ----chunk636----------------------------------------------------------------------------------------------------
set.seed(1)
cv.out <- cv.glmnet(x[train, ], y[train], alpha = 0)
plot(cv.out)
bestlam <- cv.out$lambda.min
bestlam


## ----chunk637----------------------------------------------------------------------------------------------------
ridge.pred <- predict(ridge.mod, s = bestlam,
    newx = x[test, ])
mean((ridge.pred - y.test)^2)


## ----chunk638----------------------------------------------------------------------------------------------------
out <- glmnet(x, y, alpha = 0)
predict(out, type = "coefficients", s = bestlam)[1:20, ]


## ----chunk639----------------------------------------------------------------------------------------------------
lasso.mod <- glmnet(x[train, ], y[train], alpha = 1,
    lambda = grid)
plot(lasso.mod)


## ----chunk640----------------------------------------------------------------------------------------------------
set.seed(1)
cv.out <- cv.glmnet(x[train, ], y[train], alpha = 1)
plot(cv.out)
bestlam <- cv.out$lambda.min
lasso.pred <- predict(lasso.mod, s = bestlam,
    newx = x[test, ])
mean((lasso.pred - y.test)^2)


## ----chunk641----------------------------------------------------------------------------------------------------
out <- glmnet(x, y, alpha = 1, lambda = grid)
lasso.coef <- predict(out, type = "coefficients",
    s = bestlam)[1:20, ]
lasso.coef
lasso.coef[lasso.coef != 0]


## ----chunk71-----------------------------------------------------------------------------------------------------
library(ISLR2)
attach(Wage)


## ----chunk72-----------------------------------------------------------------------------------------------------
fit <- lm(wage ~ poly(age, 4), data = Wage)
coef(summary(fit))


## ----chunk73-----------------------------------------------------------------------------------------------------
fit2 <- lm(wage ~ poly(age, 4, raw = T), data = Wage)
coef(summary(fit2))


## ----chunk74-----------------------------------------------------------------------------------------------------
fit2a <- lm(wage ~ age + I(age^2) + I(age^3) + I(age^4),
    data = Wage)
coef(fit2a)


## ----chunk75-----------------------------------------------------------------------------------------------------
fit2b <- lm(wage ~ cbind(age, age^2, age^3, age^4),
    data = Wage)


## ----chunk76-----------------------------------------------------------------------------------------------------
agelims <- range(age)
age.grid <- seq(from = agelims[1], to = agelims[2])
preds <- predict(fit, newdata = list(age = age.grid),
    se = TRUE)
se.bands <- cbind(preds$fit + 2 * preds$se.fit,
    preds$fit - 2 * preds$se.fit)


## ----chunk77-----------------------------------------------------------------------------------------------------
par(mfrow = c(1, 2), mar = c(4.5, 4.5, 1, 1),
    oma = c(0, 0, 4, 0))
plot(age, wage, xlim = agelims, cex = .5, col = "darkgrey")
title("Degree-4 Polynomial", outer = T)
lines(age.grid, preds$fit, lwd = 2, col = "blue")
matlines(age.grid, se.bands, lwd = 1, col = "blue", lty = 3)


## ----chunk78-----------------------------------------------------------------------------------------------------
preds2 <- predict(fit2, newdata = list(age = age.grid),
    se = TRUE)
max(abs(preds$fit - preds2$fit))


## ----chunk718----------------------------------------------------------------------------------------------------
table(cut(age, 4))
fit <- lm(wage ~ cut(age, 4), data = Wage)
coef(summary(fit))


## ----chunk719----------------------------------------------------------------------------------------------------
library(splines)
fit <- lm(wage ~ bs(age, knots = c(25, 40, 60)), data = Wage)
pred <- predict(fit, newdata = list(age = age.grid), se = T)
plot(age, wage, col = "gray")
lines(age.grid, pred$fit, lwd = 2)
lines(age.grid, pred$fit + 2 * pred$se, lty = "dashed")
lines(age.grid, pred$fit - 2 * pred$se, lty = "dashed")


## ----chunk720----------------------------------------------------------------------------------------------------
dim(bs(age, knots = c(25, 40, 60)))
dim(bs(age, df = 6))
attr(bs(age, df = 6), "knots")


## ----chunk721----------------------------------------------------------------------------------------------------
fit2 <- lm(wage ~ ns(age, df = 4), data = Wage)
pred2 <- predict(fit2, newdata = list(age = age.grid),
     se = T)
plot(age, wage, col = "gray")
lines(age.grid, pred2$fit, col = "red", lwd = 2)


## ----chunk722----------------------------------------------------------------------------------------------------
plot(age, wage, xlim = agelims, cex = .5, col = "darkgrey")
title("Smoothing Spline")
fit <- smooth.spline(age, wage, df = 16)
fit2 <- smooth.spline(age, wage, cv = TRUE)
fit2$df
lines(fit, col = "red", lwd = 2)
lines(fit2, col = "blue", lwd = 2)
legend("topright", legend = c("16 DF", "6.8 DF"),
    col = c("red", "blue"), lty = 1, lwd = 2, cex = .8)


## ----chunk723----------------------------------------------------------------------------------------------------
plot(age, wage, xlim = agelims, cex = .5, col = "darkgrey")
title("Local Regression")
fit <- loess(wage ~ age, span = .2, data = Wage)
fit2 <- loess(wage ~ age, span = .5, data = Wage)
lines(age.grid, predict(fit, data.frame(age = age.grid)),
    col = "red", lwd = 2)
lines(age.grid, predict(fit2, data.frame(age = age.grid)),
    col = "blue", lwd = 2)
legend("topright", legend = c("Span = 0.2", "Span = 0.5"),
    col = c("red", "blue"), lty = 1, lwd = 2, cex = .8)


## ----chunk724----------------------------------------------------------------------------------------------------
gam1 <- lm(wage ~ ns(year, 4) + ns(age, 5) + education,
    data = Wage)


## ----chunk725----------------------------------------------------------------------------------------------------
library(gam)
gam.m3 <- gam(wage ~ s(year, 4) + s(age, 5) + education,
    data = Wage)


## ----chunk726----------------------------------------------------------------------------------------------------
par(mfrow = c(1, 3))
plot(gam.m3, se = TRUE, col = "blue")


## ----chunk727----------------------------------------------------------------------------------------------------
plot.Gam(gam1, se = TRUE, col = "red")


## ----chunk729----------------------------------------------------------------------------------------------------
summary(gam.m3)

