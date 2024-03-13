## ----setup, echo = FALSE-------------------------------------------------------------
knitr::opts_chunk$set(error = TRUE)


## ----chunk1--------------------------------------------------------------------------
library(ISLR2)
names(Smarket)
dim(Smarket)
summary(Smarket)
pairs(Smarket)


## ----chunk2, error=TRUE--------------------------------------------------------------
cor(Smarket)
cor(Smarket[, -9])


## ----chunk3--------------------------------------------------------------------------
attach(Smarket)
plot(Volume)


## ----chunk4--------------------------------------------------------------------------
glm.fits <- glm(
    Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume,
    data = Smarket, family = binomial
  )
summary(glm.fits)


## ----chunk5--------------------------------------------------------------------------
coef(glm.fits)
summary(glm.fits)$coef
summary(glm.fits)$coef[, 4]


## ----chunk6--------------------------------------------------------------------------
glm.probs <- predict(glm.fits, type = "response")
glm.probs[1:10]
contrasts(Direction)


## ----chunk7--------------------------------------------------------------------------
glm.pred <- rep("Down", 1250)
glm.pred[glm.probs > .5] = "Up"


## ----chunk8--------------------------------------------------------------------------
table(glm.pred, Direction)
(507 + 145) / 1250
mean(glm.pred == Direction)


## ----chunk9--------------------------------------------------------------------------
train <- (Year < 2005)
Smarket.2005 <- Smarket[!train, ]
dim(Smarket.2005)
Direction.2005 <- Direction[!train]


## ----chunk10-------------------------------------------------------------------------
glm.fits <- glm(
    Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume,
    data = Smarket, family = binomial, subset = train
  )
glm.probs <- predict(glm.fits, Smarket.2005,
    type = "response")


## ----chunk11-------------------------------------------------------------------------
glm.pred <- rep("Down", 252)
glm.pred[glm.probs > .5] <- "Up"
table(glm.pred, Direction.2005)
mean(glm.pred == Direction.2005)
mean(glm.pred != Direction.2005)


## ----chunk12-------------------------------------------------------------------------
glm.fits <- glm(Direction ~ Lag1 + Lag2, data = Smarket,
    family = binomial, subset = train)
glm.probs <- predict(glm.fits, Smarket.2005,
    type = "response")
glm.pred <- rep("Down", 252)
glm.pred[glm.probs > .5] <- "Up"
table(glm.pred, Direction.2005)
mean(glm.pred == Direction.2005)
106 / (106 + 76)


## ----chunk13-------------------------------------------------------------------------
predict(glm.fits,
    newdata =
      data.frame(Lag1 = c(1.2, 1.5),  Lag2 = c(1.1, -0.8)),
    type = "response"
  )


## ----grid----------------------------------------------------------------------------
color <- c('#e66101','#5e3c99','#fdb863','#b2abd2')
grid_n <- 400
grid_L1 <- seq(from = min(Smarket[["Lag1"]]), to = max(Smarket[["Lag1"]]),
               length.out = grid_n)
grid_L2 <- seq(from = min(Smarket[["Lag2"]]), to = max(Smarket[["Lag2"]]),
               length.out = grid_n)
grid_L <- expand.grid(Lag1 = grid_L1, Lag2 = grid_L2)


## ----plot_glm------------------------------------------------------------------------
p_grid_glm <- matrix(
  predict(glm.fits, newdata = grid_L, type = "response") > 0.5,
  nrow = grid_n, ncol = grid_n)

plot(NA, main = "Logistic Classifier", xlab = "Lag1", ylab = "Lag2",
      xlim = range(grid_L1), ylim = range(grid_L2))
.filled.contour(x = grid_L1, y = grid_L2, z = p_grid_glm, levels = c(0, 0.5, 1),
      col = color[3:4])
points(Smarket[["Lag1"]], Smarket[["Lag2"]],
      col = color[Smarket[["Direction"]]], pch = 16)
legend("topright", legend = c("Down", "Up"), col = color[1:2], pch = 16)


## ----chunk14-------------------------------------------------------------------------
library(MASS)
lda.fit <- lda(Direction ~ Lag1 + Lag2, data = Smarket,
    subset = train)
lda.fit
plot(lda.fit)


## ----chunk15-------------------------------------------------------------------------
lda.pred <- predict(lda.fit, Smarket.2005)
names(lda.pred)


## ----chunk16-------------------------------------------------------------------------
lda.class <- lda.pred$class
table(lda.class, Direction.2005)
mean(lda.class == Direction.2005)


## ----chunk17-------------------------------------------------------------------------
sum(lda.pred$posterior[, 1] >= .5)
sum(lda.pred$posterior[, 1] < .5)


## ----chunk18-------------------------------------------------------------------------
lda.pred$posterior[1:20, 1]
lda.class[1:20]


## ----chunk19-------------------------------------------------------------------------
sum(lda.pred$posterior[, 1] > .9)


## ----plot_lda------------------------------------------------------------------------
p_grid_lda <- matrix(as.numeric(predict(lda.fit, newdata = grid_L)[["class"]]),
                     nrow = grid_n, ncol = grid_n)

plot(NA, main = "Linear Discriminant Analysis", xlab = "Lag1", ylab = "Lag2",
      xlim = range(grid_L1), ylim = range(grid_L2))
.filled.contour(x = grid_L1, y = grid_L2, z = p_grid_lda, levels = c(1, 1.5, 2),
      col = color[3:4])
points(Smarket[["Lag1"]], Smarket[["Lag2"]],
      col = color[Smarket[["Direction"]]], pch = 16)
points(lda.fit[["means"]], pch = "+", cex = 3, col = color[3:4])
legend("topright", legend = c("Down", "Up"), col = color[1:2], pch = 16)


## ----chunk20-------------------------------------------------------------------------
qda.fit <- qda(Direction ~ Lag1 + Lag2, data = Smarket,
    subset = train)
qda.fit


## ----chunk21-------------------------------------------------------------------------
qda.class <- predict(qda.fit, Smarket.2005)$class
table(qda.class, Direction.2005)
mean(qda.class == Direction.2005)


## ----plot_qda------------------------------------------------------------------------
p_grid_qda <- matrix(as.numeric(predict(qda.fit, newdata = grid_L)[["class"]]),
                     nrow = grid_n, ncol = grid_n)

plot(NA, main = "Quadratic Discriminant Analysis", xlab = "Lag1", ylab = "Lag2",
     xlim = range(grid_L1), ylim = range(grid_L2))
.filled.contour(x = grid_L1, y = grid_L2, z = p_grid_qda, levels = c(1, 1.5, 2),
                col = color[3:4])
points(Smarket[["Lag1"]], Smarket[["Lag2"]],
       col = color[Smarket[["Direction"]]], pch = 16)
points(qda.fit[["means"]], pch = "+", cex = 3, col = color[3:4])
legend("topright", legend = c("Down", "Up"), col = color[1:2], pch = 16)

