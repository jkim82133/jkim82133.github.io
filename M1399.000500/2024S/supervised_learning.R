require(caret)


mse <- function(y_pred, y_true) {

  return (mean((y_pred - y_true)^2))
}


linear_knn_compare <- function(data_train, data_test) {
  
  par(mfrow = c(2, 2))
  
  model_lr <- lm(y ~ x, data = data_train)
  plot(data_train[["x"]], data_train[["y"]], main = "Linear Regression",
       xlab = "x", ylab = "y", pch = 16)
  abline(model_lr)
  cat(c("MSE on Training:",
        mse(data_train[["y"]], predict(model_lr, newdata = data_train)), "\n"))
  cat(c("MSE on Test:",
        mse(data_test[["y"]], predict(model_lr, newdata = data_test)), "\n"))
  
  for (k in c(1, 5, 15)) {
    model_knn <- caret::knnreg(y ~ x, data = data_train, k = k)
    plot(data_train[["x"]], data_train[["y"]],
         main = paste("Nearest Neighbor with k =", k), xlab = "x", ylab = "y",
         pch = 16)
    lines(data_train[["x"]], predict(model_knn, newdata = data_train))
    cat(c("MSE on Training:",
          mse(data_train[["y"]], predict(model_knn, newdata = data_train)),
          "\n"))
    cat(c("MSE on Test:",
          mse(data_test[["y"]], predict(model_knn, newdata = data_test)), "\n"))
  }
}


set.seed(0)
x <- rnorm(1000)
ep <- rnorm(1000)
train <- sort(x[1:100], index.return = TRUE)[["ix"]]
test <- 101:1000


# y = x + ep
y_l <- x + ep
data_l_train <- list(x = x[train], y = y_l[train])
data_l_test <- list(x = x[test], y = y_l[test])

linear_knn_compare(data_l_train, data_l_test)

# y = x(1-x) + ep
y_q <- x * (1 - x) + ep
data_q_train <- list(x = x[train], y = y_q[train])
data_q_test <- list(x = x[test], y = y_q[test])

linear_knn_compare(data_q_train, data_q_test)
