set.seed(1)
x <- matrix(rnorm(20 * 2), ncol = 2)
y <- c(rep(-1, 10), rep(1, 10))
x[y == 1, ] <- x[y == 1, ] + 1
plot(x, col = (3 - y))

dat <- data.frame(x = x, y = as.factor(y))

library(e1071)
svm.fit <- svm(y~. , data = dat, kernel = "linear", 
    cost = 10, scale = FALSE)

plot(svm.fit, data = dat)

svm.fit$index

summary(svm.fit)

svm.fit <- svm(y~. , data = dat, kernel = "linear",
               cost = 0.1, scale = FALSE)
summary(svm.fit)


## Cross Validation to select optimum cost 

set.seed(1)

tune.out <- tune(svm, y ~ . , data = dat, kernel = "linear",
     ranges = list(cost = c(0.001, 0.01, 0.1, 1, 5, 10, 100))
)

summary(tune.out)

best.model <- tune.out$best.model
summary(best.model)

# create test data 

xtest <- matrix(rnorm(20 * 2), ncol = 2)
ytest <- sample(c(-1, 1), 20, rep = TRUE)
xtest[ytest == 1, ] <- xtest[ytest == 1, ] + 1
testdat <- data.frame(x = xtest, y = as.factor(ytest))

testdat

ypred <- predict(best.model, testdat)
table(predict = ypred, truth = testdat$y)


# try to build a model with 0.01 cost 

svm.fit2 <- svm(y ~ . , data = dat, kernel = "linear",
                cost = 0.01, scale = FALSE)
y_pred2 <- predict(svm.fit2, testdat)
table(predict = y_pred2, truth = testdat$y)

















