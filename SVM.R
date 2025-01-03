library(e1071)

set.seed(1)
x <- matrix(rnorm(20 * 2), ncol = 2)
y <- c(rep(-1, 10), rep(1, 10))
x[y == 1, ] <- x[y == 1, ] + 1
plot(x, col = (3 - y))

dat <- data.frame(x = x, y = as.factor(y))


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


# Linearly seperable classes 
x[y == 1, ] <- x[y == 1, ] + 0.5
plot(x, col = (y + 5) / 2, pch = 19)

#  We fit the support vector classifier and plot the resulting hyperplane, 
# using a very large value of cost so that no observations are misclassified.

dat <- data.frame(x = x, y = as.factor(y))

svm.fit <- svm(y~ ., data = dat, kernel = "linear", 
    cost = 1e5)

summary(svm.fit)

plot(svm.fit, dat)

# decrease the cost 

svm.fit2 <- svm(y~. , data = dat, kernel = "linear",
    cost = 1)

summary(svm.fit2)

plot(svm.fit2, dat)

# Using cost = 1, we misclassify a training observation, but we also obtain a much wider margin and make use of seven support vectors. 
# It seems likely that this model will perform better on test data than the model with cost = 1e5.


### SVM - Non Linear Kernel - 
# 1 polynomial  - use degree argument 
# 2 radial - use gamma 


# generate data 
set.seed(1)
x <- matrix(rnorm(200 * 2), ncol = 2)
x[1:100, ] <- x[1:100, ] + 2
x[101:150, ] <- x[101:150, ] - 2
y <- c(rep(1, 150), rep(2, 50))
dat <- data.frame(x = x, y = as.factor(y))

plot(x, col = y)

# split data 

train <- sample(200, 100)
svm.fit <- svm(y ~ ., data = dat[train, ], kernel = "radial",
               gamma = 1 , cost = 1)

plot(svm.fit, dat[train, ])

summary(svm.fit)


# increase cost 
# increase cost : reduce misclassification errors
# but irregular decision boundary increase risk of overfitting 

svm.fit2 <- svm(y ~., data = dat[train, ], kernel = "radial",
    gamma = 1, cost = 1e5)

plot(svm.fit2, dat[train, ])

summary(svm.fit2)


# cross-validation using tune() to select the best choice of 𝛾and cost for an SVM with a radial kernel:

set.seed(1)
tune.out <- tune(svm, y~., data = dat[train, ],
                 kernel = "radial", 
                 ranges = list(
                   cost = c(0.1, 1, 10, 100, 1000),
                   gamma = c(0.5, 1, 2, 3, 4)
                 ))

summary(tune.out)

# predict using best model 

table(
  true = dat[-train, "y"],
  pred = predict(
    tune.out$best.model, newdata = dat[-train, ]
  )
)


# ROC Curves 

library(ROCR)


rocplot <- function(pred, truth, ...) {
  predob <- prediction(pred, truth)
  perf <- performance(predob, "tpr", "fpr")
  plot(perf, ...)
}



svm.fit.opt <- svm(y~., data = dat[train, ],
                   kernel = "radial", gamma = 2, cost = 1, 
                   decision.values = T)

fitted <- attributes(
  predict(svm.fit.opt, dat[train, ], decision.values = TRUE)
)$decision.values

fitted

par(mfrow = c(1, 2))


rocplot(-fitted, dat[train, "y"], main = "Training Data")

# increase gamma 

rocplot(-fitted, dat[train, "y"], main = "Training Data")
svmfit.flex <- svm(y ~ ., data = dat[train, ], 
                   kernel = "radial", gamma = 50, cost = 1, 
                   decision.values = T)
fitted <- attributes(
  predict(svmfit.flex, dat[train, ], decision.values = T)
)$decision.values
rocplot(-fitted, dat[train, "y"], add = T, col = "red")


fitted <- attributes(
  predict(svmfit.opt, dat[-train, ], decision.values = T)
)$decision.values
rocplot(-fitted, dat[-train, "y"], main = "Test Data")
fitted <- attributes(
  predict(svmfit.flex, dat[-train, ], decision.values = T)
)$decision.values
rocplot(-fitted, dat[-train, "y"], add = T, col = "red")


# SVM with Multiple Classes

set.seed(1)
x <- rbind(x, matrix(rnorm(50 * 2), ncol = 2))
y <- c(y, rep(0, 50))
x[y == 0, 2] <- x[y == 0, 2] + 2
dat <- data.frame(x = x, y = as.factor(y))
par(mfrow = c(1, 1))
plot(x, col = (y + 1))

svmfit <- svm(y ~ ., data = dat, kernel = "radial", 
              cost = 10, gamma = 1)
plot(svmfit, dat)


# Application to Gene Expression Data

library(ISLR2)
names(Khan)

# 20 observation ans 2308 genes 

dim(Khan$xtrain)


dim(Khan$xtest)


# We will use a support vector approach to predict cancer subtype using gene expression measurements. 
# In this data set, there are a very large number of features relative to the number of observations. 
# This suggests that we should use a linear kernel, 
# because the additional flexibility that will result from using a polynomial or radial kernel is unnecessary.

dat <- data.frame(
  x = Khan$xtrain,
  y = as.factor(Khan$ytrain)
)

out <- svm(y~. , data = dat, kernel = "linear", cost = 10)

summary(out)

table(out$fitted, dat$y)

#  no training errors : large number of variables relative to the number of observations implies that it is easy to find hyperplanes that fully separate the classes.

# testing data 

dat.te <- data.frame(
  x = Khan$xtest, 
  y = as.factor(Khan$ytest))
pred.te <- predict(out, newdata = dat.te)
table(pred.te, dat.te$y)

