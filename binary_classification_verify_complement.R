library(data.table)
library(ggplot2)
library(ROCR)
library(randomForest)
library(xgboost)
library(dplyr)
library(glmnet)
gen_dat = function(n_sample = 100, c1 = c(0, 1), c2 = c(1, 0), sig = 0.1) {
  tmp1 = data.table(x1=rnorm(n_sample, c1[1], sig), x2=rnorm(n_sample, c1[2], sig) )
  tmp2 = cbind(x1=rnorm(n_sample, c2[1], sig), x2=rnorm(n_sample, c2[2], sig) )
  x = rbind(tmp1, tmp2)
  y = c(rep(0, n_sample), rep(1, n_sample))
  dat = data.table(x, y=y)
  dat
}


inv_label = function(aDat) {
  aDat$y = (aDat$y - 1) * (-1)
  aDat
}


dat = gen_dat(1000)
inv_dat = inv_label(dat)
plot(dat$x1, dat$x2, t='p')


m1 = glmnet(data.matrix(dat[, 1:2]), dat$y)
m2 = glmnet(data.matrix(inv_dat[, 1:2]), inv_dat$y)

# m1$lambda
# m2$lambda
dat[1]
anewx = matrix(c(0,0), 1, 2)
newxs = matrix(rnorm(10, 0, 1), ncol = 2)
predict(m1, newx=anewx, s=0.003226780) + predict(m2, newx=anewx, s=0.003226780)
predict(m1, newx=newxs, s=0.003226780) + predict(m2, newx=newxs, s=0.003226780)
