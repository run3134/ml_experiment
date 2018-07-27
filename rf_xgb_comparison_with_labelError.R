# conclusion
# 1. if label mistake ratio < 50%, gbm still have a better solution


### simulate data
# cluster 1=centor(0, 1), cluster 2=cnetor(1, 0)
library(data.table)
library(ggplot2)
library(ROCR)
library(randomForest)
library(xgboost)
library(dplyr)
gen_dat = function(n_sample = 100, c1 = c(0, 1), c2 = c(1, 0), sig = 0.1) {
  tmp1 = data.table(x1=rnorm(n_sample, c1[1], sig), x2=rnorm(n_sample, c1[2], sig) )
  tmp2 = cbind(x1=rnorm(n_sample, c2[1], sig), x2=rnorm(n_sample, c2[2], sig) )
  x = rbind(tmp1, tmp2)
  y = c(rep(0, n_sample), rep(1, n_sample))
  dat = data.table(x, y=y)
  dat
}
# inject in-correct labels
inject_incorr = function(dat, prop=0.1) {
  sz = nrow(dat)
  rand = runif(sz)
  dat$y[rand<prop] = ((dat$y[rand<prop]*2-1) * (-1)+1) / 2
  dat
}

ret = NULL
for (sig in seq(0.5, 1.8, 0.3)) {
  for (aprop in c(0.05, 0.3, 0.5, 0.7)) {
    for(i in 1:10) {
      tr_dat = gen_dat(n_sample = 500, sig=sig)
      te_dat = gen_dat(n_sample = 100, sig=sig)
      # ggplot(te_dat, aes(x=x1, y=x2, color=y))+geom_point()
      
      #### orig data
      rf_tr_dat = tr_dat
      rf_tr_dat$y = factor(tr_dat$y)
      rf_te_dat = te_dat
      rf_te_dat$y = factor(te_dat$y)
      rf = randomForest(y ~., data = rf_tr_dat)
      rf_scores = predict(rf, rf_te_dat, type = 'prob')[, 2]
      rf_pred = prediction(labels = te_dat$y, predictions = rf_scores)
      
      
      dtrain <- xgb.DMatrix(data = data.matrix(tr_dat[, 1:2]), label = tr_dat$y)
      dtest <- xgb.DMatrix(data = data.matrix(te_dat[, 1:2]), label = te_dat$y)
      xgb = xgboost(data=dtrain, objective = "binary:logistic", nround=25, verbose = FALSE)
      xgb_score = predict(xgb, dtest)
      xgb_pred = prediction(labels = te_dat$y, predictions = xgb_score)
      (rf_auc_perf_orig = performance(rf_pred, measure="auc")@y.values[[1]])
      (xgb_auc_perf_orig = performance(xgb_pred, measure="auc")@y.values[[1]])
      
      #### inject noise data
      tr_dat = inject_incorr(tr_dat, prop = aprop)
      # te_dat = inject_incorr(te_dat, prop = aprop)
      rf_tr_dat = tr_dat
      rf_tr_dat$y = factor(tr_dat$y)
      rf_te_dat = te_dat
      rf_te_dat$y = factor(te_dat$y)
      rf = randomForest(y ~., data = rf_tr_dat)
      rf_scores = predict(rf, rf_te_dat, type = 'prob')[, 2]
      rf_pred = prediction(labels = te_dat$y, predictions = rf_scores)
      
      dtrain <- xgb.DMatrix(data = data.matrix(tr_dat[, 1:2]), label = tr_dat$y)
      dtest <- xgb.DMatrix(data = data.matrix(te_dat[, 1:2]), label = te_dat$y)
      xgb = xgboost(data=dtrain, objective = "binary:logistic", nround=25, verbose = FALSE)
      xgb_score = predict(xgb, dtest)
      xgb_pred = prediction(labels = te_dat$y, predictions = xgb_score)
      (rf_auc_perf = performance(rf_pred, measure="auc")@y.values[[1]])
      (xgb_auc_perf = performance(xgb_pred, measure="auc")@y.values[[1]])
      
      t = data.table(sig=sig, prop=aprop, rf_auc_perf_orig=rf_auc_perf_orig, xgb_auc_perf_orig=xgb_auc_perf_orig, 
                     rf_auc_perf=rf_auc_perf, xgb_auc_perf=xgb_auc_perf)
      ret = rbind(ret, t)
    }
  }
}


ret
res = ret %>% group_by(sig, prop) %>% summarise(rf2xgb_ratio_orig=mean(rf_auc_perf_orig)/mean(xgb_auc_perf_orig),
                                     rf2xgb_ratio = mean(rf_auc_perf)/mean(xgb_auc_perf))
View(res)
# sig	incorr_prop	rf2xgb_ratio_orig	rf2xgb_ratio
# 0.5	0.05	0.9966747	0.9954532
# 0.5	0.3	0.9977823	0.9634131
# 0.5	0.5	0.9958014	0.9833821
# 0.5	0.7	1.0007272	1.3628702
# 0.8	0.05	0.9907662	0.9852119
# 0.8	0.3	0.9954131	0.970365
# 0.8	0.5	0.9923813	1.0077596
# 0.8	0.7	0.9927	1.1392188
# 1.1	0.05	0.9680536	0.9636707
# 1.1	0.3	0.9754546	0.952755
# 1.1	0.5	0.9795604	1.0105416
# 1.1	0.7	0.9888885	1.0848903
# 1.4	0.05	0.9868222	0.975029
# 1.4	0.3	0.9755513	0.957831
# 1.4	0.5	0.9765308	0.9753933
# 1.4	0.7	0.972904	1.125729
# 1.7	0.05	0.9762131	0.9783096
# 1.7	0.3	0.9746101	0.9581474
# 1.7	0.5	0.9776708	1.0094142
# 1.7	0.7	0.9658042	1.0440445



