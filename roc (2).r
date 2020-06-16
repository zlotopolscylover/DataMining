thresh<-seq(0,1,0.001)
roc.plot(x=test$Y_test=="1",pred=cbind(lda.predicted,log.predicted ,rpart.predicted[,2], rf.predicted,bag,gbm.model.prediction),legend = T,
         leg.text = c("LDA", "Logistic","DecisionTree","RandomForest", "bagging","GBM"),thresholds = thresh)$roc.vol