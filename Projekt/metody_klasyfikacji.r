#rpart

library(rpart)
drzewo1 <- rpart(Y~., data=df_treningowy,
                 method="class", minsplit=20, minbucket=3, cp=0.01,maxdepth=30)
pred_drzewo1 <- predict(drzewo1,newdata = df_test, type="class") #prawd. klasy 1 tu jest jezeli nie zrobimy type="class"
df_test$Y
tabela_rpart <- table(df_test$Y, pred_drzewo1)
sum(diag(tabela_rpart))/nrow(df_test)
# 0.78


#lasy losowe

xtest <- df_test[,-501]
ytest <- df_test[,501]
library(randomForest)
las <- randomForest(factor(Y)~V29 + V49 +V65 + V106 + V129 + V154 +V242 + V282 +V319 +V337 +V339 +V379 +V434
                    +V443 +V452+V454 +V473+V476+V494,data=df_treningowy, ntree=500)
pred_rF <- predict(las,newdata = df_test,type="response")
tabela_fF <- table(df_test$Y, pred_rF)
sum(diag(tabela_fF))/nrow(df_test)
#0.7075

importance(las)
?randomForest

#wzmocnione drzewa decyzyjne xgboost


library(useful)
formula <- Y ~.
daneX <- build.x(formula, data=df_treningowy,contrasts = FALSE)
daneY <- build.y(formula, data=df_treningowy)
library(xgboost)
boost <- xgboost(data=data.matrix(df_treningowy[,-501]), label=df_treningowy[,501], nrounds=3, maxdepth=4,num_parallel_tree=1000,subsample=0.5,colsample_bytree=0.5,objective="binary:logistic")
prediction <- predict(boost, newdata =data.matrix(df_test[,-501]),type="class")
prediction <-  round(prediction, digits = 0)
tabela_fF <- table(df_test$Y, prediction)
sum(diag(tabela_fF))/nrow(df_test)

xgb.importance(boost,feature_names = colnames(daneX))


cv.res <- xgb.cv(data = data.matrix(df_treningowy[,-501]), nfold = 3, label =df_treningowy[,501], nrounds = 100, verbose = FALSE,
                 objective = 'binary:logistic', eval_metric = 'auc', prediction = T)
it = which.max(cv.res$evaluation_log$test_auc_mean)
best.iter = cv.res$evaluation_log$iter[it]

prawd.post <- cv.res$pred # prawdopodobienstwa P(1|X)
library(ROCR)
pred <- prediction(prawd.post, df_treningowy$Y)
# AUC
auc.perf <-  performance(pred, measure = "auc")
auc.perf@y.values

#wzmocnione drzewa decyzyjne randomForest

rF <- randomForest(daneX,daneY)
prediction <- predict(rF,newdata=df_test)
prediction <-  round(prediction, digits = 0)
tabela_fF <- table(df_test$Y, prediction)
sum(diag(tabela_fF))/nrow(df_test)

prawd.post <- rF$predicted # prawdopodobienstwa P(1|X)
library(ROCR)
pred <- prediction(prawd.post, df_treningowy$Y)
perf <- performance(pred, "tpr", "fpr")
# AUC
auc.perf <-  performance(pred, measure = "auc")
auc.perf@y.values



#glm (tutaj spróbowaæ jak¹œ selekcje)

mod <- glm(Y~., data=df_treningowy, family=binomial)
prawd.post <- mod$fitted.values # prawdopodobienstwa P(1|X)

library(ROCR)
pred <- prediction(prawd.post, df_treningowy$Y)
# AUC
auc.perf <-  performance(pred, measure = "auc")
auc.perf@y.values


# drzewo oparte o "information"
# (inne kryterium podzia3u)

tree.info <- rpart(as.factor(Y)~., data=df_treningowy,
                   cp=0.01, minsplit=5,
                   parms=list(split="information"))
prawd.post <- predict(tree.info)[,2] # prawdopodobienstwa P(1|X)

library(ROCR)
pred <- prediction(prawd.post, df_treningowy$Y)
# AUC
auc.perf <-  performance(pred, measure = "auc")
auc.perf@y.values


#te¿ rpart

tree <- rpart(as.factor(Y)~., data=df_treningowy,
              cp=0.01, minsplit=5,method="class")
par(xpd=TRUE)
plot(tree)
text(tree, use.n=TRUE)

prawd.post <- predict(tree)[,2]
pred <- prediction(prawd.post, df_treningowy$Y)
auc.perf <-  performance(pred, measure = "auc")
auc.perf@y.values


pred.bag <- predict(tree, df_test, type="class")
tabela_fF <- table(df_test$Y, pred.bag)
sum(diag(tabela_fF))/nrow(df_test)
#wybrano mniej zmiennych

?rpart
#bagging

library(adabag)
df$Y <- as.factor(df$Y)

bag <- bagging(Y~., data=df_treningowy)

prawd.post <- bag$prob[,2]
pred <- prediction(prawd.post, df_treningowy$Y)
auc.perf <-  performance(pred, measure = "auc")
auc.perf@y.values

pred.bag <- predict(bag, df_test)
1 - pred.bag$error
tabela_fF <- table(df_test$Y, pred.bag)
sum(diag(tabela_fF))/nrow(df_test)
#boosting

ada <- boosting(Y~., data=df_treningowy)
prawd.post <- ada$prob[,2]
pred <- prediction(prawd.post, df_treningowy$Y)
auc.perf <-  performance(pred, measure = "auc")
auc.perf@y.values

pred.ada <- predict(ada, df_test)
1 - pred.ada$error


###################### MARS
install.packages("earth")
library(earth)
model <-  earth(Y~.,data=df_treningowy,degree=2,penalty=0,minspan=0)
pred_mod <- predict(model,newdata = df_test)
prawd.post <- model$fitted.values
library(ROCR)
pred <- prediction(prawd.post, df_treningowy$Y)
# AUC
auc.perf <-  performance(pred, measure = "auc")
auc.perf@y.values

pred.bag <- predict(model, df_test, type="class")
tabela_fF <- table(df_test$Y, pred.bag)
sum(diag(tabela_fF))/nrow(df_test)


#gam
install.packages("gam")
library(gam)
gam.lr <- gam(Y ~ . , data=df_treningowy, family=binomial)
summary(gam.lr)
predict(gam.lr,newdata = df_test,type="class")
