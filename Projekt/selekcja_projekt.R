install.packages("rmcfs")
library(rmcfs)

train <- read.csv("train_projekt2.csv")
test <- read.csv("test_projekt2.csv")


df <- train[sample(1:nrow(train)),]
df_test <- df[1:400,]
df_treningowy <- df[401:nrow(df),]

###  metody selekcji

#Boruta 

library(Boruta)
install.packages("Boruta")
# https://www.datacamp.com/community/tutorials/feature-selection-R-boruta
# https://www.jstatsoft.org/article/view/v036i11
boruta.fs <- Boruta(Y~., data = df_treningowy, doTrace=2)
x <- boruta.fs$finalDecision
x[which(x=="Confirmed")]
#tylko te zmienne

library(rmcfs)
# https://cran.r-project.org/web/packages/rmcfs/vignettes/v85i12.pdf

mcfs.fs <- mcfs(Y~., data = df_treningowy, cutoffPermutations = 3)
mcfs.fs$RI # "wa??nooa"
plot(mcfs.fs, type="features", size=21)

graf <- build.idgraph(mcfs.fs)
plot(graf, label_dist = 1)

#lasy losowe ze zmiennymi wybranymi Borut¹

xtest <- df_test[,-501]
ytest <- df_test[,501]
library(randomForest)
las <- randomForest(factor(Y)~V29 + V49 +V65 + V106 + V129 + V154 +V242 + V282 +V319 +V337 +V339 +V379 +V434
                    +V443 +V452+V454 +V473+V476+V494,data=df_treningowy, ntree=500)
las$
pred_rF <- predict(las,newdata = df_test,type="response")
tabela_fF <- table(df_test$Y, pred_rF)
sum(diag(tabela_fF))/nrow(df_test)
#0.9

importance(las)

#lasy losowe ze zmiennymi wybranymi rmcfs

xtest <- df_test[,-501]
ytest <- df_test[,501]
library(randomForest)
las1 <- randomForest(factor(Y)~V29 + V49 +V65 + V106 + V129 +V242 + V282 +V319 +V337 +V339 +V379 +V434+V128+V343
                    +V443 +V473+V476+V494,data=df_treningowy, ntree=500,method="class")

pred_las1 <- predict(las1,newdata = df_test,type="response")
tabela_fF <- table(df_test$Y, pred_rF)
sum(diag(tabela_fF))/nrow(df_test)
#0.91
#troszkê lepiej

# rpart - selekcja zmiennych nic nie da³a, wgl parametry nic nie daj¹?

tree <- rpart(as.factor(Y)~V29 + V49 +V65 + V106 + V129 + V154 +V242 + V282 +V319 +V337 +V339 +V379 +V434
              +V443 +V452+V454 +V473+V476+V494, data=df_treningowy,cp=0.01, minsplit=3,method="class")
par(xpd=TRUE)
plot(tree)
text(tree, use.n=TRUE)

prawd.post <- predict(tree)[,2]
pred <- prediction(prawd.post, df_treningowy$Y)
auc.perf <-  performance(pred, measure = "auc")
auc.perf@y.values

pred_tree <- predict(tree, df_test, type="class")
tabela_fF <- table(df_test$Y, pred.bag)
sum(diag(tabela_fF))/nrow(df_test)


#bagging po selekcji rmcfs
df$Y <- as.factor(df$Y)
library(adabag)
bag <- bagging(Y~V29 + V49 +V65 + V106 + V129 +V242 + V282 +V319 +V337 +V339 +V379 +V434+V128+V343
               +V443 +V473+V476+V494, data=df_treningowy)
prawd.post <- bag$prob[,2]
pred <- prediction(prawd.post, df_treningowy$Y)
auc.perf <-  performance(pred, measure = "auc")
auc.perf@y.values
pred.bag <- predict(bag, df_test)$prob[,2]
1 - pred.bag$error

#boosting po selekcji rmcfs

ada <- boosting(Y~V29 + V49 +V65 + V106 + V129 +V242 + V282 +V319 +V337 +V339 +V379 +V434+V128+V343
                +V443 +V473+V476+V494, data=df_treningowy)
prawd.post <- ada$prob[,2]
pred <- prediction(prawd.post, df_treningowy$Y)
auc.perf <-  performance(pred, measure = "auc")
auc.perf@y.values
pred.bag <- predict(ada, df_test)
1 - pred.bag$error
?performance
pred.ada <- predict(ada, df_test, type="class")$prob[,2]
tabela_fF <- table(df_test$Y, pred.bag$class)
sum(diag(tabela_fF))/nrow(df_test)


#bagging po selekcji Boruta - lepszy bagging z Borut¹
df$Y <- as.factor(df$Y)
library(adabag)
bag <- bagging(Y~V29 + V49 +V65 + V106 + V129 + V154 +V242 + V282 +V319 +V337 +V339 +V379 +V434
               +V443 +V452+V454 +V473+V476+V494, data=df_treningowy)
prawd.post <- bag$prob[,2]
pred <- prediction(prawd.post, df_treningowy$Y)
auc.perf <-  performance(pred, measure = "auc")
auc.perf@y.values
pred.bag <- predict(bag, df_test)
1 - pred.bag$error

#boosting po selekcji Boruta - tez lepiej

ada <- boosting(Y~V29 + V49 +V65 + V106 + V129 + V154 +V242 + V282 +V319 +V337 +V339 +V379 +V434
                +V443 +V452+V454 +V473+V476+V494, data=df_treningowy)
prawd.post <- ada$prob[,2]
pred <- prediction(prawd.post, df_treningowy$Y)
auc.perf <-  performance(pred, measure = "auc")
auc.perf@y.values
pred.bag <- predict(ada, df_test)
1 - pred.bag$error

pred.bag <- predict(ada, df_test, type="class")
tabela_fF <- table(df_test$Y, pred.bag$class)
sum(diag(tabela_fF))/nrow(df_test)

###################### MARS -tak chyba najlepiej
install.packages("earth")
library(earth)
model <-  earth(Y~.,data=df_treningowy,degree=2,penalty=0,minspan=0,nfold=10,ncross = 10)
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


#glm
mod <- glm(as.factor(Y)~., data=df_treningowy)
prawd.post <- mod$fitted.values # prawdopodobienstwa P(1|X)

library(ROCR)
pred <- prediction(prawd.post, df$Y)
perf <- performance(pred, "tpr", "fpr")
plot(perf)

# AUC
auc.perf = performance(pred, measure = "auc")
auc.perf@y.values


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

boost$call
prawd.post <- cv.res$pred# prawdopodobienstwa P(1|X)
library(ROCR)
pred_xgb <- prediction(prediction, df_test$Y)$predictions
# AUC
auc.perf <-  performance(pred, measure = "auc")
auc.perf@y.values

??cv.glm
