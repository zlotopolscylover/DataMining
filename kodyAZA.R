train <- read.csv("train_projekt2.csv")
test <- read.csv("test_projekt2.csv")
df <- train[sample(1:nrow(train)),]
df_test <- df[1:400,]
df_treningowy <- df[401:nrow(df),]

#pierwsza metoda selekcji
library(Boruta)
boruta.fs <- Boruta(Y~., data = df_treningowy, doTrace=2)
x <- boruta.fs$finalDecision
x[which(x=="Confirmed")]

#druga metoda selekcji
library(rmcfs)
mcfs.fs <- mcfs(Y~., data = df_treningowy, cutoffPermutations = 3)
#pakiet randomForest
#predyktory wybrane metod? selekcji z pakietu Boruta
library(randomForest)
las <- randomForest(factor(Y)~V29 + V49 +V65 + V106 + V129 + V154 +V242 + V282 +V319 +V337 +V339 +V379 +V434
                    +V443 +V452+V454 +V473+V476+V494,data=df_treningowy, ntree=500)
#policzy?am miar? jako?ci klasyfikacji AUC dla tego modelu
library(ROCR)
prawd.post <- predict(las,newdata=df_test,type="prob")[,2]
pred <- prediction(prawd.post, df_test$Y)
auc.perf <-  performance(pred, measure = "auc")
auc.perf@y.values
#0.9526018
pred_rF <- predict(las,newdata = df_test,type="response")
#tablica kontyngencji
tabela_fF <- table(df_test$Y, pred_rF)
sum(diag(tabela_fF))/nrow(df_test)
#0.8825

#krzywa roc dla randomForest
thresh <- seq(0,1,0.0001)
plot.roc(x=df_test$Y=="1",pred=prawd.post,thresholds = thresh, main="ROC")$roc.vol

#pakiet rpart
tree <- rpart(as.factor(Y)~V29 + V49 +V65 + V106 + V129 + V154 +V242 + V282 +V319 +V337 +V339 +V379 +V434
              +V443 +V452+V454 +V473+V476+V494, data=df_treningowy,cp=0.01, minsplit=3,method="class")
par(xpd=TRUE)
plot(tree)
text(tree, use.n=TRUE)
#policzy?am miar? jako?ci klasyfikacji AUC dla tego modelu
library(ROCR)
prawd.post <- predict(tree,newdata=df_test)[,2]
pred <- prediction(prawd.post, df_test$Y)
auc.perf <-  performance(pred, measure = "auc")
auc.perf@y.values
#0.8681828
#predykcja na zbiorze testowym 
pred.bag <- predict(tree, df_test, type="class")
#tablica kontyngencji
tabela_fF <- table(df_test$Y, pred.bag)
#poprawno?c klasyfikacji na zbiorze testowym
sum(diag(tabela_fF))/nrow(df_test)
#0.83

#pakiet adabag - metoda bagging
#predyktory wybrane metod? selekcji z pakietu Boruta
df$Y <- as.factor(df$Y)
library(adabag)
bag <- bagging(Y~V29 + V49 +V65 + V106 + V129 + V154 +V242 + V282 +V319 +V337 +V339 +V379 +V434
               +V443 +V452+V454 +V473+V476+V494, data=df_treningowy)
#policzy?am miar? jako?ci klasyfikacji AUC dla tego modelu
pred.bag <- predict(bag, df_test)
prawd.post <- pred.bag$prob[,2]
pred <- prediction(prawd.post, df_test$Y)
auc.perf <-  performance(pred, measure = "auc")
auc.perf@y.values
# 0.889564
#poprawno?c klasyfikacji na zbiorze testowym
1 - pred.bag$error
#0.805

#pakiet adabag - metoda boosting
#predyktory wybrane metod? selekcji z pakietu Boruta
ada <- boosting(Y~V29 + V49 +V65 + V106 + V129 + V154 +V242 + V282 +V319 +V337 +V339 +V379 +V434
                +V443 +V452+V454 +V473+V476+V494, data=df_treningowy)
#policzy?am miar? jako?ci klasyfikacji AUC dla tego modelu
pred.bag <- predict(ada, df_test)
prawd.post <- pred.bag$prob[,2]
pred <- prediction(prawd.post, df_test$Y)
auc.perf <-  performance(pred, measure = "auc")
auc.perf@y.values
# 0.9450945
#poprawno?c klasyfikacji na zbiorze testowym
1 - pred.bag$error
#0.8675

#pakiet earth - metoda MARS 
install.packages("earth")
library(earth)
model <-  earth(Y~V29 + V49 +V65 + V106 + V129 + V154 +V242 + V282 +V319 +V337 +V339 +V379 +V434
                +V443 +V452+V454 +V473+V476+V494,data=df_treningowy,degree=2,penalty=0,minspan=0)
pred_mod <- predict(model,newdata = df_test)
# AUC
pred <- prediction(pred_mod, df_test$Y)
auc.perf <-  performance(pred, measure = "auc")
auc.perf@y.values
#0.8349335
#poprawno?c klasyfikacji na zbiorze testowym
pred.bag <- predict(model, df_test, type="class")
tabela_fF <- table(df_test$Y, pred.bag)
sum(diag(tabela_fF))/nrow(df_test)
# 0.765

#pakiet xgboost
library(xgboost)
boost <- xgboost(data=data.matrix(df_treningowy[,-501]), label=df_treningowy[,501], nrounds=3, maxdepth=4,num_parallel_tree=1000,subsample=0.5,colsample_bytree=0.5,objective="binary:logistic")
prediction <- predict(boost, newdata =data.matrix(df_test[,-501]),type="class")
#wa¿noœæ 20 pierwszych zmiennych porówna³am ze zmiennymi uzyskanymi za pomocê wczêsniejszych selekcji 
head(xgb.importance(boost,feature_names = colnames(df_treningowy[,-501])),20)
pred <- prediction(prediction, df_test$Y)
# AUC
auc.perf <-  performance(pred, measure = "auc")
auc.perf@y.values
