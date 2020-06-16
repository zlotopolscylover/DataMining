library(randomForest)
las1 <- randomForest(factor(Y)~V29 + V49 +V65 + V106 + V129 +V242 + V282 +V319 +V337 +V339 +V379 +V434+V128+V343
                     +V443 +V473+V476+V494,data=train, ntree=500,method="class")
pred_rF <- predict(las1,newdata = test,type="prob")
pred_rF[,2]#prawd klasy 1
