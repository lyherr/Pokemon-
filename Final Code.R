# Part 1 Defining Type
## 0.data preprocessing
pokemon <- read.csv("Pokemon.csv", header = TRUE,stringsAsFactors = TRUE, na.strings = "")
attach(pokemon)
Type.2 <- as.character(Type.2)
Type.2[is.na(Type.2)] <- "NoType2"
Type.2 <- as.factor(Type.2)
##Convert NA as a new lelve of Type.2 and encoded it as NoType2
pokemon$Type.2 <- Type.2
set.seed(1)
train = sample(1:nrow(pokemon),0.75*nrow(pokemon))
train.data = pokemon[train,]
test.data = pokemon[-train,]

#Make sure all the Types are included in both training and testing subset
summary(train.data$Type.1)
summary(test.data$Type.1)
summary(train.data$Type.2)
summary(test.data$Type.2)
```

## 1.Tree-based model
### 1.1 simple decision tree for type 1 and type 2
#Type 1
library(tree)
tree.pokemon1 = tree(Type.1 ~ Total+HP+Attack+Defense+Sp..Atk+Sp..Def+Speed+Generation+Legendary, train.data)
tree.pred1 = predict(tree.pokemon1, test.data, type = "class")
plot(tree.pokemon1, main = "Type.1")
text(tree.pokemon1)
error.rate <- mean(tree.pred1 != test.data$Type.1)
error.rate

#Type 2
tree.pokemon2 = tree(Type.2 ~ Total+HP+Attack+Defense+Sp..Atk+Sp..Def+Speed+Generation+Legendary, train.data)
tree.pred2 = predict(tree.pokemon1, test.data, type = "class")
plot(tree.pokemon1, main = "Type.2")
text(tree.pokemon1)
error.rate2 <- mean(tree.pred1 != test.data$Type.2)
error.rate2


### 1.2 Tree pruning for type 2

set.seed(88)
cv.pokemon = cv.tree(tree.pokemon2, FUN = prune.misclass)
cv.pokemon

par(mfrow=c(1,2))
plot(cv.pokemon$size, cv.pokemon$dev, type = 'b')

#The best tree size from the results is 1, but we cannot get a tree with size 1
#Thus we choose the second least dev with size 6
prune.pokemon = prune.misclass(tree.pokemon2, best=6)
par(mfrow=c(1,1))
plot(prune.pokemon)
text(prune.pokemon, pretty =0)

prune=predict(prune.pokemon,test.data,type="class")
table(prune,test.data$Type.2)
mean(prune != test.data$Type.2)


### 1.3 Bagging and random forest performance comparison

##type 1
tree.err <- array()
bagging.err <- array()
rf.err <- array()
for (i in 1:50){
  set.seed(i)
  train = sample(1:nrow(pokemon),0.75*nrow(pokemon))
  train.data = pokemon[train,]
  test.data = pokemon[-train,]
  
  ##Tree
  tree.pokemon1 = tree(Type.1 ~ Total+HP+Attack+Defense+Sp..Atk+Sp..Def+Speed+Generation+Legendary, train.data)
  tree.pred1 = predict(tree.pokemon1, test.data, type = "class")
  tree.err[i] <- mean(tree.pred1 != test.data$Type.1)
  
  ##bagging
  library(randomForest)
  bagging.type.1 <- randomForest(formula = Type.1 ~ Total+HP+Attack+Defense+Sp..Atk+Sp..Def+Speed+Generation+Legendary, data=train.data, 
                                 mtry=9,
                                 importance = TRUE,
                                 ntree = 500
  )
  bagging.pred <- predict(bagging.type.1, newdata = test.data)
  bagging.err[i] <- mean(bagging.pred != test.data$Type.1)
  
  ##randomForest
  rf.type.1 <- randomForest(formula = Type.1 ~ Total+HP+Attack+Defense+Sp..Atk+Sp..Def+Speed+Generation+Legendary, data=train.data, 
                            mtry=3,
                            importance = TRUE,
                            ntree = 500
  )
  rf.pred <- predict(rf.type.1, newdata = test.data)
  rf.err[i] <- mean(rf.pred != test.data$Type.1)
}

boxplot(tree.err, bagging.err, rf.err, names = c("tree", "bagging", "randomForest"), main = "Model Performance", ylab = "error rate")

#type 2
tree.err2 <- array()
bagging.err2 <- array()
rf.err2 <- array()

for (i in 1:50){
  set.seed(i)
  train = sample(1:nrow(pokemon),0.75*nrow(pokemon))
  train.data = pokemon[train,]
  test.data = pokemon[-train,]
  
  ##Tree
  tree.pokemon2 = tree(Type.2 ~ Total+HP+Attack+Defense+Sp..Atk+Sp..Def+Speed+Generation+Legendary, train.data)
  tree.pred2 = predict(tree.pokemon2, test.data, type = "class")
  tree.err2[i] <- mean(tree.pred2 != test.data$Type.2)
  
  ##bagging
  library(randomForest)
  bagging.type.2 <- randomForest(formula = Type.2 ~ Total+HP+Attack+Defense+Sp..Atk+Sp..Def+Speed+Generation+Legendary, data=train.data, 
                                 mtry=9,
                                 importance = TRUE,
                                 ntree = 500
  )
  bagging.pred2 <- predict(bagging.type.2, newdata = test.data)
  bagging.err2[i] <- mean(bagging.pred2 != test.data$Type.2)
  
  ##randomForest
  rf.type.2 <- randomForest(formula = Type.2 ~ Total+HP+Attack+Defense+Sp..Atk+Sp..Def+Speed+Generation+Legendary, data=train.data, 
                            mtry=3,
                            importance = TRUE,
                            ntree = 500
  )
  rf.pred2 <- predict(rf.type.2, newdata = test.data)
  rf.err2[i] <- mean(rf.pred2 != test.data$Type.2)
}

boxplot(tree.err2, bagging.err2, rf.err2, names = c("tree", "bagging", "randomForest"), main = "Model Performance", ylab = "error rate")


### 1.4 importance plot of random forest

#Type 1
#random forest is the most accurate one
#using the rf model with smallest error rate to get the importance of each feature
which.min(rf.err)
set.seed(7)
train = sample(1:nrow(pokemon),0.75*nrow(pokemon))
train.data = pokemon[train,]
test.data = pokemon[-train,]
rf.type.1 <- randomForest(formula = Type.1 ~ Total+HP+Attack+Defense+Sp..Atk+Sp..Def+Speed+Generation+Legendary, data=train.data, 
                          mtry=3,
                          importance = TRUE,
                          ntree = 500
)
rf.pred <- predict(rf.type.1, newdata = test.data)
mean(rf.pred != test.data$Type.1)
varImpPlot(rf.type.1)

#Type 2
#random forest is the most accurate one for Type 2 as well
#using the rf model with smallest error rate to get the importance of each feature
which.min(rf.err2)
set.seed(6)
train = sample(1:nrow(pokemon),0.75*nrow(pokemon))
train.data = pokemon[train,]
test.data = pokemon[-train,]

rf.type.2 <- randomForest(formula = Type.2 ~ Total+HP+Attack+Defense+Sp..Atk+Sp..Def+Speed+Generation+Legendary, data=train.data, 
                          mtry=3,
                          importance = TRUE,
                          ntree = 500
)
rf.pred2 <- predict(rf.type.2, newdata = test.data)
mean(rf.pred2 != test.data$Type.2)
varImpPlot(rf.type.2)


### 1.5 lasso multinomial logistic regression
#Type 1
grid=10^seq(10,-2,length=100)
x.train=model.matrix(~Total+HP+Attack+Defense+Sp..Atk+Sp..Def+Speed+Generation+Legendary-1,train.data)
y.train1=train.data$Type.1
x.test=model.matrix(~Total+HP+Attack+Defense+Sp..Atk+Sp..Def+Speed+Generation+Legendary-1,test.data)
library(glmnet)
cv.lasso = cv.glmnet(x.train,y.train1,alpha=1,family = c("multinomial"),lambda=grid, nfolds =10)
cv.lasso
best.lasso = cv.lasso$lambda.min
best.lasso
glm.fit = glmnet(x.train,y.train1,family = c("multinomial"), alpha=1,lambda=best.lasso)
coef(glm.fit)
ls.fit = predict(glm.fit, s = best.lasso, newx = x.test, type='class' )
ls.fit
#Type 2
y.train2=train.data$Type.2
cv.lasso2 = cv.glmnet(x.train,y.train2,alpha=1,family = c("multinomial"),lambda=grid, nfolds =10)
cv.lasso2
best.lasso2 = cv.lasso2$lambda.min
best.lasso2
glm.fit2 = glmnet(x.train,y.train,family = c("multinomial"), alpha=1,lambda=best.lasso2)
coef(glm.fit)
ls.fit = predict(glm.fit2, s = best.lasso2, newx = x.test, type='class' )
ls.fit

### 1.6 lasso and random forest performance comparison

lasso.err1 <- array()
lasso.err2 <- array()

for (i in 1:50){
  set.seed(i)
  train = sample(1:nrow(pokemon),0.75*nrow(pokemon))
  train.data = pokemon[train,]
  test.data = pokemon[-train,]
  x.train=model.matrix(~Total+HP+Attack+Defense+Sp..Atk+Sp..Def+Speed+Generation+Legendary-1,train.data)
  y.train1=train.data$Type.1
  y.train2=train.data$Type.2
  x.test=model.matrix(~Total+HP+Attack+Defense+Sp..Atk+Sp..Def+Speed+Generation+Legendary-1,test.data)
  
  cv.lasso1 = cv.glmnet(x.train,y.train1,alpha=1,family = c("multinomial"),lambda=grid, nfolds =5)
  cv.lasso1
  best.lasso1 = cv.lasso1$lambda.min
  glm.fit1 = glmnet(x.train,y.train1,family = c("multinomial"), alpha=1,lambda=best.lasso1)
  ls.fit1 = predict(glm.fit1, s = best.lasso1, newx = x.test, type='class' )
  lasso.err1[i] <- mean(ls.fit1 != test.data$Type.1)
  
  cv.lasso2 = cv.glmnet(x.train,y.train2,alpha=1,family = c("multinomial"),lambda=grid, nfolds =5)
  cv.lasso2
  best.lasso2 = cv.lasso2$lambda.min
  glm.fit2 = glmnet(x.train,y.train2,family = c("multinomial"), alpha=1,lambda=best.lasso2)
  ls.fit2 = predict(glm.fit2, s = best.lasso2, newx = x.test, type='class' )
  lasso.err2[i] <- mean(ls.fit2 != test.data$Type.2)
}

par(mfrow=c(1,2))
boxplot(lasso.err1,rf.err, main = "Model Performance for Type 1", ylab = "Test Error rate", names = c("Lasso","Random Forest"))
boxplot(lasso.err2,rf.err2, main = "Model Performance for Type 2", ylab = "Test Error rate", names = c("Lasso","Random Forest"))
```
# part 2 legendary prediction
### 0. package preparation

packages.used=c("dummies","e1071","ROCR","randomForest","adabag","DMwR","knitr","splitstackshape",'caret')
# check the packages needed
packages.needed=setdiff(packages.used, 
                        intersect(installed.packages()[,1], 
                                  packages.used))

if(length(packages.needed)>0){
  install.packages(packages.needed, dependencies = TRUE)
}

library(dummies)
library(ROCR) 
library(e1071)
library(randomForest)
library(adabag)
library(DMwR)
library(knitr)
library(splitstackshape)
library(caret)

set.seed(1)

data<-data.frame(read.csv("Pokemon.csv",header=T,as.is=T))
# use as.is=T to read the data to avoid forcing character type data to become factor type
head(data)
str(data)
```
### 1. data preprocessing

data1<-data

## for column one 
## change the colname to be 'id'
colnames(data1)[1]<-'id'

## delete column one and two
data1 <- data1[,seq(3,ncol(data1))]

## for column four, fill the missing value with vacant
data1['Type.2'][data1['Type.2'] == ''] <- 'vacant'

## for column three & four, check their distinct level which is 18,19, so we make them one-hot encoding
length(unique(data1$Type.1)) 
length(unique(data1$Type.2)) 
length(unique(data1$Generation)) 
data1 <- dummy.data.frame(data1, names = 'Type.1',sep="_")
data1 <- dummy.data.frame(data1, names = 'Type.2',sep="_")


## for last column, make it to be two level factor
data1$Legendary<- ifelse(data1$Legendary == 'True',1,0)


## preview the data preprocessed
head(data1)
str(data1)

### 2. feature engineering

## make sure the data preprocessed has no NA
sum(is.na(data1))

## check the correlation between the dependent variable and independent variables.
legendary<- ifelse(data$Legendary == 'True',1,0)
corr<-NULL
for(i in 1:(ncol(data1)-1))
{
  corr[i]<-cor(data1[,i],legendary)
}
mat<-as.data.frame(cbind(colnames(data1)[1:(ncol(data1)-1)],corr))
mat$corr<-as.numeric(as.character(mat$corr))
colnames(mat)<-c('variable',"absolute_correlation")
selected = as.numeric(rownames(mat[order(abs(mat[,2]),decreasing=T),][0][1:20,]))
data1 = data1[,c(selected,46)]




## standardarize

data1_sub<-apply(data1[, 1:(ncol(data1)-1) ],2,scale)
data2<-as.data.frame(cbind(data1_sub,data1$Legendary))
colnames(data2)[21]="Y"
head(data2)
str(data2)



## split the data into 60/20/20 train/validation/test
dim(data2)
test_index<-sample(1:800,800 * 0.2)
test<-data2[test_index,]
train<-data2[-test_index,]
folds <- createFolds(c(1:640), k = 5, list = FALSE, returnTrain = FALSE)
dim(test)
dim(train)


### 3. modeling

table(data2$Y)
# asymmetric class sizes

#### 3.1 SVM

#choose 5-fold cross validation 
#since it's a classification problem, let type = "C-classification"
#due to asymmetric class sizes, we set class.weights according to the ratio the legendary being different levels

for (gamma in c(0.01,0.1,0.15)){
error = 0
for (i in c(1:5)){
model<-svm(Y~.,data=train[folds!=i,],type = "C-classification",
cost=10,kernel="radial",gamma=gamma,scale = F)
pred<-predict(model,train[folds==i,])
error = error + sum(pred!=train$Y[folds==i])/128.
}

print(gamma)
print(error/5)
}
model<-svm(Y~.,data=train,type = "C-classification",
cost=10,scale = F)
pred<-predict(model,test)
print(paste0("error is ", sum(pred!=test$Y)/160.))


library(class)
Error = c()
for (k in c(1:100)){
error = 0
for (i in c(1:5)){
model<-knn(train[folds!=i,],train[folds==i,],
train$Y[folds!=i],k=k)
error = error + mean(model!=train$Y[folds==i])
}

Error = cbind(Error,error/5)
}
k=which(Error==min(Error))
print(k)
model <-knn(train,test,train$Y,k=1)
print(paste0("error is ", mean(model!=test$Y)))


#### 3.2 RF

set.seed(1)
for (ntree in c(50,100,200,300)){
error = 0
for (i in c(1:5)){
model<-randomForest(as.factor(Y)~.,data=train[folds!=i,],ntree=100)
pred<-predict(model,train[folds==i,])
error = error + sum(pred!=train$Y[folds==i])/128.
}

print(ntree)
print(error/5)
}
model<-randomForest(as.factor(Y)~.,data=train,ntree=ntree)
pred<-predict(model,test)
print(paste0("error is ", sum(pred!=test$Y)/160.))
```
#### 3.3 boosting


set.seed(1)
train$Y = as.factor(train$Y)
for (mfinal in c(100,200,300)){
error = 0
for (i in c(1:5)){
model<-boosting(Y~.,data=train[folds!=i,],mfinal=mfinal)
pred<-predict(model,train[folds==i,])
error = error + sum(pred!=train$Y[folds==i])/128.
}

print(mfinal)
print(error/5)
}
model<-boosting(Y~.,data=train,mfinal=100)
pred<-predict(model,test)
print(paste0("error is ", sum(pred$class!=test$Y)/160.))

###LDA

library(MASS)
{
  error = 0
  for (i in c(1:5)){
  fit.lda=lda(Y~HP+Attack+Defense+Sp..Atk+Sp..Def+Speed,data=train[folds!=i,])
  pred.lda=predict(fit.lda,train[folds==i,])
  error = error + sum(pred.lda$class!=train$Y[folds==i])/128.
  }
  
  print(error/5)
}
fit.lda=lda(Y~HP+Attack+Defense+Sp..Atk+Sp..Def+Speed,data=train)
pred.lda=predict(fit.lda,test)
testerror_LDA=sum(pred.lda$class!=test$Y)/160.
print(paste0("error is ", sum(pred.lda$class!=test$Y)/160.))
```

###QDA

library(corpcor)
cor2pcor(cov(data2))



library(MASS)
{
  error = 0
  for (i in c(1:5)){
  fit.qda=qda(Y~HP+Attack+Defense+Sp..Atk+Sp..Def+Speed,data=train[folds!=i,])
  pred.qda=predict(fit.qda,train[folds==i,])
  error = error + sum(pred.qda$class!=train$Y[folds==i])/128.
  }
  
  print(error/5)
}
fit.qda=qda(Y~HP+Attack+Defense+Sp..Atk+Sp..Def+Speed,data=train)
pred.qda=predict(fit.qda,test)
testerror_QDA=sum(pred.qda$class!=test$Y)/160.
print(paste0("error is ", sum(pred.qda$class!=test$Y)/160.))


###Logistic

{ error = 0
for (i in c(1:5)){
fit.logit=glm(Y~.,data=train[folds!=i,],family="binomial")
glm.probs=predict(fit.logit,train[folds==i,],type="response")
glm.pred=rep(0,length(glm.probs))
glm.pred[glm.probs>0.5]=1
error = error + sum(glm.pred!=train$Y[folds==i])/128.
}

print(error/5)
}

fit.logit=glm(Y~.,data=train,family="binomial")
summary(fit.logit)
glm.probs=predict(fit.logit,test,type="response")
glm.pred=rep("0",length(glm.probs))
glm.pred[glm.probs>0.5]="1"

testerror_logit=sum(glm.pred!=test$Y)/160.
print(paste0("error is ", sum(glm.pred!=test$Y)/160.))

###KNN

library(class)
Error = c()
for (k in c(1:100)){
error = 0
for (i in c(1:5)){
model<-knn(train[folds!=i,],train[folds==i,],
train$Y[folds!=i],k=k)
error = error + mean(model!=train$Y[folds==i])
}

Error = cbind(Error,error/5)
}
k=which(Error==min(Error))
print(k)
model <-knn(train,test,train$Y,k=1)
testerror_KNN=mean(model!=test$Y)
print(paste0("error is ", mean(model!=test$Y)))

###Plot for Comparison
library(graphics)
Test_Error=c(testerror_logit,testerror_LDA,testerror_QDA,testerror_KNN,0.05625,0.05,0.04375)
Test_Error
barplot(Test_Error,main="Test Error Comparison",xlab="Models",ylab="Test Error",
names.arg=c("Logistic","LDA","QDA","KNN","SVM","RF","Boosting"))
