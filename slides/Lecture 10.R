library(randomForest)
library(MASS)
library(tree)
#Bagging and Random Forest

set.seed(1)
train=sample(1:nrow(Boston),nrow(Boston)/2)

#plot(medv.tree)


#cv.medv=cv.tree(medv.tree)
#cv.medv
#names(cv.medv)


set.seed(1)
bag.boston=randomForest(medv~.,data=Boston,subset=train,mtry=13,importance=TRUE)
bag.boston
par(mfrow=c(1,2))
yhat.bag=predict(bag.boston,newdata=Boston[-train,])
boston.test=Boston[-train,"medv"]
plot(yhat.bag,boston.test)
abline(0,1)
mean((yhat.bag-boston.test)^2)

medv.tree=tree(medv~.,Boston,subset=train)
yhat2=predict(medv.tree,newdata=Boston[-train,])
plot(yhat2,boston.test)
abline(0,1)
mean((yhat2-boston.test)^2)

par(mfrow=c(1,1))

set.seed(1)
rf.boston=randomForest(medv~.,data=Boston,subset=train,mtry=4,importance=TRUE)
yhat.rf=predict(rf.boston,newdata=Boston[-train,])
plot(yhat.rf,boston.test)
abline(0,1)


mean((yhat.rf-boston.test)^2)
importance(rf.boston)
varImpPlot(rf.boston)


#Boosting

library(gbm)
set.seed(1)
boost.boston=gbm(medv~.,data=Boston[train,],distribution="gaussian",n.trees=50000,interaction.depth=2)
#if we instead used distribution "bernoulli" we would be dealing with classification data
summary(boost.boston)

par(mfrow=c(1,2))
plot(boost.boston,i="rm")
plot(boost.boston,i="lstat")

yhat.boost=predict(boost.boston,newdata=Boston[-train,],n.trees=50000)
mean((yhat.boost-boston.test)^2)
#running this again with a different lambda
boost.boston=gbm(medv~.,data=Boston[train,],distribution="gaussian",n.trees=50000,interaction.depth=2,shrinkage=.2,verbose=F)
yhat.boost=predict(boost.boston,newdata=Boston[-train,],n.trees=50000)
mean((yhat.boost-boston.test)^2)


#Bumping

error.vec=sequence(500)
boot.sub=matrix(NA,500,length(train))
for (i in 1:500)
{
  boot.sub[i,]=sample(length(train),replace=TRUE)
}
for (i in 1:500)
{
  medv.tree=tree(medv~.,Boston,subset=boot.sub[i,])
  yhat.bag=predict(medv.tree,newdata=Boston[-train,])
  error.vec[i]=mean((yhat.bag-boston.test)^2)
}

which(error.vec==min(error.vec))

medv.tree=tree(medv~.,Boston,subset=boot.sub[253,])
yhat.bag=predict(medv.tree,newdata=Boston[-train,])
mean((yhat.bag-boston.test)^2)


x1=c(runif(100,-2,0),runif(100,-2,0),runif(100,0,2),runif(100,0,2))
x2=c(runif(100,-2,0),runif(200,0,2),runif(100,-2,0))
y=as.factor(sign(x1*x2))
checker=data.frame(y,x1,x2)
randomize=sample(400,replace=FALSE)
checker.rand=checker[randomize,]
checker.rand$y
y1=sign(x1*x2)+2
par(mfrow=c(1,1))
plot(x1,x2,col=y1)

check.tree=tree(y~.,data=checker.rand)
plot(check.tree)
text(check.tree)

check.pred1=predict(check.tree,newdata=checker.rand)
check.pred1=ifelse(check.pred1[,1]==1,1,-1)
table(check.pred1,checker.rand$y)

check.rf=randomForest(y~.,data=checker.rand,mtry=2,importance=TRUE)
check.pred2=predict(check.rf,newdata=checker.rand)

table(check.pred2,checker.rand$y)

y2=ifelse(checker.rand$y==1,1,0)
y2fact=as.factor(y2)
checker.rand2=data.frame(checker.rand,y2fact)
check.boost=gbm(y2~x1+x2,data=checker.rand2,distribution="bernoulli",n.trees=5000)
check.pred3=predict(check.boost,newdata=checker.rand,n.trees=5000,type = "link")
table(check.pred3,checker.rand$y)
