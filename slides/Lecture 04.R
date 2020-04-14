
library(leaps) #must install packages leaps first
url="http://www-bcf.usc.edu/~gareth/ISL/Credit.csv"
credit=read.csv(url,row.names=1)


a=regsubsets(Balance~., data=credit,method="forward",nvmax=4)
t(summary(a)$which)

a=regsubsets(Balance~.,data=credit,nvmax=4)
t(summary(a)$which)

a=regsubsets(Balance~., data=credit,method="forward",nvmax=11)
t(summary(a)$which)

a=regsubsets(Balance~.,data=credit,nvmax=11)
t(summary(a)$which)

a=regsubsets(Balance~., data=credit,method="backward",nvmax=11)
t(summary(a)$which)

#note that for 4 predictors, the Best SS gives a different result than FSS


a=regsubsets(Balance~.,data=credit,nvmax=11)
cp=summary(a)$cp
bic=summary(a)$bic
j=which.min(bic)
adjr2=summary(a)$adjr2
k=which.max(adjr2)
i=which.min(cp)
plot(cp,type='b',col="blue",xlab="Number of Predictors",ylab=expression("Mallows C"[P]))
points(i,cp[i],pch=19,col="red")

coef(a,6)

x1=rnorm(500,0,2)
x2=rnorm(500,.5,.1)
x3=rnorm(500,.1,.001)
x4=rnorm(500,-1,1)
y=3*x1+.2*x3+rnorm(500,0,1)

examp=data.frame(y,x1,x2,x3,x4)

ex.sub=regsubsets(y~.,data=examp,nvmax = 4)
t(summary(ex.sub)$which)

r2=summary(ex.sub)$rsq

which.max(r2)
cp=summary(ex.sub)$cp
which.min(cp)
bic=summary(ex.sub)$bic
which.min(bic)
adj=summary(ex.sub)$adj
which.max(adj)


library(MASS)
lambda=10^seq(-2,4,by=.01)
a=lm.ridge(Balance~.,data=credit,lambda=lambda)
leg.col=rainbow(dim(a$coef)[1])
matplot(lambda,t(a$coef),
        type='l',
        lty=1,
        lwd=2,
        log="x",
        col=leg.col,
        xlab="lambda",
        ylab="Normalized Coefficients")
to.label=c(1,2,3,8)
legend("topright",rownames(a$coef)[to.label],fill=leg.col[to.label],box.lty="blank")
box()

#Lab on Best subset selection

library(ISLR)
fix(Hitters)
names(Hitters)
dim(Hitters)
sum(is.na(Hitters$Salary))

Hitters=na.omit(Hitters)
dim(Hitters)
sum(is.na(Hitters))
fix(Hitters)

regfit.full=regsubsets(Salary~.,Hitters)
summary(regfit.full)
names(regfit.full)

regfit.full=regsubsets(Salary~.,data=Hitters,nvmax=19)
reg.summary=summary(regfit.full)
names(reg.summary)
reg.summary
reg.summary$rsq
plot(reg.summary$rsq,type='b',col="blue",pch=19)
points(which.max(reg.summary$rsq),reg.summary$rsq[which.max(reg.summary$rsq)],col="red",pch=19)

plot(reg.summary$adjr2,type='b',col="blue",pch=19)
points(which.max(reg.summary$adjr2),reg.summary$adjr2[which.max(reg.summary$adjr2)],col="red",pch=19)

plot(reg.summary$cp,type='b',col="blue",pch=19)
points(which.min(reg.summary$cp),reg.summary$cp[which.min(reg.summary$cp)],col="red",pch=19)

plot(reg.summary$bic,type='b',col="blue",pch=19)
points(which.min(reg.summary$bic),reg.summary$bic[which.min(reg.summary$bic)],col="red",pch=19)

par(mfrow=c(2,2))
plot(reg.summary$rss,xlab="Number of Variables",ylab="RSS",type='l')
plot(reg.summary$adjr2,xlab="Number of Variables",ylab="Adjusted RSq",type="l")
points(which.max(reg.summary$adjr2),reg.summary$adjr2[which.max(reg.summary$adjr2)],col="red",cex=2,pch=20)

plot(reg.summary$cp,xlab="Number of Variables",ylab="Cp",type="l")
points(which.min(reg.summary$cp),reg.summary$cp[which.min(reg.summary$cp)],col="red",pch=20,cex=2)

plot(reg.summary$bic,xlab="Number of Variables",ylab="BIC",type='l')
points(which.min(reg.summary$bic),reg.summary$bic[which.min(reg.summary$bic)],col="red",pch=20,cex=2)
par(mfrow=c(1,1))

plot(regfit.full,scale="r2")
plot(regfit.full,scale="adjr2")
plot(regfit.full,scale="Cp")
plot(regfit.full,scale="bic")

coef(regfit.full,6)


#Lab for Forward and Backward Stepwise Selection

regfit.fwd=regsubsets(Salary~.,data=Hitters,nvmax=19,method="forward")
summary(regfit.fwd)
regfit.bwd=regsubsets(Salary~.,data=Hitters,nvmax=19,method="backward")
summary(regfit.bwd)

coef(regfit.full,7)
coef(regfit.fwd,7)
coef(regfit.bwd,7)



#Lab for Ridge Regression

x=model.matrix(Salary~.,Hitters)[,-1]
y=Hitters$Salary
#install package glmnet
library(glmnet)
grid=10^seq(10,-2,length=100)
ridge.mod=glmnet(x,y,alpha=0,lambda=grid)

dim(coef(ridge.mod))

ridge.mod$lambda[50]
coef(ridge.mod)[,50]
sqrt(sum(coef(ridge.mod)[-1,50]^2))

ridge.mod$lambda[60]
coef(ridge.mod)[,60]
sqrt(sum(coef(ridge.mod)[-1,60]^2))

predict(ridge.mod,s=50,type="coefficients")[1:20,]

set.seed(1)
train=sample(1:nrow(x),nrow(x)/2)
test=(-train)
y.test=y[test]

ridge.mod=glmnet(x[train,],y[train],alpha=0,lambda=grid,thresh=1e-12)
ridge.pred=predict(ridge.mod,s=4,newx=x[test,])
mean((ridge.pred-y.test)^2)

mean((mean(y[train])-y.test)^2)

ridge.pred=predict(ridge.mod,s=1e10,newx=x[test,])
mean((ridge.pred-y.test)^2)

ridge.pred=predict(ridge.mod,s=0,newx=x[test,])
mean((ridge.pred-y.test)^2)
lm(y~x,subset=train)
predict(ridge.mod,s=0,type="coefficients")[1:20,]






#Lab for Lasso

lasso.mod=glmnet(x[train,],y[train],alpha=1,lambda=grid)
plot(lasso.mod)









#Lab for Extensions

#Interaction Terms

summary(lm(medv~lstat*age,data=Boston))

#Non-linear Transformations

lm.fit2=lm(medv~lstat+I(lstat^2),data=Boston)
summary(lm.fit2)

lm.fit=lm(medv~lstat,data=Boston)
anova(lm.fit,lm.fit2)

par(mfrow=c(2,2))
plot(lm.fit2)

par(mfrow=c(1,1))
lm.fit5=lm(medv~poly(lstat,5),data=Boston)
summary(lm.fit5)

summary(lm(medv~log(rm),data=Boston))

