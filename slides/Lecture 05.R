#ROC Curve
library(ROCR)
library(ISLR)


rocplot=function(pred,truth, ...)
{
  predob=prediction(pred,truth)
  perf=performance(predob,"tpr","fpr")
  plot(perf,...)
}

train=sample(392,196)
MPG=Auto$mpg
MPG[MPG<23]=0
MPG[MPG!=0]=1
HORSEPOWER=Auto$horsepower
glm1.fit=glm(MPG~HORSEPOWER,subset=train,family=binomial)
DATA=data.frame(cbind(MPG,HORSEPOWER))
fitted1=predict(glm1.fit,DATA)[-train]
rocplot(fitted1,DATA[train,1])
glm2.fit=glm(MPG~poly(HORSEPOWER,5),subset=train,family=binomial)
fitted2=predict(glm2.fit,DATA)[-train]
rocplot(fitted2,DATA[train,1])

#Validation Set Approach 


set.seed(1)
train=sample(392,196)

lm.fit=lm(mpg~horsepower,data=Auto,subset=train)

attach(Auto)
mean((mpg-predict(lm.fit,Auto))[-train]^2)

lm.fit2=lm(mpg~poly(horsepower,2),data=Auto,subset=train)
mean((mpg-predict(lm.fit2,Auto))[-train]^2)
lm.fit3=lm(mpg~poly(horsepower,3),data=Auto,subset=train)
mean((mpg-predict(lm.fit3,Auto))[-train]^2)

set.seed(2)
train=sample(392,196)
lm.fit=lm(mpg~horsepower,subset=train)
mean((mpg-predict(lm.fit,Auto))[-train]^2)

lm.fit2=lm(mpg~poly(horsepower,2),data=Auto,subset=train)
mean((mpg-predict(lm.fit2,Auto))[-train]^2)
lm.fit3=lm(mpg~poly(horsepower,3),data=Auto,subset=train)
mean((mpg-predict(lm.fit3,Auto))[-train]^2)





#K-Fold CV
glm.fit=glm(mpg~horsepower,data=Auto)
coef(glm.fit)

lm.fit=lm(mpg~horsepower,data=Auto)
coef(lm.fit)

library(boot)
set.seed(17)
cv.error.10=rep(0,10)
for (i in 1:10)
{
  glm.fit=glm(mpg~poly(horsepower,i),data=Auto)
  cv.error.10[i]=cv.glm(Auto,glm.fit,K=10)$delta[1]
}
cv.error.10
}

#LOOCV
glm.fit=glm(mpg~horsepower,data=Auto)
cv.err=cv.glm(Auto,glm.fit)
cv.err$delta

cv.error=rep(0,5)
for (i in 1:5)
{
  glm.fit=glm(mpg~poly(horsepower,i),data=Auto)
  cv.error[i]=cv.glm(Auto,glm.fit)$delta[1]
}
cv.error

#Bootstrap

alpha.fn=function(data,index)
{
  X=data$X[index]
  Y=data$Y[index]
  return((var(Y)-cov(X,Y))/(var(X)+var(Y)-2*cov(X,Y)))
}
alpha.fn(Portfolio,1:100)
set.seed(1)
alpha.fn(Portfolio,sample(100,100,replace=T))
boot(Portfolio,alpha.fn,R=1000)

boot.fn=function(data,index)
{
  return(coef(lm(mpg~horsepower,data=data,subset=index)))
}
boot.fn(Auto,1:392)

set.seed(1)
boot.fn(Auto,sample(392,392,replace=T))
boot.fn(Auto,sample(392,392,replace=T))
boot(Auto,boot.fn,1000)
summary(lm(mpg~horsepower,data=Auto))$coef

boot.fn=function(data,index)
{
  coefficients(lm(mpg~horsepower+I(horsepower^2),data=data,subset=index))
}
set.seed(1)
boot(Auto,boot.fn,1000)

summary(lm(mpg~horsepower+I(horsepower^2),data=Auto))$coef


#Lab for Choosing using Validation and Cross Validation

Hitters=na.omit(Hitters)
library(leaps)

set.seed(1)
train=sample(c(TRUE,FALSE),nrow(Hitters),rep=TRUE)
test=(!train)

regfit.best=regsubsets(Salary~.,data=Hitters[train,],nvmax=19)
test.mat=model.matrix(Salary~.,data=Hitters[test,],nvmax=19)
?model.matrix

val.errors=rep(NA,19)
for(i in 1:19)
{
  coefi=coef(regfit.best,id=i)
  pred=test.mat[,names(coefi)]%*%coefi
  val.errors[i]=mean((Hitters$Salary[test]-pred)^2)
}

val.errors
which.min(val.errors)
coef(regfit.best,10)

predict.regsubsets=function(object,newdata,id,...)
{
  form=as.formula(object$call[[2]])
  mat=model.matrix(form,newdata)
  coefi=coef(object,id=id)
  xvars=names(coefi)
  mat[,xvars]%*%coefi
}

regfit.best=regsubsets(Salary~.,data=Hitters,nvmax=19)
coef(regfit.best,10)#Note this is different because this used all of the data

k=10
set.seed(1)
folds=sample(1:k,nrow(Hitters),replace=TRUE)
cv.errors=matrix(NA,k,19,dimnames=list(NULL,paste(1:19)))

for (j in 1:k)
{
  best.fit=regsubsets(Salary~.,data=Hitters[folds!=j,],nvmax=19)
  for(i in 1:19)
  {
    pred=predict(best.fit,Hitters[folds==j,],id=i)
    cv.errors[j,i]=mean((Hitters$Salary[folds==j]-pred)^2)
  }
}

mean.cv.errors=apply(cv.errors,2,mean)
mean.cv.errors
par(mfrow=c(1,1))
plot(mean.cv.errors,type='b')

which.min(mean.cv.errors)

reg.best=regsubsets(Salary~.,data=Hitters,nvmax=19)
coef(reg.best,11)
