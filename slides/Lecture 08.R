library(ISLR)
library(boot) #also contains the cv.glm command

alpha.fn=function(data,index)
{
  X=data$X[index]
  Y=data$Y[index]
  return((var(Y)-cov(X,Y))/(var(X)+var(Y)-2*cov(X,Y)))
}
alpha.fn(Portfolio,1:100)

alpha.fn(Portfolio,sample(100,100,replace=T))

boot(Portfolio,alpha.fn,R=1000)

?boot

x1=rnorm(5000,.5,1)
x2=rexp(5000,4)
x3=runif(5000,-2,2)
eps=rnorm(5000,0,.5)

y=.2*x1+2*x2-.5*x3+eps

examp=data.frame(y,x1,x2,x3)
head(examp)

train=sample(5000,2500,replace=FALSE)
exam.glm=glm(y~.,data=examp[train,])
summary(exam.glm)

exam.pred=predict(exam.glm,data=examp[-train,])
mean((examp$y[-train]-exam.pred)^2)

exam.glm2=glm(y~poly(x1,2)+poly(x2,4)+poly(x3,5),data=examp[train,])
exam.pred2=predict(exam.glm2,data=examp[-train,])
mean((examp$y[-train]-exam.pred2)^2)



B=100
param=exam.glm$coefficients
for (i in 1:B)
{
  
  boot.glm=glm(y~.,data=examp[sample(train,2500,replace=TRUE),])
  param=rbind(param,boot.glm$coefficients)
  
}
param=param[-1,]
mean(param[,1])
sort(param[,1])[5]
sort(param[,1])[95]
mean(param[,2])
sort(param[,2])[5]
sort(param[,2])[95]
mean(param[,3])
sort(param[,3])[5]
sort(param[,3])[95]
mean(param[,4])
sort(param[,4])[5]
sort(param[,4])[95]






y1=rnorm(50,5,1)
y2=rnorm(50,-5,1)
delt=sample(c(0,1),50,replace=TRUE)
y=y1*delt+(1-delt)*y2

hist(y)


#Bagging




