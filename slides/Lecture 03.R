# Lecture Examples, use around slide 7

ads=read.csv("http://www-bcf.usc.edu/~gareth/ISL/Advertising.csv")
attach(ads)
names(ads)


RSS=function(b0,b1,x,y)
{
  yhat=b0+b1*x;
  err=yhat-y;
  return(sum(err^2));
}

beta0=seq(from=0,to=14,by=.005);
N0=length(beta0);
beta1=seq(from=.02, to=.08,by=.0005);
N1=length(beta1);
z=matrix(NA,N0,N1);
for(i in 1:N0)
  for(j in 1:N1)
  {
    z[i,j]=RSS(beta0[i],beta1[j],TV,sales);
  }
contour(beta0,beta1,z/1000,
        levels=c(2.15,2.2,2.3,2.5,3),
        col="blue",
        lwd=2,
        xlab=expression(beta[0]),
        ylab=expression(beta[1]))



md=lm(sales ~ TV, data=ads)
summary(md)


#use around slide 8

set.seed(0) #very optional step
N=100;
x=rnorm(N);
epsilon=rnorm(N,sd=4);
B0=2
B1=3
y=B0+B1*x+epsilon;
plot(x,y);
abline(B0,B1,col="red",lwd=2);
mod=lm(y~x);
abline(mod,col="blue",lwd=2);
legend("topleft",c("Population Regression Line", "Least-Square Line"),col=c("red","blue"),lwd=2);

#use around slide 9

X=rnorm(100,mean=2,sd=3);
Y=sample(X,10,replace=F);
m=mean(Y);
s=sd(Y);
c(m,s)

stats=matrix(NA,1000,2)
for(k in 1:1000)
{
  Y=sample(X,10,replace=T);
  stats[k,1]=mean(Y);
  stats[k,2]=sd(Y);
}
c(mean(stats[,1]),mean(stats[,2]))
mean(X)
sd(X)

hist(stats[,1],100,main="",xlab="Estimate",col="lightblue")


nexp=100
n=60
sd.pop=function(x,m)
{
  sqrt(sum((x-m)^2)/length(x))
}
av=matrix(NA,nexp,n)
for(nsamp in seq(from=1,to=n,by=1))
{
  for (k in 1:nexp)
  {
    Y=sample(X,nsamp,replace=F)
    av[k,nsamp]=mean(Y)
  }
}
plot(3/sqrt(1:n),type="l",col="red",lwd=2,ylim=c(0,3),xlab="n",ylab=expression(s[n]))
points(1:n,apply(av,2,sd.pop,m=2),col="blue")
#the sample standard deviation are plotted as blue circles
#and the function in red is the sigma/sqrt(n)

stats=matrix(NA,50,2)
plot(x,y)
for (k in 1:50)
{
  i=sample(1:N,20,replace=F)
  mod=lm(y[i]~x[i])
  abline(mod,col="blue",lwd=1)
  stats[k,]=mod$coefficients
}
abline(B0,B1,col="red",lwd=3)
#Note that each sample gives a different estimate of the slope and intercept,
#though since we are creating this data we know the true values (3,2)

cat(colMeans(stats))#averages of intercept and slope of 50 experiments
cat(apply(stats,2,sd))#their standard deviations


#use around slide 14
summary(md)$coefficients


#use around slide 17
#Running linear regression one at a time
mRadio=lm(sales ~ radio,data=ads)
summary(mRadio)$coefficients
mNewspaper=lm(sales ~ newspaper,data=ads)
summary(mNewspaper)$coefficients

xa=rnorm(100)
ya=5*xa+rnorm(100,0,.1)
plot(xa,ya)
summary(lm(ya~xa))

#use around slide 18
mMulti=lm(sales~TV+radio+newspaper,data=ads)
round(summary(mMulti)$coefficients,4)
cor(ads[,2:5])

#use around slide 21
n=3
N=1000
x=matrix(rnorm(N*n),N,n)
s=rowSums(x^2)
hist(s,100,freq=F,col="lightblue",main="Chi-Squared Distribution")
xx=seq(0,20,by=.1)#Superimposing the actual Chi-square distribution
yy=dchisq(xx,n)
lines(xx,yy,lwd=3,col="red")

#use around slide 22
N=10000
n=50
u=rchisq(N,n)
m=45
v=rchisq(N,m)
f=(u/n)/(v/m)
hist(f,100,freq=F,col="lightblue",main="F Distribution")
xx=seq(0,3,by=.01)#superimposing the actual F distribution
yy=df(xx,50,45)
lines(xx,yy,lwd=3,col="red")


#RLab
install.packages("ISLR")#only needs to be done once
library(MASS)
library(ISLR)

fix(Boston)
names(Boston)

?Boston
lm.fit=lm(medv~lstat,data=Boston)# or attach(Boston); lm.fit=lm(medv~lstat);
lm.fit
summary(lm.fit)
names(lm.fit)
lm.fit$coefficients
coef(lm.fit)
confint(lm.fit)
predict(lm.fit,data.frame(lstat=c(5,10,15)),interval="confidence")
predict(lm.fit,data.frame(lstat=c(5,10,15)),interval="prediction")
attach(Boston)
plot(lstat,medv)
abline(lm.fit)

abline(lm.fit,lwd=3)
abline(lm.fit,lwd=3,col="red")
plot(lstat,medv,col="red")
plot(lstat,medv,pch=20)
plot(lstat,medv,pch="+")
plot(1:20,1:20,pch=1:20)


plot(lm.fit)

par(mfrow=c(2,2))
plot(lm.fit)

par(mfrow=c(1,1))



#Multiple Linear Regression Lab

lm.fit=lm(medv~lstat+age, data=Boston)
summary(lm.fit)

lm.fit=lm(medv~.-indus-age-crim-chas,data=Boston)
summary(lm.fit)
cor(Boston[,2:14])

lm.fit=lm(medv~lstat+age+dis+zn, data=Boston)
summary(lm.fit)
lm.fit2=lm(medv~age,data=Boston)
summary(lm.fit2)

lm.fit3=lm(medv~zn+dis+lstat+age,data=Boston)
summary(lm.fit3)
lm.fit4=lm(age~.,data=Boston)
summary(lm.fit4)
