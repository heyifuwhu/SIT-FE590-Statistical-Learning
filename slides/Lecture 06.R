library(MASS)
library(ISLR)
names(Smarket)
dim(Smarket)
summary(Smarket)
pairs(Smarket)

#cor(Smarket)#doesn't work because Direction is qualitative
cor(Smarket[,-9])
attach(Smarket)
plot(Volume)


#Logistic Regression Lab

glm.fit=glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume,family=binomial,data=Smarket)
summary(glm.fit)
coef(glm.fit)
summary(glm.fit)$coef
summary(glm.fit)$coef[,4]
glm.probs=predict(glm.fit,type="response")
glm.probs[1:10]
contrasts(Direction)
glm.pred=rep("Down",1250)
glm.pred[glm.probs>.5]="Up"
table(glm.pred,Direction)
mean(glm.pred==Direction)
table(glm.pred.Direction)
train=(Year<2005)
Smarket.2005=Smarket[!train,]
dim(Smarket.2005)
Direction.2005=Direction[!train]

glm.fit=glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume,data=Smarket,family=binomial,subset=train)
glm.probs=predict(glm.fit,Smarket.2005,type="response")
glm.pred=rep("Down",252)
glm.pred[glm.probs>.5]="Up"
table(glm.pred,Direction.2005)
mean(glm.pred==Direction.2005)
mean(glm.pred!=Direction.2005)

glm.fit=glm(Direction~Lag1+Lag2,data=Smarket,family=binomial,subset=train)
glm.probs=predict(glm.fit,Smarket.2005,type="response")
glm.pred=rep("Down",252)
glm.pred[glm.probs>.5]="Up"
table(glm.pred,Direction.2005)
mean(glm.pred==Direction.2005)

predict(glm.fit,newdata=data.frame(Lag1=c(1.2,1.5),Lag2=c(1.1,-.8)),type="response")


#LDA Multivariate

tau=1/12
N=500
sigma=matrix(c(1,0,.25,(1-1/16)),2,2)*sqrt(tau)
mus1=matrix(c(-.5,-.5),2,1)
mus2=matrix(c(0,-.1),2,1)
mus3=matrix(c(.5,.5),2,1)
x1=mvrnorm(N,mus1,sigma)
x1=cbind(1,x1)
x2=mvrnorm(N,mus2,sigma)
x2=cbind(2,x2)
x3=mvrnorm(N,mus3,sigma)
x3=cbind(3,x3)
#plot(x1,x2)
#points(x3)
ind=sample(seq(1,1500,by=1),N,replace=FALSE)
Xa=rbind(x1,x2,x3)[ind,]

cls=Xa[,1]
X=Xa[,-1]

clss=sort(unique(cls))
K=length(clss)
pi=rep(NA,K)
for(k in 1:K)
{
  pi[k]=sum(cls==clss[k])/N
}
pi
#X=sample(rbind(x1,x2,x3),N,replace=FALSE)

p=dim(X)[2]
Mu=matrix(NA,K,p)
for(k in 1:K)
{
  i=cls==clss[k]
  Mu[k,]=colMeans(X[i,])
}

Q.hat=matrix(0,p,p)
for (k in 1:K)
{
  i=cls==clss[k]
  n=sum(i)
  term=X[i,]-t(replicate(n,Mu[k,]))
  Q.hat=Q.hat+t(term)%*%term
}
Q.hat=Q.hat/(N-K)
Q.hat

Q.inv=solve(Q.hat)
delta=matrix(NA,N,K)
for (k in 1:K)
{
  m=Mu[k,]
  d=log(pi[k])-.5*m%*%Q.inv%*%m
  for(i in 1:N)
  {
    delta[i,k]=d+m%*%Q.inv%*%X[i,]
  }
}
my.class=apply(delta,1,which.max)
plot(X,col=my.class)
plot(X,col=cls)


Y=data.frame(P1=Xa[,2],P2=Xa[,3])
hft=lda(cls~P1+P2,data=Y)
hft.class=predict(hft,Y)$class
plot(Y,col=hft.class)


#LDA Lab

lda.fit=lda(Direction~Lag1+Lag2,data=Smarket,subset=train)
lda.fit
plot(lda.fit)
lda.pred=predict(lda.fit,Smarket.2005)
names(lda.pred)
lda.class=lda.pred$class
table(lda.class,Direction.2005)
mean(lda.class==Direction.2005)

sum(lda.pred$posterior[,1]>=.5)
sum(lda.pred$posterior[,1]<.5)
lda.pred$posterior[1:20,1]
lda.class[1:20]

sum(lda.pred$posterior[,1]>.9)

#QDA multivariate example


N=500
sigma1=matrix(c(1,0,.1,(1-1/100)),2,2)
mus1=matrix(c(-.5,-.5),2,1)
sigma2=matrix(c(1,0,.25,(1-1/16)),2,2)
mus2=matrix(c(0,-.1),2,1)
sigma3=matrix(c(1,0,0,1),2,2)
mus3=matrix(c(.5,.5),2,1)
x1=mvrnorm(N,mus1,sigma1)
x1=cbind(1,x1)
x2=mvrnorm(N,mus2,sigma2)
x2=cbind(2,x2)
x3=mvrnorm(N,mus3,sigma3)
x3=cbind(3,x3)
#plot(x1,x2)
#points(x3)
ind=sample(seq(1,1500,by=1),N,replace=FALSE)
Xa=rbind(x1,x2,x3)[ind,]

cls=Xa[,1]
X=Xa[,-1]

clss=sort(unique(cls))
K=length(clss)
pi=rep(NA,K)
for(k in 1:K)
{
  pi[k]=sum(cls==clss[k])/N
}
pi

p=dim(X)[2]
Mu=matrix(NA,K,p)
for(k in 1:K)
{
  i=cls==clss[k]
  Mu[k,]=colMeans(X[i,])
}

Q.hat=array(0,dim=c(p,p,K))
for (k in 1:K)
{
  i=cls==clss[k]
  n=sum(i)
  term=X[i,]-t(replicate(n,Mu[k,]))
  Q.hat[,,k]=t(term)%*%term/(n-1)
}

Q.hat

delta=matrix(NA,N,K)
for(k in 1:K)
{
  Q=Q.hat[,,k]
  Q.inv=solve(Q)
  d=det(Q)
  for (i in 1:N)
  {
    err=X[i,]-Mu[k,]
    delta[i,k]=log(pi[k])-.5*log(d)-.5*err%*%Q.inv%*%err
  }
}
my.class=apply(delta,1,which.max)
plot(X,col=my.class)
plot(X,col=cls)

Y=data.frame(P1=Xa[,2],P2=Xa[,3])
QDA=qda(cls~P1+P2,data=Y)
QDA.class=predict(QDA,Y)$class
plot(Y,col=QDA.class)

#QDA Lab

qda.fit=qda(Direction~Lag1+Lag2,data=Smarket,subset=train)
qda.fit
qda.class=predict(qda.fit,Smarket.2005)$class
table(qda.class,Direction.2005)
mean(qda.class==Direction.2005)







# K-Nearest Neighbors Example

k=3
P1=seq(min(X[,1]),max(X[,1]),length=100)
P2=seq(min(X[,2]),max(X[,2]),length=100)
n=length(P1)
P=matrix(0,n,n)
for (i1 in 1:n)
{
  for (i2 in 1:n)
  {
    x=c(P1[i1],P2[i2])
    d=X-t(replicate(N,x))
    d=rowSums(d*d)
    i=order(d)[1:k]
    G=cls[i]
    Ps=rep(0,K)
    for(g in 1:K)
    {
      Ps[g]=sum(G==g)/K
    }
    P[i1,i2]=(1:K)[which.max(Ps)]
  }
}

plot(P1,P2,col="white")
for (i in 1:length(P1))
{
  points(rep(P1[i],length(P1)),P2,col=P[,i],pch=15)
}
points(X,col=cls,pch=10)
plot(X,col=cls,pch=10)



#KNN Lab
library(class)#contains the knn function
train.X=cbind(Lag1,Lag2)[train,]
test.X=cbind(Lag1,Lag2)[!train,]
train.Direction=Direction[train]

set.seed(1)
knn.pred=knn(train.X,test.X,train.Direction,k=1)
table(knn.pred,Direction.2005)
mean(knn.pred==Direction.2005)

knn.pred=knn(train.X,test.X,train.Direction,k=3)
table(knn.pred,Direction.2005)
mean(knn.pred==Direction.2005)




