library(ISLR)
attach(Wage)

plot(age,wage)

fit=lm(wage~poly(age,4),data=Wage)
coef(summary(fit))

fit2=lm(wage~poly(age,4,raw=T),data=Wage)
coef(summary(fit2))
x=age^2
y=age^3
z=age^4
fit2c=lm(wage~age+x+y+z,data=Wage)

coef(summary(fit2c))

fit2a=lm(wage~age+I(age^2)+I(age^3)+I(age^4),data=Wage)
coef(fit2a)

fit2b=lm(wage~cbind(age,age^2,age^3,age^4),data=Wage)
coef(fit2b)


agelims=range(age)
age.grid=seq(from=agelims[1],to=agelims[2])
preds=predict(fit,newdata=list(age=age.grid),se=TRUE)
se.bands=cbind(preds$fit+2*preds$se.fit,preds$fit-2*preds$se.fit)

par(mfrow=c(1,2),mar=c(4.5,4.5,1,1),oma=c(0,0,4,0))
plot(age,wage,xlim=agelims,cex=.5,col="darkgrey")
title("Degree-4 Polynomial",outer=T)
lines(age.grid,preds$fit,lwd=2,col="blue")
matlines(age.grid,se.bands,lwd=1,col="blue",lty=3)

preds2=predict(fit2,newdata=list(age=age.grid),se=TRUE)
se.bands2=cbind(preds2$fit+2*preds2$se.fit,preds$fit-2*preds2$se.fit)
plot(age,wage,xlim=agelims,cex=.5,col="darkgrey")
lines(age.grid,preds2$fit,lwd=2,col="blue")
matlines(age.grid,se.bands2,lwd=1,col="blue",lty=3)

max(abs(preds$fit-preds2$fit))

fit.1=lm(wage~age,data=Wage)
fit.2=lm(wage~poly(age,2),data=Wage)
fit.3=lm(wage~poly(age,3),data=Wage)
fit.4=lm(wage~poly(age,4),data=Wage)
fit.5=lm(wage~poly(age,5),data=Wage)
anova(fit.1,fit.2,fit.3,fit.4,fit.5)

coef(summary(fit.5))


fit.1=lm(wage~education+age,data=Wage)
fit.2=lm(wage~education+poly(age,2),data=Wage)
fit.3=lm(wage~poly(age,3)+education,data=Wage)
anova(fit.1,fit.2,fit.3)
coef(summary(fit.3))

par(mfrow=c(1,1))
fit=glm(I(wage>250)~poly(age,4),data=Wage,family=binomial)
preds=predict(fit,newdata=list(age=age.grid),se=T)

pfit=exp(preds$fit)/(1+exp(preds$fit))
se.bands.logit=cbind(preds$fit+2*preds$se.fit,preds$fit-2*preds$se.fit)
se.bands=exp(se.bands.logit)/(1+exp(se.bands.logit))

preds=predict(fit,newdata=list(age=age.grid),type="response",se=T)

plot(age,I(wage>250),xlim=agelims,type="n",ylim=c(0,.2))
points(age,I((wage>250)/5),cex=.5,pch="|",col="darkgrey")
points(jitter(age),I((wage>250)/5),cex=.5,pch="|",col="darkgrey")
lines(age.grid,pfit,lwd=2,col="blue")
matlines(age.grid,se.bands,lwd=1,col="blue",lty=3)

length(wage[wage>250])
length(wage)

table(cut(age,4))
fit=lm(wage~cut(age,4),data=Wage)
coef(summary(fit))




#Splines Labs

library(splines)
fit=lm(wage~bs(age,knots=c(25,40,60)),data=Wage)
pred=predict(fit,newdata=list(age=age.grid),se=T)
plot(age,wage,col="grey")
lines(age.grid,pred$fit,lwd=2)
lines(age.grid,pred$fit+2*pred$se,lty="dashed")
lines(age.grid,pred$fit-2*pred$se,lty="dashed")

dim(bs(age,knots=c(25,40,60)))
dim(bs(age,df=6))
attr(bs(age,df=6),"knots")

fit2=lm(wage~ns(age,df=4),data=Wage)
pred2=predict(fit2,newdata=list(age=age.grid),se=T)
lines(age.grid,pred2$fit,col="red",lwd=2)
lines(age.grid,pred2$fit+2*pred2$se,lty="dashed",col="red")
lines(age.grid,pred2$fit-2*pred2$se,lty="dashed",col="red")

plot(age,wage,xlim=agelims,cex=.5,col="darkgrey")
title("Smoothing Spline")
fit=smooth.spline(age,wage,df=16)
fit2=smooth.spline(age,wage,cv=TRUE)
fit2$df
lines(fit,col="red",lwd=2)
lines(fit2,col="blue",lwd=2)
legend("topright",legend=c("16 DF","6.8 DF"),col=c("red","blue"),lty=1,lwd=2,cex=.8)

plot(age,wage,xlim=agelims,cex=.5,col="darkgrey")
title("Local Regression")
fit=loess(wage~age,span=.2,data=Wage)
fit2=loess(wage~age,span=.5,data=Wage)
lines(age.grid,predict(fit,data.frame(age=age.grid)),col="red",lwd=2)
lines(age.grid,predict(fit2,data.frame(age=age.grid)),col="blue",lwd=2)
legend("topright",legend=c("Span=0.2","Span=0.5"),col=c("red","blue"),lty=1,lwd=2,cex=.8)

#GAMs

gam1=lm(wage~ns(year,4)+ns(age,5)+education,data=Wage)
library(gam)
gam.m3=gam(wage~s(year,4)+s(age,5)+education,data=Wage)
par(mfrow=c(1,3))
plot(gam.m3,se=TRUE,col="blue") #calls plot.gam since it was generate with gam

# not working right now plot.gam(gam1,se=TRUE,col="red") #needs the specific call, since it was generated with lm

gam.m1=gam(wage~s(age,5)+education,data=Wage)
gam.m2=gam(wage~year+s(age,5)+education,data=Wage)
anova(gam.m1,gam.m2,gam.m3,test="F")

plot(gam.m2,se=TRUE,col="purple")

summary(gam.m3)

preds1=predict(gam.m1,newdata=Wage)
preds2=predict(gam.m2,newdata=Wage)
preds3=predict(gam.m3,newdata=Wage)

mean((preds1-wage)^2)
mean((preds2-wage)^2)
mean((preds3-wage)^2)




gam.lo=gam(wage~s(year,df=4)+lo(age,span=.7)+education,data=Wage)
plot(gam.lo,se=TRUE,col="green")

gam.lo.i=gam(wage~lo(year,age,span=.5)+education,data=Wage)

#library(akima)
#plot(gam.lo.i)

gam.lr=gam(I(wage>250)~year+s(age,df=5)+education,family=binomial,data=Wage)
par(mfrow=c(1,3))
plot(gam.lr,se=T,col="green")

table(education,I(wage>250))
gam.lr.s=gam(I(wage>250)~year+s(age,df=5)+education,family=binomial,data=Wage,subset=(education!="1. < HS Grad"))
plot(gam.lr.s,se=T,col="green")

#End of prefab Lab
par(mfrow=c(1,1))
X=cbind(age,age^2,age^3,age^4)
colnames(X)=c("age","squared","cubed","fourth")
fit=lm(wage~X)
round(coef(summary(fit)),4)
u=data.frame(x=seq(from=min(age),to=max(age),length.out=length(age)))
y=predict(fit,newdata=u,se.fit=T,level=.95)
plot(age,wage,col="blue")
i=order(age)
lines(age[i],(y$fit+2*y$se.fit)[i],lwd=3,lty=1,col="green")
lines(age[i],(y$fit-2*y$se.fit)[i],lwd=3,lty=1,col="green")
lines(age[i],y$fit[i],lwd=3,lty=1,col="red")


x=seq(0,10,.5)
z=cbind(x,x^2,x^3)
round(cor(z),4)
matplot(z,type='b',lty=1,lwd=3)

z=poly(seq(0,10,.5),3)
matplot(z,type='b',lty=1,lwd=3)
round(cor(z),4)


bs(x,df=7) #generates basis matrix of cubic splines evaluated at observations in x with 4 interior knots
#knots at .2, .4, .6, .8 percentiles
bs(x,degree=1,knots=c(.2,.4,.6)) #basis for linear splines with three interior knots


