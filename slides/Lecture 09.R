#GAMS Lab
library(ISLR)
library(splines)
attach(Auto)
gam1=lm(mpg~displacement+horsepower+weight+acceleration,data=Auto)

library(gam)
gam.m1=gam(mpg~displacement+horsepower+weight+acceleration,data=Auto)
par(mfrow=c(1,4))
plot(gam.m1,se=TRUE,col="blue")
plot.gam(gam1,se=TRUE,col="red")

gam.m2=gam(mpg~s(displacement,5)+s(horsepower,5)+s(weight,5)+s(acceleration,5),data=Auto)
plot(gam.m2,se=TRUE,col="blue")

gam.m3=gam(mpg~s(displacement,5)+s(horsepower,5)+weight+s(acceleration,5),data=Auto)
plot(gam.m3,se=TRUE,col="blue")
anova(gam.m1,gam.m2,gam.m3,test="F")
anova(gam.m1,gam.m3,gam.m2,test="F")




preds=predict(gam.m3,newdata=Auto)
num=length(preds)

sum((Auto$mpg-preds)^2)/num

preds.lm=predict(gam1,newdata=Auto)
sum((Auto$mpg-preds.lm)^2)/num



#Tree Lab

library(tree)
attach(Carseats)
High=ifelse(Sales<=8,"No","Yes")

Carseats=data.frame(Carseats,High)
tree.carseats=tree(High~.-Sales,Carseats)
summary(tree.carseats)

par(mfrow=c(1,1))

plot(tree.carseats)
text(tree.carseats,pretty=0)

tree.carseats

set.seed(2)
train=sample(1:nrow(Carseats),200)
Carseats.test=Carseats[-train,]
High.test=High[-train]
tree.carseats=tree(High~.-Sales,Carseats,subset=train)
tree.pred=predict(tree.carseats,Carseats.test,type="class")
table(tree.pred,High.test)

set.seed(3)
cv.carseats=cv.tree(tree.carseats,FUN=prune.misclass)
names(cv.carseats)
cv.carseats

par(mfrow=c(1,2))
plot(cv.carseats$size,cv.carseats$dev,type="b")
plot(cv.carseats$k,cv.carseats$dev,type="b")

prune.carseats=prune.misclass(tree.carseats,best=9)
plot(prune.carseats)
text(prune.carseats,pretty=0)
tree.pred=predict(prune.carseats,Carseats.test,type="class")
table(tree.pred,High.test)

prune.carseats=prune.misclass(tree.carseats,best=15)
plot(prune.carseats)
text(prune.carseats,pretty=0)
tree.pred=predict(prune.carseats,Carseats.test,type="class")
table(tree.pred,High.test)


library(ISLR)
par(mfrow=c(1,1))
salary.available <- !is.na(Hitters$Salary)
Hitters <- Hitters[salary.available,]
Hitters$Salary <- log(Hitters$Salary)
plot(Hitters$Years, Hitters$Hits, pch=19, xlab="Years", ylab="Hits",col=rainbow(7)[cut(Hitters$Salary, 7)],main="Baseball Player Salary")
sal.tree <- tree(Salary ~ Years + Hits, Hitters)
pruned.tree <- prune.tree(sal.tree, best=3)
plot(pruned.tree)
text(pruned.tree)

plot(Hitters$Years, Hitters$Hits,
      pch=19, xlab="Years", ylab="Hits",
      col=rainbow(7)[cut(Hitters$Salary, 7)],
      main="Baseball Player Salary")
partition.tree(sal.tree, ordvars=c("Years", "Hits"), add=TRUE)

cv.base=cv.tree(sal.tree)
cv.base

best.prune=prune.tree(sal.tree,best=4)
plot(best.prune)
text(best.prune)
plot(Hitters$Years, Hitters$Hits,
     pch=19, xlab="Years", ylab="Hits",
     col=rainbow(7)[cut(Hitters$Salary, 7)],
     main="Baseball Player Salary")
partition.tree(best.prune, ordvars=c("Years", "Hits"), add=TRUE)
#regression trees

library(MASS)
set.seed(16)
train=sample(1:nrow(Boston),nrow(Boston)/2)
tree.boston=tree(medv~.,Boston,subset=train)
summary(tree.boston)

plot(tree.boston)
text(tree.boston,pretty=0)

cv.boston=cv.tree(tree.boston)
cv.boston
plot(cv.boston$size,cv.boston$dev,type='b')

prune.boston=prune.tree(tree.boston,best=8)
plot(prune.boston)
text(prune.boston,pretty=0)

yhat=predict(prune.boston,newdata=Boston[-train,])
boston.test=Boston[-train,"medv"]
mean((yhat-boston.test)^2)

yhat=predict(tree.boston,newdata=Boston[-train,])
boston.test=Boston[-train,"medv"]
plot(yhat,boston.test)
abline(0,1)
mean((yhat-boston.test)^2)

means=seq(1,10,1)
for (i in 10:2)
{
prune.boston=prune.tree(tree.boston,best=i)
#plot(prune.boston)
#text(prune.boston,pretty=0)

yhat=predict(prune.boston,newdata=Boston[-train,])
boston.test=Boston[-train,"medv"]
means[i]=mean((yhat-boston.test)^2)
}
