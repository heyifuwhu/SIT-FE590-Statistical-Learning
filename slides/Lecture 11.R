
#ppr(formula, data, weights, subset, na.action,
#    contrasts = NULL, ..., model = FALSE)

#ppr(x, y, weights = rep(1,n),
#    ww = rep(1,q), nterms, max.terms = nterms, optlevel = 2,
#    sm.method = c("supsmu", "spline", "gcvspline"),
#    bass = 0, span = 0, df = 5, gcvpen = 1, ...)






library(MASS)


train=sample(1:nrow(Boston),nrow(Boston)/2)


bost.ppr=ppr(medv~.,data=Boston[train,],nterms=4,max.terms=5)
bost.ppr
summary(bost.ppr)



bost.pred=predict(bost.ppr,newdata=Boston[-train,])
boston.test=Boston[-train,"medv"]

mean((bost.pred-boston.test)^2)

bost2.ppr=ppr(medv~lstat+rm+tax+ptratio,data=Boston[train,],nterms=4,max.terms=5)


bost2.ppr
summary(bost2.ppr)


bost2.pred=predict(bost2.ppr,newdata=Boston[-train,])


mean((bost2.pred-boston.test)^2)


bost3.ppr=ppr(medv~.,data=Boston[train,],nterms=2,max.terms=10)


bost3.ppr
summary(bost3.ppr)


bost3.pred=predict(bost3.ppr,newdata=Boston[-train,])


mean((bost3.pred-boston.test)^2)


#Neural Networks


#Example taken from http://gekkoquant.com/2012/05/26/neural-networks-with-r-simple-example/
library("neuralnet")

#Going to create a neural network to perform sqare rooting
#Type ?neuralnet for more information on the neuralnet library

#Generate 50 random numbers uniformly distributed between 0 and 100
#And store them as a dataframe
traininginput <-  as.data.frame(runif(50, min=0, max=100))
trainingoutput <- sqrt(traininginput)

#Column bind the data into one variable
trainingdata <- cbind(traininginput,trainingoutput)
colnames(trainingdata) <- c("Input","Output")

#Train the neural network
#Going to have 10 hidden layers
#Threshold is a numeric value specifying the threshold for the partial
#derivatives of the error function as stopping criteria.
net.sqrt <- neuralnet(Output~Input,trainingdata, hidden=10, threshold=0.01)
print(net.sqrt)

#Plot the neural network
plot(net.sqrt)

#Test the neural network on some training data
testdata <- as.data.frame((1:10)^2) #Generate some squared numbers
net.results <- compute(net.sqrt, testdata) #Run them through the neural network

#Lets see what properties net.sqrt has
ls(net.results)

#Lets see the results
print(net.results$net.result)
print(net.results$neurons)

#Lets display a better version of the results
cleanoutput <- cbind(testdata,sqrt(testdata),
                     as.data.frame(net.results$net.result))
colnames(cleanoutput) <- c("Input","Expected Output","Neural Net Output")
print(cleanoutput)



#regression neural nets

library(neuralnet)

?neuralnet

wts=runif(nrow(Boston[train,]),-.7,.7)


bost1.nn=neuralnet(medv~lstat+rm+tax+ptratio,startweights=wts,data=Boston[train,],hidden=5,threshold=.01)
print(bost1.nn)
plot(bost1.nn)
forcomp=Boston[-train,c(13,6,10,11)]
bost1.nn.pred=compute(bost1.nn,forcomp)
summary(bost1.nn.pred)
names(bost1.nn.pred)
print(bost1.nn.pred$neurons)
print(bost1.nn.pred$net.result)

mean((bost1.nn.pred$net.result-boston.test)^2)

#with normalization
NewBoston=Boston
means=apply(as.matrix(Boston),2,mean)
sds=apply(as.matrix(Boston),2,sd)
for(i in 1:length(colnames(NewBoston))) {
  if(class(NewBoston[,i]) == "numeric" || class(NewBoston[,i]) == "integer") {
    NewBoston[,i] <- as.vector(scale(NewBoston[,i])) }
}

Tester=NewBoston$crim*sds[1]+means[1]
Tester-Boston$crim

bost4.nn=neuralnet(medv~lstat+rm+tax+ptratio,data=NewBoston[train,],hidden=5,threshold=.01)
print(bost4.nn)
plot(bost4.nn)
forcomp=NewBoston[-train,c(13,6,10,11)]
bost4.nn.pred=compute(bost4.nn,forcomp)
summary(bost4.nn.pred)
names(bost4.nn.pred)
print(bost4.nn.pred$neurons)
print(bost4.nn.pred$net.result)

mean(((bost4.nn.pred$net.result*sds[14]+means[14])-boston.test)^2)


unnorm=function(vect,avg,std)
{
  return((vect*std+avg))
}

mean((unnorm(bost4.nn.pred$net.result,means$medv,sds$medv)-boston.test)^2)

mean((unnorm(bost4.nn.pred$net.result,means[14],sds[14])-boston.test)^2)



bostall.nn=neuralnet(medv~crim+zn+indus+chas+nox+rm+age+dis+rad+tax+ptratio+black+lstat,startweights=wts,data=NewBoston[train,],hidden=5,threshold=.01)

plot(bostall.nn)
forcomp=NewBoston[-train,-14]
bostall.nn.pred=compute(bostall.nn,forcomp)


mean((unnorm(bostall.nn.pred$net.result,means[14],sds[14])-boston.test)^2)

bostall.nn.2=neuralnet(medv~crim+zn+indus+chas+nox+rm+age+dis+rad+tax+ptratio+black+lstat,startweights=wts,data=NewBoston[train,],hidden=15,threshold=.01)

plot(bostall.nn.2)
forcomp=NewBoston[-train,-14]
bostall.nn.pred.2=compute(bostall.nn.2,forcomp)


mean((unnorm(bostall.nn.pred.2$net.result,means[14],sds[14])-boston.test)^2)


#classification neural nets

library(ISLR)
#Normalize Function
Normalize=function(Data)
{
NewData=Data

for(i in 1:length(colnames(NewData))) {
  if(class(NewData[,i]) == "numeric" || class(NewData[,i]) == "integer") {
    NewData[,i] <- as.vector(scale(NewData[,i])) }
}
return(NewData)
}

#use Weekly data

NewWeekly=Normalize(Weekly)


train=sample(1:nrow(Weekly),nrow(Weekly)/2)
weekly.test=Weekly[-train,"Direction"]

weekly.nn=neuralnet(Direction~Lag1+Lag2+Lag3+Lag4+Volume,data=NewWeekly[train,],hidden=10,threshold=.01)

library(nnet)

#nnet(formula, data, weights, ...,
#     subset, na.action, contrasts = NULL)

#nnet(x, y, weights, size, Wts, mask,
#     linout = FALSE, entropy = FALSE, softmax = FALSE,
#     censored = FALSE, skip = FALSE, rang = 0.7, decay = 0,
#     maxit = 100, Hess = FALSE, trace = TRUE, MaxNWts = 1000,
#     abstol = 1.0e-4, reltol = 1.0e-8, ...)


weekly.nn=nnet(Direction~.,data=NewWeekly[train,],size=10)
summary(weekly.nn)

weekly.nn.pred=predict(weekly.nn,newdata=NewWeekly[-train,])

table(weekly.nn.pred,weekly.test)

#investigate caret

library(caret)


bost.nn.car=train(medv~.,data=NewBoston[train,],weights=wts,method="neuralnet")
bost.nn.car.pred=predict(bost3.nn.car,newdata=NewBoston[-train,])
mean((unnorm(bost3.nn.car.pred,means[14],sds[14])-boston.test)^2)
