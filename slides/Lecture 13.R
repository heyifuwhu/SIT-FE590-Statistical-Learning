#PCA Explination

states=row.names(USArrests)
states

names(USArrests)

apply(USArrests,2,mean)

apply(USArrests,2,var)

pr.out=prcomp(USArrests,scale=TRUE)

names(pr.out)

pr.out$center
pr.out$scale

pr.out$rotation

dim(pr.out$x)

biplot(pr.out,scale=0)

pr.out$rotation=-pr.out$rotation
pr.out$x=-pr.out$x
biplot(pr.out,scale=0)

pr.out$sdev
pr.var=pr.out$sdev^2
pr.var

pve=pr.var/sum(pr.var)
pve

plot(pve,xlab="Principal Component",ylab="Proportion of Variance Explained", ylim=c(0,1), type='b')
plot(cumsum(pve),xlab="Principal Component",ylab="Cumulative Proportion",ylim=c(0,1),type='b')


#Cluster Explination

set.seed(2)
x=matrix(rnorm(50*2),ncol=2)
x[1:25,1]=x[1:25,1]+3
x[1:25,2]=x[1:25,2]-4

km.out=kmeans(x,2,nstart=20)
km.out$cluster

plot(x,col=(km.out$cluster+1),main="K-Means Clustering K=2",xlab="",ylab="",pch=20,cex=2)

set.seed(4)
km.out=kmeans(x,3,nstart=20)
km.out

set.seed(3)
km.out=kmeans(x,3,nstart=1)
km.out$tot.withins
plot(x,col=(km.out$cluster+1),main="K-Means Clustering K=3",xlab="",ylab="",pch=20,cex=2)
km.out=kmeans(x,3,nstart=20)
km.out$tot.withins
plot(x,col=(km.out$cluster+1),main="K-Means Clustering K=3",xlab="",ylab="",pch=20,cex=2)

#hierarchical

hc.complete=hclust(dist(x),method="complete")
hc.average=hclust(dist(x),method="average")
hc.single=hclust(dist(x),method="single")

par(mfrow=c(1,3))
plot(hc.complete,main="Complete Linkage",cex=.9)
plot(hc.average,main="Average Linkage",cex=.9)
plot(hc.single,main="Single Linkage",cex=.9)

cutree(hc.complete,2)
cutree(hc.average,2)
cutree(hc.single,2)

cutree(hc.single,4)

xsc=scale(x)
par(mfrow=c(1,1))
plot(hclust(dist(xsc),method="complete"),main="Hierarchical Clustering with Scaled")

x=matrix(rnorm(30*3),ncol=3)
dd=as.dist(1-cor(t(x)))
plot(hclust(dd,method="complete"),main="Complete Linkage with Correlation Distance")
