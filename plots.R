dataset<- read.table("D:\\Anushi\\R_programs\\haberman.data",header=FALSE,sep=",")
norm_vec<- rnorm(n=10,mean=5,sd=2)
v<- seq(from=1,to=20,by=1)

plot(dataset[,1],dataset[,3],main="Scatterplot",xlab="Age",ylab="NumberofNodes",pch=20)

hist(dataset[,2],main="Histogram",xlab="Year",ylab="Count")

boxplot(dataset[,1],main="Boxplot",xlab="Age")

lm_model<- lm(y~x1+x2,data=as.data.frame(cbind(y,x1,x2)))
