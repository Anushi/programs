library(caret);data(faithful); set.seed(333)
inTrain <- createDataPartition(y=faithful$waiting,
                              p=0.5, list=FALSE)
trainFaith <- faithful[inTrain,]; testFaith <- faithful[-inTrain,]
head(trainFaith)

lm1 <- lm(eruptions ~ waiting,data=trainFaith)
summary(lm1)

newdata <- data.frame(waiting=80)
predict(lm1,newdata)