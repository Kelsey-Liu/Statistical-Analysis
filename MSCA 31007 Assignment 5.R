dataPath <- getwd()
dat <- read.table(paste(dataPath,'Week5_Test_Sample.csv',sep = '/'), header=TRUE)
head(dat)
plot(dat$Input, dat$Output, type="p", pch=19)
nSample <- length(dat$Input)

#Estimated.Linear.Model.Case2<-lm(Output~Input,LinearModel.Case2)
m1 <- lm(Output ~ Input, dat)
m1$coefficients
matplot(dat$Input,cbind(dat$Output,m1$fitted.values),type="p",pch=16,ylab="Sample and Fitted Values")
summary(m1)

## Interpret the summary of the model. Analize the residuals, plot them.
#EstimatedResiduals.Case2<-Estimated.Linear.Model.Case2$residuals
estimatedResiduals <- m1$residuals
plot(dat$Input,estimatedResiduals)

##probability density function
prob.dens.residuals <- density(estimatedResiduals)
str(prob.dens.residuals)
plot(prob.dens.residuals, ylim = c(0, 0.6))
lines(prob.dens.residuals$x,
      dnorm(prob.dens.residuals$x, mean=mean(estimatedResiduals), sd=sd(estimatedResiduals)))

## Create training sample with Input >= 5 and separate the points above the fitted line and below.
# Create NA vectors
Train.Sample <- data.frame(trainInput=dat$Input,trainOutput=rep(NA,nSample))
Train.Sample.Steeper <- data.frame(trainSteepInput=dat$Input,
                                 trainSteepOutput=rep(NA,nSample))  
Train.Sample.Flatter <- data.frame(trainFlatInput=dat$Input,
                                 trainFlatOutput=rep(NA,nSample))  
##The result is: data frames Train.Sample.Case2, Train.Sample.Case2.Steeper and Train.Sample.Case2.Flatter with the first column equal to Input and the second column of NA.

head(cbind(dat,
           Train.Sample,
           Train.Sample.Steeper,
           Train.Sample.Flatter))
##Select parts of the sample with Input greater than 5 and Output either above the estimated regression line or below it.
# Create selectors
Train.Sample.Selector<-dat$Input>=2
Train.Sample.Steeper.Selector<-Train.Sample.Selector&
  (dat$Output>m1$fitted.values)
Train.Sample.Flatter.Selector<-Train.Sample.Selector&
  (dat$Output<=m1$fitted.values)

##Create training samples for steep and flat slopes.
# Select subsamples
Train.Sample[Train.Sample.Selector,2]<-dat[Train.Sample.Selector,2]
Train.Sample.Steeper[Train.Sample.Steeper.Selector,2]<-dat[Train.Sample.Steeper.Selector,2]
Train.Sample.Flatter[Train.Sample.Flatter.Selector,2]<-dat[Train.Sample.Flatter.Selector,2]
head(Train.Sample)

head(cbind(dat,
           Train.Sample,
           Train.Sample.Steeper,
           Train.Sample.Flatter),10)
str(Train.Sample.Steeper)

m.steep <- lm(trainSteepOutput ~ trainSteepInput, Train.Sample.Steeper)
m.flat <- lm(trainFlatOutput ~ trainFlatInput, Train.Sample.Flatter)
  
summary(m.steep)

res <- list( GeneralModel = m1, mSteep = m.steep, mFlat = m.flat)
saveRDS(res, file = paste(dataPath,'result_week5.rds',sep = '/'))
