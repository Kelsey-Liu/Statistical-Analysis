############## Assignment 8 ##############
dataPath <- getwd()
test_dat <- read.table(paste(dataPath,'Week8_Test_Sample.csv',sep = '/'), header=TRUE)
head(test_dat)
summary(test_dat)

# Model_all <- lm(Output ~ Treatment)
# Model_1 <- lm(Output~Treatement-1).

summaryByGroup<-aggregate(Output ~ Treatment,data=test_dat,FUN=summary)
means<-cbind(Means=summaryByGroup$Output[,4],Sizes=aggregate(Output ~ Treatment,data=test_dat,FUN=length)$Output)
rownames(means)<-as.character(summaryByGroup$Treatment)
means

Output.model<-lm(Output ~ Treatment,data=test_dat)
summary(Output.model)
anova(Output.model)

# grand.mean & group.mean
grand.mean<-mean(test_dat$Output)
grand.mean

create.vector.of.means<-function(my.group.data) {
  rep(my.group.data[1],my.group.data[2])
}
group.mean<-unlist(apply(means,1,create.vector.of.means))

group.mean


SST<-sum((test_dat$Output-grand.mean)^2)
SSE<-sum((test_dat$Output-group.mean)^2)
SSM<-sum((group.mean-grand.mean)^2)
c(SST=SST,SSE=SSE,SSM=SSM)


# anova(Output.model)
# anova(Output.model.null,Output.model.full)
# 
# model.matrix(Output.model)
Output.altmodel<-lm(Output~Treatment-1,data=test_dat)
summary(Output.model)
anova(Output.altmodel)


##### Quiz Answer #####

# Within group sum of squares: SSE
SSE
# Between groups sum of squares: SSM
SSM
# Not all group means are the same


# Sum of squares of predictor for the model testing H0: "all group mean values equal to zero"
# Sum Sq -- Treatment
anova(Output.altmodel)[1,2]
# Sum of squares of residuals for the model testing H0: "all group mean values equal to zero""
# Sum Sq -- Residuals
anova(Output.altmodel)[2,2]

# All group mean values equal to zero
