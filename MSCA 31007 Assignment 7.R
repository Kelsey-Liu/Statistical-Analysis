dataPath <- getwd()
test_dat <- read.table(paste(dataPath,'Week7_Test_Sample.csv',sep = '/'), header=TRUE)
head(test_dat)
## Fit linear models using: no inputs, only Input1, only Input2, both Input1 and Input2.
fit.1<-lm(Output~1,data=test_dat)
fit.1.2<-lm(Output~1+Input1,data=test_dat)
fit.1.3<-lm(Output~1+Input2,data=test_dat)
fit.1.2.3<-lm(Output~.,data=test_dat)

# Compare ANOVA table of each fit with the summary
## Outputs of anova().
anova(fit.1.2)

# Compare summary(fit.1) and anova(fit.1)
summary(fit.1)
anova(fit.1)

#Note that anova(fit.1)$"Sum Sq" is the same as sum(fit.1$residuals^2)
c(anova(fit.1)$"Sum Sq",sum(fit.1$residuals^2))
#numbers of degrees of freedom are also the same
c(anova(fit.1)$Df,fit.1$df.residual,summary(fit.1)$df[2])

# Compare summary(fit.1.2) and anova(fit.1.2)
anova(fit.1.2)
summary(fit.1.2)

## F-statistic and Pr(>F) in both outputs are equivalent
summary(fit.1.2)$fstatistic
c(F.value=anova(fit.1.2)$"F value"[1],Df=anova(fit.1.2)$Df,P.value=anova(fit.1.2)$"Pr(>F)"[1])

summary(fit.1.2)$r.squared

# Compare summary(fit.1.3) and anova(fit.1.3)
summary(fit.1.3)
anova(fit.1.3)

## Compare F-values
c(F.value=anova(fit.1.3)$"F value"[1],Df=anova(fit.1.3)$Df,P.value=anova(fit.1.3)$"Pr(>F)"[1])
summary(fit.1.3)$fstatistic

# Compare summary(fit.1.2.3) and anova(fit.1.2.3)
summary(fit.1.2.3)
anova(fit.1.2.3)

## Use anova() to compare nested linear models
anova(fit.1.2,fit.1.2.3)
summary(fit.1.2.3)

## Compare the following 2 outputs.
anova(fit.1,fit.1.2.3)
summary(fit.1.2.3)
c(anova(fit.1,fit.1.2.3)$F[2],summary(fit.1.2.3)$fstatistic[1])

anova(fit.1,fit.1.2.3)$F
anova(fit.1,fit.1.2.3)$'Pr(>F)'
  