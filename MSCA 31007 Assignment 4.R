getwd()
dataPath <- getwd()
dat <- read.table(paste(dataPath,'Week4_Test_Sample.csv',sep = '/'), header=TRUE)
head(dat)
plot(dat$X, dat$Y)
Estimated.LinearModel <- lm(Y ~ X,data=dat)
names(Estimated.LinearModel)
Estimated.LinearModel$coefficients
Estimated.LinearModel$residuals
plot(Estimated.LinearModel$residuals)
Estimated.LinearModel$fitted.values
summary(Estimated.LinearModel)
names(summary(Estimated.LinearModel))
summary(Estimated.LinearModel)$sigma
summary(Estimated.LinearModel)$sigma^2


estimated_residuals <- Estimated.LinearModel$residuals
plot(dat$X, estimated_residuals)

Probability.Density.Residuals <- density(estimated_residuals)
plot(Probability.Density.Residuals, ylim = c(0, 1))
lines(Probability.Density.Residuals$x, dnorm(Probability.Density.Residuals$x, 
                                             mean = mean(estimated_residuals), sd = sd(estimated_residuals)))

c(Left.Mean = mean(estimated_residuals[estimated_residuals < 0]), 
  Right.Mean = mean(estimated_residuals[estimated_residuals > 0]))

a <- estimated_residuals[estimated_residuals < 0]

b <- estimated_residuals[estimated_residuals > 0]

Unscrambled.Selection.Sequence <- numeric(1000)
for (i in 1:1000) {
  if (estimated_residuals[i] > 0) {
    Unscrambled.Selection.Sequence[i] <- 1
  } else {
    Unscrambled.Selection.Sequence[i] <- 0
  }
}
head(Unscrambled.Selection.Sequence,30)

res <- list(Unscrambled.Selection.Sequence =  Unscrambled.Selection.Sequence)
write.table(res, file = paste(dataPath,'result_w4.csv',sep = '/'), row.names = F)
