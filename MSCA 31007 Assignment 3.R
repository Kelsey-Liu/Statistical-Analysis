getwd()
dataPath <- getwd()
data <- read.table(paste(dataPath,'Week3_Test_Sample.csv',sep = '/'), header=TRUE)
head(data)
str(data)

mean <- data$x[1]
sd <- data$x[2]
rate <- data$x[3]
sample <- data$x[4:503]

datNorm <- qnorm(sample, mean = mean, sd = sd)
datExp <- qexp(sample, rate = rate)
res <- cbind(datNorm = datNorm, datExp = datExp)

write.table(res, file = paste(dataPath,'result_w3.csv',sep = '/'), row.names = F)
