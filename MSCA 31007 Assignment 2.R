dataPath <- getwd()
df <- read.table(paste(dataPath,'Week2_Test_Sample.csv',sep = '/'), header=TRUE)

head(df)
str(df)

sdX <- round(sd(df$x), 2)
sdY <- round(sd(df$y), 2)
cXY <- round(cor(df$x, df$y), 2)

a <- sdY * cXY / sdX
a
lm(df$y ~ df$x)
result <- data.frame(sdX=sdX, sdY=sdY, cXY=cXY, a=a)  
write.table(result, file = paste(dataPath,'W2_result.csv',sep = '/'), row.names = F)
