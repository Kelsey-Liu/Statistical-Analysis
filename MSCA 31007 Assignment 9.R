dataPath <- getwd()
Project.Data <- read.table(paste(dataPath,'Week9_Test_Sample.csv',sep = '/'), header=TRUE)
head(Project.Data)
matplot(Project.Data,type="l")
Project.Data.PCA <- princomp(Project.Data)
Project.Data.PCA
names(Project.Data.PCA)
summary(Project.Data.PCA)
plot(Project.Data.PCA)
0.98295*0.9
