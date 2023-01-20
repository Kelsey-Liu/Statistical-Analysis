getwd()
setwd("/Users/kelseyliu/Documents/01. UChicago/Courses/
      Autumn 2022/Statistical Analysis/Assignment")

data <- read.table("Week1_Test_Sample.csv", header=T)
data
summary(data)
str(data)
View(data)

data$u <- as.factor(data$u)
data$v <- as.factor(data$v)

sum(data$u==1)
sum(data$u==2)
sum(data$u==3)

sum(data$v==1)
sum(data$v==2)
sum(data$v==3)
sum(data$v==4)

table(data)
freq <- matrix(table(data), 3)
freq

nrow(data)

joint_distribution <- freq /nrow(data)
joint_distribution

u_Marginal <- c(sum(joint_distribution[1,]),
                sum(joint_distribution[2,]),
                sum(joint_distribution[3,]))
u_Marginal

v_Marginal <- c(sum(joint_distribution[,1]),
                sum(joint_distribution[,2]),
                sum(joint_distribution[,3]),
                sum(joint_distribution[,4]))      

v_Marginal


u_Conditional_v <- joint_distribution[,4]/sum(joint_distribution[,4])
v_Conditional_u <- joint_distribution[3,]/sum(joint_distribution[3,])

res <-list(Joint_distribution = joint_distribution,
           u_Marginal = u_Marginal,
           v_Marginal = v_Marginal,
           u_Conditional_v = u_Conditional_v,
           v_Conditional_u = v_Conditional_u)
res

saveRDS(res, file = "result.rds")


