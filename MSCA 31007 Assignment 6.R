dataPath <- getwd()
train_dat <- read.table(paste(dataPath,'Week6_Test_Sample_Train.csv',sep = '/'), header=TRUE)
main_dat <- read.table(paste(dataPath,'Week6_Test_Sample_Test.csv',sep = '/'), header=TRUE)

head(train_dat)
nsample = length(train_dat[,1])

train_dat_1 = cbind(train_dat[,2], rep(NA, nsample))
train_dat_2 = cbind(train_dat[,2], rep(NA, nsample))
head(train_dat_1)

train_dat_1[train_dat[,3]*(1:nsample),2] = train_dat[train_dat[,3]*(1:nsample),1]
train_dat_2[(1 - train_dat[,3])*(1:nsample),2] = train_dat[(1 - train_dat[,3])*(1:nsample),1]

head(cbind(train_dat, training1 = train_dat_1[,2], training2 = train_dat_2[,2]))


model = lm(train_dat$Output~train_dat$Input)
summary(model)
train_res = model$residuals

train_res_1 = train_res
train_res_2 = train_res

train_res_1[(train_dat[,3] == 0) * (1:nsample)] = NA
train_res_2[(train_dat[,3] == 1) * (1:nsample)] = NA

head(cbind(allres = train_res, train1res = train_res_1, train2res = train_res_2, trainclass = train_dat[,3]))

# logistic regression

glm_dat = data.frame(output = train_dat$Selection.Sequence,
                     input = train_res)
model2 = glm(output ~ input, data = glm_dat, family = binomial(link = logit))
summary(model2)



Unscrambling.Sequence.TL = (predict(model2, type = 'response') > 0.5) * 1
class_train_1 = train_res
class_train_2 = train_res
class_train_1 [(Unscrambling.Sequence.TL == 0) * (1:nsample)] = NA
class_train_2 [(Unscrambling.Sequence.TL == 1) * (1:nsample)] = NA

head(cbind(alltrain = train_res, train1class = class_train_1, train2class = class_train_2))

nmain = length(main_dat[,1])
head(main_dat)

model3 = lm(main_dat$Output ~ main_dat$Input)
estimated_res = model3$residuals
summary(model3)
Unscrambling.Sequence.Logistics = (predict(model2, newdata = data.frame(output = estimated_res,
                                                                        input = estimated_res),
                                           type = 'response') > 0.5) * 1

length(Unscrambling.Sequence.Logistics)
Unscrambling.Sequence.Logistics[1:10]
class_res_1 = estimated_res
class_res_2 = estimated_res
class_res_1[(Unscrambling.Sequence.Logistics == 0) * (1:nmain)] = NA
class_res_2[(Unscrambling.Sequence.Logistics == 1) * (1:nmain)] = NA
cbind(estimated_res, class_res_1, class_res_2)[1:10,]

res <- list(Unscrambling.Sequence.Logistic =  Unscrambling.Sequence.Logistics)
write.table(res, file = paste(dataPath,'result_w6.csv',sep = '/'), row.names = F)