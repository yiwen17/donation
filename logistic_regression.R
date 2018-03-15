setwd("C:/Users/zgscsljason/Google Drive/Northwestern/Predictive Analytics/Project")
setwd("K:/Google Drive/Northwestern/Predictive Analytics/Project")
donation <- read.csv("donation data.csv")
code <- read.csv("dmef1code.csv")
# ===========================Data Cleaning============================================================

# Setting the missing value in CNDOL2 and CNDOL3 to 0
donate$CNDOL2[is.na(donate$CNDOL2)] <- 0
donate$CNDOL3[is.na(donate$CNDOL3)] <- 0


# Find the solicitation time 
solTime = rowSums(!is.na(donate[, c('sol1', 'sol2', 'sol3')]))
donate$solTime = as.factor(solTime)

# Get the index of data
ind = 1:dim(donate)[1]
# The test values is divisble by 3
test_ind = ind %% 3 == 0
# Get the test set
test = donate[test_ind, ]
# Get the training set
train = donate[!test_ind, ]

# Create the lable for logistic regression
train$TARGDOL_BI = 0
train$TARGDOL_BI[which(train$TARGDOL>0)] = 1
test$TARGDOL_BI = 0
test$TARGDOL_BI[which(test$TARGDOL>0)] = 1



# Training set 1 is the set for those who have only donated once
train1 = train[train$CNTMLIF==1,]
# Training set 2 is the set for those who have donated twice 
train2 =  train[train$CNTMLIF==2,]
# Training set 3 is the set for those who have donated three times or more 
train3 = train[train$CNTMLIF>=3,]

test1 = test[test$CNTMLIF==1,]
test2 = test[test$CNTMLIF==2,]
test3 = test[test$CNTMLIF>=3,]

# Data Exploration
plot(donate$TARGDOL,donate$CNDOL1)



#============================= Model 1 is for training data set 1=======================================


model_12 <- glm(formula = TARGDOL_BI ~ CNDOL1 + CNMON1+SEX  + cnc1  +sol1,
               family = binomial, data = train1)
summary(model_12)


# Model Chisquare test
qchisq(.95, df=25)  

model_1 <- model_12
#lrtest(model_11,model_12)
train1_predict = predict(model_1,newdata = train1,type = 'response')

tab=table(train1$TARGDOL_BI, train1_predict >0.40)
tab
CCR=sum(diag(tab))/sum(tab)
CCR
library(pROC)
plot.roc(train1$TARGDOL_BI, train1_predict,xlab="1-Specificity")


t = table(train1$TARGDOL_BI, p)

# Creating probability List
s = c("test1","test2","test3")
df = data.frame(s)

# Model 1 Validation

test1_predict = predict(model_1,newdata = test1,type = 'response')
probability$test1 = test1_predict 
tab_test1=table(test1$TARGDOL_BI, test1_predict >0.4)
tab_test1
CCR_test1=sum(diag(tab_test1))/sum(tab_test1)
CCR_test1

# Modle 2 Constructing========================================================


model_2 <- glm(formula = TARGDOL_BI ~ CNDOL1 + I(CNDOL1* CNDOL2) +CONLARG + CONTRFST + SEX +
                  CNMON1 +sol2 , 
                family = binomial(logit), data = train2)
summary(model_2)

# Model 2 training set test
train2_predict = predict(model_2,newdata = train2,type = 'response')
train2_predict
library(pROC)
plot.roc(train2$TARGDOL_BI, train2_predict,xlab="1-Specificity")
tab_train2=table(train2$TARGDOL_BI, train2_predict >0.4)
tab_train2
CCR_2=sum(diag(tab_train2))/sum(tab_train2)
CCR_2

# Model 2 Validation Test
test2_predict = predict(model_2,newdata = test2,type = 'response')
tab_test2=table(test2$TARGDOL_BI, test2_predict >0.4)
tab_test2
CCR_test2=sum(diag(tab_test2))/sum(tab_test2)
CCR_test2

# =======================Model 3===========================================
model_3 <- glm(formula = TARGDOL_BI ~ CNDOL1 + CNDOL2+ CNDOL3+ I(CNDOL1*CNDOL3) +I(CNDOL2*CNDOL3)+CNTMLIF
                +CONLARG +cnc1 +sol3+cnc2+ cnc3+CNMON1+CNMON2+CNMON3 , 
               family = binomial(logit), data = train3)
summary(model_3)

# Model 3 Training Test 
train3_predict = predict(model_3,newdata = train3,type = 'response')
train3_predict

library(pROC)
plot.roc(train3$TARGDOL_BI, train3_predict,xlab="1-Specificity")
tab_train3=table(train3$TARGDOL_BI, train3_predict >0.45)
tab_train3
CCR_train3=sum(diag(tab_train3))/sum(tab_train3)
CCR_train3

# Model e Validation Test
test3_predict = predict(model_3,newdata = test3,type = 'response')
tab_test3=table(test3$TARGDOL_BI, test3_predict >0.45)
tab_test3
CCR_test3=sum(diag(tab_test3))/sum(tab_test3)
CCR_test3


save(test1_predict,file = "C:/Users/zgscsljason/Google Drive/Northwestern/Predictive Analytics/Project/test1P.RData")
save(test2_predict,file = "C:/Users/zgscsljason/Google Drive/Northwestern/Predictive Analytics/Project/test2P.RData")
save(test3_predict,file = "C:/Users/zgscsljason/Google Drive/Northwestern/Predictive Analytics/Project/test3P.RData")
