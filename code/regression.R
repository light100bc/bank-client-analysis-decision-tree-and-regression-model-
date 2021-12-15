rm(list=ls())

train_data<-readRDS("./data/loan_dataset_train.Rda")
test_data<-readRDS("./data/loan_dataset_test_0.1.Rda")
summary(test_data)
names(test_data)
for(i in 1:ncol(test_data)){
  cat(names(train_data)[i])
  print(class(train_data[1,i]))
}

#t/t date set
#all numeric
train_numeric2<-subset(train_data,
                      select=c(loan_amnt,int_rate,dti,grade
                               ,pub_rec
                               ,tot_cur_bal,pub_rec_bankruptcies,chargeoff_within_12_mths
                               ,class))
test_numeric2<-subset(test_data,
                     select=c(loan_amnt,int_rate,dti,grade
                              ,pub_rec
                              ,tot_cur_bal,pub_rec_bankruptcies,chargeoff_within_12_mths
                              ,class))

#kim
train_numeric2<-subset(train_data,
                      select=c(loan_amnt,int_rate,dti,grade
                               ,annual_inc
                               ,application_type,pub_rec_bankruptcies
                               ,class))
test_numeric2<-subset(test_data,
                     select=c(loan_amnt,int_rate,dti,grade
                              ,annual_inc
                              ,application_type,pub_rec_bankruptcies
                              ,class))

#from tree(important vars)
train_numeric2<-subset(train_data,
                       select=c(int_rate,term,total_acc,installment,dti,grade
                                ,class))
test_numeric2<-subset(test_data,
                      select=c(int_rate,term,total_acc,installment,dti,grade
                               ,class))

#3000 int_rate,term,total_acc,grade,dti
#2000 installment
#1000 application_type(Individual Joint App),verification_status0.055,open_acc
#addr_state0.019,emp_length0.02,home_ownership
train_numeric2<-subset(train_data,
                       select=c(int_rate,term,total_acc,installment,dti,grade
                                ,application_type,open_acc,home_ownership
                                ,class))
test_numeric2<-subset(test_data,
                      select=c(int_rate,term,total_acc,installment,dti,grade
                               ,application_type,open_acc,home_ownership
                               ,class))

#learn
logfit<-glm(class~int_rate*grade+term+total_acc*open_acc+installment+dti
            +application_type+home_ownership,data=train_numeric2,family=binomial)
#learn svm
library(MASS)
#qda remove home_ownership
qdafit<-qda(class~int_rate*grade+term+total_acc*open_acc+installment+dti
            +application_type,data=train_numeric2) 

ldafit<-lda(class~int_rate*grade+term+total_acc*open_acc+installment+dti
            +application_type+home_ownership,data=train_numeric2) 

qdafit
#summary(logfit)
#names(glm_train)


#test
glm_train<-train_numeric2
glm_test<-test_numeric2

glm_train$probs<-predict(logfit,subset(glm_train,select=-c(class)),type="response")
glm_train$predclass<-as.numeric(glm_train$probs>=0.5)
matrix<-table(pre=glm_train$predclass,gt=glm_train$class)

glm_test$probs<-predict(logfit,subset(glm_test,select=-c(class)),type="response")
glm_test$predclass<-as.numeric(glm_test$probs>=0.5)
matrix<-table(pre=glm_test$predclass,gt=glm_test$class)


#test svm
glm_train<-train_numeric2
glm_test<-test_numeric2

temp<-predict(ldafit,subset(glm_train,select=-c(class)), type="response")

temp<-predict(qdafit,subset(glm_train,select=-c(class)), type="response") 

glm_train$probs<-temp$posterior[,2] 
glm_train$preclass<-as.numeric(glm_train$probs>=0.5)
matrix<-table(pre=glm_train$preclass,gt=glm_train$class)


temp<-predict(ldafit,subset(glm_test,select=-c(class)), type="response") 

temp<-predict(qdafit,subset(glm_test,select=-c(class)), type="response")

glm_test$probs<-temp$posterior[,2] 
glm_test$predclass<-as.numeric(glm_test$probs>=0.6)
matrix<-table(pre=glm_test$predclass,gt=glm_test$class)
length(which(glm_test$predclass==1))

#matrix
matrix
recall<-(matrix[2,2])/(matrix[1,2]+matrix[2,2])
precision<-(matrix[2,2])/(matrix[2,1]+matrix[2,2])
accuracy<-(matrix[1,1]+matrix[2,2])/sum(matrix)
recall
precision
accuracy
2/(1/recall+1/precision)

#ROC
library("ROCR")
trainROC<-performance(prediction(glm_train$probs,glm_train$class),"tpr","fpr")
plot(trainROC)
abline(a=0, b= 1,lty=2)
trainAUC<-as.double(performance(prediction(glm_train$probs,glm_train$class),"auc")@y.values)
trainAUC

testROC<-performance(prediction(glm_test$probs,glm_test$class),"tpr","fpr")
plot(testROC)
abline(a=0, b= 1,lty=2)
testAUC<-as.double(performance(prediction(glm_test$probs,glm_test$class),"auc")@y.values)
testAUC

#AUC
#glm 0.716/train 0.7206
#lda 0.715/train 0.7198
#qda 0.7066/0.7036 higher recall.low specificity 

#income
train_gooddata<-train_data[which(train_data$class==1),]
train_baddata<-train_data[which(train_data$class==0),]
train_pdgood<-train_data[which(glm_train$predclass==1),]
train_pdbad<-train_data[which(glm_train$predclass==1),]

sum(train_gooddata$loan_amnt*train_gooddata$int_rate)-sum(train_baddata$loan_amnt)
sum(train_pdgood$loan_amnt*train_pdgood$int_rate)-sum(train_pdbad$loan_amnt)


test_gooddata<-test_data[which(test_data$class==1),]
test_baddata<-test_data[which(test_data$class==0),]
test_pdgood<-test_data[which(glm_test$predclass==1),]
test_pdbad<-test_data[which(glm_test$predclass==1),]
test_FP<-test_data[test_data$class==0 & glm_test$predclass==1,]
test_TP<-test_data[which(test_data$class==1 & glm_test$predclass==1),]

sum(test_gooddata$loan_amnt*(1+test_gooddata$int_rate))-sum(test_baddata$loan_amnt)
sum(test_TP$loan_amnt*(1+test_TP$int_rate))-sum(test_FP$loan_amnt)
nrow(test_FP)
matrix
#qda best performance