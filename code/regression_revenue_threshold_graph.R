rm(list=ls())

#read data
raw_data<-readRDS("./data/loan_dataset3.Rda")


#=========================imputation=================================
#delete NA rows, they only share 10% in either of the set class=0 and 1

#x<-raw_data[which(complete.cases(raw_data)==FALSE),]
#raw_data<-raw_data[which(complete.cases(raw_data)==TRUE),]
nrow(raw_data)
raw_data<-na.omit(raw_data)

#====================construct train/test data set===================
#basic idea:   3:7 in both class=1 and 0.
#              training set should has more class=0, testing set should more realistic
#training set: 7/10 of class=0 combine with sampling from class=1
#              ratio of class=1 and 0 is 5:5
#testing set:  3/7 of the class=0 combine with sampling from class=1
#              ratio of class=1 and 0 is 0.019

# subset(raw_data,class==1)
# raw_data[raw_data$class==1,]
# raw_data[which(raw_data$class==1),]
# which(raw_data$class==1) =>integer
# raw_data$class==1 => factor

#split data(good/bad)
raw_bad<-subset(raw_data,class==0)
raw_good<-subset(raw_data,class==1)
#split bad class 7/3
id<-sample(nrow(raw_bad), floor(nrow(raw_bad) *0.7))
train_bad <- raw_bad[id,]
test_bad <- raw_bad[-id,]
nrow(raw_good)/nrow(raw_bad)
nrow(raw_good)

#========================graph for income-ratio===================
###################################################################

plot(NA,type="l",xlim=c(0,1),ylim = c(0,1),xlab="threshold",ylab="revenue",main="threshold-income")
#ratio_list<-c(0.1,1,10,20,51)
ratio_list<-c(0.1,1,10,51)
abline(0,0,lty=2)
legend("bottomright", as.character(ratio_list),fill=ratio_list*100/20+5,cex=0.6,title="ratio(good:bad)")

#=====================different test set ratio


for(q in ratio_list){
ratio<-q


#split good class => test/train
id<-sample(nrow(raw_good), size=nrow(train_bad))
train_good <- raw_good[id,]
test_good <- raw_good[-id,]
train_data<-rbind(train_good,train_bad)

#test_good class ratio control
id<-sample(nrow(test_good), size=nrow(test_bad)*ratio)
test_good<-test_good[id,]

#mix good&bad for train&test
test_data<-rbind(test_bad,test_good)


#3000 int_rate,term,total_acc,grade,dti
#2000 installment
#1000 application_type(Individual Joint App),verification_status0.055,open_acc
#addr_state0.019,emp_length0.02,home_ownership

#===================prepare data set
train_numeric2<-subset(train_data,
                       select=c(int_rate,term,total_acc,installment,dti,grade
                                ,application_type,open_acc,home_ownership
                                ,class))
test_numeric2<-subset(test_data,
                      select=c(int_rate,term,total_acc,installment,dti,grade
                               ,application_type,open_acc,home_ownership
                               ,class))


#==========================learn svm
library(MASS)
#qda remove home_ownership
qdafit<-qda(class~int_rate*grade+term+total_acc*open_acc+installment+dti
            +application_type,data=train_numeric2) 

qdafit<-qda(class ~ loan_amnt+int_rate+grade+annual_inc+dti+
              application_type+pub_rec_bankruptcies,data=train_data) 
#==========================train&test svm&graph
glm_train<-train_numeric2
glm_test<-test_numeric2

#temp<-predict(qdafit,subset(glm_test,select=-c(class,home_ownership)), type="response")
temp<-predict(qdafit,subset(test_data,select=-c(class)), type="response")

glm_test$probs<-temp$posterior[,2] 



threshold_list<-list()
revenue_list<-list()
accuracy_list<-list()
precision_list<-list()
specificity_list<-list()
f<-list()
recall_list<-list()
j<-1
for(i in seq(0.01,0.99,by=0.05)){
  threshold<-i
  glm_test$predclass<-as.numeric(glm_test$probs>=threshold)
  
  matrix<-table(pre=glm_test$predclass,gt=glm_test$class)
  matrix
  test_FP<-test_data[test_data$class==0 & glm_test$predclass==1,]
  test_TP<-test_data[which(test_data$class==1 & glm_test$predclass==1),]
  revenue<-(sum(test_TP$loan_amnt*test_TP$int_rate/100)-sum(test_FP$loan_amnt))/(sum(test_TP$loan_amnt)+sum(test_FP$loan_amnt))
  #points(threshold,revenue)
  accuracy_list[j]<-(matrix[2,2]+matrix[1,1])/sum(matrix)
  precision_list[j]<-(matrix[2,2])/(matrix[1,2]+matrix[2,2])
  specificity_list[j]<-(matrix[1,1])/(matrix[1,1]+matrix[2,1])
  threshold_list[j]<-threshold
  revenue_list[j]<-revenue
  recall_list[j]<-(matrix[2,2])/(matrix[2,1]+matrix[2,2])
  f[j]<-2/(1/precision_list[[j]]+1/recall_list[[j]])
  j<-j+1
}


#nrow(test_TP)
#nrow(test_FP)
#plot(threshold,revenue,xlim=c(0,1),ylim=c(7.0e+8,8.0e+10))

library("fields")
x<-spline(threshold_list,f)
lines(x,type="l",col=q*100/20+5)

}

#inital revenue for bank
(sum(raw_good$loan_amnt*(1+raw_good$int_rate/100))-sum(raw_data$loan_amnt))/sum(raw_data$loan_amnt)
#accuracy of bank
nrow(test_good)/nrow(test_data)


#========================graph for income===================
###################################################################

plot(NA,type="l",xlim=c(0,1),ylim = c(-0.6e+5,6e+7),xlab="threshold",ylab="revenue",main="threshold-income")
ratio_list<-c(10)
abline(0,0,lty=2)
legend("topright", as.character(ratio_list),fill=ratio_list*100/20+2,cex=0.6,title="ratio(good:bad)")

#=====================different test set ratio


for(q in ratio_list){
  ratio<-q
  
  #split good class => test/train
  id<-sample(nrow(raw_good), size=nrow(train_bad))
  train_good <- raw_good[id,]
  test_good <- raw_good[-id,]
  train_data<-rbind(train_good,train_bad)
  
  #test_good class ratio control
  id<-sample(nrow(test_good), size=nrow(test_bad)*ratio)
  test_good<-test_good[id,]
  
  #mix good&bad for train&test
  test_data<-rbind(test_bad,test_good)
  
  
  #3000 int_rate,term,total_acc,grade,dti
  #2000 installment
  #1000 application_type(Individual Joint App),verification_status0.055,open_acc
  #addr_state0.019,emp_length0.02,home_ownership
  
  #===================prepare data set
  train_numeric2<-subset(train_data,
                         select=c(int_rate,term,total_acc,installment,dti,grade
                                  ,application_type,open_acc,home_ownership
                                  ,class))
  test_numeric2<-subset(test_data,
                        select=c(int_rate,term,total_acc,installment,dti,grade
                                 ,application_type,open_acc,home_ownership
                                 ,class))
  
  
  #==========================learn svm
  library(MASS)
  #qda remove home_ownership
  qdafit<-qda(class~int_rate*grade+term+total_acc*open_acc+installment+dti
              +application_type,data=train_numeric2) 
  
  #==========================train&test svm&graph
  glm_train<-train_numeric2
  glm_test<-test_numeric2
  
  temp<-predict(qdafit,subset(glm_test,select=-c(class,home_ownership)), type="response")
  glm_test$probs<-temp$posterior[,2] 
  
  
  threshold_list<-list()
  revenue_list<-list()
  j<-1
  for(i in seq(0,1,by=0.05)){
    threshold<-i
    glm_test$predclass<-as.numeric(glm_test$probs>=threshold)
    matrix<-table(pre=glm_test$predclass,gt=glm_test$class)
    test_FP<-test_data[test_data$class==0 & glm_test$predclass==1,]
    test_TP<-test_data[which(test_data$class==1 & glm_test$predclass==1),]
    revenue<-sum(test_TP$loan_amnt*test_TP$int_rate/100)-sum(test_FP$loan_amnt)
    #points(threshold,revenue)
    threshold_list[j]<-threshold
    revenue_list[j]<-revenue
    j<-j+1
  }
  
  
  #nrow(test_TP)
  #nrow(test_FP)
  #plot(threshold,revenue,xlim=c(0,1),ylim=c(7.0e+8,8.0e+10))
  
  library("fields")
  x<-spline(threshold_list,revenue_list)
  lines(x,type="l",col=q*100/20+2)
  
}

#initial bank income
(sum(raw_good$loan_amnt*(1+raw_good$int_rate/100))-sum(raw_data$loan_amnt))/sum(raw_data$loan_amnt)

library(caret)

########################################################
########################################################

