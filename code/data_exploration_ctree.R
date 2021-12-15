rm(list=ls())

train_data<-readRDS("./data/loan_dataset_train.Rda")
test_data<-readRDS("./data/loan_dataset_test.Rda")

nrow(test_data[which(test_data$class==1),])


names(train_data)
summary(train_data)
for(i in 1:ncol(cor_impvar)){
  cat(names(cor_impvar)[i])
  print(class(cor_impvar[1,i]))
}
library("party")
names(train_data)

#use ctree to see important variables
#exclude loan_state: it's already the result
#exclude "character"  "yearmon"
data_exclude<-subset(train_data,select=-c(earliest_cr_line,emp_title,loan_status,issue_d))
names(data_exclude)
ct_loan<-ctree(class~.,data_exclude,controls=ctree_control(mincriterion = 0.6,minsplit=2000, minbucket=2000))
plot(ct_loan,main="ctree of patient(default)")
#important variables
#3000 int_rate,term,total_acc,grade,dti
#2000 installment
#1000 application_type(Individual Joint App),verification_status0.055,open_acc
#addr_state0.019,emp_length0.02,home_ownership

#see correlation
#using only important variables
#all vars
cor_impvar<-subset(train_data,select=c(int_rate,term,total_acc,grade,dti,application_type,verification_status,open_acc,
                                 addr_state,emp_length,home_ownership,installment,class))
#p<0.001vars
cor_impvar1<-subset(train_data,select=c(int_rate,term,total_acc,grade,dti,installment,open_acc,
                                  application_type,home_ownership,class))
#p<0.001 & interesting vars
#installment is already in the dti
#addr_state,home_ownership,emp_length,application_type exclude
#loan_amnt,pub_rec_bankruptcies add
cor_impvar2<-subset(train_data,select=c(dti,int_rate,term,total_acc,grade,installment,
                                  open_acc,loan_amnt,pub_rec_bankruptcies,
                                  class))


#final
cor_impvarf<-subset(train_data,select=c(int_rate,term,total_acc,installment,dti,grade
                                        ,application_type,open_acc,home_ownership
                                        ,class))
#class ~ loan_amnt+int_rate+grade+annual_inc+dti+application_type+pub_rec_bankruptcies


################################
##################income of ctree
plot(NA,type="l",xlim=c(0,1),ylim = c(0,0.75),xlab="mincriterion",ylab="accuracy",main="mincriterion-accuracy")
#ratio_list<-c(0.1,1,10,20,51)
cri_list<-seq(0,1,0.1)
bucket_list<-c(100)
abline(0,0,lty=2)
legend("topright", as.character(bucket_list),fill=bucket_list*100/17+3,cex=0.6,title="bucket")


for(j in bucket_list){
value<-list()
count<-1
for(i in cri_list){
my_formula <- class ~ loan_amnt+int_rate+grade+annual_inc+dti+application_type+pub_rec_bankruptcies
ctl1 <- ctree_control(teststat="quad",  minsplit = j, testtype="Teststatistic", maxdepth=30,
                      mincriterion = i, minbucket = j )

# train the decision tree
#loan_train_ctree <- ctree(my_formula, data = train_data)
loan_train_ctree <- ctree(my_formula, data = train_data, controls=ctl1)
#plot(loan_train_ctree)
loan_test_predict <- predict(loan_train_ctree, test_data)
#table(loan_test_predict,test_data$class)
library(zoo)
library(caret)
measure_test<-confusionMatrix(table(loan_test_predict,test_data$class), positive="1")
print(measure_test$overall["Accuracy"])  # 
print(measure_test$byClass["Precision"]) # 
print(measure_test$byClass["Recall"])  # 
print(measure_test$byClass["F1"])     # 0.8456
print(measure_test$byClass["Specificity"])
#calculate revenue
test_FP<-test_data[test_data$class==0 & loan_test_predict==1,]
test_TP<-test_data[which(test_data$class==1 & loan_test_predict==1),]
revenue<-sum(test_TP$loan_amnt*test_TP$int_rate/100)-sum(test_FP$loan_amnt)
print(revenue)
revenue_ratio<-(sum(test_TP$loan_amnt*test_TP$int_rate/100)-sum(test_FP$loan_amnt))/(sum(test_TP$loan_amnt)+sum(test_FP$loan_amnt))
print(revenue_ratio)

value[count]<-measure_test$byClass["Specificity"]
count<-count+1
}
library("fields")
x<-spline(cri_list,value)
lines(x,type="l",col=j*100/17+3)
}

#sample 100
library("psych") 
id<-sample(nrow(train_data),size=100)
names(x)
cor_sam<-subset(data_exclude,select = -c(class))
cor_sam<-cor_sam[id,]
#exclude dti>300 noise
cor_sam<-cor_sam[cor_sam$dti<100,]

#panel
x<-subset(cor_impvar2,select=-c(application_type))
x<-x[id,]
pairs.panels(x, scale = TRUE, bg = c("red","blue")[x$class], 
             pch = 21, main = "Correlation Matrix of Data")

names(x)
nrow(x)
#change ctree para
#exclude dti>300 noise,exclude application
#data_exclude<-data_exclude[data_exclude$dti<100,]
ct_loan<-ctree(class~dti+int_rate+grade+loan_amnt+installment+pub_rec_bankruptcies+annual_inc+application_type
                 ,data_exclude)
plot(ct_loan,main="ctree of patient(default)")
matrix<-table(predict=predict(ct_loan,test_data),ground_truth=test_data$class)
matrix

#one para
ct_loan<-ctree(class ~ loan_amnt+int_rate+grade+annual_inc+dti+application_type+pub_rec_bankruptcies
               ,train_data)
plot(ct_loan,main="ctree of patient(default)")

#test
test_data<-readRDS("./data/loan_dataset_test.Rda")
matrix<-table(predict=predict(ct_loan,test_data),ground_truth=test_data$class)

test_FP<-test_data[test_data$class==0 & predict(ct_loan,test_data)==1,]
test_TP<-test_data[which(test_data$class==1 & predict(ct_loan,test_data)==1),]
revenue<-(sum(test_TP$loan_amnt*(1+test_TP$int_rate/100))-sum(test_data$loan_amnt))/sum(test_data$loan_amnt)


matrix
recall<-(matrix[2,2])/(matrix[1,2]+matrix[2,2])
precision<-(matrix[2,2])/(matrix[2,1]+matrix[2,2])
accuracy<-(matrix[1,1]+matrix[2,2])/sum(matrix)
matrix
recall
precision
accuracy
2/(1/recall+1/precision)

