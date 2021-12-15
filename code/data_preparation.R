rm(list=ls())

#read data
raw_data<-readRDS("./data/loan_dataset3.Rda")
summary(raw_data)
names(raw_data)
for(i in 1:ncol(raw_data)){
  cat(names(raw_data)[i])
  print(class(raw_data[1,i]))
}


#=========================imputation=================================
#delete NA rows, they only share 10% in either of the set class=0 and 1

#x<-raw_data[which(complete.cases(raw_data)==FALSE),]
#raw_data<-raw_data[which(complete.cases(raw_data)==TRUE),]
nrow(raw_data)
raw_data<-na.omit(raw_data)
#test
nrow(raw_data)
nrow(x[which(x$class==0),])
nrow(raw_data[which(raw_data$class==0),])

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
nrow(train_bad)
nrow(test_bad)
#split good class => test/train
id<-sample(nrow(raw_good), size=nrow(train_bad))
train_good <- raw_good[id,]
test_good <- raw_good[-id,]
ratio<-nrow(raw_bad)/nrow(raw_good)

id<-sample(nrow(test_good), size=nrow(test_bad)*10)
#id<-sample(nrow(test_good), size=nrow(test_bad)*0.1)

test_good<-test_good[id,]
#nrow(test_good)
#nrow(test_bad)
#mix good&bad for train&test
train_data<-rbind(train_good,train_bad)
test_data<-rbind(test_bad,test_good)
#test
dim(test_bad)
dim(test_good)
dim(test_data)


saveRDS(train_data,"./data/loan_dataset_train.Rda")
saveRDS(test_data,"./data/loan_dataset_test_10.Rda")

is.complete(train_data)