rm(list=ls())

raw_data<-readRDS("./data/loan_dataset_train.Rda")

train_data<-subset(raw_data,select=c(int_rate,total_acc,dti,open_acc,installment))

normalize_fun=function(X) {(X - min(X))/diff(range(X))}
data_rescale<-apply(train_data, MARGIN = 2, normalize_fun)

km2fit<-kmeans(train_data,5)
clusters_num <- as.factor(km2fit$cluster)   #convert to factor, because: ggplot color=[factor]
train_data2<-cbind(train_data,clusters_num)

library("ggplot2")
id<-sample(nrow(cor_impvar),size=500)
ggplot(train_data, aes(int_rate, dti, color = clusters_num)) + geom_point() #color here is a vector
ggplot(raw_data, aes(int_rate, dti, color = raw_data$class)) + geom_point() #color here is a vector
ggplot(train_data, aes(open_acc,installment, color = clusters_num)) + geom_point() #color here is a vector
ggplot(raw_data, aes(open_acc,installment, color = raw_data$class)) + geom_point() #color here is a vector

ggplot(train_data, aes(total_acc,installment, color = clusters_num)) + geom_point() #color here is a vector
ggplot(raw_data, aes(total_acc,installment, color = raw_data$class)) + geom_point() #color here is a vector

ggplot(train_data, aes(int_rate, dti, color = clusters_num)) + geom_point() #color here is a vector


pairs.panels(train_data2, scale = FALSE, bg = c("red","blue")[train_data2$clusters_num],  
             pch = 21, main = "Correlation Matrix of Data")

