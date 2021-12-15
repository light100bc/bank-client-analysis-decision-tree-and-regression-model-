library(readr)
accepted_2007_to_2018Q4 <- read_csv("./data/accepted_2007_to_2018Q4.csv")

# delete column
accepted_2007_to_2018Q4$desc=NULL
accepted_2007_to_2018Q4$url=NULL

saveRDS(accepted_2007_to_2018Q4,file="./loan_dataset.Rda")
loan_dataset<-readRDS(file="./loan_dataset.Rda")

dim(loan_dataset)
str(loan_dataset)

library(dplyr)

loan_dataset %>% distinct(loan_status)  
distinct(loan_dataset,loan_status)

# loan_status                                        
# <chr>                                              
# 1 Fully Paid   => 1                                      
# 2 Current    => 1 ?                                        
# 3 Charged Off  => 1                                        
# 4 In Grace Period => 1                                                                           
# 5 Late (31-120 days)=> 0                               
# 6 Late (16-30 days) => 0                             
# 7 Default  => 0                                          
# 8 NA     => delete                                            
# 9 Does not meet the credit policy. Status:Fully Paid  => delete
# 10 Does not meet the credit policy. Status:Charged Off => delete


loan_dataset[loan_dataset$loan_status=="Does not meet the credit policy. Status:Fully Paid",]
loan_dataset$loan_status[loan_dataset$loan_status=="Does not meet the credit policy. Status:Charged Off"]
loan_dataset %>% filter(is.na(loan_status))

loan_dataset %>% filter(is.na(loan_status))
subset(loan_dataset, is.na(loan_dataset$loan_status)==TRUE)

# delete rows
loan_dataset = loan_dataset[!is.na(loan_dataset$loan_status)==TRUE,]
loan_dataset = loan_dataset[!loan_dataset$loan_status=="Does not meet the credit policy. Status:Fully Paid",]
loan_dataset = loan_dataset[!loan_dataset$loan_status=="Does not meet the credit policy. Status:Charged Off",]
loan_dataset = loan_dataset[!loan_dataset$loan_status=="Current",]


# add "class" column
class <- loan_dataset$loan_status
loan_dataset$class <- NULL
loan_dataset <- cbind(loan_dataset, class)
loan_dataset$class <- as.character(loan_dataset$loan_status)

# set class column binary class
loan_dataset$class[loan_dataset$loan_status=="Fully Paid"] = 1
loan_dataset$class[loan_dataset$loan_status=="Charged Off"] = 1
loan_dataset$class[loan_dataset$loan_status=="In Grace Period"] = 1
#loan_dataset$class[loan_dataset$loan_status=="Current"] = 1
loan_dataset$class[loan_dataset$loan_status=="Default"] = 0
loan_dataset$class[loan_dataset$loan_status=="Late (31-120 days)"] = 0
loan_dataset$class[loan_dataset$loan_status=="Late (16-30 days)"] = 0
loan_dataset$class <- as.factor(loan_dataset$class)                                 
table(loan_dataset$loan_status=="Current")

# set column data type
str(loan_dataset$loan_amnt)
str(loan_dataset$grade)
str(loan_dataset$annual_inc)
str(loan_dataset$home_ownership)
str(loan_dataset$term)
str(loan_dataset$issue_d)
str(loan_dataset$int_rate)
str(loan_dataset$dti)
loan_dataset$grade <- as.factor(loan_dataset$grade)
loan_dataset$home_ownership <- as.factor(loan_dataset$home_ownership)
loan_dataset$term <- as.factor(loan_dataset$term)
library(zoo)
loan_dataset$issue_d<-as.yearmon(loan_dataset$issue_d, "%b-%Y")
?as.yearmon

saveRDS(loan_dataset,file="./loan_dataset2.Rda")

column_names <- c("loan_amnt", "funded_amnt", "term", "int_rate", "installment",
                  "grade", "emp_title", "emp_length", "home_ownership", "annual_inc",
                  "verification_status", "issue_d", "loan_status", "purpose", "addr_state",
                  "dti", "delinq_2yrs", "earliest_cr_line", "pub_rec", "open_acc",
                  "total_acc", "application_type", "tot_cur_bal", "pub_rec_bankruptcies", "chargeoff_within_12_mths",
                  "class")

loan<-data.frame(loan_dataset$loan_amnt, loan_dataset$funded_amnt, loan_dataset$term, loan_dataset$int_rate, loan_dataset$installment,
                 loan_dataset$grade, loan_dataset$emp_title, loan_dataset$emp_length, loan_dataset$home_ownership,loan_dataset$annual_inc,
                 loan_dataset$verification_status, loan_dataset$issue_d, loan_dataset$loan_status, loan_dataset$purpose, loan_dataset$addr_state,
                 loan_dataset$dti, loan_dataset$delinq_2yrs, loan_dataset$earliest_cr_line, loan_dataset$pub_rec, loan_dataset$open_acc,
                 loan_dataset$total_acc, loan_dataset$application_type, loan_dataset$tot_cur_bal, loan_dataset$pub_rec_bankruptcies, loan_dataset$chargeoff_within_12_mths,
                 loan_dataset$class)

colnames(loan)<- column_names
str(loan)

saveRDS(loan,file="./loan_dataset3.Rda")

###################################################################################################
# divide into train, test, validation set
table(loan$class)
table(is.na(loan_dataset$purpose))

head(loan)
# divide good/bad loans
loan_bad <- loan[loan$class==0,]
loan_good <- loan[loan$class==1,]

# divide bad loans into 3 parts
loan_bad_sample <- sample(3, nrow(loan_bad), replace = TRUE,prob=c(0.33,0.33,0.33))
loan_bad_sample1 <- loan_bad[loan_bad_sample==1,]
loan_bad_sample2 <- loan_bad[loan_bad_sample==2,] 
loan_bad_sample3 <- loan_bad[loan_bad_sample==3,] 

# divide good loans into 3 parts
loan_good_sample <- sample(3, nrow(loan_good), replace = TRUE,prob=c(0.004,0.30,0.30))
loan_good_sample1 <- loan_good[loan_good_sample==1,]
loan_good_sample2 <- loan_good[loan_good_sample==2,]
loan_good_sample3 <- loan_good[loan_good_sample==3,]

# train/test/validation set
loan_train <- rbind(loan_bad_sample1, loan_good_sample1)
loan_test <- rbind(loan_bad_sample2, loan_good_sample2)
loan_valid <- rbind(loan_bad_sample3, loan_good_sample3)

table(loan_train$class)
table(loan_test$class)
table(loan_valid$class)


#################################################################################################################
loan<-readRDS(file="./loan_dataset3.Rda")
loan$emp_title <- as.character(loan$emp_title)
loan$earliest_cr_line <- as.character(loan$earliest_cr_line)


# decision tree
library(party)
library(caret)
library(zoo)
# set formula
#my_formula <- class ~ loan_amnt+funded_amnt+ term+ int_rate+ installment+ grade+ emp_length+ home_ownership+
#  annual_inc+verification_status+  purpose+ addr_state+ dti+ delinq_2yrs+pub_rec+ open_acc+
#  total_acc+ application_type+ tot_cur_bal+ pub_rec_bankruptcies+ chargeoff_within_12_mths

my_formula <- class ~ loan_amnt+funded_amnt+ term+ int_rate+ installment+ grade+ emp_length+ home_ownership+
  annual_inc+verification_status+  purpose+  dti+ delinq_2yrs+pub_rec+ open_acc+
  total_acc+ application_type+ tot_cur_bal+ pub_rec_bankruptcies+ chargeoff_within_12_mths

my_formula <- class ~ loan_amnt+int_rate+grade+annual_inc+dti+application_type+pub_rec_bankruptcies
  
ctl1 <- ctree_control(teststat="quad",  minsplit = 130, testtype="Teststatistic", maxdepth=30,
                      mincriterion = 0.80, minbucket = 3 )

# train the decision tree
loan_train_ctree <- ctree(my_formula, data = loan_train)
loan_train_ctree <- ctree(my_formula, data = loan_train, controls=ctl1)
plot(loan_train_ctree)
?ctree_control
?confusionMatrix
loan_test_predict <- predict(loan_train_ctree, loan_test)
loan_test_predict
table(loan_test_predict,loan_test$class)
measure_test<-confusionMatrix(table(loan_test_predict,loan_test$class), positive="1")
measure_test$overall["Accuracy"]  # 
measure_test$byClass["Precision"] # 
measure_test$byClass["Recall"]    # 
measure_test$byClass["F1"]        # 0.8456

###################################################################################################
plot(table(loan_dataset$grade))
plot(table(loan_dataset$grade[loan_dataset$class==0]))
plot(table(loan_dataset$grade[loan_dataset$class==1]))
table(loan_dataset$grade)
loan_dataset$issue_d
plot(table(loan$issue_d))
plot(table(loan_dataset$issue_d[loan_dataset$class==0]))

table(is.na(loan_dataset$grade))


###################################################################################################
# just random sampling : no class consideration
sample0 <- sample(3, nrow(loan), replace = TRUE,prob=c(10,10,80))
loan_sample0 <- loan[sample0==1,]
loan_sample1 <- loan[sample0==2,]
loan_sample0_ctree <- ctree(my_formula, data = loan_sample0)
loan_sample_predict <- predict(loan_sample0_ctree, loan_sample1)

table(loan_sample_predict,loan_sample1$class)
measure_test<-confusionMatrix(table(loan_sample_predict,loan_sample1$class), positive="1")
measure_test$overall["Accuracy"]  # 
measure_test$byClass["Precision"] # 
measure_test$byClass["Recall"]    # 
measure_test$byClass["F1"]        # 0.9905
###################################################################################################


?qda
