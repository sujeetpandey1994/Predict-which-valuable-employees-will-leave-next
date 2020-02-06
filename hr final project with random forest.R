setwd("C:\\Users\\Sujeet\\Documents\\learning")
getwd()
train=read.csv("hr_train.csv",stringsAsFactors = F)
test=read.csv("hr_test.csv",stringsAsFactors = F)
library(dplyr)
glimpse(train)
table(train$left)
test$left=NA
train$data='train'
test$data='test'
all=rbind(train,test)
glimpse(all)
table(all$sales)
table(all$salary)

CreateDummies=function(data,var,freq_cutoff=0){
  t=table(data[,var])
  t=t[t>freq_cutoff]
  t=sort(t)
  categories=names(t)[-1]
  
  for( cat in categories){
    name=paste(var,cat,sep="_")
    name=gsub(" ","",name)
    name=gsub("-","_",name)
    name=gsub("\\?","Q",name)
    name=gsub("<","LT_",name)
    name=gsub("\\+","",name)
    name=gsub("\\/","_",name)
    name=gsub(">","GT_",name)
    name=gsub("=","EQ_",name)
    name=gsub(",","",name)
    data[,name]=as.numeric(data[,var]==cat)
  }
  
  data[,var]=NULL
  return(data)
}

#create dummy variable 

all=CreateDummies(all ,"salary",50)
all=CreateDummies(all ,"sales",50)


# cahnge variable to factor 
all$promotion_last_5years = as.factor(all$promotion_last_5years)
all$Work_accident = as.factor(all$Work_accident)
all$left=as.factor(all$left)

#seprate data 

train=all %>% filter(data=='train') %>% select(-data)
test=all %>% filter(data=='test') %>% select (-data,-left )

set.seed(123)
s=sample(1:nrow(train),0.80*nrow(train))
train1=train[s,]
train2=train[-s,]

library(car)
for_vif=lm(left~.,data=train1)
sort(vif(for_vif),decreasing = T)[1:3]

library(randomForest)

fit_hr= randomForest(as.factor(left)~.,data=train)

fit_hr
importance(fit_hr)
varImpPlot(fit_hr)

score=predict(fit_hr,newdata= test, type="prob")[,2]
write.csv(score,'hr_ train.csv',row.names = F)

