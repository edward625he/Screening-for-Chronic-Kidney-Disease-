#preprocessing
library(mice)
data=read.csv("casestudydata.csv")
out_sample=which(is.na(data$CKD)==1)
data_out=data[out_sample,]   ## the ones without a disease status
data_in=data[-out_sample,] 
data_in=data_in[,-1]
names(data_in)
new=mice(data_in,method=c("pmm","pmm","pmm","logreg","logreg","logreg","logreg","logreg","pmm","pmm","pmm","logreg","pmm","pmm","pmm","pmm","pmm","pmm","pmm","logreg","pmm","pmm","logreg","logreg","logreg","logreg","logreg","logreg","logreg","logreg","logreg","logreg","logreg"),m=10)
new_data_combination=complete(new,(1+2+3+4+5+6+7+8+9+10)/10)
new_data_combination=read.csv("data_inputation.csv")
model_data=new_data_combination[1:4500,]#model data
test_data=new_data_combination[4501:6000,]#test_data
write.csv(new_data_combination,"data_inputation.csv")
write.csv(model_data,"model_data.csv")
write.csv(test_data,"test_data.csv")
data_out=data_out[,-1]
names(data_out)
new_data=rbind(new_data_combination,data_out)
dim(new_data)

model_data=read.csv("model_data.csv")
test_data=read.csv("test_data.csv")


#logreg-test model
model_data=model_data[,-1]
logistics2=glm(CKD~.,family="binomial",data=model_data)
summary(logistics2)

#feature-selection backward
backward=step(logistics2,direction="backward")

#final model
logistics3=glm(CKD~Age+PVD+Female+Waist+HDL+Activity+Hypertension+Diabetes+CVD+Anemia
               ,family="binomial",data=model_data)
summary(logistics3)
prediction=predict(logistics3,newdata=test_data,type = "response")
CKD_predict=classify
test_data=test_data[,-1]
summary(test_data)

#feature-selection backward
backward=step(model,direction="backward")

#accuracy
classify=ifelse(prediction>.061,1,0)  # this is a threshold, we say if probability >50% , then say "yes"
summary(classify)  # notice that not many are "yes"  - is this desirable?

round(c_accuracy(test_data$CKD,classify),2) 

#loop
c_accuracy=function(actuals,classifications){
  df=data.frame(actuals,classifications);
  
  
  tp=nrow(df[df$classifications==1 & df$actuals==1,]);        
  fp=nrow(df[df$classifications==1 & df$actuals==0,]);
  fn=nrow(df[df$classifications==0 & df$actuals==1,]);
  tn=nrow(df[df$classifications==0 & df$actuals==0,]); 
  
  
  recall=tp/(tp+fn)
  precision=tp/(tp+fp)
  accuracy=(tp+tn)/(tp+fn+fp+tn)
  tpr=recall
  fpr=fp/(fp+tn)
  fmeasure=2*precision*recall/(precision+recall)
  scores=c(recall,precision,accuracy,tpr,fpr,fmeasure,tp,tn,fp,fn)
  names(scores)=c("recall","precision","accuracy","tpr","fpr","fmeasure","tp","tn","fp","fn")
  
  #print(scores)
  return(scores);
}

round(acc,2)
#cost
a1=1300 # award $1300 for true positive
c1=100  # penalize me  $100 for a false positive
# penalize me $200 for a false negatives
threshold <- c(.050,.060, .061, .062, .063, .064, .065,.066,.067,.068,.069,.070,.08)
cost2 <- list()
maximize2 <- list()
for (i in 1:length(threshold)) {
  
  classify <- ifelse(prediction > threshold[i], 1, 0)
  acc <- unlist(c_accuracy(test_data$CKD, classify))
  #cost2 <- append(cost2, (acc[11]*c1 + acc[12]*c2))
  maximize2 <- append(maximize2, (acc[7]*a1 - acc[9]*c1))
  
}
#names(cost2) <- threshold
names(maximize2) <- threshold
maximize2
write.csv(maximize2,"threshold.csv")

#curve
install.packages("ROCR")
library(ROCR)
library(pROC)

ROCRpred = prediction(prediction, test_data$CKD)

ROCRperf = performance(ROCRpred, "tpr", "fpr")
plot(ROCRperf)

plot(ROCRperf, colorize=TRUE)
plot(ROCRperf, colorize=TRUE, print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.2,1.7))
auc(test_data$CKD~prediction, percent=T)

#second-round inputation
new=mice(new_data,method=c("pmm","pmm","pmm","logreg","logreg","logreg","logreg","logreg","pmm","pmm","pmm","logreg","pmm","pmm","pmm","pmm","pmm","pmm","pmm","logreg","pmm","pmm","logreg","logreg","logreg","logreg","logreg","logreg","logreg","logreg","logreg","logreg","logreg"),m=10)
final_data=complete(new,(1+2+3+4+5+6+7+8+9+10)/10)


#prediction
data_inputation=read.csv("data_inputation.csv")
data_CKD=read.csv("final_data_withoutCKD.csv")
summary(data_CKD)
logistics4=glm(CKD~Age+PVD+Female+Waist+HDL+Activity+Hypertension+Diabetes+CVD+Anemia
               ,family="binomial",data=data_inputation)
summary(logistics4)
prediction2=predict(logistics4,newdata=data_CKD,type = "response")
summary(prediction2)
classify2=ifelse(prediction2>.061,1,0)  # this is a threshold, we say if probability >50% , then say "yes"
CKD_new=classify2
probability=prediction2
test_data_new=cbind(data_CKD,CKD_new,probability)
summary(test_data_new)
write.csv(test_data_new,"final24-.csv")

#new curve
library(ROCR)
ROCRpred = prediction(prediction, test_data$CKD)
ROCRperf = performance(ROCRpred, "tpr", "fpr")
plot(ROCRperf)
plot(ROCRperf, colorize=TRUE, text.adj=c(-0.2,1.7))

