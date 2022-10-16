load("data/r0913_tbss.RData")

##########################################################################
##########################################################################
########externalizing persistent & improving group in ABCD################
##########################################################################
##########################################################################

id<-unique(data_cbcls_tbss$src_subject_id)
index1<-data_cbcls_tbss[data_cbcls_tbss$visit==0&data_cbcls_tbss$cbcl_scr_syn_external_t>60,]$src_subject_id
index2<-data_cbcls_tbss[data_cbcls_tbss$visit==12&data_cbcls_tbss$cbcl_scr_syn_external_t>60,]$src_subject_id
index3<-data_cbcls_tbss[data_cbcls_tbss$visit==24&data_cbcls_tbss$cbcl_scr_syn_external_t>60,]$src_subject_id
index4<-data_cbcls_tbss[data_cbcls_tbss$visit==36&data_cbcls_tbss$cbcl_scr_syn_external_t>60,]$src_subject_id
index_ext<-intersect(index1,intersect(index2,intersect(index3,index4)))
data01<-data_cbcls_tbss[data_cbcls_tbss$src_subject_id%in%index_ext,]
data01$class<-"allhigh_ex"
data03<-data_cbcls_tbss[data_cbcls_tbss$src_subject_id%in%index1[!index1%in%index_ext],]
data03$class<-"high_ex"

index1<-data_cbcls_tbss[data_cbcls_tbss$visit==0&data_cbcls_tbss$cbcl_scr_syn_external_t<60,]$src_subject_id
index2<-data_cbcls_tbss[data_cbcls_tbss$visit==12&data_cbcls_tbss$cbcl_scr_syn_external_t<60,]$src_subject_id
index3<-data_cbcls_tbss[data_cbcls_tbss$visit==24&data_cbcls_tbss$cbcl_scr_syn_external_t<60,]$src_subject_id
index4<-data_cbcls_tbss[data_cbcls_tbss$visit==36&data_cbcls_tbss$cbcl_scr_syn_external_t<60,]$src_subject_id
index_ext1<-intersect(index1,intersect(index2,intersect(index3,index4)))
data02<-data_cbcls_tbss[data_cbcls_tbss$src_subject_id%in%index_ext1,]
data02$class<-"alllow_ex"
data04<-data_cbcls_tbss[data_cbcls_tbss$src_subject_id%in%index1[!index1%in%index_ext1],]
data04$class<-"low_ex"



data00<-rbind(data01,data02,data03,data04)
data00$class<-factor(data00$class,levels = c("alllow_ex","low_ex","high_ex","allhigh_ex"),labels = c("healthy","increasing","decreasing","persistent"))
table(data00$class)/4
data00$visit<-data00$visit/12+10
data00<-data00[,-36]
index<-unique(data00$src_subject_id)

smri<-read.table("data/abcd_smrip10201.txt",header = T,stringsAsFactors = F)
qc<-read.table("data/abcd_imgincl01.txt",header = T,stringsAsFactors = F)
smri_qc<-merge(smri,qc,by = c("src_subject_id","eventname"))
table(smri$eventname)
smri_base<-smri_qc[smri_qc$eventname=="baseline_year_1_arm_1",]
smri_2year<-smri_qc[smri_qc$eventname=="2_year_follow_up_y_arm_1",]
table(smri_base$imgincl_t1w_include)
smri_base<-smri_base[smri_base$imgincl_t1w_include==1,]

dmri1<-read.table("data/abcd_dmdtifp101.txt",header = T,stringsAsFactors = F)
dmri1<-dmri1[,c(-1:-4,-6:-7,-(ncol(dmri1)-2):-ncol(dmri1))]
dmri2<-read.table("data/abcd_dmdtifp201.txt",header = T,stringsAsFactors = F)
dmri2<-dmri2[,c(-1:-4,-6:-7,-(ncol(dmri2)-2):-ncol(dmri2))]
dmri<-merge(dmri1,dmri2,by = c("src_subject_id","eventname","sex"))
dmri_qc<-merge(dmri,qc,by = c("src_subject_id","eventname"))
table(dmri_qc$eventname)
dmri_base<-dmri_qc[dmri_qc$eventname=="baseline_year_1_arm_1",]
dmri_2year<-dmri_qc[dmri_qc$eventname=="2_year_follow_up_y_arm_1",]
table(dmri_base$imgincl_dmri_include)
dmri_base<-dmri_base[dmri_base$imgincl_dmri_include==1,]
index_qc<-intersect(dmri_base$src_subject_id,intersect(smri_base$src_subject_id,index))
data_predict<-cbind(data00[match(index_qc,data00$src_subject_id),],smri_base[match(index_qc,smri_base$src_subject_id),],
                          dmri_base[match(index_qc,dmri_base$src_subject_id),])
df_tem<-as.data.frame(cbind(data_predict$nihtbx_fluidcomp_fc,data_predict$smri_thick_cdk_mean))
df_tem<-na.omit(df_tem)
cor.test(df_tem[,1],df_tem[,2])
library(nnet)
data_predict<-cbind(data_predict,class.ind(data_predict$sex)[,-1],class.ind(data_predict$household.income)[,-1],class.ind(data_predict$site_id_l)[,-1],class.ind(data_predict$race_ethnicity)[,-1])
colnames(data_predict)[1755:1757]<-c("sex","income1","income2")

num_cov<-c(46,15,29,1755:1782)
num_cbcl<-8
num_tbss<-40
num_smri_total<-c(163,377,484)
num_thick<-57:124
num_area<-271:338
num_vol<-378:445
num_dti<-553:720
num_sub<-883:1166

data_predict_select<-data_predict[,c(num_cov,num_tbss,num_smri_total,num_thick,num_area,num_vol,num_dti,num_sub)]
data_predict_select<-data_predict[,c(num_cov,num_tbss,num_cbcl,num_smri_total,num_thick,num_area,num_vol)]

#demographics
df<-data_predict[data_predict$class=="decreasing"|data_predict$class=="persistent",]
aggregate(df$interview_age,list(df$class),mean)
aggregate(df$interview_age,list(df$class),sd)
t.test(df[df$class=="persistent",]$interview_age,df[df$class=="decreasing",]$interview_age)
table(df$sex,df$class)
chisq.test(matrix(table(df$sex,df$class),nrow = 2)[,3:4])
table(df$household.income,df$class)
chisq.test(matrix(table(df$household.income,df$class),nrow = 2)[,4:6])
table(df$famhx_ss_momdad_hspd_p,df$class)
chisq.test(matrix(table(df$famhx_ss_momdad_hspd_p,df$class),nrow = 2)[,3:4])
aggregate(df$cbcl_scr_syn_external_t,list(df$class),mean)
aggregate(df$cbcl_scr_syn_external_t,list(df$class),sd)
t.test(df[df$class=="persistent",]$cbcl_scr_syn_external_t,df[df$class=="decreasing",]$cbcl_scr_syn_external_t)
aggregate(df$cbcl_scr_syn_internal_t,list(df$class),mean)
aggregate(df$cbcl_scr_syn_internal_t,list(df$class),sd)
t.test(df[df$class=="persistent",]$cbcl_scr_syn_internal_t,df[df$class=="decreasing",]$cbcl_scr_syn_internal_t)
meanx<-function(x) mean(x,na.rm=T)
sdx<-function(x) sd(x,na.rm=T)
aggregate(df$nihtbx_fluidcomp_fc,list(df$class),meanx)
aggregate(df$nihtbx_fluidcomp_fc,list(df$class),sdx)
t.test(df[df$class=="persistent",]$nihtbx_fluidcomp_fc,df[df$class=="decreasing",]$nihtbx_fluidcomp_fc)



library(caret)
library(pROC)
library(nricens)
#predict decreasing and persistent
df<-data_predict_select[data_predict_select$class=="decreasing"|data_predict_select$class=="persistent",]
df$class <- make.names(df$class,unique = FALSE, allow_ = TRUE)
df$class <- as.factor(df$class)
newdata1 <- df[,-1]
set.seed(1234)
Process = preProcess(newdata1,method = 'knnImpute')
#These variables have zero variances: site22
newdata1<-newdata1[,-26]
Process = preProcess(newdata1,method = 'knnImpute')
newdata = predict(Process, newdata1)
newdata<-cbind(df$class,newdata)
colnames(newdata)[1]<-"class"
set.seed(1234)
datTrain <- upSample(newdata[,-1],newdata[,1],yname = "class")
table(datTrain$class)
# modeling and plot ROC curve
trctrl <- trainControl(method = "repeatedcv",
                       number = 10,
                       repeats =3,
                       classProbs = TRUE,
                       summaryFunction = twoClassSummary)
set.seed(1234)
index<-createFolds(datTrain$class,k=5,list = F)
rsq<-data.frame()
glm.probs1<-data.frame()
glm.probs2<-data.frame()
for(i in 1:5){
  df_train<-datTrain[index!=i,]
  df_test<-datTrain[index==i,]
  
  set.seed(1234)
  glmboost_fit1 <- train(class ~ ., data = df_train,
                         method = "glmboost",
                         metric = "ROC",
                         preProcess = c("center", "scale"),
                         #tuneGrid = grid,
                         trControl = trctrl
                         
  )
  glm.probs1 = rbind(glm.probs1,predict(glmboost_fit1,df_test[,!names(df_test) %in% c("class")],type = "prob"))
  glm.ROC = roc(response = df_test[,c("class")],predictor = predict(glmboost_fit1,df_test[,!names(df_test) %in% c("class")],type = "prob")$decreasing,levels = levels(df_test[,c("class")]))
  plot(glm.ROC,type = "S",col = "red")
  rsq[1,i]<-glm.ROC[["auc"]]
  print(date())
  
  set.seed(1234)
  glmboost_fit2 <- train(class ~ ., data = df_train[,-30],
                         method = "glmboost",
                         metric = "ROC",
                         preProcess = c("center", "scale"),
                         #tuneGrid = grid,
                         trControl = trctrl
                         
  )
  glm.probs2 = rbind(glm.probs2,predict(glmboost_fit2,df_test[,!names(df_test) %in% c("class")],type = "prob"))
  glm.ROC = roc(response = df_test[,c("class")],predictor = predict(glmboost_fit2,df_test[,!names(df_test) %in% c("class")],type = "prob")$decreasing,levels = levels(df_test[,c("class")]))
  plot(glm.ROC,type = "S",col = "red")
  rsq[2,i]<-glm.ROC[["auc"]]
  print(date())
}
a<-as.numeric(df_test$class)-1
a<-c(a,a,a,a,a)
NRIb = nribin( event = a, p.std = glm.probs2[,2], p.new = glm.probs1[,2], updown = 'category', cut=0.5,niter =1000)
rsq[,6]<-rowMeans(rsq)
View(rsq)

#plot importance figure
coef_glm<-list()
library(mboost)
for(i in 1:5){
  df_train<-datTrain[index!=i,]
  df_test<-datTrain[index==i,]
  
  set.seed(1234)
  enet_fit <- train(class ~ ., data = df_train,
                    method = "glmboost",
                    metric = "ROC",
                    preProcess = c("center", "scale"),
                    #tuneGrid = grid,
                    trControl = trctrl
                    
  )
  
  y<-df_train[,ncol(df_train)]
  x<-df_train[,-ncol(df_train)]
  x<-as.matrix(x)
  x<-scale(x)
  fit.glmboost<-glmboost(y=y,x=x,family =  Binomial(),control = boost_control(mstop = enet_fit[["finalModel"]][[".org.mstop"]]))
  aaaa<-coef(fit.glmboost)
  aaaa[order(abs(aaaa),decreasing = T)]
  coef_glm[[i]]<-aaaa[order(abs(aaaa),decreasing = T)]
}

pre_var<-union(union(union(union(names(coef_glm[[1]]),names(coef_glm[[2]])),names(coef_glm[[3]])),names(coef_glm[[4]])),names(coef_glm[[5]]))
data_var<-as.data.frame(matrix(NA,nrow = 5,ncol = length(pre_var)))
colnames(data_var)<-pre_var
for (i in 1:5) {
  for(j in 1:length(coef_glm[[i]])){
    for(k in 1:length(pre_var)){
      if(names(coef_glm[[i]])[j]==colnames(data_var)[k]){
        data_var[i,k]<-coef_glm[[i]][j]
      }
    }
  }
}
data_var1<-data_var
data_var1[is.na(data_var1)]<-0
#View(data_var1)
data_var1[6,]<-colSums(data_var1[1:5,])
or<-order(abs(data_var1[6,]),decreasing = T)
library(RColorBrewer)
data_var2<-as.matrix(abs(data_var1[1:5,or]))
names(data_var2)<-colnames(data_var1)
par(mfrow=c(1,1),mar=c(10,4,4,2)+2)
barplot(data_var2,names.arg = colnames(data_var1),
        main = "importance",las=2,
        col = colorRampPalette(brewer.pal(9,"Paired"))(5))
legend("topright",c("one","two","three","four","five"),text.col = colorRampPalette(brewer.pal(9,"Paired"))(5))
save.image("result/result0930_prediction_ABCD.RData")


##########################################################################
##########################################################################
########internalizing persistent & improving group in ABCD################
##########################################################################
##########################################################################


id<-unique(data_cbcls_tbss$src_subject_id)
index1<-data_cbcls_tbss[data_cbcls_tbss$visit==0&data_cbcls_tbss$cbcl_scr_syn_internal_t>60,]$src_subject_id
index2<-data_cbcls_tbss[data_cbcls_tbss$visit==12&data_cbcls_tbss$cbcl_scr_syn_internal_t>60,]$src_subject_id
index3<-data_cbcls_tbss[data_cbcls_tbss$visit==24&data_cbcls_tbss$cbcl_scr_syn_internal_t>60,]$src_subject_id
index4<-data_cbcls_tbss[data_cbcls_tbss$visit==36&data_cbcls_tbss$cbcl_scr_syn_internal_t>60,]$src_subject_id
index_int<-intersect(index1,intersect(index2,intersect(index3,index4)))
data01<-data_cbcls_tbss[data_cbcls_tbss$src_subject_id%in%index_int,]
data01$class<-"allhigh_in"
data03<-data_cbcls_tbss[data_cbcls_tbss$src_subject_id%in%index1[!index1%in%index_int],]
data03$class<-"high_in"

index1<-data_cbcls_tbss[data_cbcls_tbss$visit==0&data_cbcls_tbss$cbcl_scr_syn_internal_t<60,]$src_subject_id
index2<-data_cbcls_tbss[data_cbcls_tbss$visit==12&data_cbcls_tbss$cbcl_scr_syn_internal_t<60,]$src_subject_id
index3<-data_cbcls_tbss[data_cbcls_tbss$visit==24&data_cbcls_tbss$cbcl_scr_syn_internal_t<60,]$src_subject_id
index4<-data_cbcls_tbss[data_cbcls_tbss$visit==36&data_cbcls_tbss$cbcl_scr_syn_internal_t<60,]$src_subject_id
index_int1<-intersect(index1,intersect(index2,intersect(index3,index4)))
data02<-data_cbcls_tbss[data_cbcls_tbss$src_subject_id%in%index_int1,]
data02$class<-"alllow_in"
data04<-data_cbcls_tbss[data_cbcls_tbss$src_subject_id%in%index1[!index1%in%index_int1],]
data04$class<-"low_in"



data00<-rbind(data01,data02,data03,data04)
data00$class<-factor(data00$class,levels = c("alllow_in","low_in","high_in","allhigh_in"),labels = c("healthy","increasing","decreasing","persistent"))
table(data00$class)/4
data00$visit<-data00$visit/12+10
data00<-data00[,-36]
index<-unique(data00$src_subject_id)
smri_qc<-merge(smri,qc,by = c("src_subject_id","eventname"))
table(smri$eventname)
smri_base<-smri_qc[smri_qc$eventname=="baseline_year_1_arm_1",]
smri_2year<-smri_qc[smri_qc$eventname=="2_year_follow_up_y_arm_1",]
table(smri_base$imgincl_t1w_include)
smri_base<-smri_base[smri_base$imgincl_t1w_include==1,]

data_predict<-cbind(data00[match(index_qc,data00$src_subject_id),],smri_base[match(index_qc,smri_base$src_subject_id),],
                    dmri_base[match(index_qc,dmri_base$src_subject_id),])
df_tem<-as.data.frame(cbind(data_predict$nihtbx_fluidcomp_fc,data_predict$smri_thick_cdk_mean))
df_tem<-na.omit(df_tem)
cor.test(df_tem[,1],df_tem[,2])
library(nnet)
data_predict<-cbind(data_predict,class.ind(data_predict$sex)[,-1],class.ind(data_predict$household.income)[,-1],class.ind(data_predict$site_id_l)[,-1],class.ind(data_predict$race_ethnicity)[,-1])
colnames(data_predict)[1755:1757]<-c("sex","income1","income2")

#demographics
df<-data_predict[data_predict$class=="decreasing"|data_predict$class=="persistent",]
aggregate(df$interview_age,list(df$class),mean)
aggregate(df$interview_age,list(df$class),sd)
t.test(df[df$class=="persistent",]$interview_age,df[df$class=="decreasing",]$interview_age)
table(df$sex,df$class)
chisq.test(matrix(table(df$sex,df$class),nrow = 2)[,3:4])
table(df$household.income,df$class)
chisq.test(matrix(table(df$household.income,df$class),nrow = 2)[,4:6])
table(df$famhx_ss_momdad_hspd_p,df$class)
chisq.test(matrix(table(df$famhx_ss_momdad_hspd_p,df$class),nrow = 2)[,3:4])
aggregate(df$cbcl_scr_syn_external_t,list(df$class),mean)
aggregate(df$cbcl_scr_syn_external_t,list(df$class),sd)
t.test(df[df$class=="persistent",]$cbcl_scr_syn_external_t,df[df$class=="decreasing",]$cbcl_scr_syn_external_t)
aggregate(df$cbcl_scr_syn_internal_t,list(df$class),mean)
aggregate(df$cbcl_scr_syn_internal_t,list(df$class),sd)
t.test(df[df$class=="persistent",]$cbcl_scr_syn_internal_t,df[df$class=="decreasing",]$cbcl_scr_syn_internal_t)
meanx<-function(x) mean(x,na.rm=T)
sdx<-function(x) sd(x,na.rm=T)
aggregate(df$nihtbx_fluidcomp_fc,list(df$class),meanx)
aggregate(df$nihtbx_fluidcomp_fc,list(df$class),sdx)
t.test(df[df$class=="persistent",]$nihtbx_fluidcomp_fc,df[df$class=="decreasing",]$nihtbx_fluidcomp_fc)

##could not improve NRI


##########################################################################
##########################################################################
########externalizing persistent & improving group in BNU#################
##########################################################################
##########################################################################

library(readxl)
mydata<-read_xlsx("data/data_base_1year.xlsx")
mydata<-as.data.frame(mydata)
for (i in 1:nrow(mydata)) {
  for(j in 1:ncol(mydata))
    if(!is.na(mydata[i,j])&mydata[i,j]==999)
      mydata[i,j]<-NA
}
mydata$Gender<-factor(mydata$Gender,levels = c(0,1),labels = c("boy","girl"))

id<-unique(mydata$User.code)
index1<-mydata[mydata$visit==0&mydata$Exterlizing>7,]$User.code
index2<-mydata[mydata$visit==1&mydata$Exterlizing>7,]$User.code
mydata1<-mydata[mydata$User.code%in%intersect(index1,index2),]
mydata1$class<-"persistent"
mydata2<-mydata[mydata$User.code%in%index1[!index1%in%intersect(index1,index2)],]
mydata2$class<-"decreasing"

index1<-mydata[mydata$visit==0&mydata$Exterlizing<7,]$User.code
index2<-mydata[mydata$visit==1&mydata$Exterlizing<7,]$User.code
mydata3<-mydata[mydata$User.code%in%intersect(index1,index2),]
mydata3$class<-"healthy"
mydata4<-mydata[mydata$User.code%in%index1[!index1%in%intersect(index1,index2)],]
mydata4$class<-"increasing"

mydata0<-rbind(mydata1,mydata2,mydata3,mydata4)
mydata0$class<-factor(mydata0$class,levels = c("healthy","increasing","decreasing","persistent"),labels = c("healthy","increasing","decreasing","persistent"))
table(mydata0$class)/2

#demograpihcs
mydata00<-mydata0[mydata0$visit==0,]
df<-mydata00[mydata00$class=="decreasing"|mydata00$class=="persistent",]
aggregate(df$age,list(df$class),mean)
aggregate(df$age,list(df$class),sd)
t.test(df[df$class=="persistent",]$age,df[df$class=="decreasing",]$age)
table(df$Gender,df$class)
chisq.test(matrix(table(df$Gender,df$class),nrow = 2)[,3:4])
aggregate(df$education,list(df$class),mean)
aggregate(df$education,list(df$class),sd)
t.test(df[df$class=="persistent",]$education,df[df$class=="decreasing",]$education)
aggregate(df$pay_1,list(df$class),mean)
aggregate(df$pay_1,list(df$class),sd)
t.test(df[df$class=="persistent",]$pay_1,df[df$class=="decreasing",]$pay_1)
aggregate(df$Exterlizing,list(df$class),mean)
aggregate(df$Exterlizing,list(df$class),sd)
t.test(df[df$class=="persistent",]$Exterlizing,df[df$class=="decreasing",]$Exterlizing)
aggregate(df$Interlizing,list(df$class),mean)
aggregate(df$Interlizing,list(df$class),sd)
t.test(df[df$class=="persistent",]$Interlizing,df[df$class=="decreasing",]$Interlizing)
meanx<-function(x) mean(x,na.rm=T)
sdx<-function(x) sd(x,na.rm=T)
aggregate(df$Cognition,list(df$class),meanx)
aggregate(df$Cognition,list(df$class),sdx)
t.test(df[df$class=="persistent",]$Cognition,df[df$class=="decreasing",]$Cognition)

library(caret)
library(pROC)
library(nricens)
#predict decreasing and persistent
df<-data_predict_select[data_predict_select$class=="decreasing"|data_predict_select$class=="persistent",]
df$class <- make.names(df$class,unique = FALSE, allow_ = TRUE)
df$class <- as.factor(df$class)
newdata1 <- df[,-7]
set.seed(1234)
Process = preProcess(newdata1,method = 'knnImpute')
newdata = predict(Process, newdata1)
newdata<-cbind(df$class,newdata)
colnames(newdata)[1]<-"class"
set.seed(1234)
datTrain <- upSample(newdata[,-1],newdata[,1],yname = "class")
table(datTrain$class)
# modeling and plot ROC curve
trctrl <- trainControl(method = "repeatedcv",
                       number = 10,
                       repeats =3,
                       classProbs = TRUE,
                       summaryFunction = twoClassSummary)
set.seed(1234)
index<-createFolds(datTrain$class,k=5,list = F)
rsq<-data.frame()
glm.probs1<-data.frame()
glm.probs2<-data.frame()
for(i in 1:5){
  df_train<-datTrain[index!=i,]
  df_test<-datTrain[index==i,]
  
  set.seed(1234)
  glmboost_fit1 <- train(class ~ ., data = df_train,
                         method = "glmboost",
                         metric = "ROC",
                         preProcess = c("center", "scale"),
                         #tuneGrid = grid,
                         trControl = trctrl
                         
  )
  glm.probs1 = rbind(glm.probs1,predict(glmboost_fit1,df_test[,!names(df_test) %in% c("class")],type = "prob"))
  glm.ROC = roc(response = df_test[,c("class")],predictor = predict(glmboost_fit1,df_test[,!names(df_test) %in% c("class")],type = "prob")$decreasing,levels = levels(df_test[,c("class")]))
  plot(glm.ROC,type = "S",col = "red")
  rsq[1,i]<-glm.ROC[["auc"]]
  print(date())
  
  set.seed(1234)
  glmboost_fit2 <- train(class ~ ., data = df_train[,-6],
                         method = "glmboost",
                         metric = "ROC",
                         preProcess = c("center", "scale"),
                         #tuneGrid = grid,
                         trControl = trctrl
                         
  )
  glm.probs2 = rbind(glm.probs2,predict(glmboost_fit2,df_test[,!names(df_test) %in% c("class")],type = "prob"))
  glm.ROC = roc(response = df_test[,c("class")],predictor = predict(glmboost_fit2,df_test[,!names(df_test) %in% c("class")],type = "prob")$decreasing,levels = levels(df_test[,c("class")]))
  plot(glm.ROC,type = "S",col = "red")
  rsq[2,i]<-glm.ROC[["auc"]]
  print(date())
}
a<-as.numeric(df_test$class)-1
a<-c(a,a,a,a,a)
NRIb = nribin( event = a, p.std = glm.probs2[,2], p.new = glm.probs1[,2], updown = 'category', cut=0.5,niter =1000)
rsq[,6]<-rowMeans(rsq)
View(rsq)

#plot importance figure
coef_glm<-list()
library(mboost)
for(i in 1:5){
  df_train<-datTrain[index!=i,]
  df_test<-datTrain[index==i,]
  
  set.seed(1234)
  enet_fit <- train(class ~ ., data = df_train,
                    method = "glmboost",
                    metric = "ROC",
                    preProcess = c("center", "scale"),
                    #tuneGrid = grid,
                    trControl = trctrl
                    
  )
  
  y<-df_train[,ncol(df_train)]
  x<-df_train[,-ncol(df_train)]
  x$Gender<-as.numeric(x$Gender)
  x<-as.matrix(x)
  x<-scale(x)
  fit.glmboost<-glmboost(y=y,x=x,family =  Binomial(),control = boost_control(mstop = enet_fit[["finalModel"]][[".org.mstop"]]))
  aaaa<-coef(fit.glmboost)
  aaaa[order(abs(aaaa),decreasing = T)]
  coef_glm[[i]]<-aaaa[order(abs(aaaa),decreasing = T)]
}

pre_var<-union(union(union(union(names(coef_glm[[1]]),names(coef_glm[[2]])),names(coef_glm[[3]])),names(coef_glm[[4]])),names(coef_glm[[5]]))
data_var<-as.data.frame(matrix(NA,nrow = 5,ncol = length(pre_var)))
colnames(data_var)<-pre_var
for (i in 1:5) {
  for(j in 1:length(coef_glm[[i]])){
    for(k in 1:length(pre_var)){
      if(names(coef_glm[[i]])[j]==colnames(data_var)[k]){
        data_var[i,k]<-coef_glm[[i]][j]
      }
    }
  }
}
data_var1<-data_var
data_var1[is.na(data_var1)]<-0
#View(data_var1)
data_var1[6,]<-colSums(data_var1[1:5,])
or<-order(abs(data_var1[6,]),decreasing = T)
library(RColorBrewer)
data_var2<-as.matrix(abs(data_var1[1:5,or]))
names(data_var2)<-colnames(data_var1)
par(mfrow=c(1,1),mar=c(10,4,4,2)+2)
barplot(data_var2,names.arg = colnames(data_var1),
        main = "importance",las=2,
        col = colorRampPalette(brewer.pal(9,"Paired"))(5))

legend("topright",c("one","two","three","four","five"),text.col = colorRampPalette(brewer.pal(9,"Paired"))(5))
save.image("result/result0930_prediction_BNU.RData")



##########################################################################
##########################################################################
########internalizing persistent & improving group in BNU#################
##########################################################################
##########################################################################



id<-unique(mydata$User.code)
index1<-mydata[mydata$visit==0&mydata$Interlizing>5,]$User.code
index2<-mydata[mydata$visit==1&mydata$Interlizing>5,]$User.code
mydata1<-mydata[mydata$User.code%in%intersect(index1,index2),]
mydata1$class<-"persistent"
mydata2<-mydata[mydata$User.code%in%index1[!index1%in%intersect(index1,index2)],]
mydata2$class<-"decreasing"

index1<-mydata[mydata$visit==0&mydata$Interlizing<5,]$User.code
index2<-mydata[mydata$visit==1&mydata$Interlizing<5,]$User.code
mydata3<-mydata[mydata$User.code%in%intersect(index1,index2),]
mydata3$class<-"healthy"
mydata4<-mydata[mydata$User.code%in%index1[!index1%in%intersect(index1,index2)],]
mydata4$class<-"increasing"

mydata0<-rbind(mydata1,mydata2,mydata3,mydata4)
mydata0$class<-factor(mydata0$class,levels = c("healthy","increasing","decreasing","persistent"),labels = c("healthy","increasing","decreasing","persistent"))
table(mydata0$class)/2

#demographics
mydata00<-mydata0[mydata0$visit==0,]
df<-mydata00[mydata00$class=="decreasing"|mydata00$class=="persistent",]
aggregate(df$age,list(df$class),mean)
aggregate(df$age,list(df$class),sd)
t.test(df[df$class=="persistent",]$age,df[df$class=="decreasing",]$age)
table(df$Gender,df$class)
chisq.test(matrix(table(df$Gender,df$class),nrow = 2)[,3:4])
aggregate(df$education,list(df$class),mean)
aggregate(df$education,list(df$class),sd)
t.test(df[df$class=="persistent",]$education,df[df$class=="decreasing",]$education)
aggregate(df$pay_1,list(df$class),mean)
aggregate(df$pay_1,list(df$class),sd)
t.test(df[df$class=="persistent",]$pay_1,df[df$class=="decreasing",]$pay_1)
aggregate(df$Exterlizing,list(df$class),mean)
aggregate(df$Exterlizing,list(df$class),sd)
t.test(df[df$class=="persistent",]$Exterlizing,df[df$class=="decreasing",]$Exterlizing)
aggregate(df$Interlizing,list(df$class),mean)
aggregate(df$Interlizing,list(df$class),sd)
t.test(df[df$class=="persistent",]$Interlizing,df[df$class=="decreasing",]$Interlizing)
meanx<-function(x) mean(x,na.rm=T)
sdx<-function(x) sd(x,na.rm=T)
aggregate(df$Cognition,list(df$class),meanx)
aggregate(df$Cognition,list(df$class),sdx)
t.test(df[df$class=="persistent",]$Cognition,df[df$class=="decreasing",]$Cognition)

##could not improve NRI
