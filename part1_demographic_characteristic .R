setwd("D:/lqy/ABCD/r0901")
load("data/r0901_1.RData")
#table1
#ABCD cohort
#sex/age/cbcl
data_cbcl<-list(data_cbcls_base,data_cbcls_1year,data_cbcls_2year,data_cbcls_3year)
meanx<-function(x) mean(x,na.rm=T)
sdx<-function(x) sd(x,na.rm=T)
res<-as.data.frame(matrix(NA,nrow = 10,ncol = 12))
for(i in 1:4){
  for(j in c(3,5:12)){
    res[j-2,(3*i-2)]<-sum(!is.na(data_cbcl[[i]][,j]))
    res[j-2,(3*i-1)]<-sum(is.na(data_cbcl[[i]][,j]))
    res[j-2,3*i]<-paste(round(meanx(data_cbcl[[i]][,j]),2),"(",round(sdx(data_cbcl[[i]][,j]),2),")")
  }
  res[2,(3*i-2)]<-sum(!is.na(data_cbcl[[i]][,4]))
  res[2,(3*i-1)]<-sum(is.na(data_cbcl[[i]][,4]))
  res[2,3*i]<-paste(table(data_cbcl[[i]][,4])[2],"/",table(data_cbcl[[i]][,4])[1],sep = "")
}
rownames(res)<-colnames(data_cbcls_base)[3:12]
write.csv(res,"tmp.csv",quote = F)

#tbss
rm(list = ls())
load("data/r0901_tbss.RData")
res<-as.data.frame(matrix(NA,nrow = 10,ncol = 3))
meanx<-function(x) mean(x,na.rm=T)
sdx<-function(x) sd(x,na.rm=T)
for(j in 2:11){
  res[j-1,1]<-sum(!is.na(data_tbss_base[,j]))
  res[j-1,2]<-sum(is.na(data_tbss_base[,j]))
  res[j-1,3]<-paste(round(meanx(data_tbss_base[,j]),2),"(",round(sdx(data_tbss_base[,j]),2),")")
}
rownames(res)<-colnames(data_tbss_base)[2:11]
write.csv(res,"tmp.csv",quote = F)

#BNU cohort
#demographic
rm(list = ls())
library(readxl)
data_base<-as.data.frame(read_xlsx("data/0414_FUDAN_WCST_CORSI_cognition_SDQ_BL.xlsx"))
data_1year<-as.data.frame(read_xlsx("data/20211206_WCST_CORSI_cognition_SDQ_FUDAN（第二年）.xlsx"))
for (i in 1:nrow(data_base)) {
  for(j in 1:ncol(data_base))
    if(!is.na(data_base[i,j])&data_base[i,j]==999)
      data_base[i,j]<-NA
}
for (i in 1:nrow(data_1year)) {
  for(j in 1:ncol(data_1year))
    if(!is.na(data_1year[i,j])&data_1year[i,j]==999)
      data_1year[i,j]<-NA
}

meanx<-function(x) mean(x,na.rm=T)
sdx<-function(x) sd(x,na.rm=T)
#15,16,10
df<-data_base[,2]
sum(!is.na(df))
paste(round(meanx(df),2),"(",round(sdx(df),2),")")
table(data_base$Gender)

for(i in c(15,16,10,36:39)){
  print(colnames(data_base[i]))
  df<-data_base[,i]
  print(sum(!is.na(df)))
  print(paste(round(meanx(df),2),"(",round(sdx(df),2),")"))
}

#37,38,32
df<-data_1year[,3]
sum(!is.na(df))
paste(round(meanx(df),2),"(",round(sdx(df),2),")")
mydata<-read_xlsx("data/data_base_1year.xlsx")
table(mydata$Gender)/2

for(i in c(37,38,32,21,22,19,18)){
  print(colnames(data_1year[i]))
  df<-data_1year[,i]
  print(sum(!is.na(df)))
  print(paste(round(meanx(df),2),"(",round(sdx(df),2),")"))
}
