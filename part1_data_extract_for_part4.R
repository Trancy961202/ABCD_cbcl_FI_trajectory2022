setwd("/share/inspurStorage/home1/liqy/ABCD/test0510")
rm(list = ls())
library(dplyr)
library(matrixStats)

#########################################
#########################################
###########data preprocessing############
#########################################
#########################################

###corvariables
nda = readRDS("/share/inspurStorage/home1/luoqiang/ABCDR/Package_1193390/results/nda3.0.Rds")
corvariables = nda[,c(which(names(nda)=="src_subject_id"),which(names(nda)=='high.educ'),
                      which(names(nda)=='household.income'),which(names(nda)=='interview_age'),
                      which(names(nda)=='sex'),which(names(nda)=='rel_family_id'),
                      which(names(nda)=='site_id_l'),which(names(nda)=="event_name"),
                      which(names(nda)=='race_ethnicity'),which(names(nda)=='anthro_1_height_in'),
                      which(names(nda)=='anthro_2_height_in'),which(names(nda)=='anthro_3_height_in'),
                      which(names(nda)=='anthro_weight1_lb'),which(names(nda)=='anthro_weight2_lb'),
                      which(names(nda)=='anthro_weight3_lb'))]
corvariables_total<-corvariables
corvariables<-corvariables[corvariables$event_name=="baseline_year_1_arm_1",]
corvariables$site_id_l<-factor(corvariables$site_id_l,levels = paste("site",c(paste(0,1:9,sep = ""),10:22),sep = ""))
table(corvariables$site_id_l)
summary(corvariables)
for(i in 10:12){
  for(j in 1:nrow(corvariables)){
    if(!is.na(corvariables[j,i])&corvariables[j,i]<30){
      corvariables[j,i]<-NA
    }
  }
}
for(i in 13:15){
  for(j in 1:nrow(corvariables)){
    if(!is.na(corvariables[j,i])&corvariables[j,i]<20){
      corvariables[j,i]<-NA
    }
  }
}

#BMI
corvariables$weightmean<-rowMeans(corvariables[,13:15],na.rm=T)
corvariables$weightsd<-rowSds(as.matrix(corvariables[,13:15]),na.rm=T)
for(j in 1:nrow(corvariables)){
  if(!is.na(corvariables[j,]$weightsd)&corvariables[j,]$weightsd>5){
    corvariables[j,]$weightmean<-NA
  }
}
corvariables$heightmean<-rowMeans(corvariables[,10:12],na.rm=T)
corvariables$bmi<-(corvariables$weightmean/2.20462)/(2.54*corvariables$heightmean/100)**2
hist(corvariables$bmi,breaks = 20)

#puberty
puberty<-read.table("../abcd_ypdms01.txt",header = T,stringsAsFactors = F)
puberty<-puberty[puberty$eventname=="baseline_year_1_arm_1",]
for (i in 1:nrow(puberty)) {
  for(j in 1:ncol(puberty))
    if(!is.na(puberty[i,j])&puberty[i,j]==999|!is.na(puberty[i,j])&puberty[i,j]==777)
      puberty[i,j]<-NA
}
puberty$score<-NA
for(i in 1:nrow(puberty)){
  if(puberty[i,]$sex=="M"){
    puberty[i,]$score<-puberty[i,]$pds_bdyhair_y+puberty[i,]$pds_m4_y+puberty[i,]$pds_m5_y
  }
  else if(puberty[i,]$sex=="F"){
    puberty[i,]$score<-puberty[i,]$pds_bdyhair_y+puberty[i,]$pds_f4_2_y+puberty[i,]$pds_f5_y
  }
}

a<-corvariables
b<-puberty[,c(5,41)]
c<-merge(a,b,by="src_subject_id",all = T)

#mental_health
mental_health<-read.table("../abcd_fhxssp01.txt",header = T,stringsAsFactors = F)
b<-as.data.frame(cbind(mental_health$src_subject_id,mental_health$famhx_ss_momdad_hspd_p))
names(b)<-c("src_subject_id","famhx_ss_momdad_hspd_p")
d<-merge(c,b,by="src_subject_id",all = T)

#asr/fes/premature
asrs<-read.table("../abcd_asrs01.txt",header = T)
data_asrs_base<-asrs[asrs$eventname=="baseline_year_1_arm_1",c("src_subject_id","asr_scr_totprob_r")]
fes<-read.table("../abcd_sscey01.txt",header = T)
data_fes_base<-fes[fes$eventname=="baseline_year_1_arm_1",c("src_subject_id","fes_y_ss_fc_pr","pmq_y_ss_mean")]
dhx<-read.table("../dhx01.txt",header = T)
data_dhx_base<-dhx[,c("src_subject_id","devhx_12a_p","birth_weight_lbs")]
d<-cbind(d,data_asrs_base[match(d$src_subject_id,data_asrs_base$src_subject_id),],data_fes_base[match(d$src_subject_id,data_fes_base$src_subject_id),2:3],data_dhx_base[match(d$src_subject_id,data_dhx_base$src_subject_id),2:3])
d<-d[,-22]

#cbcl
cbcls<-read.table("../abcd_cbcls01.txt",header = T)
data_cbcls_base<-cbcls[cbcls$eventname=="baseline_year_1_arm_1",c("src_subject_id","eventname","interview_age","sex",
                                                                  "cbcl_scr_dsm5_anxdisord_t",
                                                                  "cbcl_scr_dsm5_depress_t","cbcl_scr_dsm5_somaticpr_t",
                                                                  "cbcl_scr_dsm5_adhd_t","cbcl_scr_dsm5_opposit_t",
                                                                  "cbcl_scr_dsm5_conduct_t","cbcl_scr_syn_external_t","cbcl_scr_syn_internal_t","cbcl_scr_syn_totprob_t")]
data_cbcls_1year<-cbcls[cbcls$eventname=="1_year_follow_up_y_arm_1",c("src_subject_id","eventname","interview_age","sex",
                                                                      "cbcl_scr_dsm5_anxdisord_t",
                                                                      "cbcl_scr_dsm5_depress_t","cbcl_scr_dsm5_somaticpr_t",
                                                                      "cbcl_scr_dsm5_adhd_t","cbcl_scr_dsm5_opposit_t",
                                                                      "cbcl_scr_dsm5_conduct_t","cbcl_scr_syn_external_t","cbcl_scr_syn_internal_t","cbcl_scr_syn_totprob_t")]
data_cbcls_2year<-cbcls[cbcls$eventname=="2_year_follow_up_y_arm_1",c("src_subject_id","eventname","interview_age","sex",
                                                                      "cbcl_scr_dsm5_anxdisord_t",
                                                                      "cbcl_scr_dsm5_depress_t","cbcl_scr_dsm5_somaticpr_t",
                                                                      "cbcl_scr_dsm5_adhd_t","cbcl_scr_dsm5_opposit_t",
                                                                      "cbcl_scr_dsm5_conduct_t","cbcl_scr_syn_external_t","cbcl_scr_syn_internal_t","cbcl_scr_syn_totprob_t")]
data_cbcls_3year<-cbcls[cbcls$eventname=="3_year_follow_up_y_arm_1",c("src_subject_id","eventname","interview_age","sex",
                                                                      "cbcl_scr_dsm5_anxdisord_t",
                                                                      "cbcl_scr_dsm5_depress_t","cbcl_scr_dsm5_somaticpr_t",
                                                                      "cbcl_scr_dsm5_adhd_t","cbcl_scr_dsm5_opposit_t",
                                                                      "cbcl_scr_dsm5_conduct_t","cbcl_scr_syn_external_t","cbcl_scr_syn_internal_t","cbcl_scr_syn_totprob_t")]

index_cbcls<-intersect(intersect(data_cbcls_base$src_subject_id,data_cbcls_1year$src_subject_id),data_cbcls_2year$src_subject_id)
length(index_cbcls)
aa<-data_cbcls_3year[match(index_cbcls,data_cbcls_3year$src_subject_id),]
aa$src_subject_id<-index_cbcls
aa$eventname<-"3_year_follow_up_y_arm_1"
data_1_1<-cbind(rbind(data_cbcls_base[match(index_cbcls,data_cbcls_base$src_subject_id),],data_cbcls_1year[match(index_cbcls,data_cbcls_1year$src_subject_id),],
                      data_cbcls_2year[match(index_cbcls,data_cbcls_2year$src_subject_id),],aa),
                rbind(d[match(index_cbcls,d$src_subject_id),],d[match(index_cbcls,d$src_subject_id),],d[match(index_cbcls,d$src_subject_id),],
                      d[match(index_cbcls,d$src_subject_id),]))
data_1_1$visit<-0
data_1_1[data_1_1$eventname=="1_year_follow_up_y_arm_1",]$visit<-12
data_1_1[data_1_1$eventname=="2_year_follow_up_y_arm_1",]$visit<-24
data_1_1[data_1_1$eventname=="3_year_follow_up_y_arm_1",]$visit<-36
summary(data_1_1)

#################################################
#################################################
###########prepare no-imputation data############
#################################################
#################################################

data_1_1_1<-data_1_1[,c(-2,-3,-4,-14,-21)]
str(data_1_1_1)
head(data_1_1_1$high.educ)
head(data_1_1_1$household.income)
data_1_1_1$sex<-factor(data_1_1_1$sex,levels = c("M","F"),labels = c("boy","girl"))
head(data_1_1_1$sex)
for(i in 29:32){
  data_1_1_1[,i]<-as.numeric(as.matrix(data_1_1_1[,i]))
}
str(data_1_1_1)
summary(data_1_1_1)
rm(nda)
save.image("r0913_noimpute_1.RData")

tbss<-read.table("../abcd_tbss01.txt",header = T)
data_tbss_base<-tbss[tbss$eventname=="baseline_year_1_arm_1",
                     c("src_subject_id","nihtbx_totalcomp_fc","nihtbx_cryst_fc",
                       "nihtbx_picvocab_fc","nihtbx_reading_fc","nihtbx_fluidcomp_fc",
                       "nihtbx_picture_fc","nihtbx_cardsort_fc","nihtbx_flanker_fc",
                       "nihtbx_list_fc","nihtbx_pattern_fc")]

#combination of cbcls and tbss
data_cbcls_tbss<-cbind(data_1_1_1,data_tbss_base[match(data_1_1_1$src_subject_id,data_tbss_base$src_subject_id),])
save.image("r0913_noimp_tbss.RData")


###########################################
###########################################
###########prepare part1&2 data############
###########################################
###########################################


rm(list = ls())
load("data/r0913_noimpute_1.RData")
library(mice)
data_pre_perm<-data_1_1_1[,c(2:9,12,14:17,35)]
tpm<-mice(data_pre_perm,method = "rf",m = 1,seed = 1234)
data_af_perm<-complete(tpm)
data_1_1_1[,c(2:9,12,14:17,35)]<-data_af_perm
summary(data_1_1_1)

#cbcl_teacher
bpm<-read.table("data/abcd_ssbpmtf01.txt",header = T)
table(bpm$eventname)

save.image("data/r0913_1.RData")

tbss<-read.table("../abcd_tbss01.txt",header = T)
data_tbss_base<-tbss[tbss$eventname=="baseline_year_1_arm_1",
                     c("src_subject_id","nihtbx_totalcomp_fc","nihtbx_cryst_fc",
                       "nihtbx_picvocab_fc","nihtbx_reading_fc","nihtbx_fluidcomp_fc",
                       "nihtbx_picture_fc","nihtbx_cardsort_fc","nihtbx_flanker_fc",
                       "nihtbx_list_fc","nihtbx_pattern_fc")]
data_tbss_base[,2:11]<-sapply(data_tbss_base[,2:11], as.numeric)
#sum(is.na(data_tbss_base$nihtbx_totalcomp_fc))
#combination of cbcls and tbss
data_cbcls_tbss<-cbind(data_1_1_1,data_tbss_base[match(data_1_1_1$src_subject_id,data_tbss_base$src_subject_id),])
save.image("data/r0913_tbss.RData")


#########################################
#########################################
###########prepare part3 data############
#########################################
#########################################

#smri
rm(list = ls())
load("data/r0913_1.RData")
library(lmerTest)
library(effectsize)
lmer_no_cov<-as.data.frame(matrix(NA,nrow = 8,ncol = 4))
fit<-list()
for(i in 2:10){
  fit[[i-1]]<-lmer(data_1_1_1[,i]~visit+(visit|src_subject_id),data = data_1_1_1)
  lmer_no_cov[i-1,c(1,3,4)]<-summary(fit[[i-1]])[["coefficients"]][2,c(1,4,5)]
  lmer_no_cov[i-1,2]<-eta_squared(fit[[i-1]])[1,2]
}
colnames(lmer_no_cov)<-c("estimate","eta2","t","p")
rownames(lmer_no_cov)<-colnames(data_1_1_1)[2:10]
View(lmer_no_cov)

anxi<-ranef(fit[[1]])[["src_subject_id"]][["(Intercept)"]]
anxs<-ranef(fit[[1]])[["src_subject_id"]][["visit"]]
depi<-ranef(fit[[2]])[["src_subject_id"]][["(Intercept)"]]
deps<-ranef(fit[[2]])[["src_subject_id"]][["visit"]]
somi<-ranef(fit[[3]])[["src_subject_id"]][["(Intercept)"]]
soms<-ranef(fit[[3]])[["src_subject_id"]][["visit"]]
adhi<-ranef(fit[[4]])[["src_subject_id"]][["(Intercept)"]]
adhs<-ranef(fit[[4]])[["src_subject_id"]][["visit"]]
oppi<-ranef(fit[[5]])[["src_subject_id"]][["(Intercept)"]]
opps<-ranef(fit[[5]])[["src_subject_id"]][["visit"]]
coni<-ranef(fit[[6]])[["src_subject_id"]][["(Intercept)"]]
cons<-ranef(fit[[6]])[["src_subject_id"]][["visit"]]
exti<-ranef(fit[[7]])[["src_subject_id"]][["(Intercept)"]]
exts<-ranef(fit[[7]])[["src_subject_id"]][["visit"]]
inti<-ranef(fit[[8]])[["src_subject_id"]][["(Intercept)"]]
ints<-ranef(fit[[8]])[["src_subject_id"]][["visit"]]
toti<-ranef(fit[[9]])[["src_subject_id"]][["(Intercept)"]]
tots<-ranef(fit[[9]])[["src_subject_id"]][["visit"]]
intercept<-cbind(anxi,depi,somi,adhi,oppi,coni,exti,inti,toti)
slope<-cbind(anxs,deps,soms,adhs,opps,cons,exts,ints,tots)
com<-as.data.frame(cbind(intercept,slope))

a<-ranef(fit[[1]])
aa<-as.data.frame(rownames(a[["src_subject_id"]]))
colnames(aa)<-"V1"
#library(tidyr)
#aaa<-separate(data = aa,col = V1,into = c("V2","V3","v4"),sep=':')
index<-aa[,1]
com<-as.data.frame(cbind(index,com))
smri<-read.table("data/abcd_smrip10201.txt",header = T,stringsAsFactors = F)
qc<-read.table("data/abcd_imgincl01.txt",header = T,stringsAsFactors = F)
smri_qc<-merge(smri,qc,by = c("src_subject_id","eventname"))
table(smri$eventname)
smri_base<-smri_qc[smri_qc$eventname=="baseline_year_1_arm_1",]
smri_2year<-smri_qc[smri_qc$eventname=="2_year_follow_up_y_arm_1",]
table(smri_base$imgincl_t1w_include)
smri_base<-smri_base[smri_base$imgincl_t1w_include==1,]
tbss<-read.table("data/abcd_tbss01.txt",header = T)
data_tbss_base<-tbss[tbss$eventname=="baseline_year_1_arm_1",c("src_subject_id","nihtbx_totalcomp_fc","nihtbx_cryst_fc",
                                                               "nihtbx_picvocab_fc","nihtbx_reading_fc","nihtbx_fluidcomp_fc",
                                                               "nihtbx_picture_fc","nihtbx_cardsort_fc","nihtbx_flanker_fc",
                                                               "nihtbx_list_fc","nihtbx_pattern_fc")]
for(i in 2:11){
  data_tbss_base[data_tbss_base[,i]=="",i]<-NA
  data_tbss_base[,i]<-as.numeric(as.matrix(data_tbss_base[,i]))
}
index_qc<-intersect(smri_base$src_subject_id,index)
data_com_smri_tbss<-cbind(com[match(index_qc,com$index),],data_tbss_base[match(index_qc,data_tbss_base$src_subject_id),-1],
                          smri_base[match(index_qc,smri_base$src_subject_id),],corvariables[match(index_qc,corvariables$src_subject_id),c(3,6,7,9)],
                          mental_health[match(index_qc,mental_health$src_subject_id),]$famhx_ss_momdad_hspd_p)
colnames(data_com_smri_tbss)[ncol(data_com_smri_tbss)]<-"famhx_ss_momdad_hspd_p"
df_tem<-as.data.frame(cbind(data_com_smri_tbss$nihtbx_fluidcomp_fc,data_com_smri_tbss$adhi))
df_tem<-na.omit(df_tem)
cor.test(df_tem[,1],df_tem[,2])

#normalization
normalize <- function(x) {
  return((x - min(x,na.rm = T)) / (max(x,na.rm = T) - min(x,na.rm = T)))
}
num_cbcl<-2:19
num_tbss<-20:29
num_smri_total<-c(146,360,467)
num_thick<-40:107
num_area<-254:321
num_vol<-361:428

data_com_smri_tbss[,c(num_cbcl,num_tbss,num_smri_total,num_thick,num_area,num_vol)] <- as.data.frame(lapply(data_com_smri_tbss[,c(num_cbcl,num_tbss,num_smri_total,num_thick,num_area,num_vol)],normalize))
library(nnet)
data_com_smri_tbss<-cbind(data_com_smri_tbss,class.ind(data_com_smri_tbss$sex.y)[,-1],class.ind(data_com_smri_tbss$household.income)[,-1],class.ind(data_com_smri_tbss$site_id_l)[,-1],class.ind(data_com_smri_tbss$race_ethnicity)[,-1])
colnames(data_com_smri_tbss)[538:540]<-c("sex","income1","income2")
save.image("data/inter_slope0913.RData")



rm(list = ls())
load("data/inter_slope0913.RData")
data_com_smri_tbss1<-data_com_smri_tbss
data_com_smri_tbss<-data_com_smri_tbss[complete.cases(data_com_smri_tbss$smri_thick_cdk_banksstslh),]
a<-as.data.frame(table(data_com_smri_tbss$rel_family_id))
length(a[a$Freq==1,1])
family1<-data_com_smri_tbss[data_com_smri_tbss$rel_family_id%in%a[a$Freq==1,1],]
bb<-as.data.frame(table(family1$rel_family_id))
family2<-data_com_smri_tbss[data_com_smri_tbss$rel_family_id%in%a[a$Freq!=1,1],]
cc<-as.numeric(as.matrix(a[a$Freq!=1,1]))
ccc<-a[a$Freq!=1,]
#One person from each family was randomly selected
df<-family1
for(i in 1:nrow(ccc)){
  set.seed(1234)
  n<-sample.int(ccc[i,2],1)
  temp<-data_com_smri_tbss[data_com_smri_tbss$rel_family_id==ccc[i,1],]
  temp1<-temp[n,]
  df<-rbind(df,temp1)
}
dd<-as.data.frame(table(df$rel_family_id))
df$src_subject_id
sum(is.na(df$anxi))
save.image("data/r0913_smri.RData")



#dmri
rm(list = ls())
load("data/r0913_1.RData")
library(lmerTest)
library(effectsize)
lmer_no_cov<-as.data.frame(matrix(NA,nrow = 8,ncol = 4))
fit<-list()
for(i in 2:10){
  fit[[i-1]]<-lmer(data_1_1_1[,i]~visit+(visit|src_subject_id),data = data_1_1_1)
  lmer_no_cov[i-1,c(1,3,4)]<-summary(fit[[i-1]])[["coefficients"]][2,c(1,4,5)]
  lmer_no_cov[i-1,2]<-eta_squared(fit[[i-1]])[1,2]
}
colnames(lmer_no_cov)<-c("estimate","eta2","t","p")
rownames(lmer_no_cov)<-colnames(data_1_1_1)[2:10]


anxi<-ranef(fit[[1]])[["src_subject_id"]][["(Intercept)"]]
anxs<-ranef(fit[[1]])[["src_subject_id"]][["visit"]]
depi<-ranef(fit[[2]])[["src_subject_id"]][["(Intercept)"]]
deps<-ranef(fit[[2]])[["src_subject_id"]][["visit"]]
somi<-ranef(fit[[3]])[["src_subject_id"]][["(Intercept)"]]
soms<-ranef(fit[[3]])[["src_subject_id"]][["visit"]]
adhi<-ranef(fit[[4]])[["src_subject_id"]][["(Intercept)"]]
adhs<-ranef(fit[[4]])[["src_subject_id"]][["visit"]]
oppi<-ranef(fit[[5]])[["src_subject_id"]][["(Intercept)"]]
opps<-ranef(fit[[5]])[["src_subject_id"]][["visit"]]
coni<-ranef(fit[[6]])[["src_subject_id"]][["(Intercept)"]]
cons<-ranef(fit[[6]])[["src_subject_id"]][["visit"]]
exti<-ranef(fit[[7]])[["src_subject_id"]][["(Intercept)"]]
exts<-ranef(fit[[7]])[["src_subject_id"]][["visit"]]
inti<-ranef(fit[[8]])[["src_subject_id"]][["(Intercept)"]]
ints<-ranef(fit[[8]])[["src_subject_id"]][["visit"]]
toti<-ranef(fit[[9]])[["src_subject_id"]][["(Intercept)"]]
tots<-ranef(fit[[9]])[["src_subject_id"]][["visit"]]
intercept<-cbind(anxi,depi,somi,adhi,oppi,coni,exti,inti,toti)
slope<-cbind(anxs,deps,soms,adhs,opps,cons,exts,ints,tots)
com<-as.data.frame(cbind(intercept,slope))

a<-ranef(fit[[1]])
aa<-as.data.frame(rownames(a[["src_subject_id"]]))
colnames(aa)<-"V1"
#library(tidyr)
#aaa<-separate(data = aa,col = V1,into = c("V2","V3","v4"),sep=':')
index<-aa[,1]
com<-as.data.frame(cbind(index,com))
dmri1<-read.table("data/abcd_dmdtifp101.txt",header = T,stringsAsFactors = F)
dmri1<-dmri1[,c(-1:-4,-6:-7,-(ncol(dmri1)-2):-ncol(dmri1))]
dmri2<-read.table("data/abcd_dmdtifp201.txt",header = T,stringsAsFactors = F)
dmri2<-dmri2[,c(-1:-4,-6:-7,-(ncol(dmri2)-2):-ncol(dmri2))]
dmri<-merge(dmri1,dmri2,by = c("src_subject_id","eventname","sex"))
qc<-read.table("data/abcd_imgincl01.txt",header = T,stringsAsFactors = F)
dmri_qc<-merge(dmri,qc,by = c("src_subject_id","eventname"))
table(dmri_qc$eventname)
dmri_base<-dmri_qc[dmri_qc$eventname=="baseline_year_1_arm_1",]
dmri_2year<-dmri_qc[dmri_qc$eventname=="2_year_follow_up_y_arm_1",]
table(dmri_base$imgincl_dmri_include)
dmri_base<-dmri_base[dmri_base$imgincl_dmri_include==1,]
tbss<-read.table("../abcd_tbss01.txt",header = T)
data_tbss_base<-tbss[tbss$eventname=="baseline_year_1_arm_1",c("src_subject_id","nihtbx_totalcomp_fc","nihtbx_cryst_fc",
                                                               "nihtbx_picvocab_fc","nihtbx_reading_fc","nihtbx_fluidcomp_fc",
                                                               "nihtbx_picture_fc","nihtbx_cardsort_fc","nihtbx_flanker_fc",
                                                               "nihtbx_list_fc","nihtbx_pattern_fc")]
for(i in 2:11){
  data_tbss_base[data_tbss_base[,i]=="",i]<-NA
  data_tbss_base[,i]<-as.numeric(as.matrix(data_tbss_base[,i]))
}
index_qc<-intersect(dmri_base$src_subject_id,index)
data_com_dmri_tbss<-cbind(com[match(index_qc,com$index),],data_tbss_base[match(index_qc,data_tbss_base$src_subject_id),-1],
                          dmri_base[match(index_qc,dmri_base$src_subject_id),],
                          corvariables[match(index_qc,corvariables$src_subject_id),c(3,6,7,9)],
                          mental_health[match(index_qc,mental_health$src_subject_id),]$famhx_ss_momdad_hspd_p)
colnames(data_com_dmri_tbss)[ncol(data_com_dmri_tbss)]<-"famhx_ss_momdad_hspd_p"
df_tem<-as.data.frame(cbind(data_com_dmri_tbss$nihtbx_fluidcomp_fc,data_com_dmri_tbss$adhi))
df_tem<-na.omit(df_tem)
cor.test(df_tem[,1],df_tem[,2])


#normalization
normalize <- function(x) {
  return((x - min(x,na.rm = T)) / (max(x,na.rm = T) - min(x,na.rm = T)))
}
data_com_dmri_tbss[,c(2:29,33:1217)] <- as.data.frame(lapply(data_com_dmri_tbss[,c(2:29,33:1217)],normalize))
library(nnet)
data_com_dmri_tbss<-cbind(data_com_dmri_tbss,class.ind(data_com_dmri_tbss$sex.y)[,-1],class.ind(data_com_dmri_tbss$household.income)[,-1],class.ind(data_com_dmri_tbss$site_id_l)[,-1],class.ind(data_com_dmri_tbss$race_ethnicity)[,-1])
colnames(data_com_dmri_tbss)[1240:1242]<-c("sex","income1","income2")
save.image("data/inter_slope_dmri0913.RData")






load("data/inter_slope_dmri0913.RData")
data_com_dmri_tbss1<-data_com_dmri_tbss
data_com_dmri_tbss<-data_com_dmri_tbss[complete.cases(data_com_dmri_tbss$dmdtifp1_1),]
a<-as.data.frame(table(data_com_dmri_tbss$rel_family_id))
length(a[a$Freq==1,1])
family1<-data_com_dmri_tbss[data_com_dmri_tbss$rel_family_id%in%a[a$Freq==1,1],]
bb<-as.data.frame(table(family1$rel_family_id))
family2<-data_com_dmri_tbss[data_com_dmri_tbss$rel_family_id%in%a[a$Freq!=1,1],]
cc<-as.numeric(as.matrix(a[a$Freq!=1,1]))
ccc<-a[a$Freq!=1,]
#One person from each family was randomly selected
df<-family1
for(i in 1:nrow(ccc)){
  set.seed(1234)
  n<-sample.int(ccc[i,2],1)
  temp<-data_com_dmri_tbss[data_com_dmri_tbss$rel_family_id==ccc[i,1],]
  temp1<-temp[n,]
  df<-rbind(df,temp1)
}
dd<-as.data.frame(table(df$rel_family_id))
df$src_subject_id
sum(is.na(df$anxi))
save.image("data/r0913_dmri.RData")
