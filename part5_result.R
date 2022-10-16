##Developmental trajectories of psychiatric symptommental health problems from the ABCD study.
load("result/result0901_no_interaction_cov.RData")
summary(results_analysis_cog_fit[[13]])[["coefficients"]]
summary(results_analysis_cog_fit[[15]])[["coefficients"]]

load("result/test.RData")

load("result/result0901_cbcls_smri_correlation_inter.RData")
lmer.fit <- lmer(ints~smri_vol_cdk_total+inti+sex+income1+income2+site02+site03+site04+site05+site06+site07+site08+
                   site09+site10+site11+site12+site13+site14+site15+site16+site17+site18+site19+site20+
                   site21+site22+famhx_ss_momdad_hspd_p+smri_vol_scs_intracranialv+(1|site_id_l/rel_family_id),data = data_com_smri_tbss)
analysis <- summary(lmer.fit)[["coefficients"]]
analysis

library(readxl)
res<-as.data.frame(read_xlsx("result/interaction_cbcls_tbss_noimp0901.xlsx",sheet = 2))
for(i in c(5,8)){
  res[1:2,i]<-p.adjust(res[1:2,i])
  res[4:7,i]<-p.adjust(res[4:7,i])
  res[9:18,i]<-p.adjust(res[9:18,i])
}
write.csv(res,"result/temp.csv",quote = F)

res<-as.data.frame(read_xlsx("result/interaction_cbcls_tbss0901.xlsx",sheet = 2))
for(i in c(5,8)){
  res[1:2,i]<-p.adjust(res[1:2,i])
  res[4:7,i]<-p.adjust(res[4:7,i])
  res[9:18,i]<-p.adjust(res[9:18,i])
}
write.csv(res,"result/temp.csv",quote = F)

#cbcl_smri_correlation
cbcl_smri_cor<-read.csv("result/cbcls_smri_correlation0901.csv")
tot_smri_cor<-cbcl_smri_cor[cbcl_smri_cor$cbcl=="toti"|cbcl_smri_cor$cbcl=="tots",]
exin_smri_cor<-cbcl_smri_cor[cbcl_smri_cor$cbcl=="exti"|cbcl_smri_cor$cbcl=="exts"|cbcl_smri_cor$cbcl=="inti"|cbcl_smri_cor$cbcl=="ints",]
subcbcl<-c("anxi","anxs","depi","deps","somi","soms","adhi","adhs","oppi","opps","coni","cons")
sub_smri_cor<-cbcl_smri_cor[cbcl_smri_cor$cbcl%in%subcbcl,]

tot_smri_cor[1:2,5]<-p.adjust(tot_smri_cor[1:2,5])
tot_smri_cor[3:4,5]<-p.adjust(tot_smri_cor[3:4,5])
tot_smri_cor[5:6,5]<-p.adjust(tot_smri_cor[5:6,5])
tot_smri_cor[7:142,5]<-p.adjust(tot_smri_cor[7:142,5])
tot_smri_cor[143:278,5]<-p.adjust(tot_smri_cor[143:278,5])
tot_smri_cor[279:nrow(tot_smri_cor),5]<-p.adjust(tot_smri_cor[279:nrow(tot_smri_cor),5])

exin_smri_cor[1:4,5]<-p.adjust(exin_smri_cor[1:4,5])
exin_smri_cor[5:8,5]<-p.adjust(exin_smri_cor[5:8,5])
exin_smri_cor[9:12,5]<-p.adjust(exin_smri_cor[9:12,5])
exin_smri_cor[13:284,5]<-p.adjust(exin_smri_cor[13:284,5])
exin_smri_cor[285:556,5]<-p.adjust(exin_smri_cor[285:556,5])
exin_smri_cor[557:nrow(exin_smri_cor),5]<-p.adjust(exin_smri_cor[557:nrow(exin_smri_cor),5])

sub_smri_cor[1:12,5]<-p.adjust(sub_smri_cor[1:12,5])
sub_smri_cor[13:24,5]<-p.adjust(sub_smri_cor[13:24,5])
sub_smri_cor[25:36,5]<-p.adjust(sub_smri_cor[25:36,5])
sub_smri_cor[37:852,5]<-p.adjust(sub_smri_cor[37:852,5])
sub_smri_cor[853:1668,5]<-p.adjust(sub_smri_cor[853:1668,5])
sub_smri_cor[1669:nrow(sub_smri_cor),5]<-p.adjust(sub_smri_cor[1669:nrow(sub_smri_cor),5])

write.csv(rbind(tot_smri_cor,exin_smri_cor,sub_smri_cor),"result/cbcl_smri_cor_fdr.csv",quote = F)

exin_slope_smri_cor<-cbcl_smri_cor[cbcl_smri_cor$cbcl=="exts"|cbcl_smri_cor$cbcl=="ints",]
exin_slope_smri_cor[1:2,5]<-p.adjust(exin_slope_smri_cor[1:2,5])
exin_slope_smri_cor[3:4,5]<-p.adjust(exin_slope_smri_cor[3:4,5])
exin_slope_smri_cor[5:6,5]<-p.adjust(exin_slope_smri_cor[5:6,5])
exin_slope_smri_cor[7:142,5]<-p.adjust(exin_slope_smri_cor[7:142,5])
exin_slope_smri_cor[143:278,5]<-p.adjust(exin_slope_smri_cor[143:278,5])
exin_slope_smri_cor[279:nrow(exin_slope_smri_cor),5]<-p.adjust(exin_slope_smri_cor[279:nrow(exin_slope_smri_cor),5])

exin_inter_smri_cor<-cbcl_smri_cor[cbcl_smri_cor$cbcl=="exti"|cbcl_smri_cor$cbcl=="inti",]
exin_inter_smri_cor[1:2,5]<-p.adjust(exin_inter_smri_cor[1:2,5])
exin_inter_smri_cor[3:4,5]<-p.adjust(exin_inter_smri_cor[3:4,5])
exin_inter_smri_cor[5:6,5]<-p.adjust(exin_inter_smri_cor[5:6,5])
exin_inter_smri_cor[7:142,5]<-p.adjust(exin_inter_smri_cor[7:142,5])
exin_inter_smri_cor[143:278,5]<-p.adjust(exin_inter_smri_cor[143:278,5])
exin_inter_smri_cor[279:nrow(exin_inter_smri_cor),5]<-p.adjust(exin_inter_smri_cor[279:nrow(exin_inter_smri_cor),5])

write.csv(rbind(exin_slope_smri_cor,exin_inter_smri_cor),"result/exin_smri_cor_fdr.csv",quote = F)

#FI_smri_correlation
FI_smri_cor<-read.csv("result/FI_smri_correlation.csv")
FI_smri_cor[4:71,5]<-p.adjust(FI_smri_cor[4:71,5])
FI_smri_cor[72:139,5]<-p.adjust(FI_smri_cor[72:139,5])
FI_smri_cor[140:nrow(FI_smri_cor),5]<-p.adjust(FI_smri_cor[140:nrow(FI_smri_cor),5])
write.csv(FI_smri_cor,"result/FI_smri_cor_fdr.csv",quote = F)

cbcl_smri_cor_fdr<-rbind(tot_smri_cor,exin_slope_smri_cor,exin_inter_smri_cor,sub_smri_cor)
cbclfdr_FIfdr_smri_cor<-merge(cbcl_smri_cor_fdr,FI_smri_cor,by = "smri")
cbclfdr_FIfdr_smri_cor_sig<-cbclfdr_FIfdr_smri_cor[cbclfdr_FIfdr_smri_cor$cog_p<0.05&cbclfdr_FIfdr_smri_cor$cog_p_fi<0.05,]
write.csv(cbclfdr_FIfdr_smri_cor_sig,"result/cbclfdr1_FIfdr_smri_cor_sig.csv",quote = F)

cbcl_FIfdr_smri_cor<-merge(cbcl_smri_cor,FI_smri_cor,by = "smri")
cbcl_FIfdr_smri_cor_sig<-cbcl_FIfdr_smri_cor[cbcl_FIfdr_smri_cor$cog_p<0.05&cbcl_FIfdr_smri_cor$cog_p_fi<0.05,]
save.image("data/r0902_cbcl_FIfdr_smri_cor_sig.RData")
write.csv(cbcl_FIfdr_smri_cor_sig,"result/cbcl_FIfdr_smri_cor_sig.csv",quote = F)

library(readxl)
cbcl_FIfdr_smri_cor_sig<-as.data.frame(read_xlsx("result/cbcl_FIfdr_smri_cor_sig.xlsx",sheet = 2))
cbcl_FIfdr_smri_cor_sig[1:4,5]<-p.adjust(cbcl_FIfdr_smri_cor_sig[1:4,5])
cbcl_FIfdr_smri_cor_sig[6:14,5]<-p.adjust(cbcl_FIfdr_smri_cor_sig[6:14,5])
cbcl_FIfdr_smri_cor_sig[15:18,5]<-p.adjust(cbcl_FIfdr_smri_cor_sig[15:18,5])
cbcl_FIfdr_smri_cor_sig[18:22,5]<-p.adjust(cbcl_FIfdr_smri_cor_sig[18:22,5])
cbcl_FIfdr_smri_cor_sig[23:46,5]<-p.adjust(cbcl_FIfdr_smri_cor_sig[23:46,5])
cbcl_FIfdr_smri_cor_sig[47:75,5]<-p.adjust(cbcl_FIfdr_smri_cor_sig[47:75,5])
cbcl_FIfdr_smri_cor_sig[76:82,5]<-p.adjust(cbcl_FIfdr_smri_cor_sig[76:82,5])
cbcl_FIfdr_smri_cor_sig[83:91,5]<-p.adjust(cbcl_FIfdr_smri_cor_sig[83:91,5])
cbcl_FIfdr_smri_cor_sig[92:95,5]<-p.adjust(cbcl_FIfdr_smri_cor_sig[92:95,5])
write.csv(cbcl_FIfdr_smri_cor_sig,"result/cbclfdr_FIfdr_smri_cor_sig.csv",quote = F)

#######cbcl_dmri_correlation

cbcl_dmri_cor<-read.csv("result/cbcls_dmri_correlation0902.csv")
exin_dmri_cor<-cbcl_dmri_cor[cbcl_dmri_cor$cbcl=="exti"|cbcl_dmri_cor$cbcl=="exts"|cbcl_dmri_cor$cbcl=="inti"|cbcl_dmri_cor$cbcl=="ints",]
exin_dmri_cor<-exin_dmri_cor[1:2456,]
dmri_name<-unique(exin_dmri_cor$dmri)
fa1<-dmri_name[1:37]
md1<-dmri_name[43:79]
ld1<-dmri_name[85:121]
td1<-dmri_name[127:163]
fa2<-dmri_name[211:240]
md2<-dmri_name[241:270]
ld2<-dmri_name[271:300]
td2<-dmri_name[301:330]
fa3<-dmri_name[331:398]
md3<-dmri_name[402:469]
ld3<-dmri_name[473:540]
td3<-dmri_name[544:611]
df1<-data.frame()
namelist<-list(fa1,md1,ld1,td1,fa3,md3,ld3,td3)
for(i in 1:8){
  df<-exin_dmri_cor[exin_dmri_cor$dmri%in%namelist[[i]],]
  df[,5]<-p.adjust(df[,5])
  df1<-rbind(df1,df)
}
test<-merge(exin_dmri_cor,df1,by = c("dmri","cbcl"),all = T)
write.csv(test,"result/cbcl_dmri_cor_fdr.csv",quote = F)

library(readxl)
exin_dmri_cor<-read_xlsx("result/cbcl_dmri_cor_fdr.xlsx",sheet = 2)
colnames(exin_dmri_cor)[1]<-"dmri"

#FI_dmri_correlation
FI_dmri_cor<-read.csv("result/FI_dmri_correlation.csv")
FI_dmri_cor[1:37,5]<-p.adjust(FI_dmri_cor[1:37,5])
FI_dmri_cor[43:79,5]<-p.adjust(FI_dmri_cor[43:79,5])
FI_dmri_cor[85:121,5]<-p.adjust(FI_dmri_cor[85:121,5])
FI_dmri_cor[127:163,5]<-p.adjust(FI_dmri_cor[127:163,5])
#FI_dmri_cor[169:205,5]<-p.adjust(FI_dmri_cor[169:205,5])
FI_dmri_cor[331:398,5]<-p.adjust(FI_dmri_cor[331:398,5])
FI_dmri_cor[402:469,5]<-p.adjust(FI_dmri_cor[402:469,5])
FI_dmri_cor[473:540,5]<-p.adjust(FI_dmri_cor[473:540,5])
FI_dmri_cor[544:611,5]<-p.adjust(FI_dmri_cor[544:611,5])

#write.csv(FI_dmri_cor,"result/FI2_dmri_cor_fdr.csv",quote = F)


cbclfdr_FIfdr_dmri_cor<-merge(exin_dmri_cor,FI_dmri_cor,by = "dmri")
cbclfdr_FIfdr_dmri_cor_sig<-cbclfdr_FIfdr_dmri_cor[cbclfdr_FIfdr_dmri_cor$cog_p.y<0.05&cbclfdr_FIfdr_dmri_cor$cog_p_fi<0.05,]

save.image("data/r0904_cbclfdr_FIfdr_dmri_cor_sig.RData")
load("data/r0904_cbclfdr_FIfdr_dmri_cor_sig.RData")


