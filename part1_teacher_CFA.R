rm(list = ls())
#teacher questionnaire
load("data/r0901_1.RData")
table(bpm$eventname)
bpm$visit<-0
bpm[bpm$eventname=="1_year_follow_up_y_arm_1",]$visit<-12
bpm[bpm$eventname=="2_year_follow_up_y_arm_1",]$visit<-24
bpm[bpm$eventname=="3_year_follow_up_y_arm_1",]$visit<-36
table(bpm$visit)
df<-merge(bpm,data_1_1_1,by = c("src_subject_id","visit"))
table(df$visit)
df$sex.x<-factor(df$sex.x,levels = c("M","F"),labels = c("boy","girl"))

library(lmerTest)
res<-as.data.frame(matrix(NA,nrow = 3,ncol = 4))
for (i in 11:13) {
  analysis<-summary(lmer(df[,i]~visit+sex.x+household.income+race_ethnicity+famhx_ss_momdad_hspd_p+
                           (1|site_id_l/rel_family_id/src_subject_id),data = df))[["coefficients"]]
  res[i-10,1:4]<-analysis[2,c(1,3,4,5)]
}
rownames(res)<-colnames(df)[11:13]
write.csv(res,"result/teacher.csv",quote = F)

#CFA
rm(list = ls())
load("result/result0901_interaction_cov.RData")
fit<-results_analysis_cog_fit
anxi<-ranef(fit[[1]])[["src_subject_id:(rel_family_id:site_id_l)"]][["(Intercept)"]]
anxs<-ranef(fit[[1]])[["src_subject_id:(rel_family_id:site_id_l)"]][["visit"]]
depi<-ranef(fit[[2]])[["src_subject_id:(rel_family_id:site_id_l)"]][["(Intercept)"]]
deps<-ranef(fit[[2]])[["src_subject_id:(rel_family_id:site_id_l)"]][["visit"]]
somi<-ranef(fit[[3]])[["src_subject_id:(rel_family_id:site_id_l)"]][["(Intercept)"]]
soms<-ranef(fit[[3]])[["src_subject_id:(rel_family_id:site_id_l)"]][["visit"]]
adhi<-ranef(fit[[4]])[["src_subject_id:(rel_family_id:site_id_l)"]][["(Intercept)"]]
adhs<-ranef(fit[[4]])[["src_subject_id:(rel_family_id:site_id_l)"]][["visit"]]
oppi<-ranef(fit[[5]])[["src_subject_id:(rel_family_id:site_id_l)"]][["(Intercept)"]]
opps<-ranef(fit[[5]])[["src_subject_id:(rel_family_id:site_id_l)"]][["visit"]]
coni<-ranef(fit[[6]])[["src_subject_id:(rel_family_id:site_id_l)"]][["(Intercept)"]]
cons<-ranef(fit[[6]])[["src_subject_id:(rel_family_id:site_id_l)"]][["visit"]]
exti<-ranef(fit[[7]])[["src_subject_id:(rel_family_id:site_id_l)"]][["(Intercept)"]]
exts<-ranef(fit[[7]])[["src_subject_id:(rel_family_id:site_id_l)"]][["visit"]]
inti<-ranef(fit[[8]])[["src_subject_id:(rel_family_id:site_id_l)"]][["(Intercept)"]]
ints<-ranef(fit[[8]])[["src_subject_id:(rel_family_id:site_id_l)"]][["visit"]]
intercept<-cbind(anxi,depi,somi,adhi,oppi,coni,exti,inti)
slope<-cbind(anxs,deps,soms,adhs,opps,cons,exts,ints)
com<-as.data.frame(cbind(intercept,slope))

library(lavaan)
inter.model<-'external =~ adhi + oppi + coni
internal =~ anxi + depi + somi'
inter.fit<-cfa(inter.model,data = com ,std.lv=T)
summary(inter.fit, fit.measures=TRUE)
fitmeasures(inter.fit,c("chisq","df","pvalue","Cfi","TLI","rmsea","bic"))
coef(inter.fit)

inter.model1<-'symptom =~ adhi + oppi + coni + anxi + depi + somi'
inter.fit<-cfa(inter.model1,data = com ,std.lv=T)
summary(inter.fit, fit.measures=TRUE)
fitmeasures(inter.fit,c("chisq","df","pvalue","Cfi","TLI","rmsea","bic"))
coef(inter.fit)


slope.model<-'external =~ adhs + opps + cons
internal =~ anxs + deps + soms'
inter.fit<-cfa(slope.model,data = com ,std.lv=T,check.gradient = FALSE)
summary(inter.fit, fit.measures=TRUE)
fitmeasures(inter.fit)
fitmeasures(inter.fit,c("chisq","df","pvalue","Cfi","TLI","rmsea","bic"))

slope.model1<-'symptom =~ adhs + opps + cons + anxs + deps + soms'
inter.fit<-cfa(slope.model1,data = com ,std.lv=T,check.gradient = FALSE)
summary(inter.fit, fit.measures=TRUE)
fitmeasures(inter.fit)
fitmeasures(inter.fit,c("chisq","df","pvalue","Cfi","TLI","rmsea","bic"))
