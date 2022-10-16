library(ggplot2)
library(ggsci)
library(ggpubr)

#########################################
#########################################
###########plot figure 1& S3#############
#########################################
#########################################

load("data/inter_slope0901.RData")
library(reshape2)
df<-data_1_1_1[,c(1,35,8:9,14)]
colnames(df)[3:4]<-c("Externalizing","Internalizing")
df<-melt(df,c("src_subject_id","visit","sex"))
colnames(df)[4]<-"symptom"
df1<-df
df1$visit<-factor(df1$visit,labels = c("10","11","12","13"))
p1<-ggplot(df1,aes(x=visit, y=value, group=src_subject_id)) + geom_line(aes(color=sex),size=0.005,alpha = 0.25) +
  geom_point(aes(color=sex),size=0.5,alpha = 0.25)+
  xlab(NULL)+ylab(NULL)+
  geom_smooth(mapping = aes(visit,value,group=sex,color=sex),method = "lm")+
  facet_wrap(.~symptom) + theme_bw() +
  scale_colour_brewer(palette = "Set1",direction = -1) +
  theme(legend.position = "top")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text = element_text(size=8, face = "bold"),
        strip.text = element_text(size = 10, face = "bold"))

library(readxl)
mydata<-read_xlsx("data/data_base_1year.xlsx")
mydata<-as.data.frame(mydata)
for (i in 1:nrow(mydata)) {
  for(j in 1:ncol(mydata))
    if(!is.na(mydata[i,j])&mydata[i,j]==999)
      mydata[i,j]<-NA
}
mydata$Gender<-factor(mydata$Gender,levels = c(0,1),labels = c("boy","girl"))
df<-mydata[,c(1,42,15,16,5)]
colnames(df)[3:5]<-c("Externalizing","Internalizing","sex")
df<-melt(df,c("User.code","visit1","sex"))
colnames(df)[4]<-"symptom"
df1<-df
df1$User.code<-as.factor(df1$User.code)
p2<-ggplot(df1,aes(x=visit1, y=value, group=User.code)) + geom_line(aes(color=sex),size=0.005,alpha = 0.25) +
  geom_point(aes(color=sex),size=0.5,alpha = 0.25)+
  xlab(NULL)+ylab(NULL)+
  geom_smooth(mapping = aes(visit1,value,group=sex,color=sex),method = "lm")+
  facet_wrap(.~symptom) + theme_bw() +
  scale_colour_brewer(palette = "Set1",direction = -1) +
  theme(legend.position = "top")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text = element_text(size=8, face = "bold"),
        strip.text = element_text(size = 10, face = "bold"))

#fig. cbcl vs cardsord
setwd("D:/lqy/ABCD/r0901")
load("data/r0901_tbss.RData")
data_cbcls_tbss1<-data_cbcls_tbss
data_cbcls_tbss1$visit<-data_cbcls_tbss1$visit/12+10
data_cbcls_tbss1$cog<-NA
data_cbcls_tbss1[!is.na(data_cbcls_tbss1$nihtbx_cardsort_fc)&data_cbcls_tbss1$nihtbx_cardsort_fc<
                   (mean(data_cbcls_tbss1$nihtbx_cardsort_fc,na.rm = T)-sd(data_cbcls_tbss1$nihtbx_cardsort_fc,na.rm = T)),]$cog<-"low_cog"
data_cbcls_tbss1[!is.na(data_cbcls_tbss1$nihtbx_cardsort_fc)&data_cbcls_tbss1$nihtbx_cardsort_fc>
                   (mean(data_cbcls_tbss1$nihtbx_cardsort_fc,na.rm = T)+sd(data_cbcls_tbss1$nihtbx_cardsort_fc,na.rm = T)),]$cog<-"high_cog"
data_cbcls_tbss1[!is.na(data_cbcls_tbss1$nihtbx_cardsort_fc)&data_cbcls_tbss1$nihtbx_cardsort_fc<=
                   (mean(data_cbcls_tbss1$nihtbx_cardsort_fc,na.rm = T)+sd(data_cbcls_tbss1$nihtbx_cardsort_fc,na.rm = T))&
                   data_cbcls_tbss1$nihtbx_cardsort_fc>=
                   (mean(data_cbcls_tbss1$nihtbx_cardsort_fc,na.rm = T)-sd(data_cbcls_tbss1$nihtbx_cardsort_fc,na.rm = T)),]$cog<-"medial_cog"
data_cbcls_tbss1$cog<-as.factor(data_cbcls_tbss1$cog)
data_cbcls_tbss1$cog<-factor(data_cbcls_tbss1$cog,levels = c("low_cog","medial_cog","high_cog"))
table(data_cbcls_tbss1$cog)
data_cbcls_tbss1<-data_cbcls_tbss1[,-34]
data_cbcls_tbss1<-data_cbcls_tbss1[complete.cases(data_cbcls_tbss1$cog),]
kruskal.test(data_cbcls_tbss1[data_cbcls_tbss1$visit==13,]$cbcl_scr_syn_internal_r~data_cbcls_tbss1[data_cbcls_tbss1$visit==13,]$cog)

p8<-ggplot(data_cbcls_tbss1,aes(x = visit,y =data_cbcls_tbss1$cbcl_scr_syn_internal_t,group=cog))+
  labs(title = "Cognitive Flexibility", y="", x = "")+
  stat_summary(geom = 'line',fun='mean',cex=2,aes(colour=cog))+
  stat_summary(fun.data = "mean_se",geom = "errorbar",width=0.2,cex=1,aes(colour=cog))+
  stat_summary(geom = 'point',fun='mean',size=4,pch=21,aes(colour=cog,fill=cog))+
  theme_bw()+theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+scale_color_npg()+scale_fill_npg()+
  theme(panel.border = element_blank())+theme(axis.line = element_line(colour = "black",size = 1))+
  theme_classic(base_size = 15)
#+theme(legend.position = "none")

p81<-ggplot(data_cbcls_tbss1,aes(x = visit,y =data_cbcls_tbss1$cbcl_scr_syn_internal_t,group=cog))+
  labs(title = "Cognitive Flexibility", y="", x = "")+
  stat_summary(geom = 'line',fun='mean',cex=2,aes(colour=cog))+
  stat_summary(fun.data = "mean_se",geom = "errorbar",width=0.2,cex=1,aes(colour=cog))+
  stat_summary(geom = 'point',fun='mean',size=4,pch=21,aes(colour=cog,fill=cog))+
  theme_bw()+theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+scale_color_npg()+scale_fill_npg()+
  theme(panel.border = element_blank())+theme(axis.line = element_line(colour = "black",size = 1))+
  theme_classic(base_size = 15)+facet_wrap(.~sex)

load("data/r0901_tbss.RData")
data_cbcls_tbss2<-data_cbcls_tbss
data_cbcls_tbss2$visit<-data_cbcls_tbss2$visit/12+10
data_cbcls_tbss2$cog<-NA
data_cbcls_tbss2[!is.na(data_cbcls_tbss2$nihtbx_list_fc)&data_cbcls_tbss2$nihtbx_list_fc<
                   (mean(data_cbcls_tbss2$nihtbx_list_fc,na.rm = T)-sd(data_cbcls_tbss2$nihtbx_list_fc,na.rm = T)),]$cog<-"low_cog"
data_cbcls_tbss2[!is.na(data_cbcls_tbss2$nihtbx_list_fc)&data_cbcls_tbss2$nihtbx_list_fc>
                   (mean(data_cbcls_tbss2$nihtbx_list_fc,na.rm = T)+sd(data_cbcls_tbss2$nihtbx_list_fc,na.rm = T)),]$cog<-"high_cog"
data_cbcls_tbss2[!is.na(data_cbcls_tbss2$nihtbx_list_fc)&data_cbcls_tbss2$nihtbx_list_fc<=
                   (mean(data_cbcls_tbss2$nihtbx_list_fc,na.rm = T)+sd(data_cbcls_tbss2$nihtbx_list_fc,na.rm = T))&
                   data_cbcls_tbss2$nihtbx_list_fc>=
                   (mean(data_cbcls_tbss2$nihtbx_list_fc,na.rm = T)-sd(data_cbcls_tbss2$nihtbx_list_fc,na.rm = T)),]$cog<-"medial_cog"
data_cbcls_tbss2$cog<-as.factor(data_cbcls_tbss2$cog)
data_cbcls_tbss2$cog<-factor(data_cbcls_tbss2$cog,levels = c("low_cog","medial_cog","high_cog"))
table(data_cbcls_tbss2$cog)
data_cbcls_tbss2<-data_cbcls_tbss2[,-34]
data_cbcls_tbss2<-data_cbcls_tbss2[complete.cases(data_cbcls_tbss2$cog),]
kruskal.test(data_cbcls_tbss2[data_cbcls_tbss2$visit==13,]$cbcl_scr_syn_external_r~data_cbcls_tbss2[data_cbcls_tbss2$visit==13,]$cog)


p7<-ggplot(data_cbcls_tbss2,aes(x = visit,y =data_cbcls_tbss2$cbcl_scr_syn_external_t,group=cog))+
  labs(title = "Working Memory", y="", x = "")+
  stat_summary(geom = 'line',fun='mean',cex=2,aes(colour=cog))+
  stat_summary(fun.data = "mean_se",geom = "errorbar",width=0.2,cex=1,aes(colour=cog))+
  stat_summary(geom = 'point',fun='mean',size=4,pch=21,aes(colour=cog,fill=cog))+
  theme_bw()+theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+scale_color_npg()+scale_fill_npg()+
  theme(panel.border = element_blank())+theme(axis.line = element_line(colour = "black",size = 1))+
  theme_classic(base_size = 15)
#+theme(legend.position = "none")

p71<-ggplot(data_cbcls_tbss2,aes(x = visit,y =data_cbcls_tbss2$cbcl_scr_syn_external_t,group=cog))+
  labs(title = "Working Memory", y="", x = "")+
  stat_summary(geom = 'line',fun='mean',cex=2,aes(colour=cog))+
  stat_summary(fun.data = "mean_se",geom = "errorbar",width=0.2,cex=1,aes(colour=cog))+
  stat_summary(geom = 'point',fun='mean',size=4,pch=21,aes(colour=cog,fill=cog))+
  theme_bw()+theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+scale_color_npg()+scale_fill_npg()+
  theme(panel.border = element_blank())+theme(axis.line = element_line(colour = "black",size = 1))+
  theme_classic(base_size = 15)+facet_wrap(.~sex)


#FI
load("data/r0901_tbss.RData")
data_cbcls_tbss$visit<-data_cbcls_tbss$visit/12+10
data_cbcls_tbss$cog<-NA
data_cbcls_tbss[!is.na(data_cbcls_tbss$nihtbx_fluidcomp_fc)&data_cbcls_tbss$nihtbx_fluidcomp_fc<
                  (mean(data_cbcls_tbss$nihtbx_fluidcomp_fc,na.rm = T)-sd(data_cbcls_tbss$nihtbx_fluidcomp_fc,na.rm = T)),]$cog<-"low_cog"
data_cbcls_tbss[!is.na(data_cbcls_tbss$nihtbx_fluidcomp_fc)&data_cbcls_tbss$nihtbx_fluidcomp_fc>
                  (mean(data_cbcls_tbss$nihtbx_fluidcomp_fc,na.rm = T)+sd(data_cbcls_tbss$nihtbx_fluidcomp_fc,na.rm = T)),]$cog<-"high_cog"
data_cbcls_tbss[!is.na(data_cbcls_tbss$nihtbx_fluidcomp_fc)&data_cbcls_tbss$nihtbx_fluidcomp_fc<=
                  (mean(data_cbcls_tbss$nihtbx_fluidcomp_fc,na.rm = T)+sd(data_cbcls_tbss$nihtbx_fluidcomp_fc,na.rm = T))&
                  data_cbcls_tbss$nihtbx_fluidcomp_fc>=
                  (mean(data_cbcls_tbss$nihtbx_fluidcomp_fc,na.rm = T)-sd(data_cbcls_tbss$nihtbx_fluidcomp_fc,na.rm = T)),]$cog<-"medial_cog"
data_cbcls_tbss$cog<-as.factor(data_cbcls_tbss$cog)
data_cbcls_tbss$cog<-factor(data_cbcls_tbss$cog,levels = c("low_cog","medial_cog","high_cog"))
table(data_cbcls_tbss$cog)
data_cbcls_tbss<-data_cbcls_tbss[,-34]
data_cbcls_tbss<-data_cbcls_tbss[complete.cases(data_cbcls_tbss$cog),]
kruskal.test(data_cbcls_tbss[data_cbcls_tbss$visit==13,]$cbcl_scr_syn_external_r~data_cbcls_tbss[data_cbcls_tbss$visit==13,]$cog)



p3<-ggplot(data_cbcls_tbss,aes(x = visit,y =data_cbcls_tbss$cbcl_scr_syn_external_t,group=cog))+
  labs(title = "Fluid Intelligence", y="", x = "")+
  stat_summary(geom = 'line',fun='mean',cex=2,aes(colour=cog))+
  stat_summary(fun.data = "mean_se",geom = "errorbar",width=0.2,cex=1,aes(colour=cog))+
  stat_summary(geom = 'point',fun='mean',size=4,pch=21,aes(colour=cog,fill=cog))+
  theme_bw()+theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+scale_color_npg()+scale_fill_npg()+
  theme(panel.border = element_blank())+theme(axis.line = element_line(colour = "black",size = 1))+
  theme_classic(base_size = 15)
#+theme(legend.position = "none")

p4<-ggplot(data_cbcls_tbss,aes(x = visit,y =data_cbcls_tbss$cbcl_scr_syn_internal_t,group=cog))+
  labs(title = "Fluid Intelligence", y="", x = "")+
  stat_summary(geom = 'line',fun='mean',cex=2,aes(colour=cog))+
  stat_summary(fun.data = "mean_se",geom = "errorbar",width=0.2,cex=1,aes(colour=cog))+
  stat_summary(geom = 'point',fun='mean',size=4,pch=21,aes(colour=cog,fill=cog))+
  theme_bw()+theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+scale_color_npg()+scale_fill_npg()+
  theme(panel.border = element_blank())+theme(axis.line = element_line(colour = "black",size = 1))+
  theme_classic(base_size = 15)
#+theme(legend.position = "none")
p10<-ggarrange(p3,p4,nrow = 2,ncol = 1,labels = c("C","D"),common.legend = TRUE, legend = "top")
p13<-ggarrange(p3,p4,nrow = 2,ncol = 1,labels = c("I","J"),common.legend = TRUE, legend = "top")

#FI_sex
p31<-ggplot(data_cbcls_tbss,aes(x = visit,y =data_cbcls_tbss$cbcl_scr_syn_external_t,group=cog))+
  labs(title = "Fluid Intelligence", y="", x = "")+
  stat_summary(geom = 'line',fun='mean',cex=2,aes(colour=cog))+
  stat_summary(fun.data = "mean_se",geom = "errorbar",width=0.2,cex=1,aes(colour=cog))+
  stat_summary(geom = 'point',fun='mean',size=4,pch=21,aes(colour=cog,fill=cog))+
  theme_bw()+theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+scale_color_npg()+scale_fill_npg()+
  theme(panel.border = element_blank())+theme(axis.line = element_line(colour = "black",size = 1))+
  theme_classic(base_size = 15)+facet_wrap(.~sex)
#+theme(legend.position = "none")

p41<-ggplot(data_cbcls_tbss,aes(x = visit,y =data_cbcls_tbss$cbcl_scr_syn_internal_t,group=cog))+
  labs(title = "Fluid Intelligence", y="", x = "")+
  stat_summary(geom = 'line',fun='mean',cex=2,aes(colour=cog))+
  stat_summary(fun.data = "mean_se",geom = "errorbar",width=0.2,cex=1,aes(colour=cog))+
  stat_summary(geom = 'point',fun='mean',size=4,pch=21,aes(colour=cog,fill=cog))+
  theme_bw()+theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+scale_color_npg()+scale_fill_npg()+
  theme(panel.border = element_blank())+theme(axis.line = element_line(colour = "black",size = 1))+
  theme_classic(base_size = 15)+facet_wrap(.~sex)
#+theme(legend.position = "none")
p15<-ggarrange(p31,p41,nrow = 2,ncol = 1,labels = c("A","B"),common.legend = TRUE, legend = "top")
p15

load("data/r0901_tbss.RData")
data_cbcls_tbss$visit<-data_cbcls_tbss$visit/12+10
data_cbcls_tbss$cog<-NA
data_cbcls_tbss[!is.na(data_cbcls_tbss$nihtbx_picture_fc)&data_cbcls_tbss$nihtbx_picture_fc<
                  (mean(data_cbcls_tbss$nihtbx_picture_fc,na.rm = T)-sd(data_cbcls_tbss$nihtbx_picture_fc,na.rm = T)),]$cog<-"low_cog"
data_cbcls_tbss[!is.na(data_cbcls_tbss$nihtbx_picture_fc)&data_cbcls_tbss$nihtbx_picture_fc>
                  (mean(data_cbcls_tbss$nihtbx_picture_fc,na.rm = T)+sd(data_cbcls_tbss$nihtbx_picture_fc,na.rm = T)),]$cog<-"high_cog"
data_cbcls_tbss[!is.na(data_cbcls_tbss$nihtbx_picture_fc)&data_cbcls_tbss$nihtbx_picture_fc<=
                  (mean(data_cbcls_tbss$nihtbx_picture_fc,na.rm = T)+sd(data_cbcls_tbss$nihtbx_picture_fc,na.rm = T))&
                  data_cbcls_tbss$nihtbx_picture_fc>=
                  (mean(data_cbcls_tbss$nihtbx_picture_fc,na.rm = T)-sd(data_cbcls_tbss$nihtbx_picture_fc,na.rm = T)),]$cog<-"medial_cog"
data_cbcls_tbss$cog<-as.factor(data_cbcls_tbss$cog)
data_cbcls_tbss$cog<-factor(data_cbcls_tbss$cog,levels = c("low_cog","medial_cog","high_cog"))
table(data_cbcls_tbss$cog)
data_cbcls_tbss<-data_cbcls_tbss[,-34]
data_cbcls_tbss<-data_cbcls_tbss[complete.cases(data_cbcls_tbss$cog),]
kruskal.test(data_cbcls_tbss[data_cbcls_tbss$visit==13,]$cbcl_scr_syn_external_r~data_cbcls_tbss[data_cbcls_tbss$visit==13,]$cog)


p5<-ggplot(data_cbcls_tbss,aes(x = visit,y =data_cbcls_tbss$cbcl_scr_syn_external_t,group=cog))+
  labs(title = "Episodic Memory", y="", x = "")+
  stat_summary(geom = 'line',fun='mean',cex=2,aes(colour=cog))+
  stat_summary(fun.data = "mean_se",geom = "errorbar",width=0.2,cex=1,aes(colour=cog))+
  stat_summary(geom = 'point',fun='mean',size=4,pch=21,aes(colour=cog,fill=cog))+
  theme_bw()+theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+scale_color_npg()+scale_fill_npg()+
  theme(panel.border = element_blank())+theme(axis.line = element_line(colour = "black",size = 1))+
  theme_classic(base_size = 15)
#+theme(legend.position = "none")

p6<-ggplot(data_cbcls_tbss,aes(x = visit,y =data_cbcls_tbss$cbcl_scr_syn_internal_t,group=cog))+
  labs(title = "Episodic Memory", y="", x = "")+
  stat_summary(geom = 'line',fun='mean',cex=2,aes(colour=cog))+
  stat_summary(fun.data = "mean_se",geom = "errorbar",width=0.2,cex=1,aes(colour=cog))+
  stat_summary(geom = 'point',fun='mean',size=4,pch=21,aes(colour=cog,fill=cog))+
  theme_bw()+theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+scale_color_npg()+scale_fill_npg()+
  theme(panel.border = element_blank())+theme(axis.line = element_line(colour = "black",size = 1))+
  theme_classic(base_size = 15)
#+theme(legend.position = "none")

p51<-ggplot(data_cbcls_tbss,aes(x = visit,y =data_cbcls_tbss$cbcl_scr_syn_external_t,group=cog))+
  labs(title = "Episodic Memory", y="", x = "")+
  stat_summary(geom = 'line',fun='mean',cex=2,aes(colour=cog))+
  stat_summary(fun.data = "mean_se",geom = "errorbar",width=0.2,cex=1,aes(colour=cog))+
  stat_summary(geom = 'point',fun='mean',size=4,pch=21,aes(colour=cog,fill=cog))+
  theme_bw()+theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+scale_color_npg()+scale_fill_npg()+
  theme(panel.border = element_blank())+theme(axis.line = element_line(colour = "black",size = 1))+
  theme_classic(base_size = 15)+facet_wrap(.~sex)
#+theme(legend.position = "none")

p61<-ggplot(data_cbcls_tbss,aes(x = visit,y =data_cbcls_tbss$cbcl_scr_syn_internal_t,group=cog))+
  labs(title = "Episodic Memory", y="", x = "")+
  stat_summary(geom = 'line',fun='mean',cex=2,aes(colour=cog))+
  stat_summary(fun.data = "mean_se",geom = "errorbar",width=0.2,cex=1,aes(colour=cog))+
  stat_summary(geom = 'point',fun='mean',size=4,pch=21,aes(colour=cog,fill=cog))+
  theme_bw()+theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+scale_color_npg()+scale_fill_npg()+
  theme(panel.border = element_blank())+theme(axis.line = element_line(colour = "black",size = 1))+
  theme_classic(base_size = 15)+facet_wrap(.~sex)
#+theme(legend.position = "none")


p11<-ggarrange(p5,p7,p6,p8,nrow = 2,ncol = 2,labels = c("E","G","F","H"),common.legend = TRUE, legend = "top")
p12<-ggarrange(p10,p11,p13,nrow = 1,widths = c(1,2,1))
p9<-ggarrange(p1,p2,widths = c(3,1),labels = c("A","B"))
pdf("figure1_0824.pdf",width = 8,height = 4)
ggarrange(p9,p12,nrow = 2,ncol = 1,heights = c(1,2))
dev.off()

p16<-ggarrange(p51,p71,p61,p81,nrow = 2,ncol = 2,labels = c("C","E","D","F"),common.legend = TRUE, legend = "top")
p17<-ggarrange(p15,p16,nrow = 1,widths = c(1,2))
p17


#########################################
#########################################
###########plot figure 2A-B##############
#########################################
#########################################

#fig2A-B
load("data/inter_slope0901.RData")
smri_base<-smri_base[match(index_qc,smri_base$src_subject_id),]
#combination of cbcls and smri
data_cbcls_smri<-cbind(data_1_1_1,smri_base[match(data_1_1_1$src_subject_id,smri_base$src_subject_id),])

data_cbcls_smri$visit<-data_cbcls_smri$visit/12+10
data_cbcls_smri$area<-NA
data_cbcls_smri[!is.na(data_cbcls_smri$smri_area_cdk_total)&data_cbcls_smri$smri_area_cdk_total<
                  (mean(data_cbcls_smri$smri_area_cdk_total,na.rm = T)-sd(data_cbcls_smri$smri_area_cdk_total,na.rm = T)),]$area<-"low_area"
data_cbcls_smri[!is.na(data_cbcls_smri$smri_area_cdk_total)&data_cbcls_smri$smri_area_cdk_total>
                  (mean(data_cbcls_smri$smri_area_cdk_total,na.rm = T)+sd(data_cbcls_smri$smri_area_cdk_total,na.rm = T)),]$area<-"high_area"
data_cbcls_smri[!is.na(data_cbcls_smri$smri_area_cdk_total)&data_cbcls_smri$smri_area_cdk_total<=
                  (mean(data_cbcls_smri$smri_area_cdk_total,na.rm = T)+sd(data_cbcls_smri$smri_area_cdk_total,na.rm = T))&
                  data_cbcls_smri$smri_area_cdk_total>=
                  (mean(data_cbcls_smri$smri_area_cdk_total,na.rm = T)-sd(data_cbcls_smri$smri_area_cdk_total,na.rm = T)),]$area<-"medial_area"
data_cbcls_smri$area<-as.factor(data_cbcls_smri$area)
data_cbcls_smri$area<-factor(data_cbcls_smri$area,levels = c("low_area","medial_area","high_area"))
table(data_cbcls_smri$area)
data_cbcls_smri<-data_cbcls_smri[,-36]
data_cbcls_smri<-data_cbcls_smri[complete.cases(data_cbcls_smri$area),]




p3<-ggplot(data_cbcls_smri,aes(x = visit,y =data_cbcls_smri$cbcl_scr_syn_external_r,group=area))+
  labs(title = "Total Surface Area", y="", x = "")+
  stat_summary(geom = 'line',fun='mean',cex=2,aes(colour=area))+
  stat_summary(fun.data = "mean_se",geom = "errorbar",width=0.2,cex=1,aes(colour=area))+
  stat_summary(geom = 'point',fun='mean',size=4,pch=21,aes(colour=area,fill=area))+
  theme_bw()+theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+scale_color_npg()+scale_fill_npg()+
  theme(panel.border = element_blank())+theme(axis.line = element_line(colour = "black",size = 1))+
  theme_classic(base_size = 15)
#+theme(legend.position = "none")

p4<-ggplot(data_cbcls_smri,aes(x = visit,y =data_cbcls_smri$cbcl_scr_syn_internal_r,group=area))+
  labs(title = "Total Surface Area", y="", x = "")+
  stat_summary(geom = 'line',fun='mean',cex=2,aes(colour=area))+
  stat_summary(fun.data = "mean_se",geom = "errorbar",width=0.2,cex=1,aes(colour=area))+
  stat_summary(geom = 'point',fun='mean',size=4,pch=21,aes(colour=area,fill=area))+
  theme_bw()+theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+scale_color_npg()+scale_fill_npg()+
  theme(panel.border = element_blank())+theme(axis.line = element_line(colour = "black",size = 1))+
  theme_classic(base_size = 15)
#+theme(legend.position = "none")
p14<-ggarrange(p3,p4,nrow = 2,ncol = 1,common.legend = TRUE, legend = "top")
p14
