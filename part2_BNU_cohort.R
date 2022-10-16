library(readxl)
mydata<-read_xlsx("data/data_base_1year.xlsx")
mydata<-as.data.frame(mydata)
for (i in 1:nrow(mydata)) {
  for(j in 1:ncol(mydata))
    if(!is.na(mydata[i,j])&mydata[i,j]==999)
      mydata[i,j]<-NA
}
mydata$Gender<-factor(mydata$Gender,levels = c(0,1),labels = c("boy","girl"))


#cross-lagged model
clpmModel <- 
  '
#Note, the data contain x1-2 and y1-2
#Latent mean Structure with intercepts
cog =~ 1*cog1 + 1*cog2
sym =~ 1*sym1 + 1*sym2
cog1 ~ mu1*1 #intercepts
cog2 ~ mu2*1
sym1 ~ pi1*1
sym2 ~ pi2*1
cog ~~ 0*cog #variance
sym ~~ 0*sym #variance
cog ~~ 0*sym #covariance
#laten vars for AR and cross-lagged effects
p1 =~ 1*cog1 #each factor loading set to 1
p2 =~ 1*cog2
q1 =~ 1*sym1
q2 =~ 1*sym2
#regressions
p2 ~ alpha2*p1 + beta2*q1 + sex + age + site + education + income
q2 ~ delta2*q1 + gamma2*p1+ sex + age + site + education + income
p1 ~ sex + age + site + education + income
q1 ~ sex + age + site + education + income
p1 ~~ p1 #variance
p2 ~~ u2*p2
q1 ~~ q1 #variance
q2 ~~ v2*q2
p1 ~~ q1 #p1 and q1 covariance
p2 ~~ q2'

####working memory
df1<-mydata
library(lavaan)
df<-df1[df1$visit==0,c(1,2,4,5,20,21,37)]
colnames(df)[1:7]<-c("id","age","site","sex","education","income","cog1")
df2<-df1[df1$visit==1,]
df$cog2<-df2[,37]
df$sym1<-df1[df1$visit==0,]$Exterlizing
df$sym2<-df2$Exterlizing
fit.clpmModel.ext <- lavaan(clpmModel, data = df,
                                missing = 'ML', #for the missing data!
                                int.ov.free = F,
                                int.lv.free = F,
                                auto.fix.first = F,
                                auto.fix.single = F,
                                auto.cov.lv.x = F,
                                auto.cov.y = F,
                                auto.var = F)

b<-standardizedSolution(fit.clpmModel.ext,type = "std.all", se = T, zstat = T,
                        pvalue = T, ci = T, level = 0.95, cov.std = T,
                        remove.eq = T, remove.ineq = T,remove.def = F,
                        partable = NULL, GLIST = NULL,est = NULL,output = "data.frame")
b$est.std[which(b$label=='gamma2')]
b$se[which(b$label=='gamma2')]
b$pvalue[which(b$label=='gamma2')]
b$ci.lower[which(b$label=='gamma2')]
b$ci.upper[which(b$label=='gamma2')]

b$est.std[which(b$label=='beta2')]
b$se[which(b$label=='beta2')]
b$pvalue[which(b$label=='beta2')]

b$est.std[which(b$label=='alpha2')]
b$se[which(b$label=='alpha2')]
b$pvalue[which(b$label=='alpha2')]

b$est.std[which(b$label=='delta2')]
b$se[which(b$label=='delta2')]
b$pvalue[which(b$label=='delta2')]



####cognitive flexibility
df<-mydata[1:713,c(1,2,4,5,20,21,38)]
colnames(df)[1:7]<-c("id","age","site","sex","education","income","cog1")
df$cog2<-mydata[714:1426,38]
df$sym1<-mydata[1:713,]$Interlizing
df$sym2<-mydata[714:1426,]$Interlizing
fit.clpmModel.int <- lavaan(clpmModel, data = df,
                                missing = 'ML', #for the missing data!
                                int.ov.free = F,
                                int.lv.free = F,
                                auto.fix.first = F,
                                auto.fix.single = F,
                                auto.cov.lv.x = F,
                                auto.cov.y = F,
                                auto.var = F)

b<-standardizedSolution(fit.clpmModel.int,type = "std.all", se = T, zstat = T,
                        pvalue = T, ci = T, level = 0.95, cov.std = T,
                        remove.eq = T, remove.ineq = T,remove.def = F,
                        partable = NULL, GLIST = NULL,est = NULL,output = "data.frame")
b$est.std[which(b$label=='gamma2')]
b$se[which(b$label=='gamma2')]
b$pvalue[which(b$label=='gamma2')]
b$ci.lower[which(b$label=='gamma2')]
b$ci.upper[which(b$label=='gamma2')]

b$est.std[which(b$label=='beta2')]
b$se[which(b$label=='beta2')]
b$pvalue[which(b$label=='beta2')]

b$est.std[which(b$label=='alpha2')]
b$se[which(b$label=='alpha2')]
b$pvalue[which(b$label=='alpha2')]

b$est.std[which(b$label=='delta2')]
b$se[which(b$label=='delta2')]
b$pvalue[which(b$label=='delta2')]

