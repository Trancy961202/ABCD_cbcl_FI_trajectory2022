load("/home/luoqiang/lqy/r0901/step1/demo/r0901_1.RData")


library(parallel)
library(foreach)
library(doParallel)
library(lmerTest)

detectCores()
numCores <- 8
registerDoParallel(numCores)


#####################
# definition of functions
####################


multiAss.parallel <- function (assList, data, outfile.name) {
  results_analysis <- foreach (number=c(1:nrow(assList)), .combine=rbind,.packages = c("lmerTest","effectsize")) %dopar% {
    out <- formula(paste( assList$y.name[number], '~', paste(c(assList$cov.names.fixed[number],  assList$cov.names.rand[number]), collapse="+")))
    lmer.fit <- lmer(out,data = data)
    
    analysis <- summary(lmer.fit)[["coefficients"]]
    analysis1 <- eta_squared(lmer.fit)[,2]
    k<-1
    mix<-as.data.frame(matrix(NA,nrow = 1,ncol = 20))
    for(j in c(1:4,9)){
      mix[1,4*k-3]<-analysis[j+1,1]
      mix[1,4*k-1]<-analysis[j+1,4]
      mix[1,4*k]<-analysis[j+1,5]
      k<-k+1
    }
    mix[,c(2,6,10,14,18)]<-analysis1[c(1,2,3,3,5)]
    result <- mix[1,]
    print(mix[1,])
  }
  for(i in 1:nrow(results_analysis)){
    rownames(results_analysis)[i]<-paste(c(assList$y.name[i], assList$cov.names.rand[i]), collapse=":")
  }
  colnames(results_analysis)<-c("visit_beta","visit_eta","visit_t","visit_p","sex_beta","sex_eta","sex_t","sex_p","income1_beta","income1_eta","income1_t","income1_p",
                                "income2_beta","income2_eta","income2_t","income2_p","fam_beta","fam_eta","fam_t","fam_p")
  write.csv(results_analysis, outfile.name)
  cat("results are exported in a csv file and ready for download\n")
  results_analysis
}

multiAss.parallel.fit <- function (assList, data) {
  results_analysis <- foreach (number=c(1:nrow(assList))) %dopar% {
    library(lmerTest)
    out <- formula(paste( assList$y.name[number], '~', paste(c(assList$cov.names.fixed[number],  assList$cov.names.rand[number]), collapse="+")))
    lmer.fit <- lmer(out,data = data)
    lmer.fit
  }
  results_analysis
}







###  1.1 association between cbcls and covariations(no interaction) #########

x.names <- ''
y.names <- colnames(data_1_1_1)[2:10]
cov.fixed <- 'visit+sex+household.income+race_ethnicity+famhx_ss_momdad_hspd_p'
cov.rand <- c('(visit|site_id_l/rel_family_id/src_subject_id)','(1|site_id_l/rel_family_id/src_subject_id)')
# define the list for the multilevel association
x.name <- rep()
y.name <- rep()
inter <- rep()
cov.names.fixed <- rep()
cov.names.rand <- rep()
n <- 0
for (i in x.names){
  for(j in y.names){
    for(z in cov.rand){
      n <- n + 1
      x.name[n] <-  i
      y.name[n] <- j
      cov.names.fixed[n] <- cov.fixed
      cov.names.rand[n] <- z
    }
  }
}
outfile.name <- 'cbcls_no_interaction_cov0901.csv'
assList.cbcl.cov<- data.frame(x.name, y.name, cov.names.fixed, cov.names.rand)
results_analysis_cog <- multiAss.parallel(assList.cbcl.cov, data_1_1_1, outfile.name)
results_analysis_cog_fit <- multiAss.parallel.fit(assList.cbcl.cov, data_1_1_1)
stopImplicitCluster()
save.image("result0901_no_interaction_cov.RData")