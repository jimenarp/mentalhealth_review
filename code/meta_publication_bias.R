setwd(dir)
source("code/aux/McShane2016_supplement/pcurve.meta.function.R") # Simonsohn (2015) code
source("code/aux/McShane2016_supplement/selection.meta.functions.R") # McShane (2016) code
source("code/aux/AndrewsKasyRCode/VariationVarianceLogLikelihood.R") # Andrews & Casy (2017) selection model
source("code/aux/AndrewsKasyRCode/AuxiliaryFunctions.R") #for calculating se of beta estimate
source("code/aux/AndrewsKasyRCode/Step_function_normal_cdf.R")
source("code/aux/AndrewsKasyRCode/ReplicationAnalyticLogLikelihood.R")
source("code/aux/PublicationbiasGMM/R/PublicationbiasPackage.R")


load(file='data/final/final.Rdata')
load(file='data/final/final_one.Rdata')

### PUNIFORM #####


### many outcomes per study ####


final<-final %>% filter(!is.na(z_se))

puniform(yi=final$z_treatment_effect, vi=final$z_se , method = "P" , side= "right")


for (i in c('P','LNP','LN1MINP','KS')){  # method: Irwin-Hall, Fisher, first computes 1 - p-value in each study before applying Fisher's , Kolmogorov-Smirnov 
  results.temp<-puniform(yi=final$z_treatment_effect, vi=final$z_se , method = i , side= "right") #effect sizes are in the right tail (positive)
  coef.name<-paste('coef',i, sep='')
  coef.temp<-toformat(results.temp$est, decimals)
  assign(coef.name,coef.temp)
  
  if (i!='KS'){
    se.name<-paste('se',i, sep='')
    se.temp<-results.temp$ci.ub-results.temp$est
    se.temp<- toformat(se.temp, decimals)
    se.temp<- paste("(",se.temp,")",sep="")
    p.name<-paste('p',i, sep='')
    p.temp<-results.temp$pval.0
    assign(p.name,p.temp)
    se.temp<-p.stars(p.temp, se.temp)
    assign(se.name,se.temp)
    
  }
}


consolidated.all<- data.frame("group"=c("","All Interventions","","" ),
                              "P"=c("",coefP,seP,""),
                              "LNP"=c("",coefLNP,seLNP,""),
                              "LN1MINP"=c("",coefLN1MINP,seLN1MINP,""),
                              "KS"=c("",coefKS,"-","")
)



setwd(diroverleaf)


puniform_all.tex <- xtable(consolidated.all,
                           caption="Publication Bias (P-Uniform) Adjusted Estimates")


print(puniform_all.tex, file = "tables/puniform_all.tex", compress = FALSE, 
      include.rownames=FALSE,
      size = "small",
      caption.placement =  "top" , justify ='centre')

rm(coefP,seP,coefLNP,seLNP,coefLN1MINP,seLN1MINP,coefKS)



for (i in c('P','LNP','ML')){  # method: Irwin-Hall, Fisher, first computes 1 - p-value in each study before applying Fisher's , ML
  results.temp<-puni_star(yi=final$z_treatment_effect, vi=final$z_se , method = i , side= "right", alpha = 0.05)#effect sizes are in the right tail (positive)
  coef.name<-paste('coef',i, sep='')
  coef.temp<-toformat(results.temp$est, decimals)
  assign(coef.name,coef.temp)
  se.name<-paste('se',i, sep='')
  se.temp<-results.temp$est-results.temp$ci.lb
  se.temp<- toformat(se.temp, decimals)
  se.temp<- paste("(",se.temp,")",sep="")
  #p.name<-paste('p',i, sep='')
  #p.temp<-results.temp$pval.0
  #assign(p.name,p.temp)
  #se.temp<-p.stars(p.temp, se.temp)
  assign(se.name,se.temp)
}


consolidated.all<- data.frame("group"=c("","All Interventions","","" ),
                              "P"=c("",coefP,seP,""),
                              "LNP"=c("",coefLNP,seLNP,""),
                              "ML"=c("",coefML,seML,"")
)



setwd(diroverleaf)


punistar_all.tex <- xtable(consolidated.all,
                           caption="Publication Bias (P-Uniform*) Adjusted Estimates")


print(punistar_all.tex, file = "tables/punistar_all.tex", compress = FALSE, 
      include.rownames=FALSE,
      size = "small",
      caption.placement =  "top" , justify ='centre')


final<-final %>% filter(!is.na(z_se))
temp<-metagen(z_treatment_effect,
              z_se,
              data = final,
              studlab = archive_name,
              comb.fixed = FALSE,
              comb.random = TRUE,
              method.tau = "SJ",
              hakn = TRUE,
              prediction = TRUE,
              sm = "SMD")



eggers.test(x = temp)



setwd(diroverlaf)
pdf(file = 'figures/pcurve_all.pdf') 
test<-pcurve(temp)

dev.off() 

#### one outcome per study ####
res <- rma(yi=one_outcome, sei=one_outcome_sd, slab=archive_name, weighted=TRUE, data=final_one, method='SJ')


for (i in c('P','LNP','LN1MINP','KS')){
  results.temp<-puniform(yi=final_one$one_outcome, vi=final_one$one_outcome_sd , method = i , side= "right")
  coef.name<-paste('coef',i, sep='')
  coef.temp<-toformat(results.temp$est, decimals)
  assign(coef.name,coef.temp)
  
  if (i!='KS'){
    se.name<-paste('se',i, sep='')
    se.temp<-results.temp$ci.ub-results.temp$est
    se.temp<- toformat(se.temp, decimals)
    se.temp<- paste("(",se.temp,")",sep="")
    p.name<-paste('p',i, sep='')
    p.temp<-results.temp$pval.0
    assign(p.name,p.temp)
    se.temp<-p.stars(p.temp, se.temp)
    assign(se.name,se.temp)
    
  }
}




consolidated.one<- data.frame("group"=c("","All Interventions","","" ),
                              "P"=c("",coefP,seP,""),
                              "LNP"=c("",coefLNP,seLNP,""),
                              "LN1MINP"=c("",coefLN1MINP,seLN1MINP,""),
                              "KS"=c("",coefKS,"-","")
)


setwd(diroverleaf)


puniform_one.tex <- xtable(consolidated.one,
                     caption="Publication Bias (P-Uniform) Adjusted Estimates")


print(puniform_one.tex, file = "tables/puniform_one.tex", compress = FALSE, 
      include.rownames=FALSE,
      size = "small",
      caption.placement =  "top" , justify ='centre')

rm(coefP,seP,coefLNP,seLNP,coefLN1MINP,seLN1MINP,coefKS)


for (i in c('P','LNP','ML')){  # method: Irwin-Hall, Fisher, first computes 1 - p-value in each study before applying Fisher's , Kolmogorov-Smirnov 
  results.temp<-puni_star(yi=final_one$one_outcome, vi=final_one$one_outcome_sd  , method = i , side= "right", alpha=0.05) #effect sizes are in the right tail (positive)
  coef.name<-paste('coef',i, sep='')
  coef.temp<-toformat(results.temp$est, decimals)
  assign(coef.name,coef.temp)
    se.name<-paste('se',i, sep='')
    se.temp<-results.temp$est-results.temp$ci.lb
    se.temp<- toformat(se.temp, decimals)
    se.temp<- paste("(",se.temp,")",sep="")
    #p.name<-paste('p',i, sep='')
    #p.temp<-results.temp$pval.0
    #assign(p.name,p.temp)
    #se.temp<-p.stars(p.temp, se.temp)
    assign(se.name,se.temp)
}


consolidated.all<- data.frame("group"=c("","All Interventions","","" ),
                              "P"=c("",coefP,seP,""),
                              "LNP"=c("",coefLNP,seLNP,""),
                              "ML"=c("",coefML,seML,"")
)



setwd(diroverleaf)


punistar_one.tex <- xtable(consolidated.all,
                           caption="Publication Bias (P-Uniform)* Adjusted Estimates")


print(punistar_one.tex, file = "tables/punistar_one.tex", compress = FALSE, 
      include.rownames=FALSE,
      size = "small",
      caption.placement =  "top" , justify ='centre')




temp<-metagen(one_outcome,
              one_outcome_sd,
              data = final_one,
              studlab = archive_name,
              comb.fixed = FALSE,
              comb.random = TRUE,
              method.tau = "SJ",
              hakn = TRUE,
              prediction = TRUE,
              sm = "SMD")


eggers.test(x = temp)



setwd(diroverleaf)
pdf(file = 'figures/pcurve_one.pdf') 
test<-pcurve(temp)

dev.off() 

#######



library(weightr)

results<-weightfunct(final$z_treatment_effect, final$z_se, steps = c(0.05, 1), mods = NULL,
            weights = NULL, fe = FALSE, table = FALSE, pval = NULL)



copy<-final %>% mutate(tstat=z_treatment_effect/z_se) %>% filter(!is.na(tstat))
select<-dplyr::select
copy<-copy %>% select(z_treatment_effect, z_se, copy_all, tstat)
write.csv(copy,"test.csv", row.names = FALSE)

copy<-final %>% select(z_treatment_effect, z_se)
write.csv(copy,"test.csv", row.names = FALSE)

#remotes::install_github("JConigrave/msemtools")
library(msemtools)
theta0 <- c(0.2, 0.5, 0.5) # initial values (mean true effect size, tau, beta) where beta = P(publication | not sig)
results.3PSM <- estimate.onestep.selection.heterogeneous(copy$tstat,copy$all_n, copy$all_n, 0.05, theta0)

# search windows for 3 parameters of AK
lower.b = c(-Inf,0,0)
upper.b= c(Inf,Inf,1)
theta0 <- c(0.2, 0.5, 0.5) # initial values (mean true effect size, tau, beta) where beta = P(publication | not sig)
stepsize <- 10^(-6)
cutoffs = 1.96
symmetric = 0

LLH_only <-function (Psi) {
  A<-VariationVarianceLogLikelihood(Psi[1], Psi[2], c(Psi[-c(1,2)],  1),cutoffs,symmetric, final$z_treatment_effect,  final$z_se , normalizedsign = 0 );
  return (A$LLH) 
} # see if they have an R package by now (AK estimator). sets the log likelihood function given the parameters

findmin <- nlminb(objective=LLH_only, start=theta0,lower=lower.b,upper=upper.b) #takes only the Log Likelihood but not other parts of the object

results.AK <-findmin$par # the three parameters that maximize the log likelihood function. using this LL and this values to find the most likely
AK.se <- robust_se(findmin$par, stepsize)



### AK package
final<-final %>% filter(!is.na(final$z_se))
X=as.matrix(final$z_treatment_effect)
sigma=as.matrix(final$z_se)
cluster_ID=as.matrix(final$id)

Studynames=as.character(final$archive_name)


symmetric=1
cutoffs=1.96

PublicationbiasGMM(X,sigma,cluster_ID,symmetric,cutoffs,Studynames)

