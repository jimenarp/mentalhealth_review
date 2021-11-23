

############## Packages
#Function to check if package exists, load if not
rm(list=ls())
ipak <- function(pkg){
  new_pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new_pkg)) 
    install.packages(new_pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

packs = c("tidyverse","pwr","metafor","devtools","xlsx", "puniform","metafore")

ipak(packs)
devtools::install_github("MathiasHarrer/dmetar")
library(dmetar)
library(meta)
library(metafor)


dir<-"/Users/jimenaromero/Dropbox/wellcome_review_2020/Revision_2021-01/code"
setwd(dir)

source("aux/McShane2016_supplement/pcurve.meta.function.R") # Simonsohn (2015) code
source("aux/McShane2016_supplement/selection.meta.functions.R") # McShane (2016) code
source("aux/AndrewsKasyRCode/VariationVarianceLogLikelihood.R") # Andrews & Casy (2017) selection model
source("aux/AndrewsKasyRCode/AuxiliaryFunctions.R") #for calculating se of beta estimate
source("aux/AndrewsKasyRCode/Step_function_normal_cdf.R")
source("aux/AndrewsKasyRCode/ReplicationAnalyticLogLikelihood.R")


decimals<-3

p.stars <-function(pval, se) {
  if (pval<=0.001) {
    se<-paste(se, '***',sep='' )
  } else if  (pval>0.001 & pval<=0.05) {
    se<-paste(se, '**',sep='' )
  } else if  (pval>0.05 & pval<=0.1) {
    se<-paste(se, '*',sep='' )
  } else {
    se<-se
  }
}


toformat <-function(number, n) {
  number=as.character(prettyNum(round(number,n),nsmall=n, big.mark=',', format=f))
}

dir<-"/Users/jimenaromero/Dropbox/wellcome_review_2020/Revision_2021-01/data/final"
setwd(dir)
outdir_overleaf<-"/Users/jimenaromero/Dropbox/Aplicaciones/Overleaf/wellcome_revision_21/figures"

load(file='final.Rdata')
load(file='final_one.Rdata')



res <- rma(yi=one_outcome, sei=one_outcome_sd, slab=archive_name, weighted=TRUE, data=final_one, method='SJ')



setwd(outdir_overleaf)
pdf(file = 'funnel.pdf') 
test<-funnel(res, main='Standard Error')

dev.off() 




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



setwd(outdir_overleaf)
pdf(file = 'pcurve_one.pdf') 
test<-pcurve(temp)

dev.off() 


## all

res <- rma(yi=z_treatment_effect, sei=z_se, slab=archive_name, weighted=TRUE, data=final, method='SJ')
funnel(res, main='Standard Error')


setwd(outdir_overleaf)
pdf(file = 'funnel_all.pdf') 
test<-funnel(res, main='Standard Error')

dev.off() 


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



#setwd(outdir_overleaf)
#pdf(file = 'pcurve_all.pdf') 
#test<-pcurve(temp)
#dev.off() 


## ALL ####


# getting one effect per group 

ids<-unique(final$id)

final<-final %>% mutate(one_outcome=NA) %>% mutate(one_outcome_sd=NA)
for (i in 1: length(ids)){
  tmp<-ids[[i]]
  tmpdata <- final[which(final$id==tmp),]
  if (nrow(tmpdata)==1) {
    one_trea=tmpdata$z_treatment_effect
    one_sd=tmpdata$z_se
  } else{
    temp_reg<-rma(yi=z_treatment_effect, sei=z_se, slab=archive_name, weighted=TRUE, data=tmpdata, method='SJ')
    one_trea<-temp_reg$beta[1,1]
    one_sd<-temp_reg$se
  }
  final[which(final$id==tmp),]$one_outcome=one_trea
  final[which(final$id==tmp),]$one_outcome_sd=one_sd
  print(one_trea)
}


final_one<- final %>% select(c("title","year","baseline_year","id","one_outcome" , "one_outcome_sd","archive_name")) %>%
  mutate(one_outcome=as.numeric(as.character(one_outcome))) %>%
  mutate(one_outcome_sd=as.numeric(as.character(one_outcome_sd)))

final_one<-aggregate(.~id,final_one, FUN='first')

final_one<-final_one %>%
  mutate(one_outcome=as.numeric(as.character(one_outcome))) %>%
  mutate(one_outcome_sd=as.numeric(as.character(one_outcome_sd)))


results.IH <- puniform(yi=final_one$one_outcome, vi=final_one$one_outcome_sd , method = "P" , side= "right")
results.LNP <- puniform(yi=final_one$one_outcome, vi=final_one$one_outcome_sd , method = "LNP" , side= "right")
results.LN1MINP <- puniform(yi=final_one$one_outcome, vi=final_one$one_outcome_sd , method = "LN1MINP" , side= "right")
results.KS <- puniform(yi=final_one$one_outcome, vi=final_one$one_outcome_sd , method = "KS" , side= "right")


library(weightr)

weightfunct(final$z_treatment_effect, final$z_se, steps = c(0.05, 1), mods = NULL,
            weights = NULL, fe = FALSE, table = FALSE, pval = NULL)



copy<-final %>% mutate(tstat=z_treatment_effect/z_se) %>% filter(!is.na(tstat))
select<-dplyr::select
copy<-copy %>% select(z_treatment_effect, z_se)
write.csv(copy,"test.csv", row.names = FALSE)
remotes::install_github("JConigrave/msemtools")

theta0 <- c(0.2, 0.5, 0.5) # initial values (mean true effect size, tau, beta) where beta = P(publication | not sig)
results.3PSM <- estimate.onestep.selection.heterogeneous(copy$tstat,copy$all_n, copy$all_n, 0.05, theta0)


LLH_only <-function (Psi) {
  A<-VariationVarianceLogLikelihood(Psi[1], Psi[2], c(Psi[-c(1,2)],  1),cutoffs,symmetric, gi, sdi , normalizedsign = 0 );
  return (A$LLH) 
} # see if they have an R package by now (AK estimator). sets the log likelihood function given the parameters


# search windows for 3 parameters of AK
lower.b = c(-Inf,0,0)
upper.b= c(Inf,Inf,1)
theta0 <- c(0.2, 0.5, 0.5) # initial values (mean true effect size, tau, beta) where beta = P(publication | not sig)

stepsize <- 10^(-6)
cutoffs = 1.96
symmetric = 0

findmin <- nlminb(objective=LLH_only, start=theta0,lower=lower.b,upper=upper.b) 



# clarifiy why one outcome per study
# send before the call
# Andrew and Casey, and McShane

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




consolidated.all<- data.frame("group"=c("","All Interventions","","" ),
                              "P"=c("",coefP,seP,""),
                              "LNP"=c("",coefLNP,seLNP,""),
                              "LN1MINP"=c("",coefLN1MINP,seLN1MINP,""),
                              "KS"=c("",coefKS,"-","")
)


rm(coefP,seP,coefLNP,seLNP,coefLN1MINP,seLN1MINP,coefKS)

## CCTs ####



# getting one effect per group 

ir<-"/Users/jimenaromero/Dropbox/wellcome_review_2020/Revision_2021-01/data/final"
setwd(dir)
outdir_overleaf<-"/Users/jimenaromero/Dropbox/Aplicaciones/Overleaf/wellcome_revision_21/figures"

load(file='final.Rdata')


final<-final %>% filter(int_CCT==1) %>% mutate(one_outcome=NA) %>% mutate(one_outcome_sd=NA)
ids<-unique(final$id)

for (i in 1: length(ids)){
  tmp<-ids[[i]]
  tmpdata <- final[which(final$id==tmp),]
  if (nrow(tmpdata)==1) {
    one_trea=tmpdata$z_treatment_effect
    one_sd=tmpdata$z_se
  } else{
    temp_reg<-rma(yi=z_treatment_effect, sei=z_se, slab=archive_name, weighted=TRUE, data=tmpdata, method='SJ')
    one_trea<-temp_reg$beta[1,1]
    one_sd<-temp_reg$se
  }
  final[which(final$id==tmp),]$one_outcome=one_trea
  final[which(final$id==tmp),]$one_outcome_sd=one_sd
  print(one_trea)
}


final_one<- final %>% select(c("title","year","baseline_year","id","one_outcome" , "one_outcome_sd","archive_name")) %>%
  mutate(one_outcome=as.numeric(as.character(one_outcome))) %>%
  mutate(one_outcome_sd=as.numeric(as.character(one_outcome_sd)))

final_one<-aggregate(.~id,final_one, FUN='first')

final_one<-final_one %>%
  mutate(one_outcome=as.numeric(as.character(one_outcome))) %>%
  mutate(one_outcome_sd=as.numeric(as.character(one_outcome_sd)))


results.IH <- puniform(yi=final_one$one_outcome, vi=final_one$one_outcome_sd , method = "P" , side= "right")
results.LNP <- puniform(yi=final_one$one_outcome, vi=final_one$one_outcome_sd , method = "LNP" , side= "right")
results.LN1MINP <- puniform(yi=final_one$one_outcome, vi=final_one$one_outcome_sd , method = "LN1MINP" , side= "right")
results.KS <- puniform(yi=final_one$one_outcome, vi=final_one$one_outcome_sd , method = "KS" , side= "right")




for (i in c('P','LNP','LN1MINP','KS')){
  results.temp<-puniform(yi=final_one$one_outcome, vi=final_one$one_outcome_sd , method = i , side= "right")
  coef.name<-paste('coef',i, sep='')
  coef.temp<-results.temp$est
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




consolidated.CCT<- data.frame("group"=c("","CCT","","" ),
                              "P"=c("",coefP,seP,""),
                              "LNP"=c("",coefLNP,seLNP,""),
                              "LN1MINP"=c("",coefLN1MINP,seLN1MINP,""),
                              "KS"=c("",coefKS,"-","")
)



results.3PSM <- estimate.onestep.selection.heterogeneous(zobs,N1, N2, 0.05, theta0)
