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
outdir_overleaf<-"/Users/jimenaromero/Dropbox/Aplicaciones/Overleaf/wellcome_revision_21/tables"

load(file='final.Rdata')

final<- final %>% mutate(int_All=1)
final<- final %>% mutate(out_All=1) %>%
  mutate(out_Depression=ifelse(outcome_category=='Depression',1,0)) %>%
  mutate(out_Stress_Anx=ifelse(outcome_category=='Stress'|outcome_category=='Anxiety',1,0)) %>%
  mutate(out_Happy_Sat=ifelse(outcome_category=='Happiness or Satisfaction',1,0)) %>%
  mutate(out_Other=ifelse(outcome_category=='Other'|outcome_category=='Self-Esteem'|outcome_category=='Optimism',1,0))

final<-final %>% mutate(intervention_gdp=intervention_value_k/gdppercapita*100)
for(i in c('int_All', 'int_UCT','int_CCT', 'int_Neighborhood', 'int_Grad', 'int_Lottery', 'int_Asset', 'int_Healthcare')) {
  for( j in c(1, 'age','intervention_value_100k', 'duration_years','LMIC','female_share','intervention_gdp', 'gdppercapita','lumpsum')){
    tryCatch({
      temp_data<-final[final[[i]]==1,]
      temp_data<-temp_data[temp_data[[j]]==1,]
      temp_reg<-metagen(z_treatment_effect,
                        se_treatment_effect,
                        data = temp_data,
                        studlab = archive_name,
                        comb.fixed = FALSE,
                        comb.random = TRUE,
                        method.tau = "SJ",
                        hakn = TRUE,
                        prediction = TRUE,
                        sm = "SMD")
      a<-metareg(temp_reg,i)
      temp_coef<-a$beta[2]
      temp_coef<- toformat(temp_coef, decimals)
      temp_se<-a$se[2]
      temp_pval<-a$pval[2]
      temp_se<- toformat(temp_se, decimals)
      temp_se<- paste("(",temp_se,")",sep="")
      temp_se<-p.stars(temp_pval, temp_se)
   
      temp_name_coef<-paste('coef',substring(i, 5 ),j,sep='_')
      temp_name_se<-paste('se',substring(i, 5 ),j,sep='_')

      temp_name_ns<-paste('ns',substring(i, 5 ),j,sep='_')
      assign(temp_name_coef, temp_coef)
      assign(temp_name_se, temp_se)
    }, error=function(e){})
  }
}
