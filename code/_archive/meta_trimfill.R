
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

final<-final %>% mutate(int_All=1)
final<- final %>% mutate(out_All=1) %>%
  mutate(out_Depression=ifelse(outcome_category=='Depression',1,0)) %>%
  mutate(out_Stress_Anx=ifelse(outcome_category=='Stress'|outcome_category=='Anxiety',1,0)) %>%
  mutate(out_Happy_Sat=ifelse(outcome_category=='Happiness or Satisfaction',1,0)) %>%
  mutate(out_Other=ifelse(outcome_category=='Other'|outcome_category=='Self-Esteem'|outcome_category=='Optimism',1,0))


temp_data<-final %>% filter(int_All==1 & out_All==1)

temp<-metagen(z_treatment_effect,
        se_treatment_effect,
        data = temp_data,
        studlab = archive_name,
        comb.fixed = FALSE,
        comb.random = TRUE,
        method.tau = "SJ",
        hakn = TRUE,
        prediction = TRUE,
        sm = "SMD")
temp_reg<-trimfill(temp)

temp_coef<-temp_reg$TE.random
temp_coef<-toformat(temp_coef, decimals)
temp_se<-temp_reg$seTE.random
temp_se<-toformat(temp_se, decimals)
temp_se<-paste("(",temp_se,")",sep="")
temp_pval<-temp_reg$pval.random
temp_se<-p.stars(temp_pval, temp_se)

coef_All_All<-temp_coef
se_All_All<-temp_se




temp_data<-final %>% filter(int_UCT==1 & out_All==1)

temp<-metagen(z_treatment_effect,
              se_treatment_effect,
              data = temp_data,
              studlab = archive_name,
              comb.fixed = FALSE,
              comb.random = TRUE,
              method.tau = "SJ",
              hakn = TRUE,
              prediction = TRUE,
              sm = "SMD")
temp_reg<-trimfill(temp)

temp_coef<-temp_reg$TE.random
temp_coef<-toformat(temp_coef, decimals)
temp_se<-temp_reg$seTE.random
temp_se<-toformat(temp_se, decimals)
temp_se<-paste("(",temp_se,")",sep="")
temp_pval<-temp_reg$pval.random
temp_se<-p.stars(temp_pval, temp_se)

coef_UCT_All<-temp_coef
se_UCT_All<-temp_se




temp_data<-final %>% filter(int_CCT==1 & out_All==1)

temp<-metagen(z_treatment_effect,
              se_treatment_effect,
              data = temp_data,
              studlab = archive_name,
              comb.fixed = FALSE,
              comb.random = TRUE,
              method.tau = "SJ",
              hakn = TRUE,
              prediction = TRUE,
              sm = "SMD")
temp_reg<-trimfill(temp)

temp_coef<-temp_reg$TE.random
temp_coef<-toformat(temp_coef, decimals)
temp_se<-temp_reg$seTE.random
temp_se<-toformat(temp_se, decimals)
temp_se<-paste("(",temp_se,")",sep="")
temp_pval<-temp_reg$pval.random
temp_se<-p.stars(temp_pval, temp_se)

coef_CCT_All<-temp_coef
se_CCT_All<-temp_se




temp_data<-final %>% filter(int_Neighborhood==1 & out_All==1)

temp<-metagen(z_treatment_effect,
              se_treatment_effect,
              data = temp_data,
              studlab = archive_name,
              comb.fixed = FALSE,
              comb.random = TRUE,
              method.tau = "SJ",
              hakn = TRUE,
              prediction = TRUE,
              sm = "SMD")
temp_reg<-trimfill(temp)

temp_coef<-temp_reg$TE.random
temp_coef<-toformat(temp_coef, decimals)
temp_se<-temp_reg$seTE.random
temp_se<-toformat(temp_se, decimals)
temp_se<-paste("(",temp_se,")",sep="")
temp_pval<-temp_reg$pval.random
temp_se<-p.stars(temp_pval, temp_se)

coef_Neighborhood_All<-temp_coef
se_Neighborhood_All<-temp_se


temp_data<-final %>% filter(int_Grad==1 & out_All==1)

temp<-metagen(z_treatment_effect,
              se_treatment_effect,
              data = temp_data,
              studlab = archive_name,
              comb.fixed = FALSE,
              comb.random = TRUE,
              method.tau = "SJ",
              hakn = TRUE,
              prediction = TRUE,
              sm = "SMD")
temp_reg<-trimfill(temp)

temp_coef<-temp_reg$TE.random
temp_coef<-toformat(temp_coef, decimals)
temp_se<-temp_reg$seTE.random
temp_se<-toformat(temp_se, decimals)
temp_se<-paste("(",temp_se,")",sep="")
temp_pval<-temp_reg$pval.random
temp_se<-p.stars(temp_pval, temp_se)

coef_Grad_All<-temp_coef
se_Grad_All<-temp_se


temp_data<-final %>% filter(int_Lottery==1 & out_All==1)

temp<-metagen(z_treatment_effect,
              se_treatment_effect,
              data = temp_data,
              studlab = archive_name,
              comb.fixed = FALSE,
              comb.random = TRUE,
              method.tau = "SJ",
              hakn = TRUE,
              prediction = TRUE,
              sm = "SMD")
temp_reg<-trimfill(temp)

temp_coef<-temp_reg$TE.random
temp_coef<-toformat(temp_coef, decimals)
temp_se<-temp_reg$seTE.random
temp_se<-toformat(temp_se, decimals)
temp_se<-paste("(",temp_se,")",sep="")
temp_pval<-temp_reg$pval.random
temp_se<-p.stars(temp_pval, temp_se)

coef_Lottery_All<-temp_coef
se_Lottery_All<-temp_se



temp_data<-final %>% filter(int_Asset==1 & out_All==1)

temp<-metagen(z_treatment_effect,
              se_treatment_effect,
              data = temp_data,
              studlab = archive_name,
              comb.fixed = FALSE,
              comb.random = TRUE,
              method.tau = "SJ",
              hakn = TRUE,
              prediction = TRUE,
              sm = "SMD")
temp_reg<-trimfill(temp)

temp_coef<-temp_reg$TE.random
temp_coef<-toformat(temp_coef, decimals)
temp_se<-temp_reg$seTE.random
temp_se<-toformat(temp_se, decimals)
temp_se<-paste("(",temp_se,")",sep="")
temp_pval<-temp_reg$pval.random
temp_se<-p.stars(temp_pval, temp_se)

coef_Asset_All<-temp_coef
se_Asset_All<-temp_se




temp_data<-final %>% filter(int_Healthcare==1 & out_All==1)

temp<-metagen(z_treatment_effect,
              se_treatment_effect,
              data = temp_data,
              studlab = archive_name,
              comb.fixed = FALSE,
              comb.random = TRUE,
              method.tau = "SJ",
              hakn = TRUE,
              prediction = TRUE,
              sm = "SMD")
temp_reg<-trimfill(temp)

temp_coef<-temp_reg$TE.random
temp_coef<-toformat(temp_coef, decimals)
temp_se<-temp_reg$seTE.random
temp_se<-toformat(temp_se, decimals)
temp_se<-paste("(",temp_se,")",sep="")
temp_pval<-temp_reg$pval.random
temp_se<-p.stars(temp_pval, temp_se)

coef_Healthcare_All<-temp_coef
se_Healthcare_All<-temp_se




temp_data<-final %>% filter(int_All==1 & out_Depression==1)

temp<-metagen(z_treatment_effect,
              se_treatment_effect,
              data = temp_data,
              studlab = archive_name,
              comb.fixed = FALSE,
              comb.random = TRUE,
              method.tau = "SJ",
              hakn = TRUE,
              prediction = TRUE,
              sm = "SMD")
temp_reg<-trimfill(temp)

temp_coef<-temp_reg$TE.random
temp_coef<-toformat(temp_coef, decimals)
temp_se<-temp_reg$seTE.random
temp_se<-toformat(temp_se, decimals)
temp_se<-paste("(",temp_se,")",sep="")
temp_pval<-temp_reg$pval.random
temp_se<-p.stars(temp_pval, temp_se)

coef_All_Depression<-temp_coef
se_All_Depression<-temp_se


temp_data<-final %>% filter(int_UCT==1 & out_Depression==1)

temp<-metagen(z_treatment_effect,
              se_treatment_effect,
              data = temp_data,
              studlab = archive_name,
              comb.fixed = FALSE,
              comb.random = TRUE,
              method.tau = "SJ",
              hakn = TRUE,
              prediction = TRUE,
              sm = "SMD")
temp_reg<-trimfill(temp)

temp_coef<-temp_reg$TE.random
temp_coef<-toformat(temp_coef, decimals)
temp_se<-temp_reg$seTE.random
temp_se<-toformat(temp_se, decimals)
temp_se<-paste("(",temp_se,")",sep="")
temp_pval<-temp_reg$pval.random
temp_se<-p.stars(temp_pval, temp_se)

coef_UCT_Depression<-temp_coef
se_UCT_Depression<-temp_se




temp_data<-final %>% filter(int_CCT==1 & out_Depression==1)

temp<-metagen(z_treatment_effect,
              se_treatment_effect,
              data = temp_data,
              studlab = archive_name,
              comb.fixed = FALSE,
              comb.random = TRUE,
              method.tau = "SJ",
              hakn = TRUE,
              prediction = TRUE,
              sm = "SMD")
temp_reg<-trimfill(temp)

temp_coef<-temp_reg$TE.random
temp_coef<-toformat(temp_coef, decimals)
temp_se<-temp_reg$seTE.random
temp_se<-toformat(temp_se, decimals)
temp_se<-paste("(",temp_se,")",sep="")
temp_pval<-temp_reg$pval.random
temp_se<-p.stars(temp_pval, temp_se)

coef_CCT_Depression<-temp_coef
se_CCT_Depression<-temp_se




temp_data<-final %>% filter(int_Neighborhood==1 & out_Depression==1)

temp<-metagen(z_treatment_effect,
              se_treatment_effect,
              data = temp_data,
              studlab = archive_name,
              comb.fixed = FALSE,
              comb.random = TRUE,
              method.tau = "SJ",
              hakn = TRUE,
              prediction = TRUE,
              sm = "SMD")
temp_reg<-trimfill(temp)

temp_coef<-temp_reg$TE.random
temp_coef<-toformat(temp_coef, decimals)
temp_se<-temp_reg$seTE.random
temp_se<-toformat(temp_se, decimals)
temp_se<-paste("(",temp_se,")",sep="")
temp_pval<-temp_reg$pval.random
temp_se<-p.stars(temp_pval, temp_se)

coef_Neighborhood_Depression<-temp_coef
se_Neighborhood_Depression<-temp_se


temp_data<-final %>% filter(int_Grad==1 & out_Depression==1)

temp<-metagen(z_treatment_effect,
              se_treatment_effect,
              data = temp_data,
              studlab = archive_name,
              comb.fixed = FALSE,
              comb.random = TRUE,
              method.tau = "SJ",
              hakn = TRUE,
              prediction = TRUE,
              sm = "SMD")
temp_reg<-trimfill(temp)

temp_coef<-temp_reg$TE.random
temp_coef<-toformat(temp_coef, decimals)
temp_se<-temp_reg$seTE.random
temp_se<-toformat(temp_se, decimals)
temp_se<-paste("(",temp_se,")",sep="")
temp_pval<-temp_reg$pval.random
temp_se<-p.stars(temp_pval, temp_se)

coef_Grad_Depression<-'-'
se_Grad_Depression<-'-'


temp_data<-final %>% filter(int_Lottery==1 & out_Depression==1)

temp<-metagen(z_treatment_effect,
              se_treatment_effect,
              data = temp_data,
              studlab = archive_name,
              comb.fixed = FALSE,
              comb.random = TRUE,
              method.tau = "SJ",
              hakn = TRUE,
              prediction = TRUE,
              sm = "SMD")
temp_reg<-trimfill(temp)

temp_coef<-temp_reg$TE.random
temp_coef<-toformat(temp_coef, decimals)
temp_se<-temp_reg$seTE.random
temp_se<-toformat(temp_se, decimals)
temp_se<-paste("(",temp_se,")",sep="")
temp_pval<-temp_reg$pval.random
temp_se<-p.stars(temp_pval, temp_se)

coef_Lottery_Depression<-'-'
se_Lottery_Depression<-'-'



temp_data<-final %>% filter(int_Asset==1 & out_Depression==1)

temp<-metagen(z_treatment_effect,
              se_treatment_effect,
              data = temp_data,
              studlab = archive_name,
              comb.fixed = FALSE,
              comb.random = TRUE,
              method.tau = "SJ",
              hakn = TRUE,
              prediction = TRUE,
              sm = "SMD")
temp_reg<-trimfill(temp)

temp_coef<-temp_reg$TE.random
temp_coef<-toformat(temp_coef, decimals)
temp_se<-temp_reg$seTE.random
temp_se<-toformat(temp_se, decimals)
temp_se<-paste("(",temp_se,")",sep="")
temp_pval<-temp_reg$pval.random
temp_se<-p.stars(temp_pval, temp_se)

coef_Asset_Depression<-temp_coef
se_Asset_Depression<-temp_se




temp_data<-final %>% filter(int_Healthcare==1 & out_Depression==1)

temp<-metagen(z_treatment_effect,
              se_treatment_effect,
              data = temp_data,
              studlab = archive_name,
              comb.fixed = FALSE,
              comb.random = TRUE,
              method.tau = "SJ",
              hakn = TRUE,
              prediction = TRUE,
              sm = "SMD")
temp_reg<-trimfill(temp)

temp_coef<-temp_reg$TE.random
temp_coef<-toformat(temp_coef, decimals)
temp_se<-temp_reg$seTE.random
temp_se<-toformat(temp_se, decimals)
temp_se<-paste("(",temp_se,")",sep="")
temp_pval<-temp_reg$pval.random
temp_se<-p.stars(temp_pval, temp_se)

coef_Healthcare_Depression<-temp_coef
se_Healthcare_Depression<-temp_se








temp_data<-final %>% filter(int_All==1 & out_Stress_Anx==1)

temp<-metagen(z_treatment_effect,
              se_treatment_effect,
              data = temp_data,
              studlab = archive_name,
              comb.fixed = FALSE,
              comb.random = TRUE,
              method.tau = "SJ",
              hakn = TRUE,
              prediction = TRUE,
              sm = "SMD")
temp_reg<-trimfill(temp)

temp_coef<-temp_reg$TE.random
temp_coef<-toformat(temp_coef, decimals)
temp_se<-temp_reg$seTE.random
temp_se<-toformat(temp_se, decimals)
temp_se<-paste("(",temp_se,")",sep="")
temp_pval<-temp_reg$pval.random
temp_se<-p.stars(temp_pval, temp_se)

coef_All_Stress_Anx<-temp_coef
se_All_Stress_Anx<-temp_se


temp_data<-final %>% filter(int_UCT==1 & out_Stress_Anx==1)

temp<-metagen(z_treatment_effect,
              se_treatment_effect,
              data = temp_data,
              studlab = archive_name,
              comb.fixed = FALSE,
              comb.random = TRUE,
              method.tau = "SJ",
              hakn = TRUE,
              prediction = TRUE,
              sm = "SMD")
temp_reg<-trimfill(temp)

temp_coef<-temp_reg$TE.random
temp_coef<-toformat(temp_coef, decimals)
temp_se<-temp_reg$seTE.random
temp_se<-toformat(temp_se, decimals)
temp_se<-paste("(",temp_se,")",sep="")
temp_pval<-temp_reg$pval.random
temp_se<-p.stars(temp_pval, temp_se)

coef_UCT_Stress_Anx<-temp_coef
se_UCT_Stress_Anx<-temp_se




temp_data<-final %>% filter(int_CCT==1 & out_Stress_Anx==1)

temp<-metagen(z_treatment_effect,
              se_treatment_effect,
              data = temp_data,
              studlab = archive_name,
              comb.fixed = FALSE,
              comb.random = TRUE,
              method.tau = "SJ",
              hakn = TRUE,
              prediction = TRUE,
              sm = "SMD")
temp_reg<-trimfill(temp)

temp_coef<-temp_reg$TE.random
temp_coef<-toformat(temp_coef, decimals)
temp_se<-temp_reg$seTE.random
temp_se<-toformat(temp_se, decimals)
temp_se<-paste("(",temp_se,")",sep="")
temp_pval<-temp_reg$pval.random
temp_se<-p.stars(temp_pval, temp_se)

coef_CCT_Stress_Anx<-temp_coef
se_CCT_Stress_Anx<-temp_se




temp_data<-final %>% filter(int_Neighborhood==1 & out_Stress_Anx==1)

temp<-metagen(z_treatment_effect,
              se_treatment_effect,
              data = temp_data,
              studlab = archive_name,
              comb.fixed = FALSE,
              comb.random = TRUE,
              method.tau = "SJ",
              hakn = TRUE,
              prediction = TRUE,
              sm = "SMD")
temp_reg<-trimfill(temp)

temp_coef<-temp_reg$TE.random
temp_coef<-toformat(temp_coef, decimals)
temp_se<-temp_reg$seTE.random
temp_se<-toformat(temp_se, decimals)
temp_se<-paste("(",temp_se,")",sep="")
temp_pval<-temp_reg$pval.random
temp_se<-p.stars(temp_pval, temp_se)

coef_Neighborhood_Stress_Anx<-temp_coef
se_Neighborhood_Stress_Anx<-temp_se


temp_data<-final %>% filter(int_Grad==1 & out_Stress_Anx==1)

temp<-metagen(z_treatment_effect,
              se_treatment_effect,
              data = temp_data,
              studlab = archive_name,
              comb.fixed = FALSE,
              comb.random = TRUE,
              method.tau = "SJ",
              hakn = TRUE,
              prediction = TRUE,
              sm = "SMD")
temp_reg<-trimfill(temp)

temp_coef<-temp_reg$TE.random
temp_coef<-toformat(temp_coef, decimals)
temp_se<-temp_reg$seTE.random
temp_se<-toformat(temp_se, decimals)
temp_se<-paste("(",temp_se,")",sep="")
temp_pval<-temp_reg$pval.random
temp_se<-p.stars(temp_pval, temp_se)

coef_Grad_Stress_Anx<-temp_coef
se_Grad_Stress_Anx<-temp_se


temp_data<-final %>% filter(int_Lottery==1 & out_Stress_Anx==1)

temp<-metagen(z_treatment_effect,
              se_treatment_effect,
              data = temp_data,
              studlab = archive_name,
              comb.fixed = FALSE,
              comb.random = TRUE,
              method.tau = "SJ",
              hakn = TRUE,
              prediction = TRUE,
              sm = "SMD")
temp_reg<-trimfill(temp)

temp_coef<-temp_reg$TE.random
temp_coef<-toformat(temp_coef, decimals)
temp_se<-temp_reg$seTE.random
temp_se<-toformat(temp_se, decimals)
temp_se<-paste("(",temp_se,")",sep="")
temp_pval<-temp_reg$pval.random
temp_se<-p.stars(temp_pval, temp_se)

coef_Lottery_Stress_Anx<-temp_coef
se_Lottery_Stress_Anx<-temp_se



temp_data<-final %>% filter(int_Asset==1 & out_Stress_Anx==1)

temp<-metagen(z_treatment_effect,
              se_treatment_effect,
              data = temp_data,
              studlab = archive_name,
              comb.fixed = FALSE,
              comb.random = TRUE,
              method.tau = "SJ",
              hakn = TRUE,
              prediction = TRUE,
              sm = "SMD")
temp_reg<-trimfill(temp)

temp_coef<-temp_reg$TE.random
temp_coef<-toformat(temp_coef, decimals)
temp_se<-temp_reg$seTE.random
temp_se<-toformat(temp_se, decimals)
temp_se<-paste("(",temp_se,")",sep="")
temp_pval<-temp_reg$pval.random
temp_se<-p.stars(temp_pval, temp_se)

coef_Asset_Stress_Anx<-'-'
se_Asset_Stress_Anx<-'-'




temp_data<-final %>% filter(int_Healthcare==1 & out_Stress_Anx==1)

temp<-metagen(z_treatment_effect,
              se_treatment_effect,
              data = temp_data,
              studlab = archive_name,
              comb.fixed = FALSE,
              comb.random = TRUE,
              method.tau = "SJ",
              hakn = TRUE,
              prediction = TRUE,
              sm = "SMD")
temp_reg<-trimfill(temp)

temp_coef<-temp_reg$TE.random
temp_coef<-toformat(temp_coef, decimals)
temp_se<-temp_reg$seTE.random
temp_se<-toformat(temp_se, decimals)
temp_se<-paste("(",temp_se,")",sep="")
temp_pval<-temp_reg$pval.random
temp_se<-p.stars(temp_pval, temp_se)

coef_Healthcare_Stress_Anx<-'-'
se_Healthcare_Stress_Anx<-'-'

temp_data<-final %>% filter(int_All==1 & out_Happy_Sat==1)

temp<-metagen(z_treatment_effect,
              se_treatment_effect,
              data = temp_data,
              studlab = archive_name,
              comb.fixed = FALSE,
              comb.random = TRUE,
              method.tau = "SJ",
              hakn = TRUE,
              prediction = TRUE,
              sm = "SMD")
temp_reg<-trimfill(temp)

temp_coef<-temp_reg$TE.random
temp_coef<-toformat(temp_coef, decimals)
temp_se<-temp_reg$seTE.random
temp_se<-toformat(temp_se, decimals)
temp_se<-paste("(",temp_se,")",sep="")
temp_pval<-temp_reg$pval.random
temp_se<-p.stars(temp_pval, temp_se)

coef_All_Happy_Sat<-temp_coef
se_All_Happy_Sat<-temp_se


temp_data<-final %>% filter(int_UCT==1 & out_Happy_Sat==1)

temp<-metagen(z_treatment_effect,
              se_treatment_effect,
              data = temp_data,
              studlab = archive_name,
              comb.fixed = FALSE,
              comb.random = TRUE,
              method.tau = "SJ",
              hakn = TRUE,
              prediction = TRUE,
              sm = "SMD")
temp_reg<-trimfill(temp)

temp_coef<-temp_reg$TE.random
temp_coef<-toformat(temp_coef, decimals)
temp_se<-temp_reg$seTE.random
temp_se<-toformat(temp_se, decimals)
temp_se<-paste("(",temp_se,")",sep="")
temp_pval<-temp_reg$pval.random
temp_se<-p.stars(temp_pval, temp_se)

coef_UCT_Happy_Sat<-temp_coef
se_UCT_Happy_Sat<-temp_se




temp_data<-final %>% filter(int_CCT==1 & out_Happy_Sat==1)

temp<-metagen(z_treatment_effect,
              se_treatment_effect,
              data = temp_data,
              studlab = archive_name,
              comb.fixed = FALSE,
              comb.random = TRUE,
              method.tau = "SJ",
              hakn = TRUE,
              prediction = TRUE,
              sm = "SMD")
temp_reg<-trimfill(temp)

temp_coef<-temp_reg$TE.random
temp_coef<-toformat(temp_coef, decimals)
temp_se<-temp_reg$seTE.random
temp_se<-toformat(temp_se, decimals)
temp_se<-paste("(",temp_se,")",sep="")
temp_pval<-temp_reg$pval.random
temp_se<-p.stars(temp_pval, temp_se)

coef_CCT_Happy_Sat<-temp_coef
se_CCT_Happy_Sat<-temp_se




temp_data<-final %>% filter(int_Neighborhood==1 & out_Happy_Sat==1)

temp<-metagen(z_treatment_effect,
              se_treatment_effect,
              data = temp_data,
              studlab = archive_name,
              comb.fixed = FALSE,
              comb.random = TRUE,
              method.tau = "SJ",
              hakn = TRUE,
              prediction = TRUE,
              sm = "SMD")
temp_reg<-trimfill(temp)

temp_coef<-temp_reg$TE.random
temp_coef<-toformat(temp_coef, decimals)
temp_se<-temp_reg$seTE.random
temp_se<-toformat(temp_se, decimals)
temp_se<-paste("(",temp_se,")",sep="")
temp_pval<-temp_reg$pval.random
temp_se<-p.stars(temp_pval, temp_se)

coef_Neighborhood_Happy_Sat<-temp_coef
se_Neighborhood_Happy_Sat<-temp_se


temp_data<-final %>% filter(int_Grad==1 & out_Happy_Sat==1)

temp<-metagen(z_treatment_effect,
              se_treatment_effect,
              data = temp_data,
              studlab = archive_name,
              comb.fixed = FALSE,
              comb.random = TRUE,
              method.tau = "SJ",
              hakn = TRUE,
              prediction = TRUE,
              sm = "SMD")
temp_reg<-trimfill(temp)

temp_coef<-temp_reg$TE.random
temp_coef<-toformat(temp_coef, decimals)
temp_se<-temp_reg$seTE.random
temp_se<-toformat(temp_se, decimals)
temp_se<-paste("(",temp_se,")",sep="")
temp_pval<-temp_reg$pval.random
temp_se<-p.stars(temp_pval, temp_se)

coef_Grad_Happy_Sat<-temp_coef
se_Grad_Happy_Sat<-temp_se


temp_data<-final %>% filter(int_Lottery==1 & out_Happy_Sat==1)

temp<-metagen(z_treatment_effect,
              se_treatment_effect,
              data = temp_data,
              studlab = archive_name,
              comb.fixed = FALSE,
              comb.random = TRUE,
              method.tau = "SJ",
              hakn = TRUE,
              prediction = TRUE,
              sm = "SMD")
temp_reg<-trimfill(temp)

temp_coef<-temp_reg$TE.random
temp_coef<-toformat(temp_coef, decimals)
temp_se<-temp_reg$seTE.random
temp_se<-toformat(temp_se, decimals)
temp_se<-paste("(",temp_se,")",sep="")
temp_pval<-temp_reg$pval.random
temp_se<-p.stars(temp_pval, temp_se)

coef_Lottery_Happy_Sat<-temp_coef
se_Lottery_Happy_Sat<-temp_se



temp_data<-final %>% filter(int_Asset==1 & out_Happy_Sat==1)

temp<-metagen(z_treatment_effect,
              se_treatment_effect,
              data = temp_data,
              studlab = archive_name,
              comb.fixed = FALSE,
              comb.random = TRUE,
              method.tau = "SJ",
              hakn = TRUE,
              prediction = TRUE,
              sm = "SMD")
temp_reg<-trimfill(temp)

temp_coef<-temp_reg$TE.random
temp_coef<-toformat(temp_coef, decimals)
temp_se<-temp_reg$seTE.random
temp_se<-toformat(temp_se, decimals)
temp_se<-paste("(",temp_se,")",sep="")
temp_pval<-temp_reg$pval.random
temp_se<-p.stars(temp_pval, temp_se)

coef_Asset_Happy_Sat<-temp_coef
se_Asset_Happy_Sat<-temp_se




temp_data<-final %>% filter(int_Healthcare==1 & out_Happy_Sat==1)

temp<-metagen(z_treatment_effect,
              se_treatment_effect,
              data = temp_data,
              studlab = archive_name,
              comb.fixed = FALSE,
              comb.random = TRUE,
              method.tau = "SJ",
              hakn = TRUE,
              prediction = TRUE,
              sm = "SMD")
temp_reg<-trimfill(temp)

temp_coef<-temp_reg$TE.random
temp_coef<-toformat(temp_coef, decimals)
temp_se<-temp_reg$seTE.random
temp_se<-toformat(temp_se, decimals)
temp_se<-paste("(",temp_se,")",sep="")
temp_pval<-temp_reg$pval.random
temp_se<-p.stars(temp_pval, temp_se)

coef_Healthcare_Happy_Sat<-temp_coef
se_Healthcare_Happy_Sat<-temp_se



temp_data<-final %>% filter(int_All==1 & out_Other==1)

temp<-metagen(z_treatment_effect,
              se_treatment_effect,
              data = temp_data,
              studlab = archive_name,
              comb.fixed = FALSE,
              comb.random = TRUE,
              method.tau = "SJ",
              hakn = TRUE,
              prediction = TRUE,
              sm = "SMD")
temp_reg<-trimfill(temp)

temp_coef<-temp_reg$TE.random
temp_coef<-toformat(temp_coef, decimals)
temp_se<-temp_reg$seTE.random
temp_se<-toformat(temp_se, decimals)
temp_se<-paste("(",temp_se,")",sep="")
temp_pval<-temp_reg$pval.random
temp_se<-p.stars(temp_pval, temp_se)

coef_All_Other<-temp_coef
se_All_Other<-temp_se


temp_data<-final %>% filter(int_UCT==1 & out_Other==1)

temp<-metagen(z_treatment_effect,
              se_treatment_effect,
              data = temp_data,
              studlab = archive_name,
              comb.fixed = FALSE,
              comb.random = TRUE,
              method.tau = "SJ",
              hakn = TRUE,
              prediction = TRUE,
              sm = "SMD")
temp_reg<-trimfill(temp)

temp_coef<-temp_reg$TE.random
temp_coef<-toformat(temp_coef, decimals)
temp_se<-temp_reg$seTE.random
temp_se<-toformat(temp_se, decimals)
temp_se<-paste("(",temp_se,")",sep="")
temp_pval<-temp_reg$pval.random
temp_se<-p.stars(temp_pval, temp_se)

coef_UCT_Other<-temp_coef
se_UCT_Other<-temp_se




temp_data<-final %>% filter(int_CCT==1 & out_Other==1)

temp<-metagen(z_treatment_effect,
              se_treatment_effect,
              data = temp_data,
              studlab = archive_name,
              comb.fixed = FALSE,
              comb.random = TRUE,
              method.tau = "SJ",
              hakn = TRUE,
              prediction = TRUE,
              sm = "SMD")
temp_reg<-trimfill(temp)

temp_coef<-temp_reg$TE.random
temp_coef<-toformat(temp_coef, decimals)
temp_se<-temp_reg$seTE.random
temp_se<-toformat(temp_se, decimals)
temp_se<-paste("(",temp_se,")",sep="")
temp_pval<-temp_reg$pval.random
temp_se<-p.stars(temp_pval, temp_se)

coef_CCT_Other<-temp_coef
se_CCT_Other<-temp_se




temp_data<-final %>% filter(int_Neighborhood==1 & out_Other==1)

temp<-metagen(z_treatment_effect,
              se_treatment_effect,
              data = temp_data,
              studlab = archive_name,
              comb.fixed = FALSE,
              comb.random = TRUE,
              method.tau = "SJ",
              hakn = TRUE,
              prediction = TRUE,
              sm = "SMD")
temp_reg<-trimfill(temp)

temp_coef<-temp_reg$TE.random
temp_coef<-toformat(temp_coef, decimals)
temp_se<-temp_reg$seTE.random
temp_se<-toformat(temp_se, decimals)
temp_se<-paste("(",temp_se,")",sep="")
temp_pval<-temp_reg$pval.random
temp_se<-p.stars(temp_pval, temp_se)

coef_Neighborhood_Other<-temp_coef
se_Neighborhood_Other<-temp_se


temp_data<-final %>% filter(int_Grad==1 & out_Other==1)

temp<-metagen(z_treatment_effect,
              se_treatment_effect,
              data = temp_data,
              studlab = archive_name,
              comb.fixed = FALSE,
              comb.random = TRUE,
              method.tau = "SJ",
              hakn = TRUE,
              prediction = TRUE,
              sm = "SMD")
temp_reg<-trimfill(temp)

temp_coef<-temp_reg$TE.random
temp_coef<-toformat(temp_coef, decimals)
temp_se<-temp_reg$seTE.random
temp_se<-toformat(temp_se, decimals)
temp_se<-paste("(",temp_se,")",sep="")
temp_pval<-temp_reg$pval.random
temp_se<-p.stars(temp_pval, temp_se)

coef_Grad_Other<-temp_coef
se_Grad_Other<-temp_se


temp_data<-final %>% filter(int_Lottery==1 & out_Other==1)

temp<-metagen(z_treatment_effect,
              se_treatment_effect,
              data = temp_data,
              studlab = archive_name,
              comb.fixed = FALSE,
              comb.random = TRUE,
              method.tau = "SJ",
              hakn = TRUE,
              prediction = TRUE,
              sm = "SMD")
temp_reg<-trimfill(temp)

temp_coef<-temp_reg$TE.random
temp_coef<-toformat(temp_coef, decimals)
temp_se<-temp_reg$seTE.random
temp_se<-toformat(temp_se, decimals)
temp_se<-paste("(",temp_se,")",sep="")
temp_pval<-temp_reg$pval.random
temp_se<-p.stars(temp_pval, temp_se)

coef_Lottery_Other<-'-'
se_Lottery_Other<-'-'



temp_data<-final %>% filter(int_Asset==1 & out_Other==1)

temp<-metagen(z_treatment_effect,
              se_treatment_effect,
              data = temp_data,
              studlab = archive_name,
              comb.fixed = FALSE,
              comb.random = TRUE,
              method.tau = "SJ",
              hakn = TRUE,
              prediction = TRUE,
              sm = "SMD")
temp_reg<-trimfill(temp)

temp_coef<-temp_reg$TE.random
temp_coef<-toformat(temp_coef, decimals)
temp_se<-temp_reg$seTE.random
temp_se<-toformat(temp_se, decimals)
temp_se<-paste("(",temp_se,")",sep="")
temp_pval<-temp_reg$pval.random
temp_se<-p.stars(temp_pval, temp_se)

coef_Asset_Other<-'-'
se_Asset_Other<-'-'




temp_data<-final %>% filter(int_Healthcare==1 & out_Other==1)

temp<-metagen(z_treatment_effect,
              se_treatment_effect,
              data = temp_data,
              studlab = archive_name,
              comb.fixed = FALSE,
              comb.random = TRUE,
              method.tau = "SJ",
              hakn = TRUE,
              prediction = TRUE,
              sm = "SMD")
temp_reg<-trimfill(temp)

temp_coef<-temp_reg$TE.random
temp_coef<-toformat(temp_coef, decimals)
temp_se<-temp_reg$seTE.random
temp_se<-toformat(temp_se, decimals)
temp_se<-paste("(",temp_se,")",sep="")
temp_pval<-temp_reg$pval.random
temp_se<-p.stars(temp_pval, temp_se)

coef_Healthcare_Other<-'-'
se_Healthcare_Other<-'-'


consolidated.all<- data.frame("group"=c("","All Interventions","","" ),
                              "Reg 1"=c("",coef_All_All,se_All_All,""),
                              "Reg 2"=c("",coef_All_Depression,se_All_Depression,""),
                              "Reg 3"=c("",coef_All_Stress_Anx,se_All_Stress_Anx,""),
                              "Reg 4"=c("",coef_All_Happy_Sat,se_All_Happy_Sat,""),
                              "Reg 5"=c("",coef_All_Other,se_All_Other,"")
)



consolidated.UCT<- data.frame("group"=c("UCT","","" ),
                              "Reg 1"=c(coef_UCT_All,se_UCT_All,""),
                              "Reg 2"=c(coef_UCT_Depression,se_UCT_Depression,""),
                              "Reg 3"=c(coef_UCT_Stress_Anx,se_UCT_Stress_Anx,""),
                              "Reg 4"=c(coef_UCT_Happy_Sat,se_UCT_Happy_Sat,""),
                              "Reg 5"=c(coef_UCT_Other,se_UCT_Other,"")
)



consolidated.CCT<- data.frame("group"=c("CCT","","" ),
                              "Reg 1"=c(coef_CCT_All,se_CCT_All,""),
                              "Reg 2"=c(coef_CCT_Depression,se_CCT_Depression,""),
                              "Reg 3"=c(coef_CCT_Stress_Anx,se_CCT_Stress_Anx,""),
                              "Reg 4"=c(coef_CCT_Happy_Sat,se_CCT_Happy_Sat,""),
                              "Reg 5"=c(coef_CCT_Other,se_CCT_Other,"")
)


consolidated.Neighborhood<- data.frame("group"=c("Neighborhood","","" ),
                                       "Reg 1"=c(coef_Neighborhood_All,se_Neighborhood_All,""),
                                       "Reg 2"=c(coef_Neighborhood_Depression,se_Neighborhood_Depression,""),
                                       "Reg 3"=c(coef_Neighborhood_Stress_Anx,se_Neighborhood_Stress_Anx,""),
                                       "Reg 4"=c(coef_Neighborhood_Happy_Sat,se_Neighborhood_Happy_Sat,""),
                                       "Reg 5"=c(coef_Neighborhood_Other,se_Neighborhood_Other,"")
)


consolidated.Grad<- data.frame("group"=c("Graduation","","" ),
                               "Reg 1"=c(coef_Grad_All,se_Grad_All,""),
                               "Reg 2"=c(coef_Grad_Depression,se_Grad_Depression,""),
                               "Reg 3"=c(coef_Grad_Stress_Anx,se_Grad_Stress_Anx,""),
                               "Reg 4"=c(coef_Grad_Happy_Sat,se_Grad_Happy_Sat,""),
                               "Reg 5"=c(coef_Grad_Other,se_Grad_Other,"")
)


consolidated.Lottery<- data.frame("group"=c("Lottery","","" ),
                                  "Reg 1"=c(coef_Lottery_All,se_Lottery_All,""),
                                  "Reg 2"=c('-','-',""),
                                  "Reg 3"=c(coef_Lottery_Stress_Anx,se_Lottery_Stress_Anx,""),
                                  "Reg 4"=c(coef_Lottery_Happy_Sat,se_Lottery_Happy_Sat,""),
                                  "Reg 5"=c("-","-","")
)


consolidated.Asset<- data.frame("group"=c("Asset","","" ),
                                "Reg 1"=c(coef_Asset_All,se_Asset_All,""),
                                "Reg 2"=c(coef_Asset_Depression,se_Asset_Depression,""),
                                "Reg 3"=c(coef_Asset_Stress_Anx,se_Asset_Stress_Anx,""),
                                "Reg 4"=c(coef_Asset_Happy_Sat,se_Asset_Happy_Sat,""),
                                "Reg 5"=c("-","-","")
)


consolidated.Healthcare<- data.frame("group"=c("Insurance","","" ),
                                     "Reg 1"=c(coef_Healthcare_All,se_Healthcare_All,""),
                                     "Reg 2"=c(coef_Healthcare_Depression,se_Healthcare_Depression,""),
                                     "Reg 3"=c(coef_Healthcare_Stress_Anx,se_Healthcare_Stress_Anx,""),
                                     "Reg 4"=c(coef_Healthcare_Happy_Sat,se_Healthcare_Happy_Sat,""),
                                     "Reg 5"=c(coef_Healthcare_Other,se_Healthcare_Other,"")
)


consolidated_final<-rbind(consolidated.all, consolidated.UCT ,consolidated.CCT,consolidated.Neighborhood,  consolidated.Grad, consolidated.Lottery,consolidated.Asset,consolidated.Healthcare )




names(consolidated_final)<-c("","(1)","(2)","(3)","(4)","(5)")

pooled.tex <- xtable(consolidated_final,
                     caption="Effect using Trim Fill",
                     align=c(
                       "c{0.01cm}",
                       "c{0.3cm}",
                       "c{0.3cm}",
                       "c{0.3cm}",
                       "c{0.3cm}",
                       "c{0.3cm}",
                       "c{0.3cm}"))


addtorow <- list()
addtorow$pos <- list()
addtorow$pos[[1]] <- 0



addtorow$command <- c(" & Overall Mental Health & Depression & Stress Or Anxiety & Happiness & Other    \\\\  ")

setwd(outdir_overleaf)


print(pooled.tex, file = "pooled_trimfill.tex", compress = FALSE, 
      include.rownames=FALSE,
      #hline.after = c(-1,0,0,4,8,12,16,23,23),
      # sanitize.colnames.function=bold, 
      add.to.row = addtorow,
      size = "small",
      caption.placement =  "top" , justify ='centre')



to_graph<-data.frame("intervention"=c("Overall","UCT","CCT","Neighborhood","Asset", 'Grad','Lottery','Insurance' ),
                     "value"=c(coef_All_All, coef_UCT_All, coef_CCT_All, coef_Neighborhood_All, coef_Asset_All, coef_Grad_All, coef_Lottery_All, coef_Healthcare_All),
                     "se"=c(substring(se_All_All,2,6), substring(se_UCT_All,2,6) , substring(se_CCT_All,2,6), substring(se_Neighborhood_All,2,6),substring(se_Asset_All,2,6),substring(se_Grad_All,2,6),substring(se_Lottery_All,2,6),substring(se_Healthcare_All,2,6)))


to_graph<-to_graph %>% mutate(value=as.numeric(as.character(value))) %>% mutate(se=as.numeric(as.character(se)))
to_graph$intervention <- factor(to_graph$intervention,levels = c("Overall","UCT","CCT","Neighborhood","Asset", 'Grad','Lottery','Insurance'))

p<- ggplot(to_graph, aes(x=intervention, y=value)) + 
  geom_bar(stat="identity", color="black") +
  geom_errorbar(aes(ymin=value-se, ymax=value+se), width=.2) +labs(title="Pooled Effect on Different Interventions - Trimfill adjustment", x="", y = "Effect (in SD)")+
  theme_minimal() +geom_text(aes(label=round(value,3)), color='white',vjust=1.2)


outdir_overleaf<-"/Users/jimenaromero/Dropbox/Aplicaciones/Overleaf/wellcome_revision_21/figures"
setwd(outdir_overleaf)

ggsave(p, file='pooled_effect_trimfill.png')                 



to_graph<-data.frame("outcome"=c("Overall","Depression","Stress or Anxiety","Happiness","Other" ),
                     "value"=c(coef_All_All, coef_All_Depression, coef_All_Stress_Anx, coef_All_Happy_Sat, coef_All_Other),
                     "se"=c(substring(se_All_All,2,6), substring(se_All_Depression,2,6) , substring(se_All_Stress_Anx,2,6), substring(se_All_Happy_Sat,2,6),substring(se_All_Other,2,6)))


to_graph<-to_graph %>% mutate(value=as.numeric(as.character(value))) %>% mutate(se=as.numeric(as.character(se)))
to_graph$outcome <- factor(to_graph$outcome,levels = c("Overall","Depression","Stress or Anxiety","Happiness","Other"))

p<- ggplot(to_graph, aes(x=outcome, y=value)) + 
  geom_bar(stat="identity", color="black") +
  geom_errorbar(aes(ymin=value-se, ymax=value+se), width=.2) +labs(title="Pooled Effect on Different Outcomes - Trimfill adjustment", x="", y = "Effect (in SD)")+
  theme_minimal() +geom_text(aes(label=round(value,3)), color='white',vjust=1.2)


outdir_overleaf<-"/Users/jimenaromero/Dropbox/Aplicaciones/Overleaf/wellcome_revision_21/figures"
setwd(outdir_overleaf)

ggsave(p, file='pooled_effect_trimfill_out.png')                 

