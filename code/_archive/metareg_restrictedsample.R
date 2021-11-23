
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

load(file='final_stata.Rdata')
final<-final_stata
final<-final %>% mutate(int_All=1)
final<- final %>% mutate(out_All=1) %>%
  mutate(out_Depression=ifelse(outcome_category=='Depression',1,0)) %>%
  mutate(out_Stress_Anx=ifelse(outcome_category=='Stress'|outcome_category=='Anxiety',1,0)) %>%
  mutate(out_Happy_Sat=ifelse(outcome_category=='Happiness or Satisfaction',1,0)) %>%
  mutate(out_Other=ifelse(outcome_category=='Other'|outcome_category=='Self-Esteem'|outcome_category=='Optimism',1,0))

final<-final %>% mutate(intervention_gdppercapita = intervention_value/gdppercapita*100)
test<-
  metagen(z_treatment_effect,
          se_treatment_effect,
          data=final,
          studlab=archive_name,
          comb.fixed = TRUE,
          comb.random = FALSE,
          prediction=TRUE,
          sm="SMD")

metareg(test)


test<-metagen(z_treatment_effect,
              se_treatment_effect,
              data = final,
              studlab = archive_name,
              comb.fixed = FALSE,
              comb.random = TRUE,
              method.tau = "REML",
              hakn = FALSE,
              prediction = TRUE,
              sm = "SMD") #0.0824

metareg(test,  age + LMIC + lumpsum + female_share   + intervention_gdppercapita + duration_years)



rma(yi = z_treatment_effect, 
    sei = se_treatment_effect, 
    data = final, 
    method = "REML", 
    mods = ~ age+ LMIC + lumpsum + female_share   + intervention_gdppercapita + duration_years, 
    test = "z", 
    weighted = TRUE,
    slab=archive_name,
    ni=all_n)



rma(yi = z_treatment_effect, 
    sei = se_treatment_effect, 
    data = final, 
    method = "REML", 
    test = "z", 
    mods = ~ age,
    weighted = TRUE,
    slab=archive_name,
    ni=all_n)


library(metafor)
select<-dplyr:: select
reg<-final %>% filter(!is.na(lumpsum)&!is.na(LMIC) & !is.na( intervention_gdppercapita)& !is.na(age)&!is.na(female_share))
cor(reg %>% select(z_treatment_effect, LMIC, lumpsum, intervention_gdppercapita, duration_years, age, female_share))

library(PerformanceAnalytics)
chart.Correlation(reg %>% select(z_treatment_effect, LMIC, lumpsum, intervention_gdppercapita, duration_years, age, female_share))

model1 <- rma(yi = z_treatment_effect, 
             sei = se_treatment_effect, 
             data = reg, 
             method = "REML", 
             mods = ~ age, 
             test = "z")


#permutest(model1)


# ALL

temp_data<-reg
temp_reg<- rma(yi = z_treatment_effect, 
       sei = se_treatment_effect, 
       data = reg, 
       method = "REML", 
       mods = ~ age+ LMIC + lumpsum + female_share   + intervention_gdppercapita + duration_years, 
       test = "z")

temp_coef_intrcpt_all<- toformat(temp_reg$beta['intrcpt',1], decimals)
temp_coef_age_all<-toformat(temp_reg$beta['age',1], decimals)
temp_coef_lmic_all<-toformat(temp_reg$beta['LMIC',1], decimals)
temp_coef_lumpsum_all<-toformat(temp_reg$beta['lumpsum',1], decimals)
temp_coef_female_all<-toformat(temp_reg$beta['female_share',1], decimals)
temp_coef_cost_all<-toformat(temp_reg$beta['intervention_gdppercapita',1], decimals)
temp_coef_delay_all<-toformat(temp_reg$beta['duration_years',1], decimals)

temp_se_intrcpt_all<-toformat(temp_reg$se[1], decimals)
temp_se_age_all<-toformat(temp_reg$se[2], decimals)
temp_se_lmic_all<-toformat(temp_reg$se[3], decimals)
temp_se_lumpsum_all<-toformat(temp_reg$se[4], decimals)
temp_se_female_all<-toformat(temp_reg$se[5], decimals)
temp_se_cost_all<-toformat(temp_reg$se[6], decimals)
temp_se_delay_all<-toformat(temp_reg$se[7], decimals)

temp_p_intrcpt_all<-temp_reg$pval[1]
temp_p_age_all<-temp_reg$pval[2]
temp_p_lmic_all<-temp_reg$pval[3]
temp_p_lumpsum_all<-temp_reg$pval[4]
temp_p_female_all<-temp_reg$pval[5]
temp_p_cost_all<-temp_reg$pval[6]
temp_p_delay_all<-temp_reg$pval[7]

temp_se_intrcpt_all<- paste("(",temp_se_intrcpt_all,")",sep="")
temp_se_intrcpt_all<-p.stars(temp_p_intrcpt_all, temp_se_intrcpt_all)
temp_se_age_all<- paste("(",temp_se_age_all,")",sep="")
temp_se_age_all<-p.stars(temp_p_age_all, temp_se_age_all)
temp_se_lmic_all<- paste("(",temp_se_lmic_all,")",sep="")
temp_se_lmic_all<-p.stars(temp_p_lmic_all, temp_se_lmic_all)
temp_se_lumpsum_all<- paste("(",temp_se_lumpsum_all,")",sep="")
temp_se_lumpsum_all<-p.stars(temp_p_lumpsum_all, temp_se_lumpsum_all)
temp_se_female_all<- paste("(",temp_se_female_all,")",sep="")
temp_se_female_all<-p.stars(temp_p_female_all, temp_se_female_all)                          
temp_se_cost_all<- paste("(",temp_se_cost_all,")",sep="")
temp_se_cost_all<-p.stars(temp_p_cost_all, temp_se_cost_all)  
temp_se_delay_all<- paste("(",temp_se_delay_all,")",sep="")
temp_se_delay_all<-p.stars(temp_p_delay_all, temp_se_delay_all)  

temp_n<-temp_reg$k
temp_s<-length(unique(temp_data$archive_name))
temp_ns_all<-paste('[',temp_n,'/',temp_s,']')


# CCT

temp_data<-reg
temp_reg<- rma(yi = z_treatment_effect, 
               sei = se_treatment_effect, 
               data = reg %>% filter(int_CCT==1), 
               method = "REML", 
               mods = ~ age+ LMIC + lumpsum + female_share   + intervention_gdppercapita + duration_years, 
               test = "z")

temp_coef_intrcpt_cct<- toformat(temp_reg$beta['intrcpt',1], decimals)
temp_coef_age_cct<-toformat(temp_reg$beta['age',1], decimals)
temp_coef_lmic_cct<-toformat(temp_reg$beta['LMIC',1], decimals)
temp_coef_lumpsum_cct<-toformat(temp_reg$beta['lumpsum',1], decimals)
temp_coef_female_cct<-toformat(temp_reg$beta['female_share',1], decimals)
temp_coef_cost_cct<-toformat(temp_reg$beta['intervention_gdppercapita',1], decimals)
temp_coef_delay_cct<-toformat(temp_reg$beta['duration_years',1], decimals)

temp_se_intrcpt_cct<-toformat(temp_reg$se[1], decimals)
temp_se_age_cct<-toformat(temp_reg$se[2], decimals)
temp_se_lmic_cct<-toformat(temp_reg$se[3], decimals)
temp_se_lumpsum_cct<-toformat(temp_reg$se[4], decimals)
temp_se_female_cct<-toformat(temp_reg$se[5], decimals)
temp_se_cost_cct<-toformat(temp_reg$se[6], decimals)
temp_se_delay_cct<-toformat(temp_reg$se[7], decimals)

temp_p_intrcpt_cct<-temp_reg$pval[1]
temp_p_age_cct<-temp_reg$pval[2]
temp_p_lmic_cct<-temp_reg$pval[3]
temp_p_lumpsum_cct<-temp_reg$pval[4]
temp_p_female_cct<-temp_reg$pval[5]
temp_p_cost_cct<-temp_reg$pval[6]
temp_p_delay_cct<-temp_reg$pval[7]

temp_se_intrcpt_cct<- paste("(",temp_se_intrcpt_cct,")",sep="")
temp_se_intrcpt_cct<-p.stars(temp_p_intrcpt_cct, temp_se_intrcpt_cct)
temp_se_age_cct<- paste("(",temp_se_age_cct,")",sep="")
temp_se_age_cct<-p.stars(temp_p_age_cct, temp_se_age_cct)
temp_se_lmic_cct<- paste("(",temp_se_lmic_cct,")",sep="")
temp_se_lmic_cct<-p.stars(temp_p_lmic_cct, temp_se_lmic_cct)
temp_se_lumpsum_cct<- paste("(",temp_se_lumpsum_cct,")",sep="")
temp_se_lumpsum_cct<-p.stars(temp_p_lumpsum_cct, temp_se_lumpsum_cct)
temp_se_female_cct<- paste("(",temp_se_female_cct,")",sep="")
temp_se_female_cct<-p.stars(temp_p_female_cct, temp_se_female_cct)                          
temp_se_cost_cct<- paste("(",temp_se_cost_cct,")",sep="")
temp_se_cost_cct<-p.stars(temp_p_cost_cct, temp_se_cost_cct)  
temp_se_delay_cct<- paste("(",temp_se_delay_cct,")",sep="")
temp_se_delay_cct<-p.stars(temp_p_delay_cct, temp_se_delay_cct)  


temp_n<-temp_reg$k
temp_s<-length(unique(temp_data$archive_name))
temp_ns_cct<-paste('[',temp_n,'/',temp_s,']')



# UCT

temp_data<-reg
temp_reg<- rma(yi = z_treatment_effect, 
               sei = se_treatment_effect, 
               data = reg %>% filter(int_UCT==1), 
               method = "REML", 
               mods = ~ age+ LMIC + lumpsum + female_share   + intervention_gdppercapita + duration_years, 
               test = "z")
temp_coef_intrcpt_uct<- toformat(temp_reg$beta['intrcpt',1], decimals)
temp_coef_age_uct<-toformat(temp_reg$beta['age',1], decimals)
temp_coef_lmic_uct<-'-'
temp_coef_lumpsum_uct<-toformat(temp_reg$beta['lumpsum',1], decimals)
temp_coef_female_uct<-toformat(temp_reg$beta['female_share',1], decimals)
temp_coef_cost_uct<-toformat(temp_reg$beta['intervention_gdppercapita',1], decimals)
temp_coef_delay_uct<-toformat(temp_reg$beta['duration_years',1], decimals)

temp_se_intrcpt_uct<-toformat(temp_reg$se[1], decimals)
temp_se_age_uct<-toformat(temp_reg$se[2], decimals)
temp_se_lmic_uct<-'-'
temp_se_lumpsum_uct<-toformat(temp_reg$se[3], decimals)
temp_se_female_uct<-toformat(temp_reg$se[4], decimals)
temp_se_cost_uct<-toformat(temp_reg$se[5], decimals)
temp_se_delay_uct<-toformat(temp_reg$se[6], decimals)

temp_p_intrcpt_uct<-temp_reg$pval[1]
temp_p_age_uct<-temp_reg$pval[2]
temp_p_lmic_uct<-'-'
temp_p_lumpsum_uct<-temp_reg$pval[3]
temp_p_female_uct<-temp_reg$pval[4]
temp_p_cost_uct<-temp_reg$pval[5]
temp_p_delay_uct<-temp_reg$pval[6]

temp_se_intrcpt_uct<- paste("(",temp_se_intrcpt_uct,")",sep="")
temp_se_intrcpt_uct<-p.stars(temp_p_intrcpt_uct, temp_se_intrcpt_uct)
temp_se_age_uct<- paste("(",temp_se_age_uct,")",sep="")
temp_se_age_uct<-p.stars(temp_p_age_uct, temp_se_age_uct)

temp_se_lumpsum_uct<- paste("(",temp_se_lumpsum_uct,")",sep="")
temp_se_lumpsum_uct<-p.stars(temp_p_lumpsum_uct, temp_se_lumpsum_uct)
temp_se_female_uct<- paste("(",temp_se_female_uct,")",sep="")
temp_se_female_uct<-p.stars(temp_p_female_uct, temp_se_female_uct)                          
temp_se_cost_uct<- paste("(",temp_se_cost_uct,")",sep="")
temp_se_cost_uct<-p.stars(temp_p_cost_uct, temp_se_cost_uct)  
temp_se_delay_uct<- paste("(",temp_se_delay_uct,")",sep="")
temp_se_delay_uct<-p.stars(temp_p_delay_uct, temp_se_delay_uct)  

temp_n<-temp_reg$k
temp_s<-length(unique(temp_data$archive_name))
temp_ns_uct<-paste('[',temp_n,'/',temp_s,']')





# int_Neighborhood

temp_data<-reg
temp_reg<- rma(yi = z_treatment_effect, 
               sei = se_treatment_effect, 
               data = reg %>% filter(int_Neighborhood==1), 
               method = "REML", 
               mods = ~ age+ LMIC + lumpsum + female_share   + intervention_gdppercapita + duration_years, 
               test = "z")
temp_coef_intrcpt_neighborhood<- toformat(temp_reg$beta['intrcpt',1], decimals)
temp_coef_age_neighborhood<-toformat(temp_reg$beta['age',1], decimals)
temp_coef_lmic_neighborhood<-'-'
temp_coef_lumpsum_neighborhood<-'-'
temp_coef_female_neighborhood<-toformat(temp_reg$beta['female_share',1], decimals)
temp_coef_cost_neighborhood<-toformat(temp_reg$beta['intervention_gdppercapita',1], decimals)
temp_coef_delay_neighborhood<-toformat(temp_reg$beta['duration_years',1], decimals)

temp_se_intrcpt_neighborhood<-toformat(temp_reg$se[1], decimals)
temp_se_age_neighborhood<-toformat(temp_reg$se[2], decimals)
temp_se_lmic_neighborhood<-'-'
temp_se_lumpsum_neighborhood<-'-'
temp_se_female_neighborhood<-toformat(temp_reg$se[3], decimals)
temp_se_cost_neighborhood<-toformat(temp_reg$se[4], decimals)
temp_se_delay_neighborhood<-toformat(temp_reg$se[5], decimals)

temp_p_intrcpt_neighborhood<-temp_reg$pval[1]
temp_p_age_neighborhood<-temp_reg$pval[2]
temp_p_lmic_neighborhood<-'-'
temp_p_lumpsum_neighborhood<-'-'
temp_p_female_neighborhood<-temp_reg$pval[3]
temp_p_cost_neighborhood<-temp_reg$pval[4]
temp_p_delay_neighborhood<-temp_reg$pval[5]

temp_se_intrcpt_neighborhood<- paste("(",temp_se_intrcpt_neighborhood,")",sep="")
temp_se_intrcpt_neighborhood<-p.stars(temp_p_intrcpt_neighborhood, temp_se_intrcpt_neighborhood)
temp_se_age_neighborhood<- paste("(",temp_se_age_neighborhood,")",sep="")
temp_se_age_neighborhood<-p.stars(temp_p_age_neighborhood, temp_se_age_neighborhood)
temp_se_female_neighborhood<- paste("(",temp_se_female_neighborhood,")",sep="")
temp_se_female_neighborhood<-p.stars(temp_p_female_neighborhood, temp_se_female_neighborhood)                          
temp_se_cost_neighborhood<- paste("(",temp_se_cost_neighborhood,")",sep="")
temp_se_cost_neighborhood<-p.stars(temp_p_cost_neighborhood, temp_se_cost_neighborhood)  
temp_se_delay_neighborhood<- paste("(",temp_se_delay_neighborhood,")",sep="")
temp_se_delay_neighborhood<-p.stars(temp_p_delay_neighborhood, temp_se_delay_neighborhood)  

temp_n<-temp_reg$k
temp_s<-length(unique(temp_data$archive_name))
temp_ns_neighborhood<-paste('[',temp_n,'/',temp_s,']')


# GRAD

temp_data<-reg
temp_reg<- rma(yi = z_treatment_effect, 
               sei = se_treatment_effect, 
               data = reg %>% filter(int_Grad==1), 
               method = "REML", 
               mods = ~ age+ LMIC + lumpsum + female_share   + intervention_gdppercapita + duration_years, 
               test = "z")
temp_coef_intrcpt_grad<- toformat(temp_reg$beta['intrcpt',1], decimals)
temp_coef_age_grad<-toformat(temp_reg$beta['age',1], decimals)
temp_coef_lmic_grad<-'-'
temp_coef_lumpsum_grad<-toformat(temp_reg$beta['lumpsum',1], decimals)
temp_coef_female_grad<-'-'
temp_coef_cost_grad<-'-'
temp_coef_delay_grad<-toformat(temp_reg$beta['duration_years',1], decimals)

temp_se_intrcpt_grad<-toformat(temp_reg$se[1], decimals)
temp_se_age_grad<-toformat(temp_reg$se[2], decimals)
temp_se_lmic_grad<-'-'
temp_se_lumpsum_grad<-toformat(temp_reg$se[3], decimals)
temp_se_female_grad<-''
temp_se_cost_grad<-''
temp_se_delay_grad<-toformat(temp_reg$se[4], decimals)

temp_p_intrcpt_grad<-temp_reg$pval[1]
temp_p_age_grad<-temp_reg$pval[2]
temp_p_lmic_grad<-'-'
temp_p_lumpsum_grad<-temp_reg$pval[3]
temp_p_female_grad<-''
temp_p_cost_grad<-''
temp_p_delay_grad<-temp_reg$pval[4]

temp_se_intrcpt_grad<- paste("(",temp_se_intrcpt_grad,")",sep="")
temp_se_intrcpt_grad<-p.stars(temp_p_intrcpt_grad, temp_se_intrcpt_grad)
temp_se_age_grad<- paste("(",temp_se_age_grad,")",sep="")
temp_se_age_grad<-p.stars(temp_p_age_grad, temp_se_age_grad)


temp_se_lumpsum_grad<- paste("(",temp_se_lumpsum_grad,")",sep="")
temp_se_lumpsum_grad<-p.stars(temp_p_lumpsum_grad, temp_se_lumpsum_grad)

temp_se_delay_grad<- paste("(",temp_se_delay_grad,")",sep="")
temp_se_delay_grad<-p.stars(temp_p_delay_grad, temp_se_delay_grad)  

temp_n<-temp_reg$k
temp_s<-length(unique(temp_data$archive_name))
temp_ns_grad<-paste('[',temp_n,'/',temp_s,']')


## Lottery


temp_data<-reg
temp_reg<- rma(yi = z_treatment_effect, 
               sei = se_treatment_effect, 
               data = reg %>% filter(int_Lottery==1), 
               method = "REML", 
               mods = ~ age+ LMIC + lumpsum + female_share   + intervention_gdppercapita + duration_years, 
               test = "z")

temp_coef_intrcpt_lottery<- toformat(temp_reg$beta['intrcpt',1], decimals)
temp_coef_age_lottery<-toformat(temp_reg$beta['age',1], decimals)
temp_coef_lmic_lottery<-'-'
temp_coef_lumpsum_lottery<-'-'
temp_coef_female_lottery<-'-'
temp_coef_cost_lottery<-'-'
temp_coef_delay_lottery<-'-'

temp_se_intrcpt_lottery<-toformat(temp_reg$se[1], decimals)
temp_se_age_lottery<-toformat(temp_reg$se[2], decimals)
temp_se_lmic_lottery<-'-'
temp_se_lumpsum_lottery<-'-'
temp_se_female_lottery<-''
temp_se_cost_lottery<-''
temp_se_delay_lottery<-''

temp_p_intrcpt_lottery<-temp_reg$pval[1]
temp_p_age_lottery<-temp_reg$pval[2]
temp_p_lmic_lottery<-'-'
temp_p_lumpsum_lottery<-'-'
temp_p_female_lottery<-'-'
temp_p_cost_lottery<-'-'
temp_p_delay_lottery<-'-'

temp_se_intrcpt_lottery<- paste("(",temp_se_intrcpt_lottery,")",sep="")
temp_se_intrcpt_lottery<-p.stars(temp_p_intrcpt_lottery, temp_se_intrcpt_lottery)

temp_se_age_lottery<- paste("(",temp_se_age_lottery,")",sep="")
temp_se_age_lottery<-p.stars(temp_p_age_lottery, temp_se_age_lottery)

temp_n<-temp_reg$k
temp_s<-length(unique(temp_data$archive_name))
temp_ns_lottery<-paste('[',temp_n,'/',temp_s,']')

## Asset

temp_data<-reg
temp_reg<- rma(yi = z_treatment_effect, 
               sei = se_treatment_effect, 
               data = reg %>% filter(int_Asset==1), 
               method = "REML", 
               mods = ~ age+ LMIC + lumpsum + female_share   + intervention_gdppercapita + duration_years, 
               test = "z")


temp_coef_intrcpt_asset<- toformat(temp_reg$beta['intrcpt',1], decimals)
temp_coef_age_asset<-toformat(temp_reg$beta['age',1], decimals)
temp_coef_lmic_asset<-'-'
temp_coef_lumpsum_asset<-'-'
temp_coef_female_asset<-toformat(temp_reg$beta['female_share',1], decimals)
temp_coef_cost_asset<-'-'
temp_coef_delay_asset<-toformat(temp_reg$beta['duration_years',1], decimals)

temp_se_intrcpt_asset<-toformat(temp_reg$se[1], decimals)
temp_se_age_asset<-toformat(temp_reg$se[2], decimals)
temp_se_lmic_asset<-'-'
temp_se_lumpsum_asset<-''
temp_se_female_asset<-toformat(temp_reg$se[3], decimals)
temp_se_cost_asset<-'-'
temp_se_delay_asset<-toformat(temp_reg$se[4], decimals)

temp_p_intrcpt_asset<-temp_reg$pval[1]
temp_p_age_asset<-temp_reg$pval[2]
temp_p_lmic_asset<-'-'
temp_p_lumpsum_asset<-''
temp_p_female_asset<-temp_reg$pval[3]
temp_p_cost_asset<-'-'
temp_p_delay_asset<-temp_reg$pval[4]

temp_se_intrcpt_asset<- paste("(",temp_se_intrcpt_asset,")",sep="")
temp_se_intrcpt_asset<-p.stars(temp_p_intrcpt_asset, temp_se_intrcpt_asset)
temp_se_age_asset<- paste("(",temp_se_age_asset,")",sep="")
temp_se_age_asset<-p.stars(temp_p_age_asset, temp_se_age_asset)

temp_se_female_asset<- paste("(",temp_se_female_asset,")",sep="")
temp_se_female_asset<-p.stars(temp_p_female_asset, temp_se_female_asset)                          

temp_se_delay_asset<- paste("(",temp_se_delay_asset,")",sep="")
temp_se_delay_asset<-p.stars(temp_p_delay_asset, temp_se_delay_asset)  

temp_n<-temp_reg$k
temp_s<-length(unique(temp_data$archive_name))
temp_ns_asset<-paste('[',temp_n,'/',temp_s,']')

#int_Healthcare

temp_data<-reg
temp_reg<- rma(yi = z_treatment_effect, 
               sei = se_treatment_effect, 
               data = reg %>% filter(int_Healthcare==1), 
               method = "REML", 
               mods = ~ age+ LMIC + lumpsum + female_share   + intervention_gdppercapita + duration_years, 
               test = "z")

temp_coef_intrcpt_healthcare<- toformat(temp_reg$beta['intrcpt',1], decimals)
temp_coef_age_healthcare<- '-'
temp_coef_lmic_healthcare<-'-'
temp_coef_lumpsum_healthcare<-'-'
temp_coef_female_healthcare<-toformat(temp_reg$beta['female_share',1], decimals)
temp_coef_cost_healthcare<-'-'
temp_coef_delay_healthcare<-'-'

temp_se_intrcpt_healthcare<-toformat(temp_reg$se[1], decimals)
temp_se_age_healthcare<-'-'
temp_se_lmic_healthcare<-'-'
temp_se_lumpsum_healthcare<-'-'
temp_se_female_healthcare<-toformat(temp_reg$se[2], decimals)
temp_se_cost_healthcare<-'-'
temp_se_delay_healthcare<-'-'

temp_p_intrcpt_healthcare<-temp_reg$pval[1]
temp_p_age_healthcare<-'-'
temp_p_lmic_healthcare<-
temp_p_lumpsum_healthcare<-'-'
temp_p_female_healthcare<-temp_reg$pval[2]
temp_p_cost_healthcare<-'-'
temp_p_delay_healthcare<-'-'


temp_n<-temp_reg$k
temp_s<-length(unique(temp_data$archive_name))
temp_ns_healthcare<-paste('[',temp_n,'/',temp_s,']')




temp_se_intrcpt_healthcare<- paste("(",temp_se_intrcpt_healthcare,")",sep="")
temp_se_intrcpt_healthcare<-p.stars(temp_p_intrcpt_healthcare, temp_se_intrcpt_healthcare)
temp_se_age_healthcare<- paste("(",temp_se_age_healthcare,")",sep="")
temp_se_age_healthcare<-p.stars(temp_p_age_healthcare, temp_se_age_healthcare)    

temp_se_female_healthcare<- paste("(",temp_se_female_healthcare,")",sep="")
temp_se_female_healthcare<-p.stars(temp_p_female_healthcare, temp_se_female_healthcare)    


consolidated.all<- data.frame("Variable"=c("","Constant","","","Age","","","LMIC","","","female Share","","","Lumpsum","","","Value/GDPPC","","","Delay-Intervention (Years)","","","Obs/Studies","" ),
                              "All"=c("",temp_coef_intrcpt_all,temp_se_intrcpt_all,'',temp_coef_age_all,temp_se_age_all,"",temp_coef_lmic_all, temp_se_lmic_all, '', temp_coef_female_all, temp_se_female_all, '',
                                      temp_coef_lumpsum_all, temp_se_lumpsum_all, '', temp_coef_cost_all, temp_se_cost_all, '', temp_coef_delay_all, temp_se_delay_all, '',temp_ns_all,''),
                              "UCT"=c("",temp_coef_intrcpt_uct,temp_se_intrcpt_uct,'',temp_coef_age_uct,temp_se_age_uct,"",temp_coef_lmic_uct, temp_se_lmic_uct, '', temp_coef_female_uct, temp_se_female_uct, '',
                                      temp_coef_lumpsum_uct, temp_se_lumpsum_uct, '', temp_coef_cost_uct, temp_se_cost_uct, '', temp_coef_delay_uct, temp_se_delay_uct, '',temp_ns_uct,''),
                              'CCT'=c("",temp_coef_intrcpt_cct,temp_se_intrcpt_cct,'',temp_coef_age_cct,temp_se_age_cct,"",temp_coef_lmic_cct, temp_se_lmic_cct, '', temp_coef_female_cct, temp_se_female_cct, '',
                                      temp_coef_lumpsum_cct, temp_se_lumpsum_cct, '', temp_coef_cost_cct, temp_se_cost_cct, '', temp_coef_delay_cct, temp_se_delay_cct, '',temp_ns_cct,''),
                              
                              'Lottery'=c("",temp_coef_intrcpt_lottery,temp_se_intrcpt_lottery,'',temp_coef_age_lottery,temp_se_age_lottery,"",temp_coef_lmic_lottery, temp_se_lmic_lottery, '', temp_coef_female_lottery, temp_se_female_lottery, '',
                                           temp_coef_lumpsum_lottery, temp_se_lumpsum_lottery, '', temp_coef_cost_lottery, temp_se_cost_lottery, '', temp_coef_delay_lottery, temp_se_delay_lottery, '',temp_ns_lottery,''),
                              'Grad'=c("",temp_coef_intrcpt_grad,temp_se_intrcpt_grad,'',temp_coef_age_grad,temp_se_age_grad,"",temp_coef_lmic_grad, temp_se_lmic_grad, '', temp_coef_female_grad, temp_se_female_grad, '',
                                      temp_coef_lumpsum_grad, temp_se_lumpsum_grad, '', temp_coef_cost_grad, temp_se_cost_grad, '', temp_coef_delay_grad, temp_se_delay_grad, '',temp_ns_grad,''),
                              'Asset'=c("",temp_coef_intrcpt_asset,temp_se_intrcpt_asset,'',temp_coef_age_asset,temp_se_age_asset,"",temp_coef_lmic_asset, temp_se_lmic_asset, '', temp_coef_female_asset, temp_se_female_asset, '',
                                       temp_coef_lumpsum_asset, temp_se_lumpsum_asset, '', temp_coef_cost_asset, temp_se_cost_asset, '', temp_coef_delay_asset, temp_se_delay_asset, '',temp_ns_asset,''),
                              'Healthcare'=c("",temp_coef_intrcpt_healthcare,temp_se_intrcpt_healthcare,'',temp_coef_age_healthcare,temp_se_age_healthcare,"",temp_coef_lmic_healthcare, temp_se_lmic_healthcare, '', temp_coef_female_healthcare, temp_se_female_healthcare, '',
                                             temp_coef_lumpsum_healthcare, temp_se_lumpsum_healthcare, '', temp_coef_cost_healthcare, temp_se_cost_healthcare, '', temp_coef_delay_healthcare, temp_se_delay_healthcare, '',temp_ns_healthcare,''),
                              'Neighborhood'=c("",temp_coef_intrcpt_neighborhood,temp_se_intrcpt_neighborhood,'',temp_coef_age_neighborhood,temp_se_age_neighborhood,"",temp_coef_lmic_neighborhood, temp_se_lmic_neighborhood, '', temp_coef_female_neighborhood, temp_se_female_neighborhood, '',
                                                temp_coef_lumpsum_neighborhood, temp_se_lumpsum_neighborhood, '', temp_coef_cost_neighborhood, temp_se_cost_neighborhood, '', temp_coef_delay_neighborhood, temp_se_delay_neighborhood, '',temp_ns_neighborhood,''))


metareg.tex <- xtable(consolidated.all,
                     caption="Meta Regression",
                     align=c(
                       "c{0.01cm}",
                       "c{0.1cm}",
                       "c{0.5cm}",
                       "c{0.5cm}",
                       "c{0.5cm}",
                       "c{0.5cm}",
                       "c{0.5cm}",
                       "c{0.5cm}","c{0.5cm}","c{0.5cm}"))
outdir_overleaf<-"/Users/jimenaromero/Dropbox/Aplicaciones/Overleaf/wellcome_revision_21/tables"

setwd(outdir_overleaf)


print(metareg.tex, file = "metareg_stata.tex", compress = FALSE, 
      include.rownames=FALSE,
      #hline.after = c(-1,0,0,4,8,12,16,23,23),
      # sanitize.colnames.function=bold, 
      size = "small",
      caption.placement =  "top" , justify ='centre')
                              