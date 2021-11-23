############## Packages
#Function to check if package exists, load if not
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



dir<-"/Users/jimenaromero/Dropbox/wellcome_review_2020/Revision_2021-01/data/final"
setwd(dir)

load(file='final.Rdata')

############## Metanalysis: Overall effects


metagen(z_treatment_effect,
        se_treatment_effect,
        data=final,
        studlab=archive_name,
        comb.fixed = TRUE,
        comb.random = FALSE,
        prediction=TRUE,
        sm="SMD") #0.0346


#  DerSimonian-Laird 

metagen(z_treatment_effect,
        se_treatment_effect,
        data=final,
        studlab=archive_name,
        comb.fixed = FALSE,
        comb.random = TRUE,
        prediction=TRUE,
        sm="SMD") #0.0826



#  Sidik-Jonkman estimator ("SJ")
metagen(z_treatment_effect,
              se_treatment_effect,
              data = final,
              studlab = archive_name,
              comb.fixed = FALSE,
              comb.random = TRUE,
              method.tau = "SJ",
              hakn = TRUE,
              prediction = TRUE,
              sm = "SMD") #0.0824

###


test<-metagen(z_treatment_effect,
        se_treatment_effect,
        data=final,
        studlab=archive_name,
        comb.fixed = FALSE,
        comb.random = TRUE,
        prediction=TRUE,
        sm="SMD") #0.0826

test<-rma(yi=z_treatment_effect, sei=se_treatment_effect, slab=archive_name, weighted=TRUE, data=final, weights=se_treatment_effect)
summary(test) #0.136 with weights

test<-rma(yi=z_treatment_effect, sei=se_treatment_effect, slab=archive_name, weighted=TRUE, data=final)
summary(test) #0.0831


test<-rma(yi=z_treatment_effect, sei=se_treatment_effect, slab=archive_name, weighted=FALSE, data=final)
summary(test) #0.0943 without weights


test<-rma(yi=z_treatment_effect, sei=se_treatment_effect, slab=archive_name, weighted=TRUE, data=final, method='SJ')
summary(test) #0.0824 Sidik-Jonkman estimator



test<-rma(yi=z_treatment_effect, sei=se_treatment_effect, slab=archive_name, weighted=TRUE, data=final, method='DL')
summary(test) #0.0826 DerSimonian-Laird estimator


test<-rma(yi=z_treatment_effect, sei=se_treatment_effect, slab=archive_name, weighted=TRUE, data=final, method='HE')
summary(test) #0.0346 Hedges estimator


test<-rma(yi=z_treatment_effect, sei=se_treatment_effect, slab=archive_name, weighted=TRUE, data=final, method='HS')
summary(test) #0.0823  Hunter-Schmidt estimator


test<-rma(yi=z_treatment_effect, sei=se_treatment_effect, slab=archive_name, weighted=TRUE, data=final, method='ML')
summary(test) #0.0831  maximum-likelihood estimator


test<-rma(yi=z_treatment_effect, sei=se_treatment_effect, slab=archive_name, weighted=TRUE, data=final, method='REML')
summary(test) #0.0831  eestricted maximum-likelihood estimator


test<-rma(yi=z_treatment_effect, sei=se_treatment_effect, slab=archive_name, weighted=TRUE, data=final, method='EB')
summary(test) #0.0823 empirical Bayes estimator

test<-rma(yi=z_treatment_effect, sei=se_treatment_effect, slab=archive_name, weighted=TRUE, data=final, method='PM')
summary(test) #0.0823 Paule-Mandel estimator


test<-rma(yi=z_treatment_effect, sei=se_treatment_effect, slab=archive_name, weighted=TRUE, data=final, method='GENQ')
summary(test) #0.0823 generalized Q-statistic estimator
