

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
load(file='final.Rdata')

outdir_overleaf<-"/Users/jimenaromero/Dropbox/Aplicaciones/Overleaf/wellcome_revision_21/figures"



forest_CCT<-metagen(z_treatment_effect,
                 se_treatment_effect,
                 data = final %>% filter(int_CCT==1),
                 studlab = archive_name,
                 comb.fixed = FALSE,
                 comb.random = TRUE,
                 method.tau = "SJ",
                 hakn = TRUE,
                 prediction = TRUE,
                 sm = "SMD")

setwd(outdir_overleaf)

pdf(file = 'forest_CCT.pdf') 
forest.jama <- forest(forest_CCT,
                      layout = "JAMA")
dev.off() 



pdf(file = 'forest_CCT_2.pdf') 
forest.jama <- forest(forest_CCT,
                      layout = "RevMan5")
dev.off() 
