

############## Packages
#Function to check if package exists, load if not
rm(list=ls())
ipak <- function(pkg){
  new_pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new_pkg)) 
    install.packages(new_pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

packs = c("tidyverse","pwr","metafor","devtools","xlsx", "puniform","metafore","RColorBrewer","latex2exp",
          "xtable","here","dmetar","meta","metafor","pvaluefunctions",'googlesheets4',
          "foreign", "ggplot2",'tidyverse', 'dplyr', 'plyr', 'pastecs', 'Hmisc',
          'doBy','expss', 'maps','data.table', 'tigris','readxl' ,'fuzzyjoin',
          "maps", "sjlabelled", "rgdal", "tmaptools", "tigris", "tmap", 'wesanderson',
          'plm','corrplot','ISLR', 'gridExtra','jtools',"stargazer", 'rgdal', 'weightr',
          'raster','vistime','webshot','plotly','hrbrthemes','factoextra','kableExtra','klaR','ggsignif',
          'vtable','ggrepel','ggsci','scales','ihs','xtable','sjmisc','stringr','readstata13','reshape2','ggthemr','haven','meta', 'PRISMAstatement','DiagrammeR')

ipak(packs)
select<-dplyr::select


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

dir<-"/Users/jimenaromero/Dropbox/wellcome_review_2020/Revision_2021-01/"
setwd(dir)
diroverleaf<-"/Users/jimenaromero/Dropbox/Aplicaciones/Overleaf/wellcome_revision_21/"
#dirdata<-"/Users/jimenaromero/Dropbox/wellcome_review_2020/Revision_2021-01/data"
#dircode<-"/Users/jimenaromero/Dropbox/wellcome_review_2020/Revision_2021-01/code"
#outdir_overleaf<-"/Users/jimenaromero/Dropbox/Aplicaciones/Overleaf/wellcome_revision_21/tables"
setwd(dir)

source("code/aux/PublicationbiasGMM/R/PublicationbiasPackage.R")
source("code/aux/AndrewsKasyRCode/VariationVarianceLogLikelihood.R") # Andrews & Casy (2017) selection model
source("code/aux/AndrewsKasyRCode/AuxiliaryFunctions.R") #for calculating se of beta estimate
source("code/aux/AndrewsKasyRCode/Step_function_normal_cdf.R")
source("code/aux/AndrewsKasyRCode/ReplicationAnalyticLogLikelihood.R")

#newfile
raw<-read_sheet("https://docs.google.com/spreadsheets/d/19PsUUHQ5__spNc4z9VT7b-aC6cbzqwRdqj2AvjtS-SM/edit#gid=280494917", sheet="Joel's full text extraction")
raw<-as.data.frame(raw)

source(paste(dir,"code/clean_july.R",sep='')) # clean raw data into final data

#source(paste(dir,"code/clean.R",sep='')) # clean raw data into final data
source(paste(dir,"code/wellcome_overview.R",sep='')) # clean raw data into final data

source(paste(dir,"code/list_excluded.R",sep='')) # list of excluded
source(paste(dir,"code/meta_oneoutcome.R",sep='')) # estimates one outcome per study for forest plot
source(paste(dir,"code/meta_forest_one.R",sep='')) # forestplot

#source(paste(dir,"code/lowess.R",sep='')) # list of excluded
#source(paste(dir,"code/meta_pooled.R",sep='')) # pooled estimates
source(paste(dir,"code/meta_pooled_clustered.R",sep='')) # pooled estimates clustered by study
source(paste(dir,"code/meta_methods.R",sep='')) # pooled estimates clustered by study

source(paste(dir,"code/meta_pooled_clustered_HIC.R",sep='')) # pooled estimates clustered by study for HIC
source(paste(dir,"code/meta_pooled_clustered_LMIC.R",sep='')) # pooled estimates clustered by study for LMIC
#source(paste(dir,"code/metareg.R",sep='')) # meta regression with covariates
source(paste(dir,"code/metareg_clustered.R",sep='')) # meta regression with covariates
source(paste(dir,"code/metareg_clustered_outcomes.R",sep='')) # meta regression with covariates
source(paste(dir,"code/meta_bias.R",sep='')) # biasestimations
source(paste(dir,"code/meta_bias_int.R",sep='')) # biasestimations
source(paste(dir,"code/meta_bias_out.R",sep='')) # biasestimations

#source(paste(dir,"code/meta_puniform.R",sep='')) # puniform & pstar estimates
#source(paste(dir,"code/meta_3PSM.R",sep='')) # 3psm publication bias 
#source(paste(dir,"code/meta_AK.R",sep='')) # Andrew&Kasy publication bias





#a ppendix

source(paste(dir,"code/two_cat/meta_pooled_clustered.R",sep='')) # pooled estimates clustered by study
source(paste(dir,"code/two_cat/meta_pooled_clustered_lmic.R",sep='')) # pooled estimates clustered by study
source(paste(dir,"code/two_cat/meta_pooled_clustered_hic.R",sep='')) # pooled estimates clustered by study
source(paste(dir,"code/two_cat/metareg_clustered.R",sep='')) # meta regression with covariates
source(paste(dir,"code/two_cat/metareg_clustered_LMIC.R",sep='')) # meta regression with covariates
source(paste(dir,"code/two_cat/metareg_clustered_HIC.R",sep='')) # meta regression with covariates



