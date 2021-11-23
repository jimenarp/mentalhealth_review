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

metagen(z_treatment_effect,
        se_treatment_effect,
        data=final,
        studlab=archive_name,
        comb.fixed = TRUE,
        comb.random = FALSE,
        prediction=TRUE,
        sm="SMD")


 metagen(z_treatment_effect,
         se_treatment_effect,
                  data = final,
                  studlab = archive_name,
                  comb.fixed = FALSE,
                  comb.random = TRUE,
                  method.tau = "SJ",
                  hakn = TRUE,
                  prediction = TRUE,
                  sm = "SMD")
 
 
 #  Sidik-Jonkman estimator ("SJ")
 test<-metagen(z_treatment_effect,
         se_treatment_effect,
         data = final,
         studlab = archive_name,
         comb.fixed = FALSE,
         comb.random = TRUE,
         method.tau = "SJ",
         hakn = TRUE,
         prediction = TRUE,
         sm = "SMD")
 
 
 #  DerSimonian-Laird 
 metagen(z_treatment_effect,
         se_treatment_effect,
         data=final,
         studlab=archive_name,
         comb.fixed = FALSE,
         comb.random = TRUE,
         hakn = FALSE,
         prediction=TRUE,
         sm="SMD")
 
forest(test)
library(dmetar)

find.outliers(test)
InfluenceAnalysis(x = test,
                  random = TRUE)
test<-rma(yi=z_treatment_effect, sei=se_treatment_effect, slab=archive_name, weighted=TRUE, data=final, weights=se_treatment_effect)
summary(test)

test<-rma(yi=z_treatment_effect, sei=se_treatment_effect, slab=archive_name, weighted=TRUE, data=final)
summary(test)

test<-rma(yi=z_treatment_effect, sei=se_treatment_effect, slab=archive_name, weighted=FALSE, data=final %>% filter(outcome_category=='Depression'))
summary(test)

test<-rma(yi=z_treatment_effect, sei=se_treatment_effect, slab=archive_name, weighted=TRUE, data=final %>% filter(outcome_category=='Self-Esteem'))
summary(test)

test<-rma(yi=z_treatment_effect, sei=se_treatment_effect, slab=archive_name, weighted=TRUE, data=final %>% filter(outcome_category=='Stress'|outcome_category=='Anxiety'))
summary(test)

subgroup.analysis.mixed.effects(x = test,
                               subgroups = final$outcome_category)
update.meta(test, 
            byvar=LMIC, 
            comb.random = TRUE, 
            comb.fixed = FALSE)

metareg(test,LMIC+ female_share+age+duration_years)

eggers.test(x = test)

trimfill(test)

funnel(trimfill(test))

pcurve(test)

pcurve(test, effect.estimation = TRUE, N = final$all_n, dmin = 0, dmax = 1)
install.packages('robvis')
library(robvis)



test<-
  metagen(z_treatment_effect,
          se_treatment_effect,
          data=final,
          studlab=archive_name,
          comb.fixed = TRUE,
          comb.random = FALSE,
          prediction=TRUE,
          sm="SMD")

metareg(test,  age + LMIC + lumpsum + female_share   + intervention_gdppercapita + duration_years)


test<-metagen(z_treatment_effect,
              se_treatment_effect,
              data = final,
              studlab = archive_name,
              comb.fixed = FALSE,
              comb.random = TRUE,
              method.tau = "SJ",
              hakn = TRUE,
              prediction = TRUE,
              sm = "SMD") #0.0824
a<-metareg(test,  age + LMIC + lumpsum + female_share   + intervention_gdppercapita + duration_years)
metareg(test,  age)
bubble(a,
       
       col.line = "blue",
       studlab = FALSE)

library(metafor)
select<-dplyr:: select
reg<-final %>% filter(!is.na(lumpsum)&!is.na(LMIC) & !is.na( intervention_gdppercapita)& !is.na(age)&!is.na(female_share))
cor(reg %>% select(z_treatment_effect, LMIC, lumpsum, intervention_gdppercapita, duration_years, age, female_share))

library(PerformanceAnalytics)
chart.Correlation(reg %>% select(z_treatment_effect, LMIC, lumpsum, intervention_gdppercapita, duration_years, age, female_share))

model1 <- rma(yi = z_treatment_effect, 
              sei = se_treatment_effect, 
              data = reg, 
              method = "ML", 
              mods = ~ age, 
              test = "knha")


model1 <- rma(yi = z_treatment_effect, 
              sei = se_treatment_effect, 
              data = reg, 
              method = "ML", 
              mods = ~ age+ LMIC + lumpsum + female_share   + intervention_gdppercapita + duration_years, 
              test = "knha")
permutest(model1)
