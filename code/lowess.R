
setwd(dir)
load(file='data/final/final.Rdata')

widthsize=10
heightsize=6

ggthemr('fresh')



### AGE
agegraph<-qplot(age, z_treatment_effect, size=invSE, data=final)+
  stat_smooth(method="loess", aes(weight=invSE))+ylab('Treatment Effect (Z-score)')+xlab('Age')+
  theme(
    legend.position="none",
    legend.title = element_blank())



### INTERVENTION VALUE
valuegraph<-qplot(intervention_value, z_treatment_effect, size=invSE, data=final)+
  stat_smooth(method="loess", aes(weight=invSE))+ylab('Treatment Effect (Z-score)')+xlab('Intervention value (USD)')+
  theme(
    legend.position="none",
    legend.title = element_blank())



### DURATION
durationgraph<-qplot(duration_month, z_treatment_effect, size=invSE, data=final)+
  stat_smooth(method="loess", aes(weight=invSE))+ylab('Treatment Effect (Z-score)')+xlab('Duration (months)')+
  theme(
    legend.position="none",
    legend.title = element_blank())



### FEMALE
femalegraph<-qplot(female_share, z_treatment_effect, size=invSE, data=final)+
  stat_smooth(method="loess", aes(weight=invSE))+ylab('Treatment Effect (Z-score)')+xlab('Female Share (%)')+
  theme(
    legend.position="none",
    legend.title = element_blank())


lowessplot<-grid.arrange(valuegraph,durationgraph,agegraph,femalegraph, nrow=2, ncol=2)

ggsave(lowessplot, filename = "figures/lowessplot.jpg",  bg = "transparent",width = 8, height = 6)

setwd(dioverleaf)

ggsave(lowessplot, filename = "figures/lowessplot.jpg",  bg = "transparent",width = 8, height = 6)

setwd(dir)
load(file='data/final/final.Rdata')

widthsize=10
heightsize=6

ggthemr('fresh')


final<-final %>% mutate(int_All=1) %>% 
  mutate(out_All=1) %>%
  mutate(out_Depression=ifelse(outcome_category=='Depression',1,0)) %>%
  mutate(out_Stress_Anx=ifelse(outcome_category=='Stress'|outcome_category=='Anxiety',1,0)) %>%
  mutate(out_Happy_Sat=ifelse(outcome_category=='Happiness or Satisfaction',1,0)) %>%
  mutate(out_Other=ifelse(outcome_category=='Other'|outcome_category=='Self-Esteem'|outcome_category=='Optimism',1,0))


final<-final %>% mutate(intervention_gdppercapita = intervention_value/gdppercapita*1000)%>% 
  mutate(age_missing=ifelse(is.na(age),1,0)) %>% 
  mutate(age=ifelse(is.na(age),0,age)) %>%
  mutate(female_missing=ifelse(is.na(female_share),1,0)) %>% 
  mutate(female_share=ifelse(is.na(female_share),0,female_share)) %>%
  mutate(intervention_gdppercapita_missing=ifelse(is.na(intervention_gdppercapita),1,0)) %>% 
  mutate(intervention_gdppercapita=ifelse(is.na(intervention_gdppercapita),0,intervention_gdppercapita)) %>%
  mutate(duration_years_missing=ifelse(is.na(duration_years),1,0)) %>% 
  mutate(duration_years=ifelse(is.na(duration_years),0,duration_years)) %>%
  mutate(intervention_peryear_gddpercapita=intervention_gdppercapita/duration_years) %>% 
  mutate(intervention_peryear_gddpercapita=ifelse(intervention_peryear_gddpercapita=='Inf',NA,intervention_peryear_gddpercapita)) %>% 
  mutate(intervention_peryear_gddpercapita_missing=ifelse(is.na(intervention_peryear_gddpercapita),1,0)) %>%
  mutate(intervention_peryear_gddpercapita=ifelse(is.na(intervention_peryear_gddpercapita),0,intervention_peryear_gddpercapita)) 


### AGE
durationgraph<-
  qplot(duration_years, z_treatment_effect, size=invSE, data=final,  colour=treatment_type)+
  stat_smooth(method="loess", aes(weight=invSE))+ylab('Treatment Effect (Z-score)')

setwd(diroverleaf)

ggsave(durationgraph, filename = "figures/duration_scatter.jpg",  bg = "transparent",width = 8, height = 6)


costgraph<-
  qplot(intervention_gdppercapita, z_treatment_effect, size=invSE, data=final,  colour=treatment_type)+
  stat_smooth(method="loess", aes(weight=invSE))+ylab('Treatment Effect (Z-score)')

setwd(diroverleaf)

ggsave(costgraph, filename = "figures/cost_scatter.jpg",  bg = "transparent",width = 8, height = 6)

