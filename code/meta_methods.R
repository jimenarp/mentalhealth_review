
setwd(dir)

load(file='data/final/final.Rdata')

fixed<-
  metagen(z_treatment_effect,
          z_se,
          data=final,
          studlab=archive_name,
          comb.fixed = TRUE,
          comb.random = FALSE,
          prediction=TRUE,
          sm="SMD")

coef_fixed<-fixed$TE.fixed
se_fixed<-fixed$seTE.fixed



sj<-rma(yi=z_treatment_effect, sei=z_se, slab=archive_name, weighted=TRUE, data=final, method='SJ')
if (sj$p<length(unique(sj$intervention_code))){
  sj<-robust(sj, cluster=sj$intervention_code)
}
coef_sj<-sj$beta[1,1]
se_sj<-sj$se



dl<-rma(yi=z_treatment_effect, sei=z_se, slab=archive_name, weighted=TRUE, data=final, method='DL')
if (dl$p<length(unique(dl$intervention_code))){
  dl<-robust(dl, cluster=dl$intervention_code)
}
coef_dl<-dl$beta[1,1]
se_dl<-dl$se

he<-rma(yi=z_treatment_effect, sei=z_se, slab=archive_name, weighted=TRUE, data=final, method='HE')
if (he$p<length(unique(he$intervention_code))){
  he<-robust(he, cluster=he$intervention_code)
}
coef_he<-he$beta[1,1]
se_he<-he$se


hs<-rma(yi=z_treatment_effect, sei=z_se, slab=archive_name, weighted=TRUE, data=final, method='HS')
if (hs$p<length(unique(hs$intervention_code))){
  hs<-robust(hs, cluster=hs$intervention_code)
}
coef_hs<-hs$beta[1,1]
se_hs<-hs$se


ml<-rma(yi=z_treatment_effect, sei=z_se, slab=archive_name, weighted=TRUE, data=final, method='ML')
if (ml$p<length(unique(ml$intervention_code))){
  ml<-robust(ml, cluster=ml$intervention_code)
}
coef_ml<-ml$beta[1,1]
se_ml<-ml$se


reml<-rma(yi=z_treatment_effect, sei=z_se, slab=archive_name, weighted=TRUE, data=final, method='REML')
if (reml$p<length(unique(reml$intervention_code))){
  reml<-robust(reml, cluster=reml$intervention_code)
}
coef_reml<-reml$beta[1,1]
se_reml<-reml$se


eb<-rma(yi=z_treatment_effect, sei=z_se, slab=archive_name, weighted=TRUE, data=final, method='EB')
if (eb$p<length(unique(eb$intervention_code))){
  eb<-robust(eb, cluster=eb$intervention_code)
}
coef_eb<-eb$beta[1,1]
se_eb<-eb$se


pm<-rma(yi=z_treatment_effect, sei=z_se, slab=archive_name, weighted=TRUE, data=final, method='PM')
if (pm$p<length(unique(pm$intervention_code))){
  pm<-robust(pm, cluster=pm$intervention_code)
}
coef_pm<-pm$beta[1,1]
se_pm<-pm$se


to_graph<-data.frame("method"=c("Fixed","Sidik-Jonkman","DerSimonian-Laird","Hedges","Hunter-Schmidt" ,"Maximum-Likelihood", "REML", "Bayes", "Paule-Mandel"),
                     "value"=c(coef_fixed, coef_sj, coef_dl,coef_he, coef_hs, coef_ml,coef_reml,coef_eb,coef_pm ),
                     "se"=c(se_fixed, se_sj, se_dl,se_he, se_hs, se_ml,se_reml,se_eb, se_pm))


to_graph<-to_graph %>% mutate(value=round(value,3)) %>% mutate(t=value/se) %>% mutate(pval=ifelse(abs(t)>=2.58,0.001, ifelse(abs(t)>=1.96& abs(t)<2.58,0.05, ifelse(abs(t)>=1.65&abs(t)<1.96, 0.1, 3)))) %>% 
  mutate(pval2=pt(q=t, df=100,lower.tail=FALSE)) %>%
  mutate(value_lab=p.stars(pval2, value))


p1<-  ggplot(to_graph, aes(x=method, y=value)) +
  geom_bar(stat="identity", color="gray", fill='gray') +
  geom_errorbar(aes(ymin=value-se, ymax=value+se), width=.2, color='black') +
  labs(title="", x="", y = "")+
  #theme(axis.text.x=element_text(size=10))+
  #theme(axis.text.y=element_text(size=10))+
  #theme(axis.title.x=element_text(size=10))+
  theme_minimal(base_size=20)+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title=element_text(size=16, face='bold'))+
  geom_text(aes(label=value_lab), color='black',position = position_nudge(y = to_graph$se+0.017), size=6)+
  coord_flip()+ ggtitle('')


setwd(diroverleaf)

ggsave(p1, file='figures/pooled_methods.png', width=12,height=8)                 




