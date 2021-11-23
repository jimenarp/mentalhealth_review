setwd(dir)
load(file='data/final/final.Rdata')

final<- final %>% mutate(int_All=1)
final<- final %>% mutate(out_All=1) %>%
  mutate(out_Depression=ifelse(outcome_category=='Depression',1,0)) %>%
  mutate(out_Stress_Anx=ifelse(outcome_category=='Stress'|outcome_category=='Anxiety',1,0)) %>%
  mutate(out_Happy_Sat=ifelse(outcome_category=='Happiness or Satisfaction',1,0)) 

# Random Effects Using Sidik-Jonkman estimator ("SJ")

all_all<-rma(yi=z_treatment_effect, sei=z_se, slab=archive_name, weighted=TRUE, data=final, method='SJ')
all_UCT<-rma(yi=z_treatment_effect, sei=z_se, slab=archive_name, weighted=TRUE, data=final %>% filter(int_UCT==1), method='SJ')

final<-final %>% mutate(int_in_kind=ifelse(treatment=='CCT'|treatment=='UCT',0,1)) %>% mutate(int_in_cash=ifelse(int_in_kind==0,1,0))

final<-final %>% filter(LMIC==1)

for(i in c('int_All','int_in_kind', 'int_in_cash')) {
  for( j in c('out_All','out_Depression', 'out_Stress_Anx','out_Happy_Sat')){
    tryCatch({
    temp_data<-final[final[[i]]==1,]
    temp_data<-temp_data[temp_data[[j]]==1,]
    temp_name_coef<-paste('coef',substring(i, 5 ),substring(j, 5 ),sep='_')
    temp_name_se<-paste('se',substring(i, 5 ),substring(j, 5 ),sep='_')
    temp_name_I2<-paste('I2',substring(i, 5 ),substring(j, 5 ),sep='_')
    temp_name_ns<-paste('ns',substring(i, 5 ),substring(j, 5 ),sep='_')
    if (nrow(temp_data)>0){
      temp_reg<-rma(yi=z_treatment_effect, sei=z_se, slab=archive_name, weighted=TRUE, data=temp_data, method='SJ')
      temp_coef<-temp_reg$beta[1,1]
      temp_coef<- toformat(temp_coef, decimals)
      if (temp_reg$p<length(unique(temp_data$intervention_code))){
        temp_reg<-robust(temp_reg, cluster=temp_data$intervention_code)
      }
      temp_se<-temp_reg$se
      temp_pval<-temp_reg$pval
      temp_se<- toformat(temp_se, decimals)
      temp_se<- paste("(",temp_se,")",sep="")
      temp_se<-p.stars(temp_pval, temp_se)
      temp_I2<-temp_reg$I2
      temp_I2<- toformat(temp_I2, decimals)
      temp_n<-temp_reg$k
      temp_s<-length(unique(temp_data$id))
      temp_ns<-paste('[',temp_n,'/',temp_s,']')
      } else {
        
      temp_coef<-"---"
      temp_se<-""
      temp_ns<-"[ 0 / 0 ]"
    }
    assign(temp_name_coef, temp_coef)
    assign(temp_name_se, temp_se)
    assign(temp_name_I2, temp_I2)
    assign(temp_name_ns, temp_ns)
    }, error=function(e){})
  }
}



consolidated.all<- data.frame("group"=c("","All Interventions","","","" ),
                              "Reg 1"=c("",coef_All_All,se_All_All,ns_All_All,""),
                              "Reg 2"=c("",coef_All_Depression,se_All_Depression,ns_All_Depression,""),
                              "Reg 3"=c("",coef_All_Stress_Anx,se_All_Stress_Anx,ns_All_Stress_Anx,""),
                              "Reg 4"=c("",coef_All_Happy_Sat,se_All_Happy_Sat,ns_All_Happy_Sat,"")
)


consolidated.cash<- data.frame("group"=c("In Cash","","","" ),
                              "Reg 1"=c(coef_in_cash_All,se_in_cash_All,ns_in_cash_All,""),
                              "Reg 2"=c(coef_in_cash_Depression,se_in_cash_Depression,ns_in_cash_Depression,""),
                              "Reg 3"=c(coef_in_cash_Stress_Anx,se_in_cash_Stress_Anx,ns_in_cash_Stress_Anx,""),
                              "Reg 4"=c(coef_in_cash_Happy_Sat,se_in_cash_Happy_Sat,ns_in_cash_Happy_Sat,"")
)



consolidated.kind<- data.frame("group"=c("In Kind","","","" ),
                               "Reg 1"=c(coef_in_kind_All,se_in_kind_All,ns_in_kind_All,""),
                               "Reg 2"=c(coef_in_kind_Depression,se_in_kind_Depression,ns_in_kind_Depression,""),
                               "Reg 3"=c(coef_in_kind_Stress_Anx,se_in_kind_Stress_Anx,ns_in_kind_Stress_Anx,""),
                               "Reg 4"=c(coef_in_kind_Happy_Sat,se_in_kind_Happy_Sat,ns_in_kind_Happy_Sat,"")
)


consolidated_final<-rbind(consolidated.all, consolidated.cash ,consolidated.kind )

names(consolidated_final)<-c("","(1)","(2)","(3)","(4)")

pooled.tex <- xtable(consolidated_final,
                             caption="Pooled  Effect (Random Effects) Clustered SEs",
                     align=c(
                       "l{0.01cm}",
                       "l{0.3cm}",
                       "c{0.3cm}",
                       "c{0.3cm}",
                       "c{0.3cm}",
                       "c{0.3cm}"),format.args = list(digits = 3))


addtorow <- list()
addtorow$pos <- list()
addtorow$pos[[1]] <- 0



addtorow$command <- c(" &  \\shortstack{All Mental Health \\\\ Outcomes} & Depression & Stress or Anxiety & Happiness    \\\\  ")

setwd(diroverleaf)


print(pooled.tex, file = "tables/pooled_clustered_twocat_lmic.tex", compress = FALSE, 
      include.rownames=FALSE,
      #hline.after = c(-1,0,0,4,8,12,16,23,23),
     # sanitize.colnames.function=bold, 
      add.to.row = addtorow,
      size = "small",
      caption.placement =  "top" , justify ='centre',
     floating=FALSE) #eliminats environment of the table 
# 
# to_graph<-data.frame("outcome"=c("All Outcomes","Depression","Stress or Anxiety","Happiness" ),
#                      "value"=c(coef_All_All, coef_All_Depression, coef_All_Stress_Anx, coef_All_Happy_Sat),
#                      "se"=c(substring(se_All_All,2,6), substring(se_All_Depression,2,6) , substring(se_All_Stress_Anx,2,6), substring(se_All_Happy_Sat,2,6)))
# 
# 
# to_graph<-to_graph %>% mutate(value=as.numeric(as.character(value))) %>% mutate(se=as.numeric(as.character(se)))
# to_graph$outcome <- factor(to_graph$outcome,levels = c("Happiness","Stress or Anxiety","Depression","All Outcomes"))
# 
# to_graph<-to_graph %>% mutate(t=value/se) %>% mutate(pval=ifelse(abs(t)>=2.58,0.001, ifelse(abs(t)>=1.96& abs(t)<2.58,0.05, ifelse(abs(t)>=1.65&abs(t)<1.96, 0.1, 3)))) %>% 
#   mutate(pval2=pt(q=t, df=100,lower.tail=FALSE)) %>%
#   mutate(value_lab=p.stars(pval2, value))
# 
# p1<-  ggplot(to_graph, aes(x=outcome, y=value)) +
#   geom_bar(stat="identity", color="gray", fill='gray') +
#   geom_errorbar(aes(ymin=value-se, ymax=value+se), width=.2, color='black') +
#   labs(title="", x="", y = "")+
#   #theme(axis.text.x=element_text(size=10))+
#   #theme(axis.text.y=element_text(size=10))+
#   #theme(axis.title.x=element_text(size=10))+
#     theme_minimal(base_size=20)+
#     theme(panel.grid.major = element_blank(),
#           panel.grid.minor = element_blank(),
#           plot.title=element_text(size=16, face='bold'))+
#     geom_text(aes(label=value_lab), color='black',position = position_nudge(y = to_graph$se+0.017), size=6)+
#     coord_flip()+ ggtitle('Mental Health Outcomes')
# setwd(diroverleaf)
# 
# 
# to_graph_int<-data.frame("intervention"=c("All Interventions","UCT","CCT","Neighborhood","Asset", 'Grad','Lottery','Insurance' ),
#                      "value"=c(coef_All_All, coef_UCT_All, coef_CCT_All, coef_Neighborhood_All, coef_Asset_All, coef_Grad_All, coef_Lottery_All, coef_Healthcare_All),
#                      "se"=c(substring(se_All_All,2,6), substring(se_UCT_All,2,6) , substring(se_CCT_All,2,6), substring(se_Neighborhood_All,2,6),substring(se_Asset_All,2,6),substring(se_Grad_All,2,6),substring(se_Lottery_All,2,6),substring(se_Healthcare_All,2,6)))
# 
# 
# to_graph_int<-to_graph_int %>% mutate(value=as.numeric(as.character(value))) %>% mutate(se=as.numeric(as.character(se)))
# to_graph_int$intervention <- factor(to_graph_int$intervention,levels = c('Insurance','Lottery','Grad','Asset','Neighborhood','CCT','UCT','All Interventions'))
# 
# to_graph_int<-to_graph_int %>% mutate(t=value/se) %>% mutate(pval=ifelse(abs(t)>=2.58,0.001, ifelse(abs(t)>=1.96& abs(t)<2.58,0.05, ifelse(abs(t)>=1.65&abs(t)<1.96, 0.1, 3)))) %>% 
#   mutate(pval2=pt(q=t, df=100,lower.tail=FALSE)) %>%
#   mutate(value_lab=p.stars(pval2, value))
# 
# 
# p2<-  ggplot(to_graph_int, aes(x=intervention, y=value)) +
#   geom_bar(stat="identity", color="gray", fill='gray') +
#   geom_errorbar(aes(ymin=value-se, ymax=value+se), width=.2, color='black') +
#   labs(title="", x="", y = "")+
#   #theme(axis.text.x=element_text(size=10))+
#   #theme(axis.text.y=element_text(size=10))+
#   #theme(axis.title.x=element_text(size=10))+
#   theme_minimal(base_size=20)+
#   theme(panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         plot.title=element_text(size=16, face='bold'))+
#   geom_text(aes(label=value_lab), color='black',position = position_nudge(y = to_graph_int$se+0.02), size=6)+
#   coord_flip()+ ggtitle('Economic Interventions')
# 
# p<-grid.arrange(p1, p2, nrow = 1)
# 
# 
# ggsave(p, file='figures/pooled_effect_clustered.png', width = 16, height=10)      
# 
# 
# 
# 
