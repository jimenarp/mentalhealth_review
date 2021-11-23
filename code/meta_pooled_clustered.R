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



for(i in c('int_All', 'int_UCT','int_CCT', 'int_Neighborhood', 'int_Grad', 'int_Lottery', 'int_Asset', 'int_Healthcare')) {
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



consolidated.UCT<- data.frame("group"=c("Unconditional Cash Transfer","","","" ),
                              "Reg 1"=c(coef_UCT_All,se_UCT_All,ns_UCT_All,""),
                              "Reg 2"=c(coef_UCT_Depression,se_UCT_Depression,ns_UCT_Depression,""),
                              "Reg 3"=c(coef_UCT_Stress_Anx,se_UCT_Stress_Anx,ns_UCT_Stress_Anx,""),
                              "Reg 4"=c(coef_UCT_Happy_Sat,se_UCT_Happy_Sat,ns_UCT_Happy_Sat,"")
)



consolidated.CCT<- data.frame("group"=c("Conditional Cash Transfer","","","" ),
                              "Reg 1"=c(coef_CCT_All,se_CCT_All,ns_CCT_All,""),
                              "Reg 2"=c(coef_CCT_Depression,se_CCT_Depression,ns_CCT_Depression,""),
                              "Reg 3"=c(coef_CCT_Stress_Anx,se_CCT_Stress_Anx,ns_CCT_Stress_Anx,""),
                              "Reg 4"=c(coef_CCT_Happy_Sat,se_CCT_Happy_Sat,ns_CCT_Happy_Sat,"")
)


consolidated.Neighborhood<- data.frame("group"=c("Housing Voucher","","","" ),
                                       "Reg 1"=c(coef_Neighborhood_All,se_Neighborhood_All,ns_Neighborhood_All,""),
                                       "Reg 2"=c(coef_Neighborhood_Depression,se_Neighborhood_Depression,ns_Neighborhood_Depression,""),
                                       "Reg 3"=c(coef_Neighborhood_Stress_Anx,se_Neighborhood_Stress_Anx,ns_Neighborhood_Stress_Anx,""),
                                       "Reg 4"=c(coef_Neighborhood_Happy_Sat,se_Neighborhood_Happy_Sat,ns_Neighborhood_Happy_Sat,"")
)


consolidated.Grad<- data.frame("group"=c("Poverty Graduation Program","","","" ),
                               "Reg 1"=c(coef_Grad_All,se_Grad_All,ns_Grad_All,""),
                               "Reg 2"=c(coef_Grad_Depression,se_Grad_Depression,ns_Grad_Depression,""),
                               "Reg 3"=c(coef_Grad_Stress_Anx,se_Grad_Stress_Anx,ns_Grad_Stress_Anx,""),
                               "Reg 4"=c(coef_Grad_Happy_Sat,se_Grad_Happy_Sat,ns_Grad_Happy_Sat,"")
)


consolidated.Lottery<- data.frame("group"=c("Lottery Win","","","" ),
                                  "Reg 1"=c(coef_Lottery_All,se_Lottery_All,ns_Lottery_All,""),
                                  "Reg 2"=c(coef_Lottery_Depression,se_Lottery_Depression,ns_Lottery_Depression,""),
                                  "Reg 3"=c(coef_Lottery_Stress_Anx,se_Lottery_Stress_Anx,ns_Lottery_Stress_Anx,""),
                                  "Reg 4"=c(coef_Lottery_Happy_Sat,se_Lottery_Happy_Sat,ns_Lottery_Happy_Sat,"")
)


consolidated.Asset<- data.frame("group"=c("Asset Transfer","","","" ),
                                "Reg 1"=c(coef_Asset_All,se_Asset_All,ns_Asset_All,""),
                                "Reg 2"=c(coef_Asset_Depression,se_Asset_Depression,ns_Asset_Depression,""),
                                "Reg 3"=c(coef_Asset_Stress_Anx,se_Asset_Stress_Anx,ns_Asset_Stress_Anx,""),
                                "Reg 4"=c(coef_Asset_Happy_Sat,se_Asset_Happy_Sat,ns_Asset_Happy_Sat,"")
)


consolidated.Healthcare<- data.frame("group"=c("Health Insurance Provision","","","" ),
                                     "Reg 1"=c(coef_Healthcare_All,se_Healthcare_All,ns_Healthcare_All,""),
                                     "Reg 2"=c(coef_Healthcare_Depression,se_Healthcare_Depression,ns_Healthcare_Depression,""),
                                     "Reg 3"=c(coef_Healthcare_Stress_Anx,se_Healthcare_Stress_Anx,ns_Healthcare_Stress_Anx,""),
                                     "Reg 4"=c(coef_Healthcare_Happy_Sat,se_Healthcare_Happy_Sat,ns_Healthcare_Happy_Sat,"")
)



consolidated_final<-rbind(consolidated.all, consolidated.UCT ,consolidated.CCT,consolidated.Neighborhood,  consolidated.Grad, consolidated.Lottery,consolidated.Asset,consolidated.Healthcare )

names(consolidated_final)<-c("","(1)","(2)","(3)","(4)")

pooled.tex <- xtable(consolidated_final,
                             caption="Pooled  Effect (Random Effects)",
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


print(pooled.tex, file = "tables/pooled_clustered.tex", compress = FALSE, 
      include.rownames=FALSE,
      #hline.after = c(-1,0,0,4,8,12,16,23,23),
     # sanitize.colnames.function=bold, 
      add.to.row = addtorow,
      size = "small",
      caption.placement =  "top" , justify ='centre',
     floating=FALSE) #eliminats environment of the table 

#
 
to_graph<-data.frame("outcome"=c("All Outcomes","Depression","Stress or Anxiety","Happiness" ),
                     "value"=c(coef_All_All, coef_All_Depression, coef_All_Stress_Anx, coef_All_Happy_Sat),
                     "se"=c(substring(se_All_All,2,6), substring(se_All_Depression,2,6) , substring(se_All_Stress_Anx,2,6), substring(se_All_Happy_Sat,2,6)),
                     "value_lab"=c(paste(coef_All_All,substring(se_All_All,8,15),sep=''), 
                                   paste(coef_All_Depression,substring(se_All_Depression,8,15),sep=''), 
                                   paste(coef_All_Stress_Anx,substring(se_All_Stress_Anx,8,15),sep=''), 
                                   paste(coef_All_Happy_Sat,substring(se_All_Happy_Sat,8,15),sep='')))




to_graph<-to_graph %>% mutate(value=as.numeric(as.character(value))) %>% mutate(se=as.numeric(as.character(se)))
to_graph$outcome <- factor(to_graph$outcome,levels = c("Stress or Anxiety","Depression","Happiness","All Outcomes"))

# to_graph<-to_graph %>% mutate(t=value/se) %>% mutate(pval=ifelse(abs(t)>=2.58,0.001, ifelse(abs(t)>=1.96& abs(t)<2.58,0.05, ifelse(abs(t)>=1.65&abs(t)<1.96, 0.1, 3)))) %>% 
#   mutate(pval2=pt(q=t, df=100,lower.tail=FALSE)) %>%
#   mutate(value_lab=p.stars(pval2, value))

p1<-  ggplot(to_graph, aes(x=outcome, y=value)) +
  geom_bar(stat="identity", aes(fill=factor(outcome))) +
  scale_fill_manual( values = c("gray","gray","gray",'black'))+
  geom_errorbar(aes(ymin=value-se, ymax=value+se), width=.2, color='black') +
  labs(title="", x="", y = "")+
  #theme(axis.text.x=element_text(size=10))+
  #theme(axis.text.y=element_text(size=10))+
  #theme(axis.title.x=element_text(size=10))+
    theme_minimal(base_size=24)+
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.position = "none",
          plot.title=element_text(size=20, face='bold'))+
    geom_text(aes(label=value_lab), color='black',position = position_nudge(y = to_graph$se+0.019), size=6)+
    coord_flip()+ ggtitle('Mental Health Outcomes')+scale_x_discrete(labels=c("Stress\n or Anxiety","Depression","Happiness","All Outcomes" ))+ylim(0,0.189)


to_graph_int<-data.frame("intervention"=c("All Interventions","UCT","CCT","Neighborhood","Asset", 'Grad','Lottery','Insurance' ),
                     "value"=c(coef_All_All, coef_UCT_All, coef_CCT_All, coef_Neighborhood_All, coef_Asset_All, coef_Grad_All, coef_Lottery_All, coef_Healthcare_All),
                     "se"=c(substring(se_All_All,2,6), substring(se_UCT_All,2,6) , substring(se_CCT_All,2,6), substring(se_Neighborhood_All,2,6),substring(se_Asset_All,2,6),substring(se_Grad_All,2,6),substring(se_Lottery_All,2,6),substring(se_Healthcare_All,2,6)),
                     "value_lab"=c(paste(coef_All_All,substring(se_All_All,8,15),sep=''), 
                                   paste(coef_UCT_All,substring(se_UCT_All,8,15),sep=''), 
                                   paste(coef_CCT_All,substring(se_CCT_All,8,15),sep=''), 
                                   paste(coef_Neighborhood_All,substring(se_Neighborhood_All,8,15),sep=''), 
                                   paste(coef_Asset_All,substring(se_Asset_All,8,15),sep=''), 
                                   paste(coef_Grad_All,substring(se_Grad_All,8,15),sep=''), 
                                   paste(coef_Lottery_All,substring(se_Lottery_All,8,15),sep=''), 
                                   paste(coef_Healthcare_All,substring(se_Healthcare_All,8,15),sep='')))
# 
# 
 to_graph_int<-to_graph_int %>% mutate(value=as.numeric(as.character(value))) %>% mutate(se=as.numeric(as.character(se)))
# #to_graph_int$intervention <- factor(to_graph_int$intervention,levels = c('Insurance','Lottery','Grad','Asset','Neighborhood','CCT','UCT','All Interventions'))
to_graph_int$intervention <- factor(to_graph_int$intervention,levels = c('CCT','Neighborhood','Lottery','Grad','Insurance','UCT','Asset','All Interventions'))
# 
# to_graph_int<-to_graph_int %>% mutate(t=value/se) %>% mutate(pval=ifelse(abs(t)>=2.58,0.001, ifelse(abs(t)>=1.96& abs(t)<2.58,0.05, ifelse(abs(t)>=1.65&abs(t)<1.96, 0.1, 3)))) %>% 
#   mutate(pval2=pt(q=t, df=100,lower.tail=FALSE)) %>%
#   mutate(value_lab=p.stars(pval2, value))


p2<-    ggplot(to_graph_int, aes(x=intervention, y=value)) +
  geom_bar(stat="identity", aes(fill=factor(intervention))) +
  scale_fill_manual( values = c("gray","gray","gray","gray","gray","gray","gray",'black'))+
  geom_errorbar(aes(ymin=value-se, ymax=value+se), width=.2, color='black') +
  labs(title="", x="", y = "")+
  theme_minimal(base_size=24)+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none",
        plot.title=element_text(size=18, face='bold'))+
  geom_text(aes(label=value_lab), color='black',position = position_nudge(y = to_graph_int$se+0.02), size=6)+
  coord_flip()+ 
  ggtitle('Economic Interventions')+scale_x_discrete(labels=c('Conditional\n Cash Transfers','Housing\n Voucher', "Lottery\n Win",'Poverty Graduation\n Program', "Health Insurance \n Provision",'Unconditional\n Cash Transfers','Asset\n Transfer','All Interventions' ))+ylim(0,0.23)

p<-grid.arrange(p1, p2, nrow = 1)


ggsave(p, file='figures/pooled_effect_clustered.png', width = 19, height=10)      




