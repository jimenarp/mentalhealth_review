
setwd(dir)
load(file='data/final/final_one.Rdata')
load(file='data/final/overview.Rdata')
load(file='./data/final/final.Rdata')

final_one<-final_one[order(final_one$one_outcome),]

final_one<-merge(final_one, overview, by='authors', all.x=TRUE)

final_one <-final_one %>% mutate(country=ifelse(archive_name=='Banerjee_15','Multiple', country))%>% 
  mutate(treatment=ifelse(treatment =='Neighborhood', 'Housing Voucher', 
                          ifelse(treatment =='Healthcare', 'Insurance Provision', 
                                 ifelse(treatment=='Asset','Asset Transfer',
                                        ifelse(treatment=='Grad','Poverty Graduation Program', treatment)))))

forest_all<-metagen(one_outcome ,
                    one_outcome_sd,
                    data = final_one,
                    studlab = authors,
                    comb.fixed = FALSE,
                    comb.random = TRUE,
                    method.tau = "SJ",
                    hakn = TRUE,
                    prediction = TRUE,
                    sm = "SMD")

setwd(diroverleaf)
pdf(file = 'figures/forest_all.pdf', height=13, width=11) 
forest<-meta::forest

forest_all<-forest_all

forest(forest_all, studlab=final_one$authors, prediction=FALSE, 
       rightcols = c( "treatment", "country"),
       rightlabs=c('Intervention Type','Country'),
      col.square= ifelse(final_one$treatment=='UCT','#cf0029',
                         ifelse(final_one$treatment=='CCT','#fa9d5f',
                                ifelse(final_one$treatment=='Housing Voucher','#d1dc5e',
                                       ifelse(final_one$treatment=='Poverty Graduation Program','#00a18f',
                                              ifelse(final_one$treatment=='Asset Transfer','#91bfdb',
                                                     ifelse(final_one$treatment=='Lottery','#374c80',
                                                            ifelse(final_one$treatment=='Insurance Provision','black', 'gray'))))))),
      sortvar=one_outcome,
      subgroup=treatment,
      leftlabs=c('Study','Treatment Effect',"Standard Error"),
      
      col.inside = "black",
      just='left',
      just.addcols = "left",
      digits.se =2)

forest.jama <- forest(forest_all, studlab=final_one$authors, prediction=FALSE, 
                      rightcols = c( "treatment", "country"),
                      rightlabs=c('Intervention Type','Country'),
                      col.square= ifelse(final_one$treatment=='UCT','#cf0029',
                                         ifelse(final_one$treatment=='CCT','#fa9d5f',
                                                ifelse(final_one$treatment=='Housing Voucher','#d1dc5e',
                                                       ifelse(final_one$treatment=='Poverty Graduation Program','#00a18f',
                                                              ifelse(final_one$treatment=='Asset Transfer','#91bfdb',
                                                                     ifelse(final_one$treatment=='Lottery','#374c80',
                                                                            ifelse(final_one$treatment=='Insurance Provision','black', 'gray'))))))),
                                           sortvar=one_outcome,
                      leftlabs=c('Study','Treatment Effect',"Standard Error"),
                      
                      col.inside = "black",
                      just='left',
                      just.addcols = "left",
                      digits.se =2)
dev.off() 

# 
# #cf0029
# dark red
# #fa9d5f
# light orange
# #d1dc5e
# yellow
# #00a18f
# teal
# #91bfdb
# light blue
# #374c80
# blue
# 
# for (i in c('UCT','CCT')){
#   temp_data<-final_one %>% filter(treatment == i )
#   temp_name<-paste(i, 'for', sep='')
#   metaest<-metagen(one_outcome ,
#                    one_outcome_sd,
#                    data = temp_data,
#                    studlab = authors,
#                    comb.fixed = FALSE,
#                    comb.random = TRUE,
#                    method.tau = "SJ",
#                    hakn = TRUE,
#                    prediction = TRUE,
#                    sm = "SMD")
#   graph<-forest(metaest, studlab=temp_data$authors, prediction=FALSE, 
#                  rightcols = c( "country","treatment"),
#                  rightlabs=c('Country','Intervention Type'),
#                  col.square= ifelse(temp_data$treatment=='UCT','#cf0029',
#                                     ifelse(temp_data$treatment=='CCT','#fa9d5f',
#                                            ifelse(temp_data$treatment=='Housing Voucher','#d1dc5e',
#                                                   ifelse(temp_data$treatment=='Poverty Graduation Program','#00a18f',
#                                                          ifelse(temp_data$treatment=='Asset','#91bfdb',
#                                                                 ifelse(temp_data$treatment=='Lottery','#374c80',
#                                                                        ifelse(temp_data$treatment=='Insurance Provision','black', 'gray'))))))),
#                  sortvar=one_outcome,
#                  subgroup=treatment,
#                  leftlabs=c('Study','Treatment Effect',"Standard Error"))
#   
#   assign(temp_name, graph  )
#   
# }
# 
# p<-grid.arrange(UCTfor, CCTfor,ncol=1, nrow = 2)
# 
# Uforest(metagen(one_outcome ,
#                        one_outcome_sd,
#                        data = final_one ,
#                        studlab = authors,
#                        comb.fixed = FALSE,
#                        comb.random = TRUE,
#                        method.tau = "SJ",
#                        hakn = TRUE,
#                        prediction = TRUE,
#                        sm = "SMD"), studlab=final_one$authors, prediction=FALSE, 
#        rightcols = c( "country","treatment"),
#        rightlabs=c('Country','Intervention Type'),
#        col.square= ifelse(final_one$treatment=='UCT','#cf0029',
#                           ifelse(final_one$treatment=='CCT','#fa9d5f',
#                                  ifelse(final_one$treatment=='Neighborhood','#d1dc5e',
#                                         ifelse(final_one$treatment=='Poverty Graduation Program','#00a18f',
#                                                ifelse(final_one$treatment=='Asset','#91bfdb',
#                                                       ifelse(final_one$treatment=='Lottery','#374c80',
#                                                              ifelse(final_one$treatment=='Healthcare','black', 'gray'))))))),
#        sortvar=one_outcome,
#        subgroup=treatment,
#        leftlabs=c('Study','Treatment Effect',"Standard Error"))
# 
# 
# 
# all<-drapery(metagen(z_treatment_effect ,
#                      z_se,
#                      data = final,
#                      studlab = authors,
#                      comb.fixed = FALSE,
#                      comb.random = TRUE,
#                      method.tau = "SJ",
#                      hakn = TRUE,
#                      prediction = TRUE,
#                      sm = "SMD"), 
#         type = "pval", 
#         legend = FALSE)
# 
# 
# UCT<-drapery(metagen(z_treatment_effect ,
#                      z_se,
#                      data = final %>% filter(treatment=='UCT'),
#                      studlab = authors,
#                      comb.fixed = FALSE,
#                      comb.random = TRUE,
#                      method.tau = "SJ",
#                      hakn = TRUE,
#                      prediction = TRUE,
#                      sm = "SMD"), 
#              type = "pval", 
#              legend = FALSE)
# 
# CCT<-drapery(metagen(z_treatment_effect ,
#                      z_se,
#                      data = final %>% filter(treatment=='CCT'),
#                      studlab = authors,
#                      comb.fixed = FALSE,
#                      comb.random = TRUE,
#                      method.tau = "SJ",
#                      hakn = TRUE,
#                      prediction = TRUE,
#                      sm = "SMD"), 
#              type = "pval", 
#              legend = FALSE)
# 
# 
# Asset<-drapery(metagen(z_treatment_effect ,
#                      z_se,
#                      data = final %>% filter(treatment=='Asset'),
#                      studlab = authors,
#                      comb.fixed = FALSE,
#                      comb.random = TRUE,
#                      method.tau = "SJ",
#                      hakn = TRUE,
#                      prediction = TRUE,
#                      sm = "SMD"), 
#              type = "pval", 
#              legend = FALSE)
# 
# Neighborhood<-drapery(metagen(z_treatment_effect ,
#                        z_se,
#                        data = final %>% filter(treatment=='Neighborhood'),
#                        studlab = authors,
#                        comb.fixed = FALSE,
#                        comb.random = TRUE,
#                        method.tau = "SJ",
#                        hakn = TRUE,
#                        prediction = TRUE,
#                        sm = "SMD"), 
#                type = "pval", 
#                legend = FALSE)
# 
# 
# Lottery<-drapery(metagen(z_treatment_effect ,
#                               z_se,
#                               data = final %>% filter(treatment=='Lottery'),
#                               studlab = authors,
#                               comb.fixed = FALSE,
#                               comb.random = TRUE,
#                               method.tau = "SJ",
#                               hakn = TRUE,
#                               prediction = TRUE,
#                               sm = "SMD"), 
#                       type = "pval", 
#                       legend = FALSE)
# Grad<-drapery(metagen(z_treatment_effect ,
#                          z_se,
#                          data = final %>% filter(treatment=='Grad'),
#                          studlab = authors,
#                          comb.fixed = FALSE,
#                          comb.random = TRUE,
#                          method.tau = "SJ",
#                          hakn = TRUE,
#                          prediction = TRUE,
#                          sm = "SMD"), 
#                  type = "pval", 
#                  legend = FALSE)
# 
# 


# 
# final_one_first<-final_one[1:30,]
# 
# forest_one<-metagen(one_outcome ,
#                  one_outcome_sd,
#                  data = final_one_first,
#                  studlab = archive_name,
#                  comb.fixed = FALSE,
#                  comb.random = TRUE,
#                 # method.tau = "SJ",
#                  hakn = TRUE,
#                 # prediction = TRUE,
#                  sm = "SMD")
# 
# 
# setwd(diroverleaf)
# pdf(file = 'figures/forest_one.pdf', height=11) 
# 
# forest.jama <- forest(forest_one,
#                       layout = "JAMA",sortvar=one_outcome)
# dev.off() 
# 
# 
# final_one_second<-final_one[31:60,]
# 
# forest_one<-metagen(one_outcome ,
#                     one_outcome_sd,
#                     data = final_one_second,
#                     studlab = archive_name,
#                     comb.fixed = FALSE,
#                     comb.random = TRUE,
#                     method.tau = "SJ",
#                     hakn = TRUE,
#                     prediction = TRUE,
#                     sm = "SMD")
# 
# forest(forest_one,sortvar=one_outcome)
# 
# setwd(diroverleaf)
# pdf(file = 'figures/forest_one_2.pdf', height=9) 
# 
# forest.jama <- forest(forest_one,
#                       layout = "JAMA",sortvar=one_outcome)
# dev.off() 

