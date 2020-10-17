library(ggplot2)
library(scales)
library(stringr)
library(tidyverse)
library(grDevices)
library(writexl)
############Define a color scheme#####################
IPbesdarkgreen <- rgb(92/255, 102/255, 93/255) #5c665d
IPbeslightgreen <- rgb(181/255, 212/255, 141/255) #b5d48d
colfunc <- colorRampPalette(c(rgb(92/255, 102/255, 93/255), rgb(181/255, 212/255, 141/255)))
#######Usefull function if you want just one legend for several figures#######
library(gridExtra)
g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}
####------Question 2.17 and 2.18 stacked bar charts----------####
# This figure only needs s3_single as an input. Make sure you have this in your environment through making in using 'Starting.R' or load it from your data folder.
# First, we need to disentangle the options in 2.18
legend<-data.frame(code=c('Inf_FA','Inf_AR','Inf_Just','Inf_AI','Dec_RG','Dec_Partic','Dec_PT','Dec_EMC','T_PS','T_D','Unclear', 'Other'),
                   txt=c('Informative - Formative or Affirmative',
                         'Informative - Awareness raising',
                         'Informative - Justification',
                         'Informative - Accounting & Indicators',
                         'Decisive - Recommendations & guidance',
                         'Decisive  - Participation',
                         'Decisive - Prioritization on Trade-offs',
                         'Decisive - Environmental management criterion',
                         'Technical - Price-setting',
                         'Technical  - Damage compensation',
                         'unclear',
                         'Other'),
                    Fulltxt=c('Informative - Formative or Affirmative (Value formation or affirmation (not used
decisively or for technical purposes))', 'Informative - Awareness raising (Advocacy and raising awareness of total value, trade-offs, conflicts, scenarios)','Informative - Justification (ex post of a decision, Evaluation of existing projects and policies ex post)','Informative - Accounting & Indicators (Assessment of historic trends)','Decisive - Recommendations & guidance (Guidances, strategies, plans)','Decisive - Participation (Negotiation, arguments for discussion, shared norms & conflict resolution // Formulation of decision problem and structuring)','Decisive - Prioritization on Trade-offs (screening alternatives // ranking alternatives)', 'Decisive - Environmental management criterion (Policy target-setting // Criteria for spatial targeting (zoning, planning) // Allocation of rights to land and natural resource use)','Technical - Price-setting (Environmental standard setting (implicit pricing) // Pricing, setting incentive levels (explicit pricing))', 'Technical - Damage compensation (Establishing levels of damage compensation)', 'unclear', 'Other'))
sbst<-s3_single[,c('paperID','2.17','2.18')]
for(i in 1:nrow(legend)){
  A <-  str_detect(sbst$`2.18`, pattern = as.character(legend[i,'txt']))
  sbst <- cbind(sbst, A)
  colnames(sbst)[ncol(sbst)] <- as.character(legend[i,'code'])
}
#from wide to long format
sbst %>%
  gather(question, value, -paperID,-`2.17`,-`2.18`) ->sbst_long
#labels for the facets
labs <- c(sprintf('Desired uptake, N=%d', sum(sbst$`2.17`=='Desired uptake - A potential, expected, or wished for use of outputs, sometimes only cursory')),
          sprintf('Testing use case, N=%d', sum(sbst$`2.17`=='Testing use case - Researcher initiated valuation study, reported test of use to enable decision making')),
          sprintf('Actual Use case, N=%d', sum(sbst$`2.17`=='Actual use case - Stakeholder commissioned valuation study, reported use to enable decision making')))
names(labs) <- c('Desired uptake - A potential, expected, or wished for use of outputs, sometimes only cursory', 'Testing use case - Researcher initiated valuation study, reported test of use to enable decision making', 'Actual use case - Stakeholder commissioned valuation study, reported use to enable decision making')
#--------making the plot-----#
p <- ggplot(sbst_long) + geom_bar(alpha=1,size=1,color = IPbeslightgreen, fill = IPbeslightgreen,aes(x=question, y=1*value, group = `2.17` ), stat = "identity")+ facet_grid(cols = vars(`2.17`), scales = "free", labeller = labeller(`2.17` = labs)) + coord_flip() + theme_bw() + scale_x_discrete(breaks = legend$code, labels = str_wrap(as.character(legend$txt),20)) + ylab('Number of papers') + xlab('Valuation results purpose')
ggsave(p, filename='output/T3-Q217-218.pdf', width= 10, height = 7)
#extract paper ID numbers for testing use case and actual use case and give the purposes that were given for this application
write_xlsx(
    sbst[sbst$`2.17`!='Desired uptake - A potential, expected, or wished for use of outputs, sometimes only cursory', -3],
    path =  "output/testing and actual use cases.xlsx",
    col_names = TRUE,
    format_headers = TRUE,
    use_zip64 = FALSE
)



#############-------Topic 4: Reliability of valuation--------###########
#Page 92, figure 3.13 in the draft text
# This figure only needs s3_single as an input. Make sure you have this in your environment through making in using 'Starting.R' or load it from your data folder.
#4.1 - Replicability of the results is assessed  (yes/ no/ unclear)
#4.2 - Consistency of the results is assessed (yes/ no/ unclear)
#4.3 - Precision of the results is assessed (yes/ no/ unclear)
#4.4 - Internal Validity of the results is assessed by (check all that apply: credibility / construct validity / content validity / criterion validity / community validity / unclear / not assessed / Other:)
#4.5 - External Validity of the results is assessed by (check all that apply: transferability / generalisability / unclear / not assessed / Other:)
#4.6 - How sure do you feel about your scores for this step?
Lbl1<-data.frame(Answer = c('Yes','unclear','No'),
                 count1 = NA, Perc1 = NA, Label1 = NA,
                 count2 = NA, Perc2 = NA, Label2 = NA,
                 count3 = NA, Perc3 = NA, Label3 = NA)
for (i in 1:3){
  for (j in 1:3){
  Lbl1[j,sprintf('count%d',i)] <- sum(s3_single[,sprintf('4.%d',i)]==as.character(Lbl1[j,'Answer']))
  Lbl1[j,sprintf('Perc%d',i)] <- Lbl1[j,sprintf('count%d',i)] / nrow(s3_single)*100
  Lbl1[j,sprintf('Label%d',i)] <- sprintf('%1.2f%%', Lbl1[j,sprintf('Perc%d',i)])
  }
}

blank_theme <- theme_minimal()+
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.border = element_blank(),
    panel.grid=element_blank(),
    axis.ticks = element_blank(),
    plot.title=element_text(size=14, face="bold")
  )

p41 <- ggplot() +
  geom_bar(aes(x = factor(1), fill = s3_single$`4.1`),width = 1) + coord_polar("y", start = 0) + blank_theme + theme(axis.text.x=element_blank(), legend.position="bottom", axis.text.y = element_blank()) + scale_fill_manual(name = '',breaks = c('Yes','unclear','No'), values  = c(IPbeslightgreen, 'orange', 'red')) + xlab('') + ylab('') + ggtitle('Replicability') +
  geom_text(aes(x = 1.2, y = Lbl1$count1/2+ c(0, cumsum(Lbl1$count1)[-length(Lbl1$count1)]),
                label = Lbl1$Label1), size=5)
p42 <- ggplot() +
  geom_bar(aes(x = factor(1), fill = s3_single$`4.2`),width = 1) + coord_polar("y", start = 0) + blank_theme + theme(axis.text.x=element_blank(), axis.text.y = element_blank())+ scale_fill_manual(name = '',breaks = c('Yes','unclear','No'), values  = c(IPbeslightgreen, 'orange', 'red')) + xlab('') + ylab('') + ggtitle('Consistency') +
  geom_text(aes(x = 1.2, y = Lbl1$count2/2+ c(0, cumsum(Lbl1$count2)[-length(Lbl1$count2)]),
                label = Lbl1$Label2), size=5)
p43 <- ggplot() +
  geom_bar(aes(x = factor(1), fill = s3_single$`4.3`),width = 1) + coord_polar("y", start = 0) + blank_theme + theme(axis.text.x=element_blank(), axis.text.y = element_blank())+ scale_fill_manual(name = '',breaks = c('Yes','unclear','No'), values  = c(IPbeslightgreen, 'orange', 'red')) + xlab('') + ylab('') + ggtitle('Precision') +
  geom_text(aes(x = 1.2, y = Lbl1$count3/2+ c(0, cumsum(Lbl1$count3)[-length(Lbl1$count3)]),
                label = Lbl1$Label3), size=5)
mylegend<-g_legend(p41)
Pane1 <-grid.arrange(
  p41 + theme(legend.position="none"),
  p42 + theme(legend.position="none"),
  p43 + theme(legend.position="none"),
  mylegend,nrow=2, heights=c(10,0.5), layout_matrix = rbind(c(1,2,3),c(4,4,4)))
ggsave(Pane1, filename='output/T3-Q41-43.pdf',width=10, height=3.5)


#--------------internal & external validity graph---------------#
legend <- data.frame(code=c('IV_cred', 'IV_ConstrVal', 'IV_ContVal', 'IV_CritVal', 'IV_CommVal', 'IV_unclear', 'IV_NA', 'IV_Other','EV_Tranf','EV_General','EV_unclear','EV_NA','EV_Other'),
                     txt=c('credibility','construct validity','content validity','criterion validity','community validity','unclear','not assessed','Other:', 'transferability','generalisability','unclear','not assessed','Other:'))
sbst<-s3_single[,c('paperID','4.4','4.5')]
for(i in 1:8){
  A <-  str_detect(sbst$`4.4`, pattern = as.character(legend[i,'txt']))
  sbst <- cbind(sbst, A)
  colnames(sbst)[ncol(sbst)] <- as.character(legend[i,'code'])
}
for(i in 9:13){
  A <-  str_detect(sbst$`4.5`, pattern = as.character(legend[i,'txt']))
  sbst <- cbind(sbst, A)
  colnames(sbst)[ncol(sbst)] <- as.character(legend[i,'code'])
}
#from wide to long format
sbst[,1:11] %>%
  gather(question, value, -paperID,-`4.4`,-`4.5`) ->sbst_long44
sbst[,c(1:3,12:16)] %>%
  gather(question, value, -paperID,-`4.4`,-`4.5`) ->sbst_long45
# #labels for the facets
# labs <- c(sprintf('Desired uptake, N=%d', sum(sbst$`2.17`=='Desired uptake - A potential, expected, or wished for use of outputs, sometimes only cursory')),
#           sprintf('Testing use case, N=%d', sum(sbst$`2.17`=='Testing use case - Researcher initiated valuation study, reported test of use to enable decision making')),
#           sprintf('Actual Use case, N=%d', sum(sbst$`2.17`=='Actual use case - Stakeholder commissioned valuation study, reported use to enable decision making')))
# names(labs) <- c('Desired uptake - A potential, expected, or wished for use of outputs, sometimes only cursory', 'Testing use case - Researcher initiated valuation study, reported test of use to enable decision making', 'Actual use case - Stakeholder commissioned valuation study, reported use to enable decision making')
p44 <- ggplot(sbst_long44) + geom_bar(alpha=1,size=1,color = IPbeslightgreen, fill = IPbeslightgreen, aes(x = question, y=1*value), stat = "identity") + coord_flip() + theme_bw() + scale_x_discrete(breaks = legend[1:8,'code'], labels = str_wrap(as.character(legend[1:8,'txt']),20)) + ylab('Number of papers') + xlab('Internal Validity is assessed by ...')
p45 <- ggplot(sbst_long45) + geom_bar(alpha=1,size=1,color = IPbeslightgreen, fill = IPbeslightgreen, aes(x = question, y=1*value), stat = "identity") + coord_flip() + theme_bw() + scale_x_discrete(breaks = legend[9:13,'code'], labels = str_wrap(as.character(legend[9:13,'txt']),20)) + ylab('Number of papers') + xlab('External Validity is assessed by ...')

#-------Pie charts--------#
sbst %>% mutate(IV = as.factor(2*(!IV_NA & !IV_unclear)+1*IV_unclear),
                  EV = as.factor(2*(!EV_NA & !EV_unclear)+1*EV_unclear))->sbst
sbst$IV <- recode_factor(sbst$IV,'0'='No', '1'='unclear', '2'= 'Yes')
sbst$EV <- recode_factor(sbst$EV,'0'='No', '1'='unclear', '2'= 'Yes')
Lbl2<-data.frame(Answer = c('Yes','unclear','No'),
                 countIV = NA, PercIV = NA, LabelIV = NA,
                 countEV = NA, PercEV = NA, LabelEV = NA)
for (i in c('IV','EV')){
  for (j in 1:3){
    Lbl2[j,sprintf('count%s',i)] <- sum(sbst[,i]==as.character(Lbl2[j,'Answer']))
    Lbl2[j,sprintf('Perc%s',i)] <- Lbl2[j,sprintf('count%s',i)] / nrow(sbst)*100
    Lbl2[j,sprintf('Label%s',i)] <- sprintf('%1.2f%%', Lbl2[j,sprintf('Perc%s',i)])
  }
}
p44pie <- ggplot() +
  geom_bar(aes(x = factor(1), fill = sbst$IV),width = 1) + coord_polar("y", start = 0) + blank_theme + theme(axis.text.x=element_blank(), axis.text.y = element_blank())+ scale_fill_manual(name = '',breaks = c('Yes','unclear','No'), values  = c(IPbeslightgreen, 'orange', 'red')) + xlab('') + ylab('') + ggtitle('Internal validity') +
  geom_text(aes(x = 1.2, y = Lbl2$countIV/2+ c(0, cumsum(Lbl2$countIV)[-length(Lbl2$countIV)]),
                label = Lbl2$LabelIV), size=5)
p45pie <- ggplot() +
  geom_bar(aes(x = factor(1), fill = sbst$EV),width = 1) + coord_polar("y", start = 0) + blank_theme + theme(axis.text.x=element_blank(), axis.text.y = element_blank())+ scale_fill_manual(name = '',breaks = c('Yes','unclear','No'), values  = c(IPbeslightgreen, 'orange', 'red')) + xlab('') + ylab('') + ggtitle('External validity') +
  geom_text(aes(x = 1.2, y = Lbl2$countEV/2+ c(0, cumsum(Lbl2$countEV)[-length(Lbl2$countEV)]),
                label = Lbl2$LabelEV), size=5)

#------ --bar plot for total number of reliability aspects covered---------#
sbst%>%mutate(Replicability =  s3_single$`4.1`,
              Consistency = s3_single$`4.2`,
              Precision = s3_single$`4.3`) -> sbst
Pane2 <- ggplot(sbst) + geom_bar(aes(x=as.factor(TotalNB), fill = IPbeslightgreen)) + theme_minimal() +  xlab('Number of reliability aspects addressed') + ylab('Number of papers') + ggtitle('Number of reliability aspect adressed per paper') + scale_fill_identity()
ggsave(Pane2,filename='output/T3-Q41-45.pdf',width=10, height=3.5)

#Add method families
sbst%>%mutate(MF1.key = s3_single$MF1.key,
              MF2.key = s3_single$MF2.key,
              MF3.key = s3_single$MF3.key,
              MF4.key = s3_single$MF4.key) ->sbst
familysummaryIV <- data.frame(MF = c('MF1','MF1','MF1','MF2','MF2','MF2','MF3','MF3','MF3','MF4','MF4','MF4'),
                              Answer = c('Yes','unclear','No','Yes','unclear','No','Yes','unclear','No','Yes','unclear','No'),
                              N = NA,
                              Percentage=NA)
familysummaryIV$N = sapply(1:12,FUN=function(x) sum(sbst[,sprintf('%s.key',as.character(familysummaryIV$MF[x]))]==1 & (sbst$IV==familysummaryIV$Answer[x]),na.rm=T))
familysummaryIV$Percentage = familysummaryIV$N/sapply(familysummaryIV$MF,FUN = function(x)sum(familysummaryIV[familysummaryIV$MF==x,'N']))

pIV.fam<-ggplot(familysummaryIV) + geom_bar(aes(x = MF, fill=Answer, y=Percentage*100),stat = 'identity',position='stack') + scale_fill_manual(name = '',breaks = c('Yes','unclear','No'), values  = c(IPbeslightgreen, 'orange', 'red')) +ylab('Percentage in the method family') + xlab('Method family') + ggtitle('Is internal validity accounted for in the method family?') + theme_minimal()

familysummaryEV <- data.frame(MF = c('MF1','MF1','MF1','MF2','MF2','MF2','MF3','MF3','MF3','MF4','MF4','MF4'),
                              Answer = c('Yes','unclear','No','Yes','unclear','No','Yes','unclear','No','Yes','unclear','No'),
                              N = NA,
                              Percentage=NA)
familysummaryEV$N = sapply(1:12,FUN=function(x) sum(sbst[,sprintf('%s.key',as.character(familysummaryEV$MF[x]))]==1 & (sbst$EV==familysummaryEV$Answer[x]),na.rm=T))
familysummaryEV$Percentage = familysummaryEV$N/sapply(familysummaryEV$MF,FUN = function(x)sum(familysummaryEV[familysummaryEV$MF==x,'N']))

pEV.fam <- ggplot(familysummaryEV) + geom_bar(aes(x = MF, fill=Answer, y=Percentage*100),stat = 'identity',position='stack') + scale_fill_manual(name = '',breaks = c('Yes','unclear','No'), values  = c(IPbeslightgreen, 'orange', 'red')) +ylab('Percentage in the method family') + xlab('Method family') + ggtitle('Is external validity accounted for in the method family?') + theme_minimal()

mylegend<-g_legend(p44pie)
Pane3 <-grid.arrange(
  p44pie + theme(legend.position="none"),
  pIV.fam + theme(legend.position="none"),
  nrow=1, layout_matrix = rbind(c(1,2,2)))
ggsave(Pane3, filename='output/T3-Q44.pdf',width=10, height=3.5)
mylegend<-g_legend(p44pie)
Pane4 <-grid.arrange(
  p45pie + theme(legend.position="none"),
  pEV.fam + theme(legend.position="none"),
  nrow=1, layout_matrix = rbind(c(1,2,2)))
ggsave(Pane4, filename='output/T3-Q45.pdf',width=10, height=3.5)
ggsave(Pane2,filename='output/T3-Q41-45.pdf',width=10, height=3.5)
