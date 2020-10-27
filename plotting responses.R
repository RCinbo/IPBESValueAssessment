library(ggplot2)
library(scales)
library(stringr)
library(tidyverse)
library(grDevices)
library(likert)
library(writexl)
library(grid)
library(gridExtra)
require (gtable)
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
###########Labeller for the method families##########
MFLabels <- c('MF1','MF2','MF3','MF4') # change the names here if needed.
names(MFLabels) <- c('MF1','MF2','MF3','MF4')

blank_theme <- theme_minimal()+
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.border = element_blank(),
    panel.grid=element_blank(),
    axis.ticks = element_blank(),
    plot.title=element_text(size=14, face="bold")
  )

##########-------- All 'How certain are you about your answers?'--------##########
A<-data.frame(colnm = c('1.7', '2.20', '3.6','4.6','5.7','6.6','7.4','8.15','8.17'),
              subject = c('topic 1: Methods and their use', 'topic 2: Context of application','topic 3: Application descriptors','topic 4: Reliability and Validity','topic 5: IPLMLC','topic 6: Human well-being','topic 7: Ecological Sustainability','topic 8: Justice','All topics'))

L<-as.data.frame(s3_single[,sapply(A$colnm,FUN=function(x)which(colnames(s3_single)==x))])

for(i in 1:ncol(L)){
  L[,i] <- recode_factor(factor(as.factor(L[,i]), levels= c('5','4','3','2','1')),'1'='very sure','2'='sure','3'='in between','4'='unsure','5'='very unsure')
  L[,i] <- factor(L[,i],levels=c('very unsure','unsure','in between','sure','very sure'),ordered=TRUE)
}
  l<-likert(L)
  a<-order(summary(l,center=3)$high)
  plotCertainty <- likert.bar.plot(l, low.color ='red', high.color='forestgreen', plot.percent.low=TRUE,plot.percent.neutral=TRUE,plot.percent.high=TRUE,legend.position= "bottom",legend='') + theme_classic()+ylab('Percentage of the respondents') +
    theme(legend.position='bottom') + scale_x_discrete(limits=summary(l,center=3)[a,'Item'],labels = sapply(summary(l,center=3)[a,'Item'], FUN=function(x)  str_wrap(A[A$colnm==x,'subject'], width = 22)))

ggsave(plotCertainty, filename = 'output/CertaintyLikertPlot.pdf', width= 8, height = 5)


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
p <- ggplot(sbst_long) + geom_bar(alpha=1, size=1, color = IPbeslightgreen, fill = IPbeslightgreen, aes(x=question, y=1*value, group = `2.17` ), stat = "identity")+ facet_grid(cols = vars(`2.17`), scales = "free", labeller = labeller(`2.17` = labs)) + coord_flip() + theme_bw() + scale_x_discrete(breaks = legend$code, labels = str_wrap(as.character(legend$txt),20)) + ylab('Number of papers') + xlab('Valuation results purpose')
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

p41 <- ggplot() +
  geom_bar(aes(x = factor(1), fill = s3_single$`4.1`),width = 1) + coord_polar("y", start = 0) + blank_theme + theme(axis.text.x=element_blank(), legend.position="bottom", axis.text.y = element_blank()) + scale_fill_manual(name = '',breaks = c('Yes','unclear','No'), values  = c(IPbeslightgreen, 'orange', 'red')) + xlab('') + ylab('') + ggtitle('Replicability') +
  geom_text(aes(x = 1.2, y = Lbl1$count1/2+ c(0, cumsum(Lbl1$count1)[-length(Lbl1$count1)]),
                label = Lbl1$Label1), size=5)
familysummary41 <- data.frame(MF = c('MF1','MF1','MF1','MF2','MF2','MF2','MF3','MF3','MF3','MF4','MF4','MF4'),
                              Answer = c('Yes','unclear','No','Yes','unclear','No','Yes','unclear','No','Yes','unclear','No'),
                              N = NA,
                              Percentage=NA)
familysummary41$N = sapply(1:12,FUN=function(x) sum(s3_single[,sprintf('%s.key',as.character(familysummary41$MF[x]))]==1 & (s3_single$`4.1`==familysummary41$Answer[x]),na.rm=T))
familysummary41$Percentage = familysummary41$N/sapply(familysummary41$MF,FUN = function(x)sum(familysummary41[familysummary41$MF==x,'N']))
p41.fam <- ggplot(familysummary41) + geom_bar(aes(x = MF, fill=Answer, y=Percentage*100),stat = 'identity',position='stack') + scale_fill_manual(name = '',breaks = c('Yes','unclear','No'), values  = c(IPbeslightgreen, 'orange', 'red')) +ylab('Percentage in the method family') + xlab('Method family') + ggtitle('Is replicability of the results assessed?') + theme_minimal() + scale_x_discrete(labels= MFLabels)

p42 <- ggplot() +
  geom_bar(aes(x = factor(1), fill = s3_single$`4.2`),width = 1) + coord_polar("y", start = 0) + blank_theme + theme(axis.text.x=element_blank(), axis.text.y = element_blank())+ scale_fill_manual(name = '',breaks = c('Yes','unclear','No'), values  = c(IPbeslightgreen, 'orange', 'red')) + xlab('') + ylab('') + ggtitle('Consistency') +
  geom_text(aes(x = 1.2, y = Lbl1$count2/2+ c(0, cumsum(Lbl1$count2)[-length(Lbl1$count2)]),
                label = Lbl1$Label2), size=5)
familysummary42 <- data.frame(MF = c('MF1','MF1','MF1','MF2','MF2','MF2','MF3','MF3','MF3','MF4','MF4','MF4'),
                              Answer = c('Yes','unclear','No','Yes','unclear','No','Yes','unclear','No','Yes','unclear','No'),
                              N = NA,
                              Percentage=NA)
familysummary42$N = sapply(1:12,FUN=function(x) sum(s3_single[,sprintf('%s.key',as.character(familysummary42$MF[x]))]==1 & (s3_single$`4.2`==familysummary42$Answer[x]),na.rm=T))
familysummary42$Percentage = familysummary42$N/sapply(familysummary42$MF,FUN = function(x)sum(familysummary42[familysummary42$MF==x,'N']))
p42.fam <- ggplot(familysummary42) + geom_bar(aes(x = MF, fill=Answer, y=Percentage*100),stat = 'identity',position='stack') + scale_fill_manual(name = '',breaks = c('Yes','unclear','No'), values  = c(IPbeslightgreen, 'orange', 'red')) +ylab('Percentage in the method family') + xlab('Method family') + ggtitle('Is consistency of the results assessed?') + theme_minimal() + scale_x_discrete(labels= MFLabels)

p43 <- ggplot() +
  geom_bar(aes(x = factor(1), fill = s3_single$`4.3`),width = 1) + coord_polar("y", start = 0) + blank_theme + theme(axis.text.x=element_blank(), axis.text.y = element_blank())+ scale_fill_manual(name = '',breaks = c('Yes','unclear','No'), values  = c(IPbeslightgreen, 'orange', 'red')) + xlab('') + ylab('') + ggtitle('Precision') +
  geom_text(aes(x = 1.2, y = Lbl1$count3/2+ c(0, cumsum(Lbl1$count3)[-length(Lbl1$count3)]),
                label = Lbl1$Label3), size=5)
familysummary43 <- data.frame(MF = c('MF1','MF1','MF1','MF2','MF2','MF2','MF3','MF3','MF3','MF4','MF4','MF4'),
                              Answer = c('Yes','unclear','No','Yes','unclear','No','Yes','unclear','No','Yes','unclear','No'),
                              N = NA,
                              Percentage=NA)
familysummary43$N = sapply(1:12,FUN=function(x) sum(s3_single[,sprintf('%s.key',as.character(familysummary43$MF[x]))]==1 & (s3_single$`4.3`==familysummary43$Answer[x]),na.rm=T))
familysummary43$Percentage = familysummary43$N/sapply(familysummary43$MF,FUN = function(x)sum(familysummary43[familysummary43$MF==x,'N']))
p43.fam <- ggplot(familysummary43) + geom_bar(aes(x = MF, fill=Answer, y=Percentage*100),stat = 'identity',position='stack') + scale_fill_manual(name = '',breaks = c('Yes','unclear','No'), values  = c(IPbeslightgreen, 'orange', 'red')) +ylab('Percentage in the method family') + xlab('Method family') + ggtitle('Is precision of the results assessed?') + theme_minimal() + scale_x_discrete(labels= MFLabels)


mylegend<-g_legend(p41)
Pane1 <-grid.arrange(
  p41 + theme(legend.position="none"),
  p42 + theme(legend.position="none"),
  p43 + theme(legend.position="none"),
  nrow=1, layout_matrix = rbind(c(1,2,3)))
ggsave(Pane1, filename='output/T3-Q41-43.pdf',width=10, height=3.5)

mylegend<-g_legend(p41)
Pane1 <-grid.arrange(
  p41 + theme(legend.position="none"),
  p41.fam + theme(legend.position="none"),
  p42 + theme(legend.position="none"),
  p42.fam + theme(legend.position="none"),
  p43 + theme(legend.position="none"),
  p43.fam + theme(legend.position="none"),
  mylegend,
  nrow=4, layout_matrix = rbind(c(1,2),c(3,4),c(5,6),c(7,7)), widths=c(1,4), heights=c(3,3,3,1))
ggsave(Pane1, filename='output/T3-Q41-43_MF.pdf',width=10, height=10)



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
sbst$TotalNB <- rowSums(sbst[,17:21]=="Yes")
Pane2 <- ggplot(sbst) + geom_bar(aes(x=as.factor(TotalNB), fill = IPbesdarkgreen)) + theme_minimal() +  xlab('Number of reliability aspects addressed') + ylab('Number of papers') + ggtitle('Number of reliability aspect adressed per paper') + scale_fill_identity()
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

pIV.fam <- ggplot(familysummaryIV) + geom_bar(aes(x = MF, fill=Answer, y=Percentage*100),stat = 'identity',position='stack') + scale_fill_manual(name = '',breaks = c('Yes','unclear','No'), values  = c(IPbeslightgreen, 'orange', 'red')) +ylab('Percentage in the method family') + xlab('Method family') + ggtitle('Is internal validity accounted for in the method family?') + theme_minimal() + scale_x_discrete(labels= MFLabels)

familysummaryEV <- data.frame(MF = c('MF1','MF1','MF1','MF2','MF2','MF2','MF3','MF3','MF3','MF4','MF4','MF4'),
                              Answer = c('Yes','unclear','No','Yes','unclear','No','Yes','unclear','No','Yes','unclear','No'),
                              N = NA,
                              Percentage=NA)
familysummaryEV$N = sapply(1:12,FUN=function(x) sum(sbst[,sprintf('%s.key',as.character(familysummaryEV$MF[x]))]==1 & (sbst$EV==familysummaryEV$Answer[x]),na.rm=T))
familysummaryEV$Percentage = familysummaryEV$N/sapply(familysummaryEV$MF,FUN = function(x)sum(familysummaryEV[familysummaryEV$MF==x,'N']))

pEV.fam <- ggplot(familysummaryEV) + geom_bar(aes(x = MF, fill=Answer, y=Percentage*100),stat = 'identity',position='stack') + scale_fill_manual(name = '',breaks = c('Yes','unclear','No'), values  = c(IPbeslightgreen, 'orange', 'red')) +ylab('Percentage in the method family') + xlab('Method family') + ggtitle('Is external validity accounted for in the method family?') + theme_minimal() + scale_x_discrete(labels= MFLabels)

Pane3 <-grid.arrange(
  p44pie + theme(legend.position="none"),
  pIV.fam + theme(legend.position="none"),
  nrow=1, layout_matrix = rbind(c(1,2,2)))
ggsave(Pane3, filename='output/T3-Q44.pdf',width=10, height=3.5)
Pane4 <-grid.arrange(
  p45pie + theme(legend.position="none"),
  pEV.fam + theme(legend.position="none"),
  nrow=1, layout_matrix = rbind(c(1,2,2)))
ggsave(Pane4, filename='output/T3-Q45.pdf',width=10, height=3.5)
ggsave(Pane2,filename='output/T3-Q41-45.pdf',width=10, height=3.5)

Theme4 <- grid.arrange(mylegend, Pane1, Pane2, Pane3, Pane4,
                       nrow=4, heights=c(1,10,1,10),widths=c(10,0.5,10),layout_matrix = rbind(c(1,NA,NA),c(2,NA,3),c(NA,NA,NA),c(4,NA,5)))
ggsave(Theme4,filename='output/Theme4.pdf',width=18, height=8)



########-----Data Overview 5 - Valuation for Human Wellbeing-----########
# @TODO Heera

##--- 6.1. The application assesses human well-being using one of the following indicators (multiple possible)
wellbeing_indicator_org = c("Subjective well-being \\(e.g., satisfaction, happiness etc.\\) linked to nature & biodiversity",
"A composite indicator \\(combining different aspects of well-being into one, such as Human Development Index, Better Life Index etc.\\) linked to  nature and biodiversity") # !!! a tab betwen "to" and "nature"

wellbeing_indicator_alt = c("Subjective well-being", "A composite indicator")
wellbeing_indicator_org_v = as.character(s3_single$"6.1")
wellbeing_indicator_alt_v = wellbeing_indicator_org_v


for (o_idx in 1:length(wellbeing_indicator_org)) {

  wellbeing_indicator_alt_v = str_replace_all(wellbeing_indicator_alt_v, pattern = wellbeing_indicator_org[o_idx], replacement = wellbeing_indicator_alt[o_idx])
}

# detects "tab"
tab_yn = str_detect(wellbeing_indicator_org_v, pattern = "  ")
table(tab_yn)

# suchas_yn = str_detect(wellbeing_indicator_alt_v, pattern = "such as")
# table(suchas_yn)
# which(suchas_yn)
#
# wellbeing_indicator_alt_v[556]


# starts_idx = which(starts_yn)
#
# detect_yn = str_detect(wellbeing_indicator_alt_v[starts_idx], pattern = wellbeing_indicator_org[1])
# table(detect_yn)
#
# replaced_v  = str_replace_all(wellbeing_indicator_alt_v[starts_idx], pattern = wellbeing_indicator_org[1], replacement = wellbeing_indicator_alt[1])
#
# replaced_detect_yn = str_detect(replaced_v, pattern = wellbeing_indicator_org[1])
#

wellbeing_given_answer = c(
  "Livelihood dependence on access to natural resources",
  "Livelihood dependence on management of land",
  "\\(Loss of\\) profits from natural resources",
  "Physical health outcomes linked to nature & biodiversity",
  "Mental health outcomes linked to nature & biodiversity",
  "Subjective well-being",
  "A composite indicator",
  "Change in utility of individuals linked to changes in nature & biodiversity",
  "Well-being indicator is assessed but not linked to nature & biodiversity",
  "Application does not assess human wellbeing with one of these indicators"
)

# Identify other answers
split_res_l_6.1 = vector("list", length = nrow(s3_single))

wellbeing_indicator_other_v = wellbeing_indicator_alt_v
for (given_idx in 1:length(wellbeing_given_answer)) {

  wellbeing_indicator_other_v = str_replace_all(wellbeing_indicator_other_v, pattern = wellbeing_given_answer[given_idx], replacement = "")
}
wellbeing_indicator_other_v = str_trim(str_replace_all(wellbeing_indicator_other_v, pattern = "[,;]", replacement = "")) # warning: other answers without comma and semicolon
# table(wellbeing_indicator_other_v)


wellbeing_indicator_other_wospace_v = str_replace_all(wellbeing_indicator_other_v, pattern = "[ ]", replacement = "") # remove whitespace
other_idx_6.1 = str_length(wellbeing_indicator_other_wospace_v) >= 1 # does it have still more characters?

wellbeing_indicator_other_v[other_idx_6.1]
n_other_6.1 = length(which(other_idx_6.1))
other_df_6.1= cbind(paperID= s3_single$paperID, OTHER_ANSWER_6_1 = wellbeing_indicator_other_v,ANSWER_RAW=as.character(s3_single$"6.1") )[other_idx_6.1,]
#write.xlsx(other_df_6.1, file = "output/6.1_otheranswers.xlsx")

# Count given and other answers individually

wellbeing_given_detected = sapply(wellbeing_given_answer, FUN = function(x) str_detect(wellbeing_indicator_alt_v, pattern = x))


wellbeing_given_cnt= apply(wellbeing_given_detected, MARGIN = 1,  FUN = function(x) (which(x)))
wellbeing_given_tb = table(unlist(wellbeing_given_cnt))
names(wellbeing_given_tb)= wellbeing_given_answer

wellbeing_all_tb= c(Other = n_other_6.1, sort(wellbeing_given_tb))
names(wellbeing_all_tb)[10] <- "(Loss of) profits from natural resources"
names(wellbeing_all_tb)

#pdf("output/Fig_6.1_25Oct.pdf", width=15, height = 8, pointsize = 12)
par(mar=c(5,25,1,1))
  barplot(wellbeing_all_tb, las=1, horiz=T, cex.names = 0.8, col = IPbeslightgreen, border =IPbeslightgreen, xlim = c(0, max(wellbeing_all_tb) *1.1))
#dev.off()

#
#
# # 21230
# which(s3_single$paperID=="21230")
#
# split_res_l_6.1 = vector("list", length = nrow(s3_single))
#
# for (row_idx in 1:nrow(s3_single)) {
#
#   split_tmp = str_trim(str_split(wellbeing_indicator_final_v[row_idx], pattern = ",")[[1]])
#   split_res_l_6.1[[row_idx]] = split_tmp
# }
#
# split_res_l_6.1[[1029]]
#
# split_6.1_v = unlist(split_res_l_6.1)
# split_6.1_v_fac = factor(split_6.1_v)
#
# levels(split_6.1_v_fac)
#
# split_6.1_sorted = sort(table(split_6.1_v_fac), decreasing = F)
# barplot(split_6.1_sorted, horiz=T, las=1, cex.names=0.5, col=IPbeslightgreen)
#
# split_6.1_tb_sorted_reduced =  c(sum(split_6.1_sorted[split_6.1_sorted<5]), split_6.1_sorted[split_6.1_sorted>=5])
# names(split_6.1_tb_sorted_reduced)[1] = "Other"



######  --- 6.2. The application assesses the preferences (or importance) that humans assign to nature and biodiversity in terms of (multiple possible)
summary(s3_single$"6.2")

preference_given_answer = c(
  "Hypothetical willingness to give up resources \\(monetary or other forms\\) to maintain \\(or increase\\) aspects of nature and biodiversity",
  "Hypothetical compensation \\(monetary or other forms\\) to give up access to nature or management rights of natural areas",
  "Scores of relative importance to people of nature’s contributions to people \\(e. g. allocation of points to /or ranking of different aspects of nature or different places\\)",
  "Spending or expenditure \\(in monetary or other forms of resources\\) to maintain \\(or increase\\) aspects of nature and biodiversity \\(or to avoid losses\\)",
  "Dialogues with communities about the importance of different aspects of nature and biodiversity",
  "Application does not assess preferences of humans to nature"
)

# Identify other answers
split_res_l_6.2 = vector("list", length = nrow(s3_single))

for (row_idx in 1:nrow(s3_single)) {

  split_tmp = str_trim(str_split(s3_single$"6.2"[row_idx], pattern = ",")[[1]])
  split_res_l_6.2[[row_idx]] = split_tmp
}

split_6.2_v = unlist(split_res_l_6.2)
split_6.2_v_fac = factor(split_6.2_v)
split_6.2_sorted = sort(table(split_6.2_v_fac), decreasing = F)
barplot(split_6.2_sorted, horiz=T, las=1, cex.names=0.5, col=IPbeslightgreen)

names(split_6.2_sorted[split_6.2_sorted==1])
#
# split_6.1_tb_sorted_reduced =  c(sum(split_6.1_sorted[split_6.1_sorted<5]), split_6.1_sorted[split_6.1_sorted>=5])
# names(split_6.1_tb_sorted_reduced)[1] = "Other"

preference_other_v = as.character(s3_single$"6.2")

for (given_idx in 1:length(preference_given_answer)) {

  preference_other_v = str_replace_all(preference_other_v, pattern = preference_given_answer[given_idx], replacement = "")
}

preference_other_v = str_trim(str_replace_all(preference_other_v, pattern = "[,;]", replacement = "")) # warning: other answers without comma and semicolon
length(table(preference_other_v))

preference_other_wospace_v = str_replace_all(preference_other_v, pattern = "[ ]", replacement = "") # remove whitespace
other_idx_6.2 = str_length(preference_other_wospace_v) >= 1 # does it have still more characters?

preference_other_v[other_idx_6.2]
n_other_6.2 = length(which(other_idx_6.2))
other_df_6.2= cbind(paperID= s3_single$paperID, OTHER_ANSWER_6_2 = preference_other_v, ANSWER_RAW=as.character(s3_single$"6.2") )[other_idx_6.2,]
#write.xlsx(other_df_6.2, file = "output/6.2_otheranswers.xlsx")

# Count given and other answers individually

prefernece_given_detected = sapply(preference_given_answer, FUN = function(x) str_detect(as.character(s3_single$"6.2"), pattern = x))
preference_given_cnt= apply(prefernece_given_detected, MARGIN = 1,  FUN = function(x) (which(x)))
preference_given_tb = table(unlist(preference_given_cnt))
names(preference_given_tb)= preference_given_answer

preference_all_tb= c(Other = n_other_6.2, sort(preference_given_tb))
shortnames_preference_all_tb = c(
  "Other",
  "Hypothetical compensation \nto give up access to nature ",
  "Spending or expenditure to maintain aspects \nof nature and biodiversity",
  "Scores of relative importance \nto people of nature's contributions to people",
  "Dialogues with communities about the importance \nof different aspects of nature and biodiversity",
  "Hypothetical willingness to give up resources \nto maintain aspects of nature and biodiversity",
  "Application does not assess preferences of humans to nature"  )
names(preference_all_tb) <-shortnames_preference_all_tb

# pdf("output/Fig_6.2_25Oct.pdf", width=15, height = 8, pointsize = 12)
par(mar=c(5,20,1,1))
barplot(preference_all_tb, las=1, horiz=T, cex.names = 0.8, col = IPbeslightgreen, border =IPbeslightgreen, xlim = c(0, max(preference_all_tb) *1.1))
# dev.off()



###### ------ 6.3. The application assesses the costs to protect nature & biodiversity for its own sake or to maintain nature's contribution to people in the form of (multiple possible)
summary(s3_single$"6.3")
length(summary(s3_single$"6.3"))

cost_org = "i.e.,"
cost_alt = "i.e."
cost_org_v = as.character(s3_single$"6.3")
cost_alt_v = cost_org_v

cost_alt_v = str_replace_all(cost_alt_v, pattern = cost_org, replacement = cost_alt)

cost_given_answer = c(
  "What has actually been spent in the past to protect nature and biodiversity or maintain nature’s contribution to people \\[This only applies if the investment was done in the past\\]",
  "The costs and the benefits from past projects to protect nature and biodiversity or maintain nature’s contribution to people \\[This only applies if the project was in the past\\]",
  "What it would cost \\(cost per unit\\) to protect nature and biodiversity or maintain nature’s contribution to people",
  "What it would cost to protect nature and biodiversity or maintain nature’s contribution to people in the most effective way \\(i.e. compare different ways of meeting a target for nature and biodiversity or ES/NCPs\\)",
  "The costs and the benefits from potential or hypothetical future projects or policies to protect nature and biodiversity or maintain nature’s contribution to people",
  "Application does not assess cost to protect nature & biodiversity for its own sake or to maintain nature’s contributions to people"
)


# Identify other answers
split_res_l_6.3 = vector("list", length = nrow(s3_single))

cost_other_v = cost_alt_v

for (given_idx in 1:length(cost_given_answer)) {

  cost_other_v = str_replace_all(cost_other_v, pattern = cost_given_answer[given_idx], replacement = "")
}
cost_other_v = str_trim(str_replace_all(cost_other_v, pattern = "[,;]", replacement = "")) # warning: other answers without comma and semicolon
table(cost_other_v)
length(table(cost_other_v))


cost_other_wospace_v = str_replace_all(cost_other_v, pattern = "[ ]", replacement = "") # remove whitespace
other_idx_6.3 = str_length(cost_other_wospace_v) >= 1 # does it have still more characters?

cost_other_v[other_idx_6.3]
n_other_6.3 = length(which(other_idx_6.3)); n_other_6.3
other_df_6.3= cbind(paperID= s3_single$paperID, OTHER_ANSWER_6_3 = cost_other_v, ANSWER_RAW=as.character(s3_single$"6.3") )[other_idx_6.3,]
#write.xlsx(other_df_6.3, file = "output/6.3_otheranswers.xlsx")

# Count given and other answers individually

cost_given_detected = sapply(cost_given_answer, FUN = function(x) str_detect(cost_alt_v, pattern = x))
cost_given_cnt= apply(cost_given_detected, MARGIN = 1,  FUN = function(x) (which(x)))
cost_given_tb = table(unlist(cost_given_cnt))
names(cost_given_tb)= cost_given_answer

cost_all_tb = c(Other = n_other_6.3, sort(cost_given_tb))
names(cost_all_tb)
shortnames_cost_all_tb = c(
  "Other",
  "What has actually been spent in the past to protect nature and biodiversity \nor maintain nature's contribution to people",
  "The costs and the benefits from past projects to protect nature \nand biodiversity or maintain nature’s contribution to people",
  "What it would cost to protect nature and biodiversity \nor maintain nature's contribution to people",
  "What it would cost to protect nature and biodiversity or maintain \nnatur's contribution to people in the most effective way",
  "The costs and the benefits from potential or \nhypothetical future projects or policies",
  "Application does not assess cost"
)
names(cost_all_tb) <- shortnames_cost_all_tb

# pdf("output/Fig_6.3_25Oct.pdf", width=15, height = 8, pointsize = 12)
par(mar=c(5,25,1,1))
barplot(cost_all_tb, las=1, horiz=T, cex.names = 0.8, col = IPbeslightgreen, border =IPbeslightgreen, xlim = c(0, max(cost_all_tb) *1.1))
# dev.off()


##### --- 6.4 The application asseses human well-being in a different way, please spcify
length(summary(s3_single$"6.4"))
as.character(s3_single$"6.4")

different_wospace_v = str_replace_all(as.character(s3_single$"6.4"), pattern = "[ ]", replacement = "") # remove whitespace
other_idx_6.4 = str_length(different_wospace_v) >= 1 # does it have still more characters?

as.character(s3_single$"6.4")[other_idx_6.4]
n_other_6.4 = length(which(other_idx_6.4)); n_other_6.4
other_df_6.4= cbind(paperID= s3_single$paperID, ANSWER_RAW= as.character(s3_single$"6.4") )[other_idx_6.4,]
#write.xlsx(other_df_6.4, file = "output/6.4_otheranswers.xlsx")


##### --- 6.5 If the application assesses human well-being (specified above), is the well-being indicator assessed for different socio-demographic groups?
summary(s3_single$"6.5")
length(summary(s3_single$"6.5"))

socio_org = c("Yes, ", "No, ")
socio_alt = c("Yes-", "No-")
socio_org_v = as.character(s3_single$"6.5")
socio_alt_v = socio_org_v


for (o_idx in 1:length(socio_org)) {

  socio_alt_v = str_replace_all(socio_alt_v, pattern = socio_org[o_idx], replacement = socio_alt[o_idx])
}

socio_given_answer = c(
  "Yes-analysed for different income groups",
  "Yes-analysed for different age groups",
  "Yes-analysed for different levels of education",
  "Yes-analysed for different gender",
  "Yes-analysed for different stakeholder groups",
  "No-difference in the well-being indicator is assessed but not attributed to different socio-demographic groups",
  "No-difference in the well-being indicator by socio-demographic group is not evaluated.",
  "irrelevant"
)

# Identify other answers
split_res_l_6.5 = vector("list", length = nrow(s3_single))

socio_other_v = socio_alt_v

for (given_idx in 1:length(socio_given_answer)) {

  socio_other_v = str_replace_all(socio_other_v, pattern = socio_given_answer[given_idx], replacement = "")
}
socio_other_v = str_trim(str_replace_all(socio_other_v, pattern = "[,;]", replacement = "")) # warning: other answers without comma and semicolon
table(socio_other_v)
length(table(socio_other_v))

socio_other_wospace_v = str_replace_all(socio_other_v, pattern = "[ ]", replacement = "") # remove whitespace
other_idx_6.5 = str_length(socio_other_wospace_v) >= 1 # does it have still more characters?

socio_other_v[other_idx_6.5]
n_other_6.5 = length(which(other_idx_6.5)); n_other_6.5
other_df_6.5= cbind(paperID= s3_single$paperID, OTHER_ANSWER_6_5 = socio_other_v, ANSWER_RAW = as.character(s3_single$"6.5") )[other_idx_6.5,]
#write.xlsx(other_df_6.5, file = "output/6.5_otheranswers.xlsx")

# Count given and other answers individually

socio_given_detected = sapply(socio_given_answer, FUN = function(x) str_detect(socio_alt_v, pattern = x))
socio_given_cnt= apply(socio_given_detected, MARGIN = 1,  FUN = function(x) (which(x)))
socio_given_tb = table(unlist(socio_given_cnt))
names(socio_given_tb)= socio_given_answer

socio_all_tb = c(Other = n_other_6.5, socio_given_tb)
names(socio_all_tb)
shortnames_socio_all_tb = c(
  "Other",
  "Yes, analysed for different income groups",
  "Yes, analysed for different age groups",
  "Yes, analysed for different levels of education",
  "Yes, analysed for different gender",
  "Yes, analysed for different stakeholder groups",
  "No, difference in the well-being indicator is assessed \nbut not attributed to different socio-demographic groups",
  "No, difference in the well-being indicator \nby socio-demographic group is not evaluated",
  "irrelevant"
)
names(socio_all_tb) <- shortnames_socio_all_tb

#pdf("output/Fig_6.5_25Oct.pdf", width=15, height = 8, pointsize = 12)
par(mar=c(5,20,1,1))
barplot(socio_all_tb, las=1, horiz=T, cex.names = 0.8, col = IPbeslightgreen, border =IPbeslightgreen, xlim = c(0, max(socio_all_tb) *1.1))
#dev.off()

#pdf("output/Topic6_combine_25Oct.pdf", width=15, height = 20, pointsize = 12)

par(mfrow=c(4,1), mar=c(5,25,1,1))
barplot(wellbeing_all_tb, las=1, horiz=T, cex.names = 0.8, col = IPbeslightgreen, border =IPbeslightgreen, xlim = c(0, max(wellbeing_all_tb) *1.1), main = "6.1 Well-being")
barplot(preference_all_tb, las=1, horiz=T, cex.names = 0.8, col = IPbeslightgreen, border =IPbeslightgreen, xlim = c(0, max(preference_all_tb) *1.1), main= "6.2 Preference")
barplot(cost_all_tb, las=1, horiz=T, cex.names = 0.8, col = IPbeslightgreen, border =IPbeslightgreen, xlim = c(0, max(cost_all_tb) *1.1), main = "6.3 Cost")
barplot(socio_all_tb, las=1, horiz=T, cex.names = 0.8, col = IPbeslightgreen, border =IPbeslightgreen, xlim = c(0, max(socio_all_tb) *1.1), main = "6.5 Socio-demographic groups")
#dev.off()



########-----Data Overview 6 - Valuation for ecological sustainability-----########
#Raïsa Topic 7

########-----Data Overview 4 - Valuation of indigenous peoples' and like-minded local communities's-----########
if(1==0){
#Topic 5, p87 --> Q5.1 - Q5.8
#Q5.1: Are there any authors from Indigenous Peoples or Like-minded Local Communities? none / one or more authors explicitly represented as such / unclear
#Q5.2: The application identifies/assesses respect towards nature by (multiple possible):
  #1) generally demonstrating and expressing deep respect to the land, sea, or their natural
#surroundings; manifestation of spiritual connection to the land showing intimate
#interaction associated with the land, sea, lakes or rivers; spiritual beings in the landscape;
#sacred sites.
  #2) identifying placed-based community ceremonies, rituals or gatherings that are linked
#to the terrestrial or marine landscape.
  #3)respecting those ceremonies, rituals or gatherings
  #4)does not assess this
#Q5.3:The application assesses responsibility or care for the land by (multiple possible):
  #1) assessing overall protection & preservation of sites (mountains, rivers, landscapes, landforms, forests, etc.) for their sacred meaning, or cultural or historical significance;
  #2) assessing whether actions/practices where natural resources use is compatible with and follows ancestral teachings about living in harmony with nature and respect for the land;
  #3) Identifying non-over-exploitative production systems out of a sense of responsibilities towards future generations
  #4) considering actions/practices to renew the sense of interconnection with terrestrial or marine surroundings and awareness about responsibility and care.
  #5) does not assess this
  #6) other
#Q5.4: The application assesses kinship/communality with other people by (multiple possible):
  #1) assessing equitable sharing use of seasonal resources,
  #2) considering the satisfaction of community members’ needs, particularly women, elders and children.
  #3) assessing whether the community shares teachings and knowledge on how to live sustainably with all elements of nature
  #4) assessing the community internal practices of reciprocity (communal work or sharing of products); equitable trade with other communities.
  #5) does not assess this
  #6) other
#Q5.5: The application assesses kinship/communality with non-human entities by  (multiple possible):
   #1) identifying and assessing kinship relationships with animal through time identifying and assessing kinship relationships with animals’ habitat and other plant species
  #2) identifying some plants as sacred,
  #3) assessing the reluctance and discouragement of using plants, animals, mountains,rivers, and other non-living entities in nature for individualistic gains
  #4) assessing the understanding of landforms as entitled to personhood or legal rights
  #5) does not assess this
  #6) Other
#Q5.6: The application assesses values of self-determination and ancestral law, by (multiple possible):
  #1) assessing the application of ancestral law, teachings, customs and uses,
  #2) assessing the acknowledgement and inclusion of people’s beliefs, spirituality, and ceremonial practices
  #3) assessing the community governance and community protocols or regulations,
  #4) assessing the implementation of the FPIC,
  #5) accounting for the intervention of Indigenous spiritual authorities and use of Indigenous language,
  #6) including the Indigenous authors’ self-positioning or elders and knowledge-keepers authorship
  #7) does not assess this
  #8) other
}
#Pie chart for whether the authors are indigenous
s3_single$`5.1`<-factor(s3_single$`5.1`, levels=c('none','unclear', 'one or more authors explicitly represented as such'),ordered=TRUE)
Lbl2<-data.frame(Answer = c('one or more authors explicitly represented as such','unclear','none'),count = NA, Perc = NA, Label = NA)
for (j in 1:3){
    Lbl2[j,'count'] <- sum(s3_single$`5.1`==as.character(Lbl2[j,'Answer']))
    Lbl2[j,'Perc'] <- Lbl2[j,'count'] / nrow(s3_single)*100
    Lbl2[j,'Label'] <- sprintf('%1.0f%%', Lbl2[j,'Perc'])
  }
p51pie <- ggplot() +
  geom_bar(aes(x = factor(1), fill = s3_single$`5.1`),width = 1) +
  blank_theme + theme(axis.text.x=element_blank(), axis.text.y = element_blank())+ scale_fill_manual(name = '',breaks = c('none','one or more authors explicitly represented as such', 'unclear'),labels = c('No','Yes, one or more', 'Unclear'), values  = c('red', IPbeslightgreen, 'orange')) +
  xlab('') + ylab('') + ggtitle('Are there any authors from \nIndigenous Peoples or Like-minded \nLocal Communities?') +geom_text(aes(x = c(1.3,1,1.2), y = Lbl2$count/2+ c(0, cumsum(Lbl2$count)[-length(Lbl2$count)]),label = Lbl2$Label), size=5) + coord_polar("y", start = 0,direction = 1)
ggsave(p51pie,file = 'output/T3-Q51.pdf', width = 5, height = 3)

#5.2 -5.2: pie&bar
legend52 <- data.frame(code=c('Q5.2_1', 'Q5.2_2', 'Q5.2_3', 'Q5.2_NA', 'Q5.2_Other'),
                       txt=c('generally demonstrating and expressing deep respect to the land, sea, or their natural surroundings; manifestation of spiritual connection to the land showing intimate interaction associated with the land, sea, lakes or rivers; spiritual beings in the landscape; sacred sites.','identifying placed-based community ceremonies, rituals or gatherings that are linked to the terrestrial or marine landscape.','respecting those ceremonies, rituals or gatherings','does not assess this','Other'),
                       keywords = c('generally demonstrating and expressing deep respect','identifying placed-based community ceremonies, rituals or gatherings','respecting those ceremonies, rituals or gatherings','does not assess this','Other'))
legend53 <- data.frame(code=c('Q5.3_1', 'Q5.3_2', 'Q5.3_3', 'Q5.3_4','Q5.3_NA', 'Q5.3_Other'),
                       txt=c('assessing overall protection and preservation of sites (mountains, rivers, landscapes, landforms, forests, etc.) for their sacred meaning, or cultural or historical significance;',
                             'assessing whether actions/practices where natural resources use is compatible with and follows ancestral teachings about living in harmony with nature and respect for the land',
                             'Identifying  non-over-exploitative production systems out of a sense of responsibilities towards future generations',
                             'considering actions/practices to renew the sense of interconnection with terrestrial or marine surroundings and awareness about responsibility and care',
                             'does not assess this','Other'),
                       keywords = c('assessing overall protection','assessing whether actions/practices where natural resources use','Identifying non-over-exploitative production systems','considering actions/practices to renew the sense of interconnection','does not assess this','Other'))
legend54 <- data.frame(code=c('Q5.4_1', 'Q5.4_2', 'Q5.4_3', 'Q5.4_4','Q5.4_NA', 'Q5.4_Other'),
                       txt=c('assessing equitable sharing use of seasonal resources',
                             'considering the satisfaction of community members’ needs, particularly women, elders and children.',
                             'assessing whether the community shares teachings and knowledge on how to live sustainably with all elements of nature',
                             'assessing the community internal practices of reciprocity (communal work or sharing of products); equitable trade with other communities.','does not assess this','Other'),
                       keywords = c('assessing equitable sharing use of seasonal resources','considering the satisfaction of community members','assessing whether the community shares teachings and knowledge','assessing the community internal practices of reciprocity','does not assess this','Other'))
legend55 <- data.frame(code=c('Q5.5_1', 'Q5.5_2', 'Q5.5_3', 'Q5.5_4','Q5.5_NA', 'Q5.5_Other'),
                       txt=c('identifying and assessing kinship relationships with animal through time identifying and assessing kinship relationships with animals’ habitat and other  plant species',
                             'identifying some plants as sacred',
                             'assessing the reluctance and discouragement of using plants, animals, mountains, rivers, and other non-living entities in nature for individualistic gains',
                             'assessing the understanding of landforms as entitled to personhood or legal rights','does not assess this','Other'),
                       keywords = c('identifying and assessing kinship relationships with animal','identifying some plants as sacred','assessing the reluctance and discouragement of using plants, animals, mountains','assessing the understanding of landforms as entitled to personhood','does not assess this','Other'))
legend56 <- data.frame(code=c('Q5.6_1', 'Q5.6_2', 'Q5.6_3', 'Q5.6_4','Q5.6_5','Q5.6_6','Q5.6_NA', 'Q5.6_Other'),
                       txt=c('assessing the application of ancestral law, teachings, customs and uses',
                             'assessing the acknowledgement and inclusion of people’s beliefs, spirituality, and ceremonial practices',
                             'assessing the community governance and community protocols or regulations','assessing the implementation of the FPIC',
                             'accounting for the intervention of Indigenous spiritual authorities and use of Indigenous language',
                             'including the Indigenous authors’ self-positioning or elders and knowledge-keepers authorship','does not assess this','Other'),
                       keywords = c('assessing the application of ancestral law, teachings, customs and uses','assessing the acknowledgement and inclusion of people','assessing the community governance and community protocols or regulations','assessing the implementation of the FPIC','accounting for the intervention of Indigenous spiritual authorities','including the Indigenous authors','does not assess this','Other'))
l<-data.frame(Q = c('5.2','5.3','5.4','5.5','5.6'),
        sbtitle = c('Respect towards nature is assessed by:', 'Responsibility or care for the land is assessed by:','Kinship/communality with other people is assessed by:','Kinship/communality with non-human entities \n is assessed by:','Values of self-determination and ancestral law \n is assessed by:'),
        title = c('Assessment of respect towards nature.', 'Assessment of responsibility or care for the land.','Assessment of kinship/communality with other people.','Assessment of inship/communality with non-human entities.','Assessment of values of self-determination and ancestral law.'))
legendlist<-list(legend52,legend53,legend54,legend55,legend56)
Panes<-list()
for(i in 1:nrow(l)){
  legend <- legendlist[[i]]
  sbst<-s3_single[,c('paperID','MF1.key','MF2.key','MF3.key','MF4.key',as.character(l[i,'Q']))]
for(j in 1:(nrow(legend)-1)){
  sbst[,as.character(l[i,'Q'])] <- gsub("â€™", "’", sbst[,as.character(l[i,'Q'])] )
  sbst[,as.character(l[i,'Q'])] <- gsub("&", "and", sbst[,as.character(l[i,'Q'])] )
  A <-  str_detect(sbst[,as.character(l[i,'Q'])], pattern = fixed(as.character(legend[j,'txt'])))
  sbst[,as.character(l[i,'Q'])] <- str_replace(sbst[,as.character(l[i,'Q'])], pattern = fixed(as.character(legend[j,'txt'])),"")
  sbst <- cbind(sbst, A)
  colnames(sbst)[ncol(sbst)] <- as.character(legend[j,'code'])
}
  Otheridx <- (str_length(gsub("[;, ]","",sbst[,as.character(l[i,'Q'])]))>1)
  sbst <- cbind(sbst, Otheridx)
  colnames(sbst)[ncol(sbst)] <- as.character(legend[nrow(legend),'code'])
#from wide to long format
sbst %>%
  gather(question, value, -paperID,-MF1.key,-MF2.key,-MF3.key,-MF4.key,-as.character(l[i,'Q'])) -> sbst_long

pie <- ggplot() +
  geom_bar(aes(x = factor(1), fill = s3_single[,as.character(l[i,'Q'])]=='does not assess this'),width = 1) +
  blank_theme + theme(axis.text.x=element_blank(), axis.text.y = element_blank())+ scale_fill_manual(name = '',breaks = c(TRUE, FALSE),labels = c('Not assessed','It is assessed'), values  = c('red', IPbeslightgreen)) +
  xlab('') + ylab('') + coord_polar("y", start = 0,direction = 1) + theme(legend.position='bottom' )
#make ordered bar charts
cnt<-aggregate(1*(sbst_long$value),by = list(sbst_long$question), FUN=function(x)sum(x))
sbst_long$question<-factor(sbst_long$question, levels= cnt[order(cnt$x, decreasing = FALSE),'Group.1'])

bar <- ggplot(sbst_long[!(str_detect(sbst_long$question,regex('_NA'))),]) + geom_bar(alpha=1,size=1,color = IPbeslightgreen, fill = IPbeslightgreen,aes(x=question, y=1*value), stat = "identity") + coord_flip() + theme_bw() + scale_x_discrete(breaks = legend$code, labels = str_wrap(as.character(legend$txt),50)) + ylab('Number of papers') + xlab('') + ggtitle('',subtitle=l[i,'sbtitle'])
Panes[[i]] <- grid.arrange(pie,bar,
             ncol=2, nrow=1, layout_matrix = rbind(c(1,2)), widths=c(1.7,4),
             top = textGrob(l[i,'title'],gp=gpar(fontsize=20,font=3)))
ggsave(Panes[[i]], filename=sprintf('output/T3-Q%s.pdf',l[i,'Q']), width= 10, height = 4)
}

#How many ILK aspects are dealt with in the paper?
A <- rowSums(cbind(!(str_detect(s3_single$`5.2`,regex('does not assess this'))),
                     !(str_detect(s3_single$`5.3`,regex('does not assess this'))),
                       !(str_detect(s3_single$`5.4`,regex('does not assess this'))),
                         !(str_detect(s3_single$`5.5`,regex('does not assess this'))),
                           !(str_detect(s3_single$`5.6`,regex('does not assess this')))))
Pane1 <- ggplot() + geom_bar(aes(x=as.factor(A), fill = IPbesdarkgreen)) + theme_minimal() +  xlab('Number of ILK aspects addressed') + ylab('Number of papers') + ggtitle('Number of ILK aspect adressed per paper') + scale_fill_identity()

Pane1 <- ggplotGrob(Pane1)
p51pie <- ggplotGrob(p51pie)
gt <- gtable(widths = unit(c(1, 2), "null"), heights = unit(c(.2, 1, 1), "null"))
gt <- gtable_add_grob(gt, Pane1, t = 1, b = 3, l = 1, r = 2)
gt <- gtable_add_grob(gt, p51pie, t = 2, l = 2.8)
#grid.draw(gt)
ggsave(gt, filename='output/T3_Q51PieBar.pdf', width= 10, height = 4)
dev.off()


Theme5 <- grid.arrange(gt, Panes[[1]], Panes[[2]], Panes[[3]], Panes[[4]], Panes[[5]],
                       nrow=3, heights=c(4,4,4),widths=c(7,7),layout_matrix = rbind(c(1,2),c(3,4),c(5,6)))
ggsave(Theme5,filename='output/Theme5.pdf',width=20, height=12)

