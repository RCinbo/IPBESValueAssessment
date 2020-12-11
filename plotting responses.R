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
#s3_single_WithDummies.Rdata is built in the starting.R script --> first run this and get a copy of the 's3_single' and 'L' dataframe on your local computer.
load('s3_single_WithDummies.Rdata')
MFkeycol <- c('MF1.key','MF2.key','MF3.key','MF4.key')#the columns in s3_single to use for method families
MFSODcol <- c('MF1.SOD','MF2.SOD','MF3.SOD','MF4.SOD')#the columns in s3_single to use
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
MFLabels <- c('Nature-based valuation','Statement-based valuation','Behaviour-based valuation','Integrated valuation', 'AllArticles') # change the names here if needed.
names(MFLabels) <- c('MF1','MF2','MF3','MF4', 'All articles')

blank_theme <- theme_minimal()+
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.border = element_blank(),
    panel.grid=element_blank(),
    axis.ticks = element_blank(),
    plot.title=element_text(size=14, face="bold")
  )
minimal_theme_bars <- theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

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
  ggsave(plotCertainty, filename = 'output/CertaintyLikertPlot.jpg', width= 8, height = 5)



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
ggsave(p, filename='output/T3-Q217-218.jpf', width= 10, height = 7)

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
ggsave(Theme4,filename='output/Theme4.pdf',width=22, height=15)
ggsave(Theme4,filename='output/Theme4.jpg',width=22, height=15)




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




########-----Data Overview 6 (theme 7)- Valuation for ecological sustainability-----########
l<-data.frame(Q = c('7.1','7.2','7.3',
                    '5.2','5.3','5.4','5.5','5.6',
                    '8.9','8.10','8.12','8.13',
                    '8.3','8.4','8.5','8.6','8.7','8.8',
                    '4.1','4.2','4.3','4.4','4.5',
                    '8.1','8.2','8.14',
                    '2.15', '2.16',
                    '6.1', '6.2', '6.3', '6.5'
                    ),
              title = c('Was ECOLOGICAL CONDITION assessed & how?','Was ECOSYSTEM CAPACITY assessed and how?','Was SUSTAINABLE USE/MANAGEMENT  of ecosystems assessed and how?',
                        'The application identifies/assesses respect towards nature by...', 'The application assesses responsibility or care for the land by...','The application assesses kinship/communality with other people by...', 'The application assesses kinship/communality with non-human entities by...','The application assesses values of self-determination and ancestral law, by...',
                        'Recognition: knowledge types','Recognition: broad values','Recognition: value types','Recognition: value dimensions',
                        'Transparency level','Stakeholder representation','Stakeholder identification','Stakeholder inclusion actions','Power','Participation', 'Replicability of the results','Consistency of the results','Prcision of the results','Internal validity of the results','External validity of the results',
                        'Was INTRA-GENERATIONAL JUSTICE assessed & how?','Was INTER-GENERATIONAL JUSTICE assessed and how?','Who is part of the community of justice?',
                        'The application assesses multiple values and brings them together into an overall value or importance by:','The application mentions interests and conflicts:',
                        'The application assesses human well-being using one of the following indicators (multiple possible):', 'The application assesses the preferences (or importance) that humans assign to nature and biodiversity in terms of (multiple possible):','The application assesses the costs to protect nature & biodiversity for its
own sake or to maintain nature’s contributions to people in the form of (multiple possible):','If the application assesses human well-being (specified above), is the well-being indicator assessed for different socio-demographic groups?'),
              short = c('EcologicalCondition','EcosystemCapacity','SustainableUseManagement',
                        'NatureRespect','ResponsibilityLand','KinshipPeople','KinshipNonHuman','SelfDeterminationAncestralLaw',
                        'RecogKnowledgeTypes', 'RecogBroadValues','RecogValueTypes','RecogValueDimensions',
                        'Transparency','StkhldrRepr','StkhldrIdent','StkhldrIncl','Power','Participation',
                        'Replicability','Consistency','Precision','IntValidity','ExtValidity',
                        'IntraJustice','InterGenJustice', 'CommunityJustice',
                        'MultipleValues','InterestsConflicts',
                        'HumanWellBeing', 'AssessBiodiversity','CostsProtect','WellBeingIndicator'),
              ordered = c(TRUE,TRUE,TRUE,
                          TRUE,TRUE,TRUE,TRUE,TRUE,
                          TRUE,TRUE,TRUE,TRUE,
                          FALSE, FALSE,TRUE, TRUE,TRUE,FALSE,
                          TRUE,TRUE,TRUE,TRUE,TRUE,
                          TRUE,TRUE,TRUE,
                          TRUE,FALSE,
                          TRUE,TRUE,TRUE,TRUE))
legendlist<-list(L[str_detect(L$Question,'7.1'),], L[str_detect(L$Question,'7.2'),], L[str_detect(L$Question,'7.3'),],
                 L[str_detect(L$Question,'5.2'),], L[str_detect(L$Question,'5.3'),], L[str_detect(L$Question,'5.4'),], L[str_detect(L$Question,'5.5'),], L[str_detect(L$Question,'5.6'),],
                 L[str_detect(L$Question,'8.9'),], L[str_detect(L$Question,'8.10'),], L[str_detect(L$Question,'8.12'),], L[str_detect(L$Question,'8.13'),],
                 L[str_detect(L$Question,'8.3'),], L[str_detect(L$Question,'8.4'),], L[str_detect(L$Question,'8.5'),], L[str_detect(L$Question,'8.6'),], L[str_detect(L$Question,'8.7'),], L[str_detect(L$Question,'8.8'),],
                 L[str_detect(L$Question,'4.1'),], L[str_detect(L$Question,'4.2'),], L[str_detect(L$Question,'4.3'),], L[str_detect(L$Question,'4.4'),], L[str_detect(L$Question,'4.5'),],
                 L[str_detect(L$code,'8.1_'),], L[str_detect(L$Question,'8.2'),], L[str_detect(L$Question,'8.14'),],
                 L[str_detect(L$code,'2.15'),], L[str_detect(L$Question,'2.16'),],
                 L[str_detect(L$Question,'6.1'),],L[str_detect(L$code,'6.2'),], L[str_detect(L$Question,'6.3'),], L[str_detect(L$Question,'6.5'),])

Panes <- list()
gt <- list()
pfam<- list()
MFkey<-T #set to true if you want figures by keyword-base method family
MFSOD<-T #set to true if you want figures by SOD method family
for(i in 1:nrow(l)){
  legend <- legendlist[[i]]
  sbst <- s3_single[,c('paperID',MFkeycol,MFSODcol,as.character(legend$code))]

  #from wide to long format
  sbst %>%
    gather(question, value, -paperID,-as.character(MFkeycol),-as.character(MFSODcol)) -> sbst_long
  if(sum(str_detect(colnames(sbst),'_unclear'))>0){
    fillCol <- as.factor(1*(sbst[,str_detect(colnames(sbst),'_unclear')]==1)+2*(pmax(sbst[,str_detect(colnames(sbst),'_none|_irrelevant')]) == 1))
    fillCol[fillCol=='3'] <-'2'
    pie <- ggplot() +
      geom_bar(aes(x = factor(1), fill = fillCol), width = 1, color='black') + blank_theme + theme(axis.text.x=element_blank(), axis.text.y = element_blank())+ scale_fill_manual(name = '',breaks = c('0','1','2'),labels = c('It is assessed','Unclear','It is assessed'), values  = c('black', 'grey','white')) +
      xlab('') + ylab('') + coord_polar("y", start = 0,direction = 1) + theme(legend.position='bottom' )
  }else if (sum(str_detect(colnames(sbst),'_none|_irrelevant'))>0){
    pie <- ggplot() +
      geom_bar(aes(x = factor(1), fill = (pmax(sbst[,str_detect(colnames(sbst),'_none|_irrelevant')]) == 1)), width = 1, color='black') +
      blank_theme + theme(axis.text.x=element_blank(), axis.text.y = element_blank())+ scale_fill_manual(name = '',breaks = c(TRUE, FALSE),labels = c('Not assessed','It is assessed'), values  = c('white', 'grey')) +
      xlab('') + ylab('') + coord_polar("y", start = 0,direction = 1) + theme(legend.position='bottom' )
  }

  if(l[i,'ordered']){#make ordered bar charts
    cnt<-aggregate(1*(sbst_long$value),by = list(sbst_long$question), FUN=function(x)sum(x))
    sbst_long$question<-factor(sbst_long$question, levels= cnt[order(cnt$x, decreasing = FALSE),'Group.1'])
    selection <- !(str_detect(sbst_long$question,regex('_none|_irrelevant|_unclear')))
}else{selection <- !(str_detect(sbst_long$question,regex('_irrelevant|_unclear')))}
  bar <- ggplot(sbst_long[selection,]) + geom_bar(alpha=1,size=1,color = 'grey', fill = 'grey',aes(x=question, y=1*value), stat = "identity") + coord_flip() + minimal_theme_bars + scale_x_discrete(breaks = legend$code, labels = str_wrap(as.character(legend$txt),50)) + ylab('Number of papers') + xlab('') + ggtitle(l[i,'title'])

  # Panes[[i]] <- grid.arrange(pie,bar,
  #                            ncol=2, nrow=1, layout_matrix = rbind(c(1,2)), widths=c(1.7,4))
  # ggsave(Panes[[i]], filename=sprintf('output/Q%s%sPieBar2.pdf',l[i,'Q'],l[i,'short']), width= 10, height = 4)
  library(cowplot)
  gt[[i]] <- ggdraw() +
    draw_plot(bar) +
    draw_plot(pie, x = 0.65, y = 0.09, width = .3, height = .4)
  ggsave(pie, filename=sprintf('output/Q%s%sPie.pdf',l[i,'Q'],l[i,'short']), width= 4, height = 4)
  ggsave(bar, filename=sprintf('output/Q%s%sBar.pdf',l[i,'Q'],l[i,'short']), width= 10, height = 4)
  ggsave(gt[[i]], filename=sprintf('output/Q%s%sPieBar.pdf',l[i,'Q'],l[i,'short']), width= 10, height = 4)
  ggsave(pie, filename=sprintf('output/Q%s%sPie.jpg',l[i,'Q'],l[i,'short']), width= 4, height = 4)
  ggsave(bar, filename=sprintf('output/Q%s%sBar.jpg',l[i,'Q'],l[i,'short']), width= 10, height = 4)
  ggsave(gt[[i]], filename=sprintf('output/Q%s%sPieBar.jpg',l[i,'Q'],l[i,'short']), width= 10, height = 4)

  familysummary <- data.frame(MF = c('MF1','MF1','MF2','MF2','MF3','MF3','MF4','MF4'),
                                Answer = c('Yes','No','Yes','No','Yes','No','Yes','No'),
                                N = NA,
                                Percentage=NA)
  if(MFkey){
      familysummary$N <- sapply(1:nrow(familysummary),FUN=function(x) ifelse(
      familysummary$Answer[x] == 'No',
      sum(sbst[,(1+as.numeric(familysummary[x,'MF']))]==1 & pmax(sbst[,str_detect(colnames(sbst),'none|irrelevant')])==1, na.rm=T),
      sum(sbst[,(1+as.numeric(familysummary[x,'MF']))]==1 & pmax(sbst[,str_detect(colnames(sbst),'none|irrelevant')])==0, na.rm=T)))

    familysummary$Percentage <- familysummary$N/sapply(familysummary$MF,FUN = function(x)sum(familysummary[familysummary$MF==x,'N']))

    pfam[[i]] <- ggplot(familysummary) + geom_bar(color='black', aes(x = MF, fill=Answer, y=Percentage*100),stat = 'identity',position='stack') + scale_fill_manual(name = '',breaks = c('Yes','No'), labels=c('It is assessed','Not assessed'),values  = c('grey', 'white')) + ylab('Percentage in the method family') + xlab('Method family') + minimal_theme_bars + scale_x_discrete(labels= MFLabels)
    ggsave(pfam[[i]], filename=sprintf('output/Q%s%sMFkeybars.pdf',l[i,'Q'],l[i,'short']), width= 10, height = 4)
    ggsave(pfam[[i]], filename=sprintf('output/Q%s%sMFkeybars.jpg',l[i,'Q'],l[i,'short']), width= 10, height = 4)
  }
  if(MFSOD){
    familysummary$N <- sapply(1:nrow(familysummary),FUN=function(x) ifelse(
      familysummary$Answer[x] == 'No',
      sum(sbst[pmax(sbst[,str_detect(colnames(sbst),'none|irrelevant')])==1,(5+as.numeric(familysummary[x,'MF']))], na.rm=T),
      sum(sbst[pmax(sbst[,str_detect(colnames(sbst),'none|irrelevant')])==0,(5+as.numeric(familysummary[x,'MF']))], na.rm=T)))

    familysummary$Percentage <- familysummary$N/sapply(familysummary$MF,FUN = function(x)sum(familysummary[familysummary$MF==x,'N']))

    pfam[[i]] <- ggplot(familysummary) + geom_bar(color='black', aes(x = MF, fill=Answer, y=Percentage*100),stat = 'identity',position='stack') + scale_fill_manual(name = '',breaks = c('Yes','No'), labels=c('It is assessed','Not assessed'),values  = c('grey', 'white')) + ylab('Percentage in the method family') + xlab('Method family') + minimal_theme_bars + scale_x_discrete(labels= MFLabels)
    ggsave(pfam[[i]], filename=sprintf('output/Q%s%sMFSODbars.pdf',l[i,'Q'],l[i,'short']), width= 10, height = 4)
    ggsave(pfam[[i]], filename=sprintf('output/Q%s%sMFSODbars.jpg',l[i,'Q'],l[i,'short']), width= 10, height = 4)
  }
}

#########-------------- Theme-based plots--------------------###################
####---Abundancy plots---###
Theme5 <- c('5.2','5.3','5.4','5.5','5.6')
Theme8 <- c('8.9','8.10','8.12','8.13')
Theme4 <- c('4.1','4.2','4.3','4.4','4.5')
Theme8Justice <- c('8.1','8.2','8.14')
themes <- list(theme=list(Theme5,Theme8, Theme4, Theme8Justice), title=c('Number of ILK aspects adressed per paper', 'Number of Recognition aspects adressed per paper', 'Number of reliability aspects adressed per paper', 'Number of Justice aspects adressed per paper'), name = c('Theme5', 'Theme8', 'Theme4', 'Theme8Justice'))

for(i in 1:length(themes$title)){
  cols<-c(str_c('Q',unlist(themes$theme[i]),'_none',sep=""), str_c('Q',unlist(themes$theme[i]),'_irrelevant',sep=""))
  a <- which(cols %in% colnames(s3_single))
  A <- rowSums(cbind(s3_single[,cols[a]])==0)
  p <- ggplot() + geom_bar(aes(x=as.factor(A), fill = 'grey46')) + minimal_theme_bars +  xlab(unlist(themes$title[i])) + ylab('Number of papers') + ggtitle(unlist(themes$title[i])) + scale_fill_identity()
  ggsave(p, filename=sprintf('output/%s_AbundancyPlot.pdf',unlist(themes$name[i])), width= 10, height = 4)
  ggsave(p, filename=sprintf('output/%s_AbundancyPlot.jpg',unlist(themes$name[i])), width= 10, height = 4)
}

#-----Frequency by subquestion in the theme by MF or in general
Theme5 <- c('5.2','5.3','5.4','5.5','5.6')
Theme4 <- c('4.1','4.2','4.3','4.4','4.5')
Theme8Justice <- c('8.1','8.2','8.14')
LabelsTheme5 <- c('Respect towards nature', 'Responsibility or care for the land','Kinship/communality with other people', 'Kinship/communality with non-human entities','Values of self-determination and ancestral law')
LabelsTheme4 <- c('Replicability','Consistency','Precision','Internal Validity','External Validity')
LabelsTheme8Justice <- c('Was INTRA-GENERATIONAL JUSTICE assessed & how?','Was INTER-GENERATIONAL JUSTICE assessed and how?','Who is part of the community of justice?')
themes <- list(theme=list(Theme5, Theme4, Theme8Justice),
               title=c('Which ILK aspects are addressed?', 'Which reliability aspects are adressed?', 'Which Justice aspects are assessed?'),
               name = c('Theme5', 'Theme4', 'Theme8Justice'),
               label = list(LabelsTheme5, LabelsTheme4, LabelsTheme8Justice))
MFkey<-T #set to true if you want figures by keyword-base method family
MFSOD<-T #set to true if you want figures by SOD method family
for(i in 1:length(themes$title)){
  cols<-c(str_c('Q',unlist(themes$theme[i]),'_none',sep=""), str_c('Q',unlist(themes$theme[i]),'_irrelevant',sep=""))
  b <- which(cols %in% colnames(s3_single))
  a <- cbind(cbind(s3_single[,cols[b]])==0, s3_single[,c('paperID', as.character(MFkeycol), as.character(MFSODcol))])#TRUE if it IS assessed
  a %>%
    gather(question, value, -paperID,-as.character(MFkeycol),-as.character(MFSODcol)) -> a_long
  dfl <- a_long[,] %>%
    group_by(question) %>%
    summarise(n = n(),
              assessed = sum(value==TRUE),
              NotAssessed = sum(value==FALSE))
  dfl$AssessedPerc <- dfl$assessed/dfl$n
  idx <- order(unlist(themes$theme[i]))
  p <- ggplot() + geom_bar(data=dfl,stat="identity",aes(x=question, y=AssessedPerc), fill='grey') + minimal_theme_bars +   xlab("") + ylab('Percentage of the papers') + ggtitle(unlist(themes$title[i])) + scale_x_discrete(labels = str_wrap(unlist(themes$label[i])[idx],20)) + scale_y_continuous(labels = scales::percent)

  ggsave(p, filename=sprintf('output/%s_Allsubquestions.pdf',unlist(themes$name[i])), width= 10, height = 4)
  ggsave(p, filename=sprintf('output/%s_Allsubquestions.jpg',unlist(themes$name[i])), width= 10, height = 4)

  if(MFkey){
  a[,-which(colnames(a)%in%as.character(MFSODcol))]%>%
    gather(question, value, -paperID,-as.character(MFkeycol),) %>%
    gather(MF,MFvalue, -paperID, -question, -value)-> a_long
  a_long<-a_long[a_long$MFvalue==1,]
  dfl <- a_long[,] %>%
    group_by(question,MF) %>%
    summarise(n = n(),
              assessed = sum(value==TRUE),
              NotAssessed = sum(value==FALSE))
  dfl$AssessedPerc <- dfl$assessed/dfl$n
  dfl$MF <- str_replace_all(dfl$MF,'.key','')
  pMF <- ggplot() + geom_bar(data=dfl,stat="identity",aes(x=question, y=AssessedPerc), fill='grey') + minimal_theme_bars + theme(axis.text.x = element_text(angle = 45,hjust = 1))+ xlab("") + ylab('Percentage of the papers in the method family') + ggtitle(unlist(themes$title[i])) + facet_wrap(vars(MF),labeller = labeller(MF=MFLabels)) + scale_x_discrete(labels = str_wrap(unlist(themes$label[i])[idx],20)) + scale_y_continuous(labels = scales::percent)
  ggsave(pMF, filename=sprintf('output/%s_AllsubquestionsByMF.pdf',unlist(themes$name[i])),  width= 12, height = 8)
  ggsave(pMF, filename=sprintf('output/%s_AllsubquestionsByMF.jpg',unlist(themes$name[i])), width= 12, height = 8)
  }
  if(MFSOD){
    a[,-which(colnames(a)%in%as.character(MFkeycol))] %>%
      gather(question, value, -paperID,-as.character(MFSODcol),) %>%
      gather(MF,MFvalue, -paperID, -question, -value)-> a_long
    a_long<-a_long[a_long$MFvalue==1,]
    dfl <- a_long[,] %>%
      group_by(question,MF) %>%
      summarise(n = n(),
                assessed = sum(value==TRUE),
                NotAssessed = sum(value==FALSE))
    dfl$AssessedPerc <- dfl$assessed/dfl$n
    dfl$MF <- str_replace_all(dfl$MF,'.SOD','')
    pMF <- ggplot() + geom_bar(data=dfl[!is.na(dfl$MF),],stat="identity",aes(x=question, y=AssessedPerc), fill='grey') + minimal_theme_bars + theme(axis.text.x = element_text(angle = 45,hjust = 1))+ xlab("") + ylab('Percentage of the papers in the method family') + ggtitle(unlist(themes$title[i])) + facet_wrap(vars(MF),labeller = labeller(MF=MFLabels)) + scale_x_discrete(labels = str_wrap(unlist(themes$label[i])[idx],20)) + scale_y_continuous(labels = scales::percent)
    ggsave(pMF, filename=sprintf('output/%s_AllsubquestionsByMF.pdf',unlist(themes$name[i])),  width= 12, height = 8)
    ggsave(pMF, filename=sprintf('output/%s_AllsubquestionsByMF.jpg',unlist(themes$name[i])), width= 12, height = 8)
  }
}

#
# require(openxlsx)
# write.xlsx(list("Q7.1" = M[[1]], "Q7.2" = M[[2]], "Q7.3" = M[[3]]), file = "Theme7_OtherAnswers.xlsx", row.names=T)
#
# Theme7 <- grid.arrange(gt[[1]], gt[[2]], gt[[3]],pfam[[1]],pfam[[2]],pfam[[3]],
#                        nrow=2, heights=c(3,3),widths=c(7,7,7),layout_matrix = rbind(c(1,2,3),c(4,5,6)))
# ggsave(Theme7,filename='output/Theme7.pdf',width=21, height=8)
# ggsave(Theme7,filename='output/Theme7.jpg',width=21, height=8)







########-----Data Overview 4 - Valuation of indigenous peoples' and like-minded local communities's-----########
s3_single$`5.1`<-factor(s3_single$`5.1`, levels=c('none','unclear', 'one or more authors explicitly represented as such'),ordered=TRUE)
Lbl2<-data.frame(Answer = c('one or more authors explicitly represented as such','unclear','none'),count = NA, Perc = NA, Label = NA)
for (j in 1:3){
    Lbl2[j,'count'] <- sum(s3_single$`5.1`==as.character(Lbl2[j,'Answer']))
    Lbl2[j,'Perc'] <- Lbl2[j,'count'] / nrow(s3_single)*100
    Lbl2[j,'Label'] <- sprintf('%1.0f%%', Lbl2[j,'Perc'])
  }
p51pie <- ggplot() +
  geom_bar(aes(x = factor(1), fill = s3_single$`5.1`),width = 1, color='black') +
  blank_theme + theme(axis.text.x=element_blank(), axis.text.y = element_blank())+ scale_fill_manual(name = '',breaks = c('none','one or more authors explicitly represented as such', 'unclear'),labels = c('No','Yes, one or more', 'Unclear'), values  = c('white', 'grey45', 'grey92')) +
  xlab('') + ylab('') + ggtitle('Are there any authors from \nIndigenous Peoples or Like-minded \nLocal Communities?') +geom_text(aes(x = c(1.3,1,1.2), y = Lbl2$count/2+ c(0, cumsum(Lbl2$count)[-length(Lbl2$count)]),label = Lbl2$Label), size=5) + coord_polar("y", start = 0,direction = 1)
ggsave(p51pie,file = 'output/Q51IndigineousAuthorsPie.pdf', width = 4, height = 4)
ggsave(p51pie,file = 'output/Q51IndigineousAuthorsPie.jpg', width = 4, height = 4)


#Alluvial plot MF and IPBES categories
library(ggalluvial)
lMF <- read.xlsx('methods x MF x IPBESclasses SOD.xlsx')
#Only keep the unique SOD methods
a <- sapply(X=unique(lMF$method.name.SOD), FUN=function(x) min(which(lMF$method.name == x)))
A <- lMF[a,c("method.name.SOD",  "MF1.-.Nature.based.valuation","MF2.-.Statement-based","MF3.-.Behaviour.based.valuation","MF4.-.Integrated.valuation" ,"Economic.valuation","socio-cultural.valuation","biophysical.valuation","health.related","ILK.related")]
limit<-1 #we only say the method belong to the metod family/ipbes category if it scores higher than 'limit'
Along<-data.frame(MF=rep(c("Nature based","Statement-based","Behaviour based valuation","Integrated"), each=5),
                  IPBES= rep(c('Economic valuation','socio-cultural valuation','biophysical valuation','health related','ILK related'),4),
                  n=NA)
for (i in 1:nrow(Along)){
Along[i,'n'] <- sum((lMF[,which(str_detect(colnames(lMF),str_replace_all(as.character(Along[i,'IPBES'])," ",".")))]>limit) & (lMF[,which(str_detect(colnames(lMF),str_replace_all(as.character(Along[i,'MF'])," ",".")))]>limit) )
}
p<-ggplot(data = Along, aes(axis1 = MF, axis2 = IPBES, y = n)) + geom_alluvium(aes(fill = MF, colour = MF), alpha = .75, width = 0, reverse = FALSE) +
  guides(fill = FALSE) +
  geom_stratum(width = 1/8, reverse = FALSE) +
  geom_text(stat = "stratum", aes(label = after_stat(stratum)),
            reverse = FALSE, angle=90) +
  scale_x_continuous(breaks = c(1,2), labels = c("Method family", "IPBES category")) + ylab('Nuber of methods') + theme_bw() + theme(legend.position = "none")
ggsave(p,file = "output/MethodsAlluvialPlot.pdf", height = 12, width=8)
ggsave(p,file = "output/MethodsAlluvialPlot.jpg", height = 12, width=8)
