# HL new script test
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
subset<-s3_single[,c('paperID','2.17','2.18')]
for(i in 1:nrow(legend)){
  A <-  str_detect(subset$`2.18`, pattern = as.character(legend[i,'txt']))
  subset <- cbind(subset, A)
  colnames(subset)[ncol(subset)] <- as.character(legend[i,'code'])
}
#from wide to long format
subset %>%
  gather(question, value, -paperID,-`2.17`,-`2.18`) ->subset_long
#labels for the facets
labs <- c(sprintf('Desired uptake, N=%d', sum(subset$`2.17`=='Desired uptake - A potential, expected, or wished for use of outputs, sometimes only cursory')),
          sprintf('Testing use case, N=%d', sum(subset$`2.17`=='Testing use case - Researcher initiated valuation study, reported test of use to enable decision making')),
          sprintf('Actual Use case, N=%d', sum(subset$`2.17`=='Actual use case - Stakeholder commissioned valuation study, reported use to enable decision making')))
names(labs) <- c('Desired uptake - A potential, expected, or wished for use of outputs, sometimes only cursory', 'Testing use case - Researcher initiated valuation study, reported test of use to enable decision making', 'Actual use case - Stakeholder commissioned valuation study, reported use to enable decision making')
#--------making the plot-----#
p <- ggplot(subset_long) + geom_bar(alpha=1,size=1,color = IPbeslightgreen, fill = IPbeslightgreen,aes(x=question, y=1*value, group = `2.17` ), stat = "identity")+ facet_grid(cols = vars(`2.17`), scales = "free", labeller = labeller(`2.17` = labs)) + coord_flip() + theme_bw() + scale_x_discrete(breaks = legend$code, labels = str_wrap(as.character(legend$txt),20)) + ylab('Number of papers') + xlab('Valuation results purpose')
ggsave(p, filename='output/T3-Q217-218.pdf', width= 10, height = 7)
#extract paper ID numbers for testing use case and actual use case and give the purposes that were given for this application
write_xlsx(
    subset[subset$`2.17`!='Desired uptake - A potential, expected, or wished for use of outputs, sometimes only cursory', -3],
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
subset<-s3_single[,c('paperID','4.4','4.5')]
for(i in 1:8){
  A <-  str_detect(subset$`4.4`, pattern = as.character(legend[i,'txt']))
  subset <- cbind(subset, A)
  colnames(subset)[ncol(subset)] <- as.character(legend[i,'code'])
}
for(i in 9:13){
  A <-  str_detect(subset$`4.5`, pattern = as.character(legend[i,'txt']))
  subset <- cbind(subset, A)
  colnames(subset)[ncol(subset)] <- as.character(legend[i,'code'])
}
#from wide to long format
subset[,1:11] %>%
  gather(question, value, -paperID,-`4.4`,-`4.5`) ->subset_long44
subset[,c(1:3,12:16)] %>%
  gather(question, value, -paperID,-`4.4`,-`4.5`) ->subset_long45
# #labels for the facets
# labs <- c(sprintf('Desired uptake, N=%d', sum(subset$`2.17`=='Desired uptake - A potential, expected, or wished for use of outputs, sometimes only cursory')),
#           sprintf('Testing use case, N=%d', sum(subset$`2.17`=='Testing use case - Researcher initiated valuation study, reported test of use to enable decision making')),
#           sprintf('Actual Use case, N=%d', sum(subset$`2.17`=='Actual use case - Stakeholder commissioned valuation study, reported use to enable decision making')))
# names(labs) <- c('Desired uptake - A potential, expected, or wished for use of outputs, sometimes only cursory', 'Testing use case - Researcher initiated valuation study, reported test of use to enable decision making', 'Actual use case - Stakeholder commissioned valuation study, reported use to enable decision making')
p44 <- ggplot(subset_long44) + geom_bar(alpha=1,size=1,color = IPbeslightgreen, fill = IPbeslightgreen, aes(x = question, y=1*value), stat = "identity") + coord_flip() + theme_bw() + scale_x_discrete(breaks = legend[1:8,'code'], labels = str_wrap(as.character(legend[1:8,'txt']),20)) + ylab('Number of papers') + xlab('Internal Validity is assessed by ...')
p45 <- ggplot(subset_long45) + geom_bar(alpha=1,size=1,color = IPbeslightgreen, fill = IPbeslightgreen, aes(x = question, y=1*value), stat = "identity") + coord_flip() + theme_bw() + scale_x_discrete(breaks = legend[9:13,'code'], labels = str_wrap(as.character(legend[9:13,'txt']),20)) + ylab('Number of papers') + xlab('External Validity is assessed by ...')



