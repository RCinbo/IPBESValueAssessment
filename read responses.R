
#STEP 2 paper screening survey
Step2_train="https://docs.google.com/spreadsheets/d/e/2PACX-1vQsNrs-GYeeDGIsr3YWZJqpXvcX9MKb44ufaOd9WDgWf5GLd0UR-8vcDstY71-aSvUYthHvQrFyD5tQ/pub?gid=27940269&single=true&output=csv"
St2tr=read.csv(url(Step2_train))
colnames(St2tr)


# summary of the table 
summary(St2tr)

# Notes on the data structure & values 
# Column 5: should be all yes or no? There's one blank (""). 
# Column 8: "" should be "No"?(17) 
# Column 9: "" should be "One" or the other way around (17)  
# Column 11, 12 and 13: there's one "" 
# Column 19: "" should be "No"?(81) 
# Column 20: "" should be "1"?(92) and "two" should be "2" (1) 
# Column 21: "" should be "No"?(81) 


# A column with multiple data points  
shortnames = St2tr$X10..please.provide.short.names.for.the.applications...A..name......B..name......C..name...Use.CLEAR.and.SENSIBLE.names.which.you.will.reuse.in.the.next.survey.and.allow.finding.it.in.the.paper..Use.at.least.one.word..
table(shortnames)
# which used "//", "/", ";", "," to seperate data points and ":" to split data name and the value 

library(stringr) # parsing strings in R 

shortnames_split_by_item = str_split(shortnames, pattern = "(,|;|//|/)") # regular expression (,|;|\/\/|\/)
shortnames_split_by_item_and_name = lapply(shortnames_split_by_item, FUN = function(x) str_split(x, pattern = ":"))
shortnames_count = sapply(shortnames_split_by_item_and_name, FUN = length) # count the maximum items 
shortnames_max_item = max(shortnames_count) # is 3 as of 25 Sep 2020 
shortnames_df = data.frame(matrix(nrow = length(shortnames), ncol = shortnames_max_item))
colnames(shortnames_df) = LETTERS[1:shortnames_max_item]

# fill each row one by one 
for (idx in 1:length(shortnames)) { 
  shorname_tmp = shortnames_split_by_item_and_name[[idx]]
  colcnt = length(shorname_tmp) # how many items does a row have? 
  shortnames_df[idx,1:colcnt] = sapply(shorname_tmp, FUN = function(x) str_trim(x[2])) # and trim the strings
}

str(shortnames_df)
table(unlist(shortnames_df)) # might want to decapitalize the strings (e.g. SolVES and SOLVES) 
library(rjson) # table to json 

shortnames_json = rjson::toJSON(shortnames_df) # Convert to a JSON string (can be stored in CSV or Excel columns)
shortnames_json
shortnames_df_inversely_fromjson = rjson::fromJSON(shortnames_json) # and converts back from it 
shortnames_df_reconstructed = data.frame(shortnames_df_inversely_fromjson)

# if wants to convert a single row 
shorname_example_row_json = rjson::toJSON(shortnames_df[92,])
rjson::fromJSON(shorname_example_row_json) # and converts back from it 

# or 
# 
# {
#   "A": "SOLVES",
#   "B": "ARIES",
#   "C": "Hotspot Analysis"
# }

# reF: http://json2table.com/#

#clean matrix step2
paperID=as.data.frame(St2tr[,2])
Rater=as.data.frame(St2tr[,15])
scores=as.data.frame(St2tr[,c(4:7,9,11:14)])
s2=cbind(paperID,Rater,scores)
colnames(s2)=c("paperID","Rater","Valuation","Application","Policy theme","challenges","appl_nr","ILKauth","locauth","nonacauth","fund")
#make test matrices for same papers
list_s2=matrix(0,length(levels(as.factor(s2$paperID))),1)
l=as.list(list_s2)
for(i in 1: length(levels(as.factor(s2$paperID)))){
  a=assign(paste("s2.", levels(as.factor(s2$paperID))[i],sep=""),s2[s2$paperID==i,])
  l[[i]]=a
  list_s2[i,1]= paste("s2.",levels(as.factor(s2$paperID))[i],sep="")
}


#Topic 1
T1_train="https://docs.google.com/spreadsheets/d/e/2PACX-1vTVJ3jLv_DRMNRb8O6MGiv6QYSYHDqFhhQwl81P5vJnzBT1ThbhX4z_FtHcevd0rj6HZj_r5vQYeoIZ/pub?gid=1885120207&single=true&output=csv"
T1tr=read.csv(url(T1_train))
colnames(T1tr)
#clean matrix topic1
paperID=as.data.frame(T1tr[,2])
apl=as.data.frame(T1tr[,4])
  # clean case format
  aplic=matrix(0,dim(apl)[1],1)
  for (i in 1:dim(apl)[1]){
    aplic[i]=toupper(substr(apl[i,],1,1))
  }
Rater=as.data.frame(T1tr[,50])
scores=as.data.frame(T1tr[,c(5,6,7,9)])
T1=cbind(paperID,aplic,Rater,scores)
colnames(T1)=c("paperID","Application","Rater","multiple","new","which","data_type")
#make test matrices for same applications
#define nr. of obs.
  z=0
  for(i in 1: length(levels(as.factor(T1$paperID)))){
   for (j in 1: length(levels(as.factor(as.matrix(T1$Application[T1$paperID==i]))))){
    z=z+1
  }}
#make list of tables per application
list_T1=matrix(0,z,1)
l1=as.list(list_T1)
q=1
for(i in 1: length(levels(as.factor(T1$paperID)))) {
  a=assign(paste("T1.", levels(as.factor(T1$paperID))[i],sep=""),T1[T1$paperID==i,])
  for (j in 1: length(levels(as.factor(as.matrix(a$Application))))) {
      b=assign(paste("T1.", levels(as.factor(T1$paperID))[i],levels(as.factor(as.matrix(a$Application)))[j],sep=""),T1[T1$paperID==i & T1$Application==levels(as.factor(as.matrix(a$Application)))[j],])
      list_T1[q,1]= paste("T1.", levels(as.factor(T1$paperID))[i], levels(as.factor(as.matrix(a$Application)))[j],sep="")
      l1[[q]]=b
      q=q+1
}}



l1
l


#list_s2[i,1]= paste("s2.",levels(as.factor(s2$paperID))[i],sep="")
#a=assign(paste("T1.", levels(as.factor(T1$paperID))[i],sep=""),T1[T1$paperID==i,])
#list_a=matrix(0,length(levels(as.factor(T1$Application))),1)
#la=as.list(list_a)



T2_train="https://docs.google.com/spreadsheets/d/e/2PACX-1vRrJG_dtzCWOTYbJ8mSRsGAB8sLMbcAFrPZqD_14ONfkbyB_rkmPPZIq4RF3Ys_cVoUXluVYC0dTaYw/pub?gid=816446898&single=true&output=csv"
T2tr=read.csv(url(T2_train))
colnames(T2tr)

T3_train="https://docs.google.com/spreadsheets/d/e/2PACX-1vRrJG_dtzCWOTYbJ8mSRsGAB8sLMbcAFrPZqD_14ONfkbyB_rkmPPZIq4RF3Ys_cVoUXluVYC0dTaYw/pub?gid=816446898&single=true&output=csv"
T3tr=read.csv(url(T2_train))
colnames(T3tr)
