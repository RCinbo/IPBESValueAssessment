#this script aims to determine which method families are represented in the applications, based on the variables
#the scripts creates a matrix with paper ID as lines and 4 columns:  MF1, MF2, MF3, MF4; and then proceeds to fill in 0 (NOT this MF), 1 (SURELY this MF) or blank.
#This procedure is performed three times, based on different variables, and then the three matrices are merged into one,
#following the logic that empty+0 = 0 ; empty +1= 1; but O + 1 sends a warning.

#d1 matrix is based on Q 2.3 and Q 2.5
#d2 matrix is based on Q 3.1
#d3 matrix is based on Q 6.2 and 6.3
#d4 is the combined result


#d1 logic
#note that d1 determines MF1, MF4 and "not MF2"

#"2.3 - The application USES data COLLECTED at  the biophysical scale of (multiple possible)"
#option "irrelevant / no biophysical data used" => IF YES MF1=0

#"2.5 - The application uses data COLLECTED at the social scale of (multiple possible)"
#option "irrelevant / no values are collected from people" => MF2=0

#d2 logic
#note that d2 determines MF2 and MF3 and also check MF1

#"3.1 - Elicitation process: through what process were  values collected?"
# options "Obtaining information from transactions in markets"
#OR "Observing individual practices/behaviors"
#OR "Observing group practices/behaviors"
#=> MF3=1

#options "Obtaining information by observing biological aspects in real-time (e.g. recording bee visitation of flowers, counting birds, measuring biomass of trees)"
#OR "Obtaining information by collecting measurement data over a period of time (e.g. weather data, sediments, nutrients etc. )"
#OR "Obtaining information about biophysical indicators using secondary data (expert estimates, land use maps, satellite images, species Atlas data etc)"
#=> MF1=1

#IF NOT one of the above AND
#options "Individuals written responses to questions" OR
#"Individual responses face-to-face, phone or similar  responses" OR
#"Individual responses following a group discussion" OR
#"Group responses to the valuation question"
#=> MF2=1

#d3 logic

#"6.2 - The application assesses the preferences (or importance) that humans assign to nature and biodiversity in terms of (multiple possible)"
#option "Application does not assess preferences of humans to nature" => MF2 = 0 and MF3 = 0
#option "Spending or expenditure (in monetary or other forms of resources) to maintain (or increase) aspects of nature and biodiversity (or to avoid losses)"
#=> MF3 = 1
# option "Hypothetical willingness to give up resources (monetary or other forms) to maintain (or increase) aspects of nature and biodiversity"
#OR "Hypothetical compensation (monetary or other forms) to give up access to nature or management rights of natural areas"
#OR "Scores of relative importance to people of nature’s contributions to people (e. g. allocation of points to /or ranking of different aspects of nature or different places)"
#OR "Dialogues with communities about the importance of different aspects of nature and biodiversity"
#=> MF2 = 1

#"6.3 - The application assesses the costs to protect nature & biodiversity for its own sake or to maintain nature’s contributions to people in the form of (multiple possible)"
#option "What has actually been spent in the past to protect nature and biodiversity or maintain nature’s contribution to people [This only applies if the investment was done in the past]"
#=> MF3 = 1
#option "The costs and the benefits from past projects to protect nature and biodiversity or maintain nature’s contribution to people [This only applies if the project was in the past]"
#=> MF3 = 1

#first run Consistency_Applicationsurvey to have s3

dim(s3)
colnames(s3)

d=s3[,c(1,2,16,18,34,57,58)]
d1=matrix(,dim(s3)[1],3)
colnames(d1)=c("MF1", "MF2","MF3")
rownames(d1)=s3$paperID
d2=d1
d3=d1
d4=d1

for (i in 1:dim(s3)[1]){
  id=s3$paperID[i]
  d1[i,1]=ifelse(d[i,3]=="irrelevant / no biophysical data used",0,NA)
  d1[i,2]=ifelse(d[i,4]=="irrelevant / no values are collected from people",0,NA)
  d1[i,3]=ifelse(d[i,4]=="irrelevant / no values are collected from people",0,NA)
  d2[i,1]=ifelse(d[i,5]=="irrelevant / no values are collected from people",0,NA)
}
d1

set1=grep("Obtaining information from transactions in markets", d[,5])
set2=grep("Observing individual practices", d[,5])
set3=grep("Observing group practices", d[,5])
setA=c(set1,set2,set3)
for (i in setA){
  d2[i,3]=1
}
setA

set1=grep("Obtaining information by observing biological aspects in", d[,5])
set2=grep("Obtaining information by collecting measurement data over", d[,5])
set3=grep("Obtaining information about biophysical indicators using", d[,5])
setB=c(set1,set2,set3)
for (i in setB){
  d2[i,1]=1
}
setB

set1=grep("Individuals written responses to questions", d[,5])
set2=grep("Individual responses", d[,5])
set3=grep("Group responses", d[,5])
setC=as.numeric(levels(as.factor(c(set1,set2,set3))))
for (i in setC){
  d2[i,2]=1
}
for (i in grep("1",d2[,1])){
  d2[i,2]=NA
}
for (i in grep("1",d2[,3])){
  d2[i,2]=NA
}

d2


set2=grep("Application does not assess preferences of humans to nature", d[,6])
for (i in set2){
  d3[i,2]=0
  d3[i,3]=0
}

set1=grep("Spending or expenditure ", d[,6])
for (i in set1){
  d3[i,3]=1

}

set1=grep("Hypothetical willingness to give up resources", d[,6])
set2=grep("Hypothetical compensation", d[,6])
set3=grep("Scores of relative importance to people ", d[,6])
set4=grep("Dialogues with communities about", d[,6])
setA=c(set1,set2,set3,set4)
for (i in setA){
  d3[i,2]=1
}
setA

set1=grep("What has actually been spent in the past to protect nature and biodiversity or", d[,7])
set2=grep("The costs and the benefits from past projects to protect nature and biodiversity or maintain", d[,7])
setB=c(set1,set2)
for (i in setB){
  d3[i,3]=1
}
setB

for (i in 1:dim(d3)[1]){
  d3[i,1]=ifelse(d3[i,2]==0,ifelse(d3[i,3]==0,1,NA),NA)
}

d3


#internal consistency check and final determination

MF1_check=cbind(d1[,1],d2[,1],d3[,1])
colnames(MF1_check)=c("2.3","3.1","6.2")
MF1_check

MF1_list=matrix("",dim(s3)[1],1)
rownames(MF1_list)=s3$paperID
for(i in 1:dim(MF1_list)[1]) {
  MF1_list[i,1]=ifelse(0 %in% MF1_check[i,],0, MF1_list[i,1])
  MF1_list[i,1]=ifelse(1 %in% MF1_check[i,],1, MF1_list[i,1])
  #MF1_list[i,1]=ifelse(0 %in% MF1_check[i,] & 1 %in% MF1_check[i,],"warning", MF1_list[i,1])
}
MF1_list

MF2_check=cbind(d1[,2],d2[,2],d3[,2])
colnames(MF2_check)=c("2.5", "3.1", "6.3")
MF2_check

MF2_list=matrix("",dim(s3)[1],1)
rownames(MF2_list)=s3$paperID
for(i in 1:dim(MF2_list)[1]) {
  MF2_list[i,1]=ifelse(0 %in% MF2_check[i,],0, MF2_list[i,1])
  MF2_list[i,1]=ifelse(1 %in% MF2_check[i,],1, MF2_list[i,1])
  #MF2_list[i,1]=ifelse(0 %in% MF2_check[i,] & 1 %in% MF2_check[i,],"warning", MF2_list[i,1])
}
MF2_list

MF3_check=cbind(d2[,3],d3[,3])
colnames(MF3_check)=c("3.1", "6.2&3")
MF3_check

MF3_list=matrix("",dim(s3)[1],1)
rownames(MF3_list)=s3$paperID
for(i in 1:dim(MF3_list)[1]) {
  MF3_list[i,1]=ifelse(0 %in% MF3_check[i,],0, MF3_list[i,1])
  MF3_list[i,1]=ifelse(1 %in% MF3_check[i,],1, MF3_list[i,1])
  #MF3_list[i,1]=ifelse(0 %in% MF3_check[i,] & 1 %in% MF3_check[i,],"warning", MF3_list[i,1])
}
MF3_list

MFtable=cbind(s3[,c(1:4)],MF1_list,MF2_list, MF3_list)
colnames(MFtable)=c("paperID","rater", "first_auth","appl_ID","MF1","MF2","MF3")
write.csv(MFtable, file="C:/local backups/IPBES VA/MFtable26_07_20.csv")
