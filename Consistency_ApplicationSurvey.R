# test reliability in first test round - step 2 and step 3
# test reliability for all repeated papers (inc/excl first ones) - step 2 and step 3
# extract ONE observation for each paper to add back to the step 2 and 3 table


#STEP2
#(waiting) for corrected script extracting all repeated papers in strating.R)

#make test matrices per paper
n=length(levels(s2$paperID))
m=matrix(0,n,1)
l2=as.list(m)
for(i in 1:n){
  lev=levels(s2$paperID)[i]
  a=assign(paste("s2.",lev,sep=""),s2[s2$paperID==lev,])
  l2[[i]]=a
  m[i,1]= paste("s2.",lev,sep="")
}

#for each paper, list scores for application & valuation: scope and nr of applications
scope=matrix(0,0,6)
colnames(scope)=c("paperID", "firstauth", "valuation", "application", "unclearYN", "applnr", "rater")
n=length(levels(s2$paperID))
for(i in 1:n){
  t=l2[[i]][,c(1,3, 4,5, 6,7,2)]
  scope=as.data.frame(rbind(scope,t))
   }
#rename scales
levels(scope$multiple)=c("NA", "1", "1*")
scope


library(irr)

irr2=matrix(0,length(levels(s2$paperID)),2)
rownames(irr2)=levels(s2$paperID)
colnames(irr2)=c("krip.alpha", "Fleiss.kappa")
for(i in 1:dim(irr2)[1]){
  m=as.matrix(l2[[i]])
  m=m[,2:8]
  mt=t(m)
  k=kripp.alpha(m, method="nominal")
  irr2[i,1]=k$value
  f=kappam.fleiss(mt, exact = FALSE, detail = FALSE)
  irr2[i,2]=f$value
  rownames(irr2)[i]=m[1,2]
}
irr2
s2.20
k
f


##STEP 3


#clean application names and count nr of applications
appnames=matrix(0,0,4)
colnames(appnames)=c("paperID", "firstauth", "applname", "rater")
z=0
n=length(levels(s3$paperID))
for(i in 1:n){
  lev=levels(s3$paperID)[i]
  m=length(levels(as.factor((as.matrix(s3$appl_ID[s3$paperID==lev])))))
  p=s3[,c(1,3,4,2)]
  p=p[p$paperID==lev,]
  appnames=as.data.frame(rbind(appnames,p))
  z=z+m
}

appnames
z

#correct appname labels where needed, recalculate z
#fill in paperID list of labels to correct
corr=c(15,16,17,18,19,20)

#correct these here or with editing links!
  #!!!!only in this case!!!! (all applications = A)
  #l=length(levels(s3$appl_ID))
  #levels(s3$appl_ID)=rep("A",l)


s3

##BACKUP TABLE 17/06/20 with first test runs only: backup_s3_170620=s3
##doesnt work properly!/ write.csv(s3, file="C:/local backups/IPBES VA/step3_testrun_170620.csv")


str(s3)



#(TO DO: adapt level names + explode all 'multiple possible' columns to separate columns 0/1)

#create a table with nr of applications per student
library(dplyr)
check=count(s3, rater, sort=T)
check


#create tables per application

l3=as.list(matrix(0,z,1))
q=1
l=length(levels(s3$paperID))
lev=levels(s3$paperID)
for(i in 1:l) {
  a=assign(paste("s3p.", lev[i],sep=""),s3[s3$paperID==lev[i],])
  levapp=levels(as.factor((as.matrix(s3$appl_ID[s3$paperID==lev[i]]))))
  m=length(levels(as.factor((as.matrix(s3$appl_ID[s3$paperID==lev[i]])))))
  for (j in 1: m) {
    b=assign(paste("s3a.", lev[i],levapp[j],sep=""),s3[s3$paperID==lev[i] & s3$appl_ID==levapp[j],])
    #list_s3[q,1]= paste("s3.", lev[i], levels(as.factor(as.matrix(a$appl_ID)))[j],sep="")
    l3[[q]]=b
    q=q+1
  }}
str(l3)
dim(s3a.15A)
s3a.15A$rater


#TO DO: create script which extracts only the tables with repeated scores from the survey

irr3=matrix(0,length(l3),2)
colnames(irr3)=c("krip.alpha", "Fleiss.kappa")
rownames(irr3)=rep("x",length(l3))
for(i in 1:dim(irr3)[1]){
  m=as.matrix(l3[[i]])
  #select non-free comment fields (use "col" and substract one nr):
  #5, 11, 12,13, 32, 33, 36, 39, 40, 46, 47, 54, 55, 61, 62, 67, 68, 81, 82, 83, 84, 85
    m=m[,c(2:4,6,8,10,14:31,34,35,37,38,41:45,48:53,56:60,63:65,68:80)]
  mt=t(m)
  k=kripp.alpha(m, method="nominal")
  irr3[i,1]=k$value
  f=kappam.fleiss(mt, exact = FALSE, detail = TRUE)
  irr3[i,2]=f$value
  rownames(irr3)[i]=paste(m[1,2],m[1,3],sep="_")
}
irr3
s2.20
k
f

#As a rule of thumb values of Kappa from 0.40 to 0.59 are considered moderate,
#0.60 to 0.79 substantial, and 0.80 outstanding (Landis & Koch, 1977).
#Most statisticians prefer for Kappa values to be at least 0.6
#and most often higher than 0.7 before claiming a good level of agreement.

#irr per topic!!

irr3b=matrix(0,length(l3),8)
colnames(irr3b)=c("1_Method&use", "2_Appl_Context", "3_Appl_descriptors", "4_Reliab&valid", "5_IPLMLC", "6_Wellbeing"	, "7_EcolSust", "8_Justice")
rownames(irr3b)=rep("x",length(l3))
for(i in 1:dim(irr3b)[1]){
  m=as.matrix(l3[[i]])
  #select non-free comment fields (use "col" and substract one nr):
  #5, 11, 12,13, 32, 33, 36, 39, 40, 46, 47, 54, 55, 61, 62, 67, 68, 81, 82, 83, 84, 85
  m1=m[,c(2,6,8,10)]
  mt1=t(m1)
  m2=m[,c(2,14:31)]
  mt2=t(m2)
  m3=m[,c(2,34,35,37,38)]
  mt3=t(m3)
  m4=m[,c(2,41:45)]
  mt4=t(m4)
  m5=m[,c(2,48:53)]
  mt5=t(m5)
  m6=m[,c(2,58:60)]
  mt6=t(m6)
  m7=m[,c(2,63:65)]
  mt7=t(m7)
  m8=m[,c(2,68:80)]
  mt8=t(m8)
  f=kappam.fleiss(mt1, exact = FALSE, detail = TRUE)
  irr3b[i,1]=f$value
  f=kappam.fleiss(mt2, exact = FALSE, detail = TRUE)
  irr3b[i,2]=f$value
  f=kappam.fleiss(mt3, exact = FALSE, detail = TRUE)
  irr3b[i,3]=f$value
  f=kappam.fleiss(mt4, exact = FALSE, detail = TRUE)
  irr3b[i,4]=f$value
  f=kappam.fleiss(mt5, exact = FALSE, detail = TRUE)
  irr3b[i,5]=f$value
  f=kappam.fleiss(mt6, exact = FALSE, detail = TRUE)
  irr3b[i,6]=f$value
  f=kappam.fleiss(mt7, exact = FALSE, detail = TRUE)
  irr3b[i,7]=f$value
  f=kappam.fleiss(mt8, exact = FALSE, detail = TRUE)
  irr3b[i,8]=f$value
  rownames(irr3b)[i]=paste(m[1,3],m[1,4],sep="_")
}
irr3b





######DRAFTS#####

n=length(levels(as.factor(s3$paperID)))
m=matrix(0,n,1)
l3=as.list(m)
for(i in 1:n){
  lev=levels(as.factor(s3$paperID))[i]
  a=assign(paste("s3.",lev,sep=""),s3[s3$paperID==lev,])
  l3[[i]]=a
  m[i,1]= paste("s3.",lev,sep="")
}
