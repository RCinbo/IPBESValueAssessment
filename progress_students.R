#create a table with nr of applications per student
#first run Consistency_Applicationsurvey to have s3

library(dplyr)
s3=s3_full
colnames(s3)
check=count(s3, Email.Address, sort=T)
check
sum (check$n)
