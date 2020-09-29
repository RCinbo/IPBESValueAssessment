#create a table with nr of applications per student
#first run Consistency_Applicationsurvey to have s3

library(dplyr)
check=count(s3, rater, sort=T)
check
sum (check$n)
