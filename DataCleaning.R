################################################################################
###-----------------------------Data cleaning--------------------------------###
### This scripts starts from the data on google drive and                    ###
### saves a cleaned dataset to you local drive                               ###
################################################################################

library(tidyverse)
library(openxlsx)
library(countrycode)
###-----------------------------Step 1 Table---------------------------------###
paste_noNA <- function(x,sep=", ") {
  gsub(", " ,sep, toString(x[!is.na(x) & x != "" & x!="NA"] ) ) }
data.final <- read.xlsx("./data/IDSWAP_ 425 AND_16June20DG_copy.xlsx",
                        colNames=T)#Read the data locally

###-----------------------------Step 2 Table---------------------------------###
# 1. read the table from the Google drive
step2 <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vQa2_J5d1RiSCZTTFJslWmA0zsaqNmoVCOnl9GeNOAHzfLQorqsrfWk2jgc6baPXgQZfWzsqDVOdYfo/pub?output=csv"
s2_full <- read.csv(url(step2))
dim(s2_full)
colnames(s2_full)
# str(s2_full)
summary(s2_full)

# add row IDs (not equal to paperID!)
s2_full$RowID <- 1:nrow(s2_full)

# 2. Remove validation trials (Step 2: the first 13 rows, Step 3: 16 rows)
s2_row_test_idx <- 1:66 # 1:16
s2_reduced <- s2_full[-s2_row_test_idx, c(3,2,4:11)] # s2 cleaned

# 3. Make short names for the col_names (more workable names)
colnames(s2_reduced) <- c("paperID","rater","first_auth","valuation",
                          "application","multiple","appl_nr","appl_names",
                          "self_rel","comments")
# 4. Subset those studies scored multiple times
# (those will come back after consistency analysis)
paperid_tb <- table(s2_reduced$paperID)
paperid_id_names <- names(paperid_tb)
paperid_ids_multiple <- paperid_id_names[which(paperid_tb > 1)]
sum(paperid_tb[which( paperid_tb > 1)]) # all 211 studies

s2_single <- subset(s2_reduced, subset = !(paperID %in% paperid_ids_multiple))
nrow(s2_reduced) - nrow(s2_single) #number of duplicates removed
which(s2_single$paperID %in% paperid_ids_multiple)

###-----------------------------Step 3 Table---------------------------------###
step3 <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vQPN4pJeP4JjJIvlbRwjqCCLSTwUSicDsBaACqvm3_E8S9yh4z2ujsLUysuA9bATmCKksTzaQe8B4S7/pub?output=csv"
s3_full <- read.csv(url(step3))

col <- matrix(data = colnames(s3_full)[1:86], nrow = 86, ncol = 1 )

#remove validation trials
s3_row_test_idx <- 1:60 # 1:16
s3_reduced <- s3_full[-s3_row_test_idx, c(3,2,4:86)]

#make the column names shorter and more sensible

colnames(s3_reduced) <- c("paperID","rater","first_auth","appl_ID","warning",
                       "1.1","1.2","1.3","1.4","1.5","1.6","1.7","1.8",
                       "2.1","2.2","2.3","2.4","2.5","2.6","2.7","2.8",
                       "2.9","2.10","2.11","2.12","2.13","2.14","2.15",
                       "2.16","2.17","2.18","2.20","2.21",
                       "3.1","3.2","3.3","3.4","3.5","3.6","3.7",
                       "4.1","4.2","4.3","4.4","4.5","4.6","4.7",
                       "5.1","5.2","5.3","5.4","5.5","5.6","5.7","5.8",
                       "6.1","6.2","6.3","6.4","6.5","6.6","6.7",
                       "7.1","7.2","7.3","7.4","7.5",
                       "8.1","8.2","8.3","8.4","8.5","8.6","8.7","8.8","8.9",
                       "8.10","8.11","8.12","8.13","8.14","8.15","8.16","8.17",
                       "8.18")
# Remove the studies omitted in the above for S2 (i.e. reviewed multiple times)
s3_single <- subset(s3_reduced, subset = !(paperID %in% paperid_ids_multiple))
# alternatively, sample a row per duplicated studies
s3_row_ids <- 1:nrow(s3_reduced)

paperid_ids_multiple_zero_idx <- sapply(paperid_ids_multiple,
                                        FUN = function(x) length(
                                          s3_row_ids[s3_reduced$paperID == x]
                                          ) > 0
                                        )
# consider only the avail. studies
paperid_ids_multiple_non_zero <- paperid_ids_multiple[
  paperid_ids_multiple_zero_idx]

# sample one per each
set.seed(2020) # seeding
paperid_ids_multiple_sampled <- sapply(paperid_ids_multiple_non_zero,
                                       FUN = function(x) sample(
                                         s3_row_ids[s3_reduced$paperID == x], 1)
                                       )
s3_multi <- s3_reduced[paperid_ids_multiple_sampled, ]

# redefine s3_single
s3_single <- rbind(subset(s3_reduced,
                          subset = !(paperID %in% paperid_ids_multiple)),
                   s3_multi)
##### Short and full questions
colnames_s3_lookup_tb = data.frame(SHORTNAME = colnames(s3_reduced),
                                   NAME = colnames(s3_full)[c(3,2,4:86)])

#Add the method families to the s3_single dataframe. Currently, we use the
#method families that were identified by keywords, we'll improve that later
#based on the questionnaires.
s3_single %>% mutate(MF1.key = NA,
                     MF2.key = NA,
                     MF3.key = NA,
                     MF4.key = NA,
                     MFA.key = NA,
                     MFB.key = NA) -> s3_single
for (i in 1:nrow(s3_single)) {
  idx <- which(data.final$TSU.ID_MERGED == s3_single[i, 'paperID'])
  if (length(idx) == 0) {
    print(sprintf('error, %d',s3_single[i, 'paperID']))}
  else{
    s3_single[i,c('MF1.key','MF2.key','MF3.key','MF4.key','MFA.key','MFB.key')] <-
      data.final[idx,c('TS20','TS21','TS22','TS23','TS18','TS19')]
  }
}

########Add the loadings to the pre-SOD method families and IPBES categories
lMF <- read.xlsx('data/methods x MF x IPBESclasses SOD.xlsx')
Methods <- read.xlsx(
  'Corrected/1.2_MethodList_ByRowID_LuizaCorrected_n1163.xlsx',
  sheet = "Sheet 1")
s3_single$MethodSOD <- NA
for (i in as.numeric(Methods$RowID)) {
  if (as.numeric(Methods[i, 'PaperID']) != s3_single[i, 'paperID']) {
    sprintf('The paperID at row %d of the methodsexcel (%s)
            does not equal the one in s3_single (%d)',
            i,
            Methods[i, 'PaperID'],
            s3_single[i, 'paperID']
            )
  }else{
    if ((Methods[i, 'MethodID'] == "") | is.na(Methods[i, 'MethodID'])) {
      s3_single[i, 'MethodSOD'] <- NA
    }else{
      s3_single[i,'MethodSOD'] <- Methods[i,'MethodID']
    }
  }
}

split <- str_split(s3_single$MethodSOD,pattern = ";")
if (sum(is.na(split)) > 0) {
  a <- which(is.na(split))
  for (i in a) {
    print(sprintf('There are no methods defined for PaperID %s, Row Number %d',
                  s3_single[i,'paperID'],i))
  }
}

b <- which(!is.na(split))
SODmethods <- unique(lMF[,'method.name.SOD'])
A <- matrix(0,nrow = nrow(s3_single), ncol = length(SODmethods))
colnames(A) <- SODmethods
s3_single %>% mutate(MF1.SOD = NA, MF2.SOD = NA, MF3.SOD = NA, MF4.SOD = NA,
                     IPBES.econ_SOD = NA, IPBES.soccul_SOD = NA,
                     IPBES.bioph_SOD = NA,IPBES.health_SOD = NA,
                     IPBES.ILK_SOD = NA) ->
  s3_single
mfcol <- c(7,8,9,10)#the columns where the methodfamily loadings are
ipbescol <- c(11,12,13,14,15)#the columns in lMF where the IPBES category loadings are
for (i in b) {
  methods <- split[[i]]
  d <- which(lMF$methods.ID_LUIZA %in% as.numeric(methods))
  s3_single[i,c('MF1.SOD', 'MF2.SOD', 'MF3.SOD', 'MF4.SOD')] <-
    colMeans(lMF[d,mfcol])
  s3_single[i,c('IPBES.econ_SOD', 'IPBES.soccul_SOD', 'IPBES.bioph_SOD',
                'IPBES.health_SOD','IPBES.ILK_SOD')] <-
    colMeans(lMF[d,ipbescol])
  SODmethods <- unique(lMF[d,'method.name.SOD'])
  A[b[i],SODmethods] <- 1
}
s3_single <- cbind(s3_single,A)

#Add monetary-nonmonetary and add biophys-socio-cultural
Mon <- read.xlsx('Corrected/Articulation_luiza_SOD.xlsx', sheet = "Sheet1")
s3_single %>% mutate(Monetary = NA,
                     NonMonetary = NA,
                     MonetaryUnclear = NA,
                     Biophysical = NA,
                     SocioCultural = NA,
                     BiophSocCulUnclear = NA) -> s3_single
for (i in 1:nrow(s3_single)) {
  if (s3_single[i,'paperID'] != Mon[i,'paperID']) {
    print(sprintf('Paper %d does not match the row number in de file',
                  s3_single[i,'paperID']))
    }else{
    s3_single[i,c('Monetary','NonMonetary','MonetaryUnclear','Biophysical',
                  'SocioCultural','BiophSocCulUnclear')] <-
      Mon[i,c("monetary","non-monetary","mon/non-mon.unclear",
              "Biophysical","Social-Cultural","bioph/socioc.unclear")]
  }
}

save(s3_single, file = 'output/s3_single_BeforeExplode.RData')

#########-----make corrections for what was filled in in the 'other' text fields
L <- data.frame(
  Q = c('2.1','2.2'),
  filename = c('s3_single_with_other_choices_n189_Raphael_NoComment.xlsx',
               's3_single_with_other_habitat_n127-corrSJ.xlsx')
)
for (q in 1:nrow(L)) {
  a <- which(colnames(s3_single) == L[q,'Q'])
  data <- read.xlsx(sprintf("Corrected/%s", as.character(L[q,'filename'])),
                    colNames = FALSE, sheet = "Sheet 1")
  header <- data[1,-c(1,2)]#the first and second column have paperid and text which we don't need
  data <- data[-1,]
  s3_single[,a] <- as.character(s3_single[,a] )
  for (j in 1:nrow(data)) {
    b <- which(s3_single[,a] == data[j,2] & s3_single$paperID == data[j,1])
    if (is_empty(b)) {
      sprintf('paper %s cannot be found',data[j,1])
      }else if (length(b) > 1) {
        sprintf('paper %s was found more than once with the text %s in question %s',
                data[j,1],data[j,2], L[q,'Q'])
        }else{
          c <- which(data[j,-c(1,2)] == 1)
          if (is_empty(c)) {
            sprintf('empty line withpaper %s, with the text %s in question %s',
                    data[j,1],data[j,2], L[q,'Q'])
            }else{
        s3_single[b,as.character(L[q,'Q'])] <- str_c(unlist(unname(header[c])),
                                                     sep = ',', collapse = "")
            }
        }
    }
  s3_single[,a] <- as.factor(s3_single[,a] )
}


#######------Now make 0-1 dummy columns for each of the miltuple choice questions
L <- read.xlsx('data/LegendListForDummyColumns.xlsx')
PrintOthers <- FALSE #set this to true if you want to print all 'other' answers to an excel file
if (PrintOthers) {
  M <- list()
  wb <- loadWorkbook("data/OtherAnswers.xlsx")
  sheets <- getSheetNames("data/OtherAnswers.xlsx")
}
questions <- unique(L$Question)
L$txt <- gsub("// ","", L$txt)
L$txt <- gsub("&", "and", L$txt)
for (q in 1:length(questions)) {
  a <- which(L$Question == questions[q])
  legend <- L[a,]
  data <- s3_single[,as.character(questions[q])]#the column we want to explore into several dummy columns
  #special characters sometimes fail to be read correctly so we correct
  data <- gsub("â€™", "’", data)
  data <- gsub("&", "and", data)
  data <- gsub("â€˜", "‘", data)
  data <- gsub("â€“", "–", data)
  data <- gsub("// ","", data)

  for (j in which(legend$txt != 'Other')) {
    A <- 1*(str_detect(data, pattern = fixed(as.character(legend[j,'txt']))))
    data <- str_replace(data, pattern = fixed(as.character(legend[j,'txt'])),"")
    s3_single <- cbind(s3_single, A)
    colnames(s3_single)[ncol(s3_single)] <- as.character(legend[j,'code'])
  }
  b <- which(legend$txt == 'Other')
  if (!is_empty(b)) {
    Otheridx <- (str_length(gsub("[;, ]","",data)) > 1)
    s3_single <- cbind(s3_single,1*(Otheridx))
    colnames(s3_single)[ncol(s3_single)] <- as.character(legend[b,'code'])
    if (PrintOthers) {
      M[[q]] <- matrix(nrow = (sum(Otheridx)), ncol = (1 + nrow(legend)))
      colnames(M[[q]]) <- c('PaperID', as.character(legend$txt))
      rownames(M[[q]]) <- data[Otheridx]
      M[[q]][,1] <- s3_single[Otheridx, 'paperID']
      if (!(questions[q] %in% sheets)) {
        addWorksheet(wb, as.character(questions[q]))
        }
      writeData(wb, sheet = as.character(questions[q]), M[[q]],
                colNames = T,
                rowNames = TRUE)
    }
  }
}
if(PrintOthers){
  saveWorkbook(wb,"data/OtherAnswers.xlsx",overwrite = T)
}


K <- data.frame(Q = c('2.10', '2.11','2.12', '2.13', '2.14'),
                filename = c('s3_single_with_other_nature_n20-corrSJ.xlsx',
                           's3_single_with_other_regul_n46-corrSJ.xlsx',
                           's3_single_with_other_material_n25_corrSJ.xlsx',
                           's3_single_with_other_non_material_n22_corrSJ.xlsx',
                           's3_single_with_other_QoL_n29_corrSJ.xlsx'))
colNB <- which(str_detect(colnames(s3_single),
                          paste(str_c("Q", as.character(K$Q)),collapse = '|')
                          ) &
                 !str_detect(colnames(s3_single),'Other|none|2.2')
               )
colNBOther <- which(str_detect(colnames(s3_single),
                               paste(str_c("Q", as.character(K$Q)),
                                     collapse = '|')) &
                      str_detect(colnames(s3_single),'Other')
                    )
s3_single[,colNBOther] <- 0 #As we correct all of these, we assume they're all
#classified in one of the value types. These dummy columns will only be 1 if the
#'Other' text answer could not be classified.
#'(see last 'else' in the for loop below)

for (q in nrow(K)) {
  a <- which(colnames(s3_single) == K[q,'Q'])
  data <- read.xlsx(sprintf("Corrected/%s",as.character(K[q,'filename'])),
                    colNames = FALSE)
  header <- data[1,-c(1,2)]#the first and second column have paperid and text which we don't need
  data <- data[-1,]#remove the header
  #s3_single[,a] <- as.character(s3_single[,a])
  for (j in 1:nrow(data)) {
    b <- which(s3_single[,a] == data[j,2] & s3_single$paperID == data[j,1])
    if (is_empty(b)) {
      sprintf('paper %s cannot be found',data[j,1])
      }else if (length(b) > 1) {
        sprintf('paper %s was found more than once with the text %s in question %s',
                data[j,1],data[j,2], K[q,'Q'])
        }else{
          if (sum(as.numeric(data[j,-c(1,2)]), na.rm = T) != 0) {
            s3_single[b,colNB] <- pmax(unlist(s3_single[b,colNB]),
                                       as.numeric(data[j,-c(1,2)]),
                                       na.rm = T)
            }else{
              s3_single[b,
                        which(colnames(s3_single) ==
                                paste("Q", as.character(K[q,'Q']),
                                      "_Other", sep = ""))] <- 1
            }
        }
    }
}

#correct 'none' and 'other':
#NONE can only be selected if none of the categories is selected and nothing was filled in with the 'other' option.
#OTHER can only be selected if something was filled in in the 'other' option but none of the categories were selected.
for (q in 1:nrow(K)) {
  colNB2 <- which(str_detect(colnames(s3_single),
                             paste(str_c("Q", as.character(K[q,'Q'])),
                                   collapse = '|')) &
                    !str_detect(colnames(s3_single),'none|2.2')
                  )
  noneNB <- which(colnames(s3_single) == paste("Q", as.character(K[q,'Q']),
                                               "_none", sep = ""))
  SUM <- rowSums(s3_single[,colNB2])
  #Correct the other column
  s3_single[,noneNB] <- 1*(SUM == 0)
}

#In Q8.4, 8.5, 8.6 and 8.8, there are a lot of 'irrelevant' answers in the 'other' question. We change these to 'none' in stead of 'other answers'.
for (i in c('8.4','8.5','8.6','8.8')) {
  a <- which(str_detect(s3_single[,as.character(i)],'irrelevant') &
              s3_single[,sprintf('Q%s_Other',i)] == 1)
  cols <- which(str_detect(colnames(s3_single),i))
  s3_single[a,cols[-1]] <- 0
  s3_single[a,sprintf('Q%s_none',i)] <- 1
}

save(s3_single,L, file = 'output/s3_single_WithDummies.RData')
write.xlsx(s3_single, file = 'output/s3_single.xlsx')
