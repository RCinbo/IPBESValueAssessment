source("Starting.R")

# 1.This script is to compile a list of method mentioned in the survey
# This looks at the Question 1.2, 1.4


summary(s3_single$"1.2")  # multiple answered were separated by ; (sometimes it is separated by :) # Numbers indicate the method name, when there is no number matched with the method, added as a text,
summary(s3_single$"1.4")

library(stringr) # parsing strings in R

main_method_1.2 = str_split(s3_single$"1.2", pattern = ";")



# Read method name look-up table
method_lutb = read.csv2("IPBES_method_LookUp.csv")
# str(method_lutb)


# str(main_method_1.2)

# Q1: unsure what ":" means - was there any confusion?
# Q2: there are cases for unspecified methods like
# [[977]]
# [1] "Camera trap survey (linked to 10)"
# -> (we can take only the number)
# [[960]]
# [1] "Connectivity Modeling System ( Paris et al. 2013)" -> ?
# -> Consider it as other methods?
# [[981]]
# [1] "Impact Pathway Approach (52?)"
# ->Can take only the number but these cases cannot be processed automatically but manually.

row_idx = 981 #
row_idx = 8 # multiple items
row_idx = 85 # use of semicolon
row_idx = 11 # use of semicolon

res_all = vector("list", length = nrow(s3_single)) #

for (row_idx in 1:nrow(s3_single)) {

  s3_single_tmp = s3_single[row_idx,]
  s3_1.2_tmp = as.character(s3_single_tmp$"1.2")
  paperID_tmp = s3_single_tmp$paperID

  method_tmp_v = str_split(s3_1.2_tmp, pattern = ";")[[1]]
  method_tmp_v = str_trim(method_tmp_v) # remove trailing spaces
  method_tmp_cnt = length(method_tmp_v)# how many?


  # store the processed information in the container
  res_tmp = data.frame(matrix(data = NA, nrow = method_tmp_cnt, ncol = 6 )) # ID, desc.,note, raw data string if needed
  colnames(res_tmp) = c("PaperID","RowID", "MethodID","Description", "NeedChecking", "RawData")


  for (method_tmp_idx in 1:method_tmp_cnt) {
    # see if it contains ":"
    method_tmp = method_tmp_v[method_tmp_idx]
    colonExists = str_detect(method_tmp , pattern = ":")

    if (colonExists) {

      res_1 = str_split(method_tmp, pattern = ":", simplify = F)[[1]] # split by ":" assuming the latter part contains detailed description.
    } else {
      # it is the case e.g. row 981. containing no colon
      res_1 = c(method_tmp ) # make it two elements - ID=NA with desc.
    }


    if (length(res_1) == 1) {
      # it has only the method ID
      id_tmp = as.numeric(res_1[1])
      if (!is.na(id_tmp)){
         res_2 = c(paperID_tmp, row_idx, id_tmp, NA, "N", s3_1.2_tmp) # ID, desc.,NeedChecking?, row data string

      } else {
        # it is null, it means the first element is not a number (means something else like desc. or name of the method)
        res_2 = c(paperID_tmp, row_idx, NA, res_1[1], "Y", s3_1.2_tmp)

      }

    } else if (length(res_1)==2) {

      # method ID and description
      id_tmp = as.numeric(res_1[1])
      desc_tmp = res_1[2]

      if (!is.na(id_tmp)){
         res_2 = c(paperID_tmp, row_idx, id_tmp, desc_tmp, "N", s3_1.2_tmp) # ID, desc.,NeedChecking?, raw data string

      } else {
        # it is null, it means the first element is not a number (means something else like desc. or name of the method)
        res_2 = c(paperID_tmp, row_idx, NA, desc_tmp, "Y", s3_1.2_tmp)

      }



    } else {
      # if it has more than three elements we throw an error for now
      # print(paste0("PaperID:", paperID_tmp, " Row ", row_idx, " has more than two elements: \"", s3_single_tmp, "\""))
      res_2 = c(paperID_tmp, row_idx, NA, NA,  "Y", s3_1.2_tmp)
    }

    # print(method_tmp_idx)
    # print(res_2)

     if (!(res_2[3]  %in% method_lutb$ID)) {
      # if the method is not enlisted, it could be possibly wrong.. (e.g. 2009)
      res_2[5] = "Y"
    }

    res_tmp[method_tmp_idx, ] = res_2
  }


  res_all[[row_idx]] = res_tmp



}

nrow(s3_single)
length(res_all)
res_all[[7]]
res_all[[8]]
res_all[[981]]

res_all_df = do.call("rbind", res_all)

nrow(res_all_df)

# warnings (+50) when the first element was not numeric. It is dealt the above.

#######################
table(res_all_df$NeedChecking)

res_all_df[,"MethodID"] = factor(as.character(res_all_df[,"MethodID"]))

# Let's first consider only well-formed values

method_tb1 = table(res_all_df[,"MethodID"])
(method_tb1)
sort(method_tb1, decreasing = T)

barplot(sort(method_tb1, decreasing = T), las=2)

method_tb_code = names(method_tb1)
method_tb_name =method_lutb$Method.name[match(method_tb_code, method_lutb$ID )]
names(method_tb1) = method_tb_name

barplot(method_tb1, las=2)

 pdf("output/Main_Method_list.pdf", width = 12, height = 8)
par(mar=c(4,17,4,4))
barplot(sort(method_tb1, F), las=1, cex.names = 0.5, horiz = T, xlab = "# of applications")
dev.off()


# figure out what was not found
method_not_found_idx = which(is.na(match(method_lutb$ID, method_tb_code)))

method_lutb$Method.name[method_not_found_idx]


which(str_detect(s3_single$"1.2", pattern ="SolVES"))
# which(str_detect(s3_single$"1.4", pattern ="Solv"))
# s3_single$"1.4"[996]



# papers to look at
checklist_v = which(res_all_df$NeedChecking == "Y")
checklist_rowid_v = res_all_df$RowID[checklist_v]
head(checklist_rowid_v)
head(checklist_v)
# res_all_df$PaperID[checklist_v ]
# res_all_df$Description[checklist_v ]
# res_all_df$RawData[checklist_v ]

# 15th row in res_all_df (11th row in s3single)
res_all_df[15,]
s3_single[11,"1.2"]
#


# 109 th row in res_all_df
res_all_df[109,]
which(s3_single$paperID == 40974) # Row 85 in s3_single

s3_single[s3_single$paperID == 40974,"1.2"]

 library(openxlsx)
#write.xlsx(res_all_df,file = "output/Step3_1.2_all.xlsx")

#write.xlsx(res_all_df[res_all_df$NeedChecking=="Y",], file="output/Step3_1.2_needchecking_only.xlsx")

#stop("ends here (8 Oct 2020) by HL")

table(s3_single$"1.1")
#   1     2     3     4     5     6     7    11    22 64502
# 859   153    46    17     6     2     1     1     1     1

which(s3_single$"1.1" == 22) # it's an error: it should be '2'? There are only two method names following..
s3_single[724,"1.1"] <-2


which(s3_single$"1.1" == 64502) # that's paperID. It should be corrected as '1'. Only one method ID is presented in the next question
s3_single[1060,"1.1"] <- 1

table(s3_single$"1.1")
#   1   2   3   4   5   6   7  11
# 860 154  46  17   6   2   1   1

pie(table(s3_single$"1.1"))
barplot(table(s3_single$"1.1"))
