library(openxlsx)
# source("Starting.R")


############Define a color scheme#####################
IPbesdarkgreen <- rgb(92/255, 102/255, 93/255) #5c665d
IPbeslightgreen <- rgb(181/255, 212/255, 141/255) #b5d48d
colfunc <- colorRampPalette(c(rgb(92/255, 102/255, 93/255), rgb(181/255, 212/255, 141/255)))

brewer.YlGnBu <- colorRampPalette(brewer.pal(9, "YlGnBu"), interpolate = "spline")
brewer.BuPu <- colorRampPalette(brewer.pal(9, "BuPu"), interpolate = "spline")
brewer.Greens <- colorRampPalette(brewer.pal(9, "Greens"), interpolate = "spline")



### Topic 1: Methods and their use

# 1.This script is to compile a list of method mentioned in the survey
# This looks at the Question 1.2, 1.4

summary(s3_single$"1.2")  # multiple answered were separated by ; (sometimes it is separated by :) # Numbers indicate the method name, when there is no number matched with the method, added as a text,
summary(s3_single$"1.4")

library(stringr) # parsing strings in R

main_method_1.2 = str_split(s3_single$"1.2", pattern = ";")


# Read method name look-up table
method_lutb = read.csv2("IPBES_method_LookUp.csv")
colnames(method_lutb)=c("ID","Method.name")
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

########## 1.2. the list of main methods..
s3_1.2_mainlist_corrected_web = "https://docs.google.com/spreadsheets/d/e/2PACX-1vQ-M3x4O_Oj9qQEoe8hasuATMoLbqeXfdBUkOtaJYwLJhmdHX0zAeQX4zNd07bmYf6t2GyF8rUvVK0L/pub?gid=730503165&single=true&output=csv"
s3_1.2_mainlist_corrected = read.csv(url(s3_1.2_mainlist_corrected_web), header = T)
colnames(s3_1.2_mainlist_corrected)[c(2, 3, 5)] = c("paperID", "appl_ID", "cr_1.2" )

table(s3_1.2_mainlist_corrected$cr_1.2)

s3_1.2_mainlist_corrected$appl_ID = as.character(s3_1.2_mainlist_corrected$appl_ID)

res_all = vector("list", length = nrow(s3_single)) #

# see paper id 19484 and 47081

# which(s3_single$paperID == 19484) # 40
# which(s3_single$paperID == 47081) # 60 61 (two rows with different applID)
# which(s3_single$paperID == 42291) # 969 970 (two rows with the same applID)

row_idx = 11


for (row_idx in 1:nrow(s3_single)) {

  s3_single_tmp = s3_single[row_idx,]
  s3_1.2_tmp = as.character(s3_single_tmp$"1.2")
  paperID_tmp = s3_single_tmp$paperID

  ###### Replace malformed answers by the corrected data on the web

  s3_cr_tmp = subset(s3_1.2_mainlist_corrected, subset = ((paperID ==paperID_tmp) & (appl_ID == s3_single_tmp$appl_ID)))

  if (nrow(s3_cr_tmp) >= 1 ) {
    # we have corrected data

    if (nrow(s3_cr_tmp) > 1 ) {
      warning(paste0("paperID:", paperID_tmp, " more than one items with the same paper ID and the appl ID!"))
      # print(paste0("paperID:", paperID_tmp))
      cat("row id:", row_idx)
      print(s3_cr_tmp$cr_1.2)
      # stop("more than one items with the same paper ID and the appl ID!")

      s3_cr_tmp = s3_cr_tmp[1, ] # ignore the rows other than the first row
    }

    s3_1.2_tmp = as.character(s3_cr_tmp$cr_1.2)


  } else {
    # do nothing
  }

  method_tmp_v = str_split(s3_1.2_tmp, pattern = ";")[[1]]
  method_tmp_v = str_trim(method_tmp_v) # remove trailing spaces

  # if the values are actually uninformative ones
  method_tmp_v[method_tmp_v==""] = NA
  method_tmp_v[method_tmp_v=="/"] = NA
  method_tmp_v = method_tmp_v[!is.na(method_tmp_v)]

  method_tmp_cnt = length(method_tmp_v)# how many?

  # store the processed information in the container
  res_tmp = data.frame(matrix(data = NA, nrow = method_tmp_cnt, ncol = 6 )) # ID, desc.,note, raw data string if needed
  colnames(res_tmp) = c("PaperID","RowID", "MethodID","Description", "NeedChecking", "RawData")


  if (method_tmp_cnt == 0 ) {
    res_tmp[1,] = c(paperID_tmp, row_idx, NA, "No data entered", "N", s3_1.4_tmp)
    res_all_1.4[[row_idx]] = res_tmp
    # next() # continue the loop
    stop(paste0("No data entered (1.2)! paperId =", paperID_tmp))
  }


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

#pdf("output/Main_Method_list.pdf", width = 12, height = 8)
par(mar=c(4,17,4,4))
barplot(sort(method_tb1, F), las=1, cex.names = 0.5, horiz = T, xlab = "# of applications")
dev.off()


# figure out what was not found

method_not_found_idx = which(is.na(match(method_lutb$ID, method_tb_code)))

n=method_lutb$Method.name[method_not_found_idx]
length(method_lutb$Method.name[method_not_found_idx])

#which(str_detect(s3_single$"1.2", pattern ="SolVES"))
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


toSave3_1.2 = FALSE # TRUE

if (toSave3_1.2){
  write.xlsx(res_all_df,file = paste0("output/Step3_1.2_all_", Sys.Date(),".xlsx"))
  write.xlsx(res_all_df[res_all_df$NeedChecking=="Y",], file=paste0("output/Step3_1.2_needchecking_only_ ", Sys.Date(),".xlsx"))
}

#stop("ends here (8 Oct 2020) by HL")


#########################################################################
############# 1.1 the number of main methods..

table(s3_single$"1.1")
#   1     2     3     4     5     6     7    11    22 64502
# 859   153    46    17     6     2     1     1     1     1

which(s3_single$"1.1" == 22) # it's an error: it should be '2'? There are only two method names following..
#s3_single[724,"1.1"] <-2 # need to check it when we have the final ver.


which(s3_single$"1.1" == 64502) # that's paperID. It should be corrected as '1'. Only one method ID is presented in the next question
#s3_single[1060,"1.1"] <- 1 # need to check it when we have the final ver.

tb_howmany = table(s3_single$"1.1")

tb_howmany_4 = c(tb_howmany[1:3], sum (tb_howmany[4:8]))
names(tb_howmany_4)[4] = 4
tb_howmany_4

pie(tb_howmany_4)

# using cut
cut_1.1 = cut(s3_single$"1.1", breaks = c(0, 1.5, 2.5, 3.5, max(s3_single$"1.1")))
levels(cut_1.1) = c(1,2,3,"4=<")
cut_1.1

pie(table(cut_1.1))

pie(table(s3_single$"1.1"))
barplot(table(s3_single$"1.1"))

length(which(s3_single$"1.1" >= 4))

which(s3_single$"1.1" >= 4)

s3_single[s3_single$"1.1" >= 4, "paperID"]
s3_single[s3_single$"paperID" == 22904, "1.1"]


s3_single[which(s3_single$"1.1" == 22),]

s3_single_1.1_corrected_web ="https://docs.google.com/spreadsheets/d/e/2PACX-1vRWkUoHry3fkwoMWmUfnkS43auXAFT580JZh5yFfUTKJkPjtbIjnf0kfIOdvSjknzlurVpLT81eIb_t/pub?output=csv"
s3_1.1_corrected=read.csv(url(s3_single_1.1_corrected_web), header = F)
colnames(s3_1.1_corrected) = c("paperID","cr_1.1","Note")


str(s3_1.1_corrected$cr_1.1)

# Warning: replace the original values by the live values on the web
s3_single[match(s3_1.1_corrected$paperID, s3_single$paperID),]$"1.1" = s3_1.1_corrected$cr_1.1

table(s3_single$"1.1")


# using cut
cut_1.1 = cut(s3_single$"1.1", breaks = c(0, 1.5, 2.5, 3.5, max(s3_single$"1.1")))
levels(cut_1.1) = c(1,2,3,"> 4")
cut_1.1

pdf("output/Q1.1_NrMainMethods_Pie_3Dec.pdf", width=15, height = 15, pointsize = 20)
png("output/Q1.1_NrMainMethods_Pie_3Dec.png", width = 800, height = 800, pointsize = 20)
pie(table(cut_1.1), main = "Number of main methods per application", init.angle = 90, col = gray.colors(4, start = 0.9, end = 0.3))
dev.off()


pie(table(s3_single$"1.1"))
barplot(table(s3_single$"1.1"))

length(which(s3_single$"1.1" >= 4))

which(s3_single$"1.1" >= 4)




####### Corrected Methods
new_method_lutb = read.xlsx("Corrected/New_Methods_List_corrected.xlsx")
new_method_lutb[new_method_lutb$ID == "91/112",]
new_method_lutb = rbind(new_method_lutb, data.frame(ID=91, Method.name = new_method_lutb[new_method_lutb$ID == "91/112", "Method.name"]))
new_method_lutb = rbind(new_method_lutb, data.frame(ID=112, Method.name = new_method_lutb[new_method_lutb$ID == "91/112", "Method.name"]))

new_method_lutb$ID == "92/100"
new_method_lutb = rbind(new_method_lutb, data.frame(ID=92, Method.name = new_method_lutb[new_method_lutb$ID == "92/100", "Method.name"]))
new_method_lutb = rbind(new_method_lutb, data.frame(ID=100, Method.name = new_method_lutb[new_method_lutb$ID == "92/100", "Method.name"]))

# method.corrected.data = read.xlsx("Corrected/Step3_1.2_all_2020-10-20_luiza.xlsx", sheet = 2)
method.corrected.data = readxl::read_xlsx("Corrected/Step3_1.2_all_2020-10-20_luiza.xlsx", sheet = 2)
# method.corrected.data = read.csv("Corrected/Step3_1.2_all_2020-10-20_luiza_format.csv", header = T)
colnames(method.corrected.data)

sum(table((method.corrected.data$MethodID)))

# match two data frames using RowID (!= uniqueID )


rowid_match = match(method.corrected.data$RowID, res_all_df$RowID)
table(is.na(rowid_match))
table(table(rowid_match)) # duplicated


### identify new data rows
newkey_added = res_all_df$PaperID[!(res_all_df$PaperID %in% res_all_df_old$PaperID)] #
newkey_added_idx = which(!(res_all_df$PaperID %in% res_all_df_old$PaperID))
res_all_df_added_n15 = res_all_df[newkey_added_idx, ]
write.xlsx(res_all_df_added_n15, file = "Corrected/added_rows.xlsx")

# visually find the altered rows in the new/corrected data frames

res_all_df$uniqueID_newdata = 1:nrow(res_all_df)
res_all_df$MethodID = as.character(res_all_df$MethodID)
res_all_onlyold_df = res_all_df[which(res_all_df$PaperID %in% method.corrected.data$PaperID),]
nrow(res_all_onlyold_df)

plot(res_all_onlyold_df$PaperID[1:1522] == as.numeric(method.corrected.data$PaperID), type="l", col="grey")

# # from where two dataset differ?
# min_diff_idx  = min(which(res_all_onlyold_df$PaperID[1:1522] != as.numeric(method.corrected.data$PaperID)))
# abline(v=min_diff_idx, col="red", lty=2)
#
# # from 656th row
# head(res_all_onlyold_df[655:1521,])
# head(method.corrected.data[655:1521,])
#
# # 656th in the old must be ignored
#
# plot(res_all_onlyold_df$PaperID[656:1521] == as.numeric(method.corrected.data$PaperID)[657:1522], type="l")
#
# min_diff_idx2 = min(which(res_all_onlyold_df$PaperID[656:1521] != as.numeric(method.corrected.data$PaperID[657:1522])))
# # 845th
#
# (res_all_onlyold_df[840:850,])
# (method.corrected.data[840:850,])
#
# plot(res_all_onlyold_df$PaperID[845:1520] == as.numeric(method.corrected.data$PaperID)[847:1522], type="l")

# final (2 extra rows in the corrected data
plot(res_all_onlyold_df$PaperID[c(1:1520)] == as.numeric(method.corrected.data$PaperID)[c(1:654, 656:844, 846:1522)], type="l")


# Extra data points at the end of the dataset..
# 14743 %in% method.corrected.data$PaperID
# which(14743 == method.corrected.data$PaperID)
# which(14743 == res_all_onlyold_df$PaperID)
#
# res_all_onlyold_df[14743 == res_all_onlyold_df$PaperID,]
# method.corrected.data[14743 == method.corrected.data$PaperID,]
#
#
# 34347 %in% method.corrected.data$PaperID
# 2198 %in% method.corrected.data$PaperID
# 14743 %in% method.corrected.data$PaperID



# 2-step matching
## we ignore last 7 data records
res_all_onlyold_df = res_all_onlyold_df[1:1520,]
res_all_onlyold_df$uniqueID_corrected = NA
res_all_onlyold_df$uniqueID_corrected[1:1520] =method.corrected.data$`unique ID`[c(1:654, 656:844, 846:1522)]

corrected2old_idx = match(res_all_onlyold_df$uniqueID_corrected, method.corrected.data$`unique ID`)
table(res_all_onlyold_df$PaperID == method.corrected.data[corrected2old_idx,]$PaperID)

old2new_idx = match( res_all_df$uniqueID_newdata, res_all_onlyold_df$uniqueID_newdata)

corrected2new_idx = corrected2old_idx[old2new_idx]


# UPDATE the new data by corrected
res_all_df$MethodID = method.corrected.data$MethodID[corrected2new_idx]


table(res_all_df$MethodID)
table(is.na(res_all_df$MethodID))

res_all_df[which(is.na(res_all_df$MethodID)),]

# add exra two rows
newrows = cbind(method.corrected.data[c(655, 845), -1], uniqueID_newdata= nrow(res_all_df) + 1:2)

# final dataset
res_all_df_final = rbind(res_all_df, newrows)
stopifnot(nrow(res_all_df_final)==1544) # must be 1522 + 2
table(table(res_all_df_final$uniqueID_newdata))
tail(res_all_df_final) # # to visually confirm

# aggregate once again by ;
res_methodid_final_agg = tapply(res_all_df_final$MethodID, INDEX = res_all_df_final$RowID, FUN = function(x) paste0(unique(x[!is.na(x)]), collapse = ";"))
res_methodid_ord = order(as.numeric(names(res_methodid_final_agg)))

res_methodid_final_agg_sorted = res_methodid_final_agg[res_methodid_ord]
res_methodid_final_df = (data.frame(names(res_methodid_final_agg_sorted), res_methodid_final_agg_sorted))


res_all_df_final[, "PaperID"]

names(res_methodid_final_df) = c("RowID", "MethodID")
head(res_methodid_final_df)
tail(res_methodid_final_df)


# row id to paper id look up table
row2paper = res_all_df_final[, c("RowID", "PaperID")]
table(table(row2paper$RowID))

uniquerowid_idx = tapply(seq_along(row2paper$RowID), row2paper$RowID, FUN = function(x) identity(x)[1])[unique(row2paper$RowID)]
row2paper = row2paper[uniquerowid_idx, ]
table(table(row2paper$RowID)) # must be 1
table(table(row2paper$PaperID)) # can be up to 5..

paperid_tmp = row2paper[match(res_methodid_final_df$RowID, table = row2paper$RowID), "PaperID"]

res_methodid_final_withPaperID_df = cbind(res_methodid_final_df, PaperID = paperid_tmp)
res_methodid_final_withPaperID_df = res_methodid_final_withPaperID_df[, c(1,3,2)]
head(res_methodid_final_withPaperID_df)

write.xlsx(res_methodid_final_withPaperID_df, file = "Corrected/1.2_MethodList_ByRowID_LuizaCorrected_n1163.xlsx")









table(s3_single$paperID %in% method.corrected.data$PaperID )
'%nin%' <- Negate('%in%')
table(s3_single$paperID %in% method.corrected.data$PaperID)

idx_method <- (s3_single$paperID %nin% method.corrected.data$PaperID)
missing_paper = s3_single$paperID[idx_method]
missing_method = s3_single$'1.2'[idx_method]

# store the processed information in the container
cols_missing = c("PaperID" , "MethodID", "RawData")

df_tmp = data.frame(matrix(data = NA, nrow =  length(missing_paper), ncol = length(cols_missing) )) # ID, desc.,note, raw data string if needed
colnames(df_tmp ) = cols_missing

df_tmp[] = cbind(missing_paper, NA, as.character(missing_method) )

# write.xlsx(df_tmp, file = "Corrected/MissingMethod.xlsx")



s3_single$'1.2'[str_detect(s3_single$'1.2', "5")]
s3_single$paperID[str_detect(s3_single$'1.2', "5")]
s3_single$'1.2'[str_detect(s3_single$'1.2', "Bayesian")]



s3_single$'1.4'[str_detect(s3_single$'1.4', "5")]
s3_single$paperID[str_detect(s3_single$'1.4', "5")]
s3_single$'1.4'[str_detect(s3_single$'1.4', "Bayesian")]
s3_single$paperID[str_detect(s3_single$'1.4', "Bayesian")]


#
# table(s3_full$what.s.the.paper.ID[str_detect(s3_full$X1.2...Carefully.list.ALL.main.valuation.methods, "5")] %in% s3_single$paperID[str_detect(s3_single$'1.2', "5")])
#
#
# s3_full$X1.2...Carefully.list.ALL.main.valuation.methods[s3_full$what.s.the.paper.ID[str_detect(s3_full$X1.2...Carefully.list.ALL.main.valuation.methods, "5")] %nin% s3_single$paperID[str_detect(s3_single$'1.2', "5")]]
#
# s3_full$X1.2...Carefully.list.ALL.main.valuation.methods[str_detect(s3_full$X1.2...Carefully.list.ALL.main.valuation.methods, "Bayesian")]


#
# method.corrected.data[,"MethodID"] = factor(as.character(method.corrected.data[,"MethodID"]))

# Let's first consider only well-formed values

method_tb2 = table(method.corrected.data[,"MethodID"])
(method_tb2)
sort(method_tb2, decreasing = T)



split_res_l_method <-list()

for(row_idx in 1:nrow(method.corrected.data)){

  split_tmp = str_trim(str_split(method.corrected.data[row_idx,"MethodID"], pattern = ";")[[1]])
  split_res_l_method[[row_idx]]<- split_tmp
}

# merge two method ids
method_split_v = unlist(split_res_l_method)
method_split_v[method_split_v == 112] = 91
method_split_v[method_split_v == 100] = 92

method_split_v_fac = factor(method_split_v)
method_tb_sorted = sort(table(method_split_v_fac), decreasing = F)





# names(method_tb_sorted)[!(names(method_tb_sorted) %in%  new_method_lutb$ID)]
#
names(method_tb_sorted) = new_method_lutb$Method.name[match(names(method_tb_sorted), new_method_lutb$ID)]

pdf("output/Main_Method_list_correxted24Nov.pdf", width = 12, height = 12)
par(mar=c(4,35,4,4))
barplot(sort(method_tb_sorted, decreasing = F), las=1, horiz=T, xlim = c(0, 180), cex.names = 0.5)
dev.off()



barplot(sort(method_tb2, decreasing = T), las=2)

method_tb_code_new = names(method_tb2)
method_tb_name =new_method_lutb$Method.name[match(method_tb_code_new, new_method_lutb$`#ID.(as.it.appears.on.the.survey.answers)`)]
names(method_tb2) = method_tb_name

barplot(method_tb2, las=2)

#pdf("output/Main_Method_list.pdf", width = 12, height = 8)
par(mar=c(4,17,4,4))
barplot(sort(method_tb2, F), las=1, cex.names = 0.5, horiz = T, xlab = "# of applications")
dev.off()


# figure out what was not found

method_not_found_idx = which(is.na(match(method_lutb$ID, method_tb_code)))

n=method_lutb$Method.name[method_not_found_idx]
length(method_lutb$Method.name[method_not_found_idx])

#which(str_detect(s3_single$"1.2", pattern ="SolVES"))
# which(str_detect(s3_single$"1.4", pattern ="Solv"))
# s3_single$"1.4"[996]





# # First we get the well-formed answers after splitting
# row_idx = 1
# tmp1.2 = s3_1.2_mainlist_corrected[row_idx,"cr_1.2"]
# method_tmp_v = str_split(tmp1.2, pattern = ";")[[1]]
# method_tmp_v = str_trim(method_tmp_v) # remove trailing spaces
#
#
# method_tmp_cnt = length(method_tmp_v)# how many?



########### 1.3. The number of additional methods..
table(s3_single$"1.3")  # Additional methods
#   0   1   2   3   4   5   6   7   8
# 501 338 175  68  30  11   4   3   1
s3_single[s3_single$"1.3" >= 4, "paperID"]
# [1] 38316 38518 17191 29506 74437 17155 58559 26512 29915 10261  4714 36263
# [13]  8869  3206 35757 34150 69884 54436 20791 59670 23700 35913 62947 44894
# [25]  2198  9727 23185 45593 28465 43314 42378 39619 12597  3734 71934  2248
# [37] 22816 41028 14021 16915  6493 25781 25094  6674 20245 38947 66234 69734
# [49] 20317  7586

# using cut
cut_1.3 = cut(s3_single$"1.3", breaks = c(0, 0.9, 1.5, 2.5, 3.5, max(s3_single$"1.1")))
levels(cut_1.3) = c(0, 1,2,3,"> 4")
cut_1.3

cut_1.3[is.na(cut_1.3)] = 0

pdf("output/Q1.3_NrAdditionalMethods_Pie_3Dec.pdf", width=15, height = 15, pointsize = 20)
png("output/Q1.3_NrAdditionalMethods_Pie_3Dec.png", width = 600, height = 600, pointsize = 20)
pie(table(cut_1.3), main = "Number of additional methods per application", init.angle = 90, col = gray.colors(5, start = 0.9, end = 0.3))
dev.off()

########### 1.4. List of additional methods..
s3_1.2_mainlist_corrected = read.csv(url(s3_1.2_mainlist_corrected_web), header = T)
colnames(s3_1.2_mainlist_corrected)[c(2, 3, 5, 7)] = c("paperID", "appl_ID", "cr_1.2", "cr_1.4")

table(s3_1.2_mainlist_corrected$cr_1.4)

s3_1.2_mainlist_corrected$appl_ID = as.character(s3_1.2_mainlist_corrected$appl_ID)

res_all_1.4 = vector("list", length = nrow(s3_single)) #


# row_idx = 989

for (row_idx in 1:nrow(s3_single)) {

  s3_single_tmp = s3_single[row_idx,]
  s3_1.4_tmp = as.character(s3_single_tmp$"1.4")
  paperID_tmp = s3_single_tmp$paperID

  ###### Replace malformed answers by the corrected data on the web

  s3_cr_tmp = subset(s3_1.2_mainlist_corrected, subset = ((paperID ==paperID_tmp) & (appl_ID == s3_single_tmp$appl_ID)))

  if (nrow(s3_cr_tmp) >= 1 ) {
    # we have corrected data

    if (nrow(s3_cr_tmp) > 1 ) {
      warning(paste0("paperID:", paperID_tmp, " more than one items with the same paper ID and the appl ID!"))
      # print(paste0("paperID:", paperID_tmp))
      cat("row id:", row_idx)
      print(s3_cr_tmp$cr_1.4)
      # stop("more than one items with the same paper ID and the appl ID!")

      s3_cr_tmp = s3_cr_tmp[1, ] # ignore the rows other than the first row
    }

    s3_1.4_tmp = as.character(s3_cr_tmp$cr_1.4)


  } else {
    # do nothing
  }

  method_tmp_v = str_split(s3_1.4_tmp, pattern = ";")[[1]]
  method_tmp_v = str_trim(method_tmp_v) # remove trailing spaces

  # if the values are actually uninformative ones
  method_tmp_v[method_tmp_v==""] = NA
  method_tmp_v[method_tmp_v=="/"] = NA
  method_tmp_v = method_tmp_v[!is.na(method_tmp_v)]

  method_tmp_cnt = length(method_tmp_v)# how many?


  # store the processed information in the container
  res_tmp = data.frame(matrix(data = NA, nrow = method_tmp_cnt, ncol = 6 )) # ID, desc.,note, raw data string if needed
  colnames(res_tmp) = c("PaperID","RowID", "MethodID","Description", "NeedChecking", "RawData")

  if (method_tmp_cnt == 0 ) {
    res_tmp[1,] = c(paperID_tmp, row_idx, NA, "No data entered", "N", s3_1.4_tmp)
    res_all_1.4[[row_idx]] = res_tmp
    next() # continue the loop
  }


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
        res_2 = c(paperID_tmp, row_idx, id_tmp, NA, "N", s3_1.4_tmp) # ID, desc.,NeedChecking?, row data string

      } else {
        # it is null, it means the first element is not a number (means something else like desc. or name of the method)
        res_2 = c(paperID_tmp, row_idx, NA, res_1[1], "Y", s3_1.4_tmp)

      }

    } else if (length(res_1)==2) {

      # method ID and description
      id_tmp = as.numeric(res_1[1])
      desc_tmp = res_1[2]

      if (!is.na(id_tmp)){
        res_2 = c(paperID_tmp, row_idx, id_tmp, desc_tmp, "N", s3_1.4_tmp) # ID, desc.,NeedChecking?, raw data string

      } else {
        # it is null, it means the first element is not a number (means something else like desc. or name of the method)
        res_2 = c(paperID_tmp, row_idx, NA, desc_tmp, "Y", s3_1.4_tmp)

      }



    } else {
      # if it has more than three elements we throw an error for now
      # print(paste0("PaperID:", paperID_tmp, " Row ", row_idx, " has more than two elements: \"", s3_single_tmp, "\""))
      res_2 = c(paperID_tmp, row_idx, NA, NA,  "Y", s3_1.4_tmp)
    }

    # print(method_tmp_idx)
    # print(res_2)

    if (!(res_2[3]  %in% method_lutb$ID)) {
      # if the method is not enlisted, it could be possibly wrong.. (e.g. 2009)
      res_2[5] = "Y"
    }

    res_tmp[method_tmp_idx, ] = res_2
  }


  res_all_1.4[[row_idx]] = res_tmp



}

nrow(s3_single)
length(res_all_1.4)
res_all_1.4[[7]]
res_all_1.4[[8]]
res_all_1.4[[981]]

res_all_1.4_df = do.call("rbind", res_all_1.4)

nrow(res_all_1.4_df)

# warnings (+50) when the first element was not numeric. It is dealt the above.

#######################
table(res_all_1.4_df$NeedChecking)

res_all_1.4_df[,"MethodID"] = factor(as.character(res_all_1.4_df[,"MethodID"]))

# Let's first consider only well-formed values

add_method_tb1 = table(res_all_1.4_df[,"MethodID"])
(add_method_tb1)
sort(add_method_tb1, decreasing = T)

barplot(sort(add_method_tb1, decreasing = T), las=2)

add_method_tb_code = names(add_method_tb1)
add_method_tb_name =method_lutb$Method.name[match(add_method_tb_code, method_lutb$ID )]
names(add_method_tb1) = method_tb_name

barplot(add_method_tb1, las=2)

#pdf("output/Main_Method_list.pdf", width = 12, height = 8)
par(mar=c(4,17,4,4))
barplot(sort(add_method_tb1, F), las=1, cex.names = 0.5, horiz = T, xlab = "# of applications")
dev.off()


# figure out what was not found

add_method_not_found_idx = which(is.na(match(method_lutb$ID, add_method_tb_code)))

method_lutb$Method.name[add_method_not_found_idx]
length(method_lutb$Method.name[add_method_not_found_idx])

#which(str_detect(s3_single$"1.2", pattern ="SolVES"))
# which(str_detect(s3_single$"1.4", pattern ="Solv"))
# s3_single$"1.4"[996]



# papers to look at
add_checklist_v = which(res_all_1.4_df$NeedChecking == "Y")
add_checklist_rowid_v = res_all_1.4_df$RowID[checklist_v]
head(add_checklist_rowid_v)
head(add_checklist_v)
# res_all_df$PaperID[checklist_v ]
# res_all_df$Description[checklist_v ]
# res_all_df$RawData[checklist_v ]

# 15th row in res_all_df (11th row in s3single)
res_all_1.4_df[15,]
s3_single[11,"1.2"]
#


res_needchecking_1.4_df = res_all_1.4_df[res_all_1.4_df$NeedChecking=="Y",]

nrow(res_needchecking_1.4_df)

# see what's the popular cases if checking is needed
nctb = table(res_needchecking_1.4_df$NeedChecking, res_needchecking_1.4_df$Description)
dimnames(nctb)[[2]][order(nctb, decreasing = T)[1:5]]

# see all excluding the above
alltb = table(res_all_1.4_df$NeedChecking, res_all_1.4_df$Description)
dimnames(alltb)[[2]][order(alltb[1,], decreasing = T)[1:5]] # 513 no data entered case



toSave3_1.4 = T # TRUE

if (toSave3_1.4){
  write.xlsx(res_all_1.4_df,file = paste0("output/Step3_1.4_all_", Sys.Date(),".xlsx"))
  write.xlsx(res_needchecking_1.4_df, file=paste0("output/Step3_1.4_needchecking_only_ ", Sys.Date(),".xlsx"))
}




########### 1.5. Goald of combining valuation methods.
summary(s3_single$"1.5")
split_res_l_1.5 = vector("list", length = nrow(s3_single))

for (row_idx in 1:nrow(s3_single)) {

  split_tmp = str_trim(str_split(s3_single$"1.5"[row_idx], pattern = ",")[[1]])
  split_res_l_1.5[[row_idx]] = split_tmp
}

goal_split_v = unlist(split_res_l_1.5)
goal_split_v_fac = factor(goal_split_v)

goal_tb_sorted = sort(table(goal_split_v_fac), decreasing = F)

# put the minor goals together (<5)
goal_tb_sorted_reduced =  c(sum(goal_tb_sorted[goal_tb_sorted<5]), goal_tb_sorted[goal_tb_sorted>=5])
names(goal_tb_sorted_reduced)[1] = "Other/unclear"

goal_tb_sorted_reduced[1] <- goal_tb_sorted_reduced[1] + goal_tb_sorted_reduced[2]

goal_tb_sorted_reduced <- goal_tb_sorted_reduced[-c(2,4)]

# names(goal_tb_sorted_reduced[1]) = "Other/unclear"

pdf("output/Q1.5_GoalCombiningMethods_bar_3Dec.pdf", width=15, height = 5, pointsize = 20)
png("output/Q1.5_GoalCombiningMethods_bar_3Dec.png", width = 1200, height = 400,  pointsize = 20)
par(mar=c(5, 25, 5, 5))
barplot(goal_tb_sorted_reduced, las=1, horiz=T, xlim=c(0, max(goal_tb_sorted_reduced) * 1.2 ), col = gray(0.7), main = "the goal of combining valuation methods", xlab= "Number of applications")
dev.off()
