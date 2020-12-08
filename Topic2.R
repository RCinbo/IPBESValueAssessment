library(stringr)
# library(doSNOW) # foreach()
library(openxlsx)
library(plyr)
library(RColorBrewer)
library(openxlsx)

source("Starting.R")

############Define a color scheme#####################
IPbesdarkgreen <- rgb(92/255, 102/255, 93/255) #5c665d
IPbeslightgreen <- rgb(181/255, 212/255, 141/255) #b5d48d
colfunc <- colorRampPalette(c(rgb(92/255, 102/255, 93/255), rgb(181/255, 212/255, 141/255)))

brewer.YlGnBu <- colorRampPalette(brewer.pal(9, "YlGnBu"), interpolate = "spline")
brewer.BuPu <- colorRampPalette(brewer.pal(9, "BuPu"), interpolate = "spline")
brewer.Greens <- colorRampPalette(brewer.pal(9, "Greens"), interpolate = "spline")


######### USE this function where stringr::functions() used
escapeForStringR = function(x) {
  gsub("([.|()\\^{}+$*?/-]|\\[|\\])", "\\\\\\1", x)
}



######  Topic 2 - Context of application

### 2.1- The application addresses the following spatial scales (multiple possible)
### Their multiple answers are separated by ','

summary(s3_single$"2.1")
# how many are 'multiple scales' ?
levels(s3_single$"2.1")
summary(s3_single$"2.2")


table(s3_single$"2.1")

## Replace the problematic answers
choices_org = c("local \\(incl. village, parish, municipality, town, city\\)",
"district, county",
"administrative region \\(as sub-national\\), \\(incl. province, state\\)",
"cross-national or cross-region Indigenous territories\\/jurisdictions\\/lands",
"cross-national or cross-region protected areas",
"national, federal",
"regional as multi-national",
"continental",
"global"
#"Option 9"
)

choices_alt = c("O1_Local", "O2_District", "O3_Administrative", "O4_CrossNationalIndigenous", "O5_CrossNationalProtected", "O6_National", "07_Regional", "O8_Continental", "O9_Global") # "Other")


choice_org_v = as.character(s3_single$"2.1")
choice_alt_v = choice_org_v

# Replace well-defined options to simple format
for (o_idx in 1:length(choices_org)) {
  choice_alt_v = str_replace_all(choice_alt_v, pattern = choices_org[o_idx], replacement = choices_alt[o_idx])
}
# head(table(choice_org_v))
# table(choice_org_v)

# Split choices
row_idx = 1
library(doMC)
# install.packages("doMC")
split_res_l<-list()

for(row_idx in 1:nrow(s3_single)){

  split_tmp = str_trim(str_split(choice_alt_v[row_idx], pattern = ",")[[1]])
  split_res_l[[row_idx]]<-split_tmp
}

choice_split_v = unlist(split_res_l)
choice_split_v_fac = factor(choice_split_v)

choice_tb_sorted = sort(table(choice_split_v_fac), decreasing = F)

barplot(choice_tb_sorted, log="x", las=1, horiz=T, cex.names = 0.5)

# choice_ordreded = names(choice_tb_sorted)

choice_tb_reduced = (table(choice_split_v_fac))[which (table(choice_split_v_fac)>2)]

barplot(sort(choice_tb_reduced, F), horiz=T, las=1, cex.names=0.5)
barplot(sort(table(choice_split_v_fac), F), log="x", las=1, horiz=T, cex.names = 0.5)


 # split list to data frame

 split_res_df = plyr::ldply(split_res_l, rbind) # automatically decide how many columns should it have

 split_res_df = sapply(split_res_df, FUN = function(x) as.character(x))


# Extract Paper IDs of the choice_malformed

# Be aware of the NA values when counting
other_choice_cnt =  apply(split_res_df, MARGIN = 1, FUN = function(x) length(which(!(x[!is.na(x)] %in% choices_alt)))) # how many choices (non-NA) are not in the given well-formed answers
# length(idx1)
table(other_choice_cnt > 0 ) # 189 studies
cnt_otherchoice = length(which(other_choice_cnt > 0))
# studies including other choices
s3_single$paperID[other_choice_cnt > 0 ]

s3_single_with_other_choices_df = s3_single[other_choice_cnt > 0, c("paperID", "2.1")] # "MF1.key"    "MF2.key"    "MF3.key"    "MF4.key"    "MFA.key"    "MFB.key" )]
#write.xlsx(s3_single_with_other_choices_df, paste0("output/s3_single_with_other_choices_n", cnt_otherchoice, ".xlsx"))


split_cnt_v = sapply(split_res_l, FUN = length)
stopifnot(max(split_cnt_v) == ncol(split_res_df))


# Multi scale studies
table(split_cnt_v)
# split_cnt_v
#    1    2    3    4    5
# 1065   86   10    1    1

#pdf("output/Fig_NumberofScalesPerApp.pdf", width=15, height = 8, pointsize = 12)
barplot(table(split_cnt_v), las= 1, col = IPbeslightgreen, border = IPbeslightgreen, ylim = c(0, 1200), xlab = "# of scales per application", ylab = "# of application")
#dev.off()

pie(table(split_cnt_v), col = brewer.YlGnBu(5), labels = NA)

# Max 5 items
str(split_res_df)
head(split_res_df)
tail(split_res_df)
table(split_cnt_v)
tail(s3_single$"2.1")
table(split_res_df)


s3_single_2.1_comparison_df = cbind(split_res_df, RawData_2.1 = as.character(s3_single$"2.1"))
colnames(s3_single_2.1_comparison_df)[1:5] = paste0("Scale_Split_", 1:5)

#write.xlsx(s3_single_2.1_comparison_df, file = "output/s3_single_2.1_for_comparison.xlsx")

################ Read corrected data (from Raphael)

scale_corrected_df = read.xlsx("Corrected/s3_single_with_other_choices_n189_Raphael_NoComment.xlsx", sheet = 1)
str(scale_corrected_df)

# Identify other answers in the uncorrected data

scale_other_v = choice_alt_v

for (given_idx in 1:length(choices_alt)) {

  scale_other_v = str_replace_all(scale_other_v, pattern = choices_alt[given_idx], replacement = "")
}

scale_other_v = str_trim(str_replace_all(scale_other_v, pattern = "[,;]", replacement = "")) # warning: other answers without comma and semicolon
table(scale_other_v)


scale_other_wospace_v = str_replace_all(scale_other_v, pattern = "[ ]", replacement = "") # remove whitespace
scale_other_idx_2.1 = str_length(scale_other_wospace_v) >= 1 # does it have still more characters?

scale_other_v[scale_other_idx_2.1]
n_other_2.1 = length(which(scale_other_idx_2.1)); n_other_2.1

# Count given and other answers individually

scale_given_detected = sapply(choices_alt, FUN = function(x) str_detect(choice_alt_v, pattern = x))

scale_given_detected_one_na = ifelse(scale_given_detected, yes = 1, no = NA)

scale_given_detected_df = data.frame(s3_single$paperID, s3_single$"2.1", scale_given_detected_one_na, other=NA)
colnames(scale_given_detected_df) = colnames(scale_corrected_df) # Make sure two datasets are in the same order

# merge two datasets using paperID
scale_given_detected_df[match(scale_corrected_df$paperID, scale_given_detected_df$paperID), ] = scale_corrected_df



scale_tb_final = colSums(scale_given_detected_df[,-c(1:2)], na.rm = T)

scale.names = c("Local (incl. village, parish, municipality, town, city)",
                "District, county",
                "Administrative region",
                "Cross-national or cross-region \nIndigenous territories/jurisdictions/lands",
                "Cross-national or cross-region protected areas",
                "National, federal",
                "Regional as multi-national",
                "Continental",
                "Global")

pdf("output/Q2.1_Scale_bar_3Dec.pdf", width= 15, height = 8, pointsize = 20)
# png("output/Q2.1_Scale_bar_3Dec.png", width= 25, height = 15, unit = "cm", res = 400)
par(mar=c(5,20,1,1))
barplot(rev(scale_tb_final[-10]), horiz=T, names.arg = rev(scale.names), las = 1, col = gray(0.7), xlim = c(0, 400), main = "Scale of application", xlab = "Number of applications")
dev.off()


MF_cols = paste0("MF", 1:4, ".key")
s3_single_MF = s3_single[,c("paperID", MF_cols)]


scale_by_mf_df = foreach (scale_idx =  1:9, .combine = "rbind") %do% {
  print(scale.names[scale_idx])
  scale_by_mf = sapply(MF_cols, FUN = function(mf_colname) by(scale_given_detected_df[,scale_idx +2 ], INDICES = s3_single_MF[,mf_colname], FUN = sum, na.rm=T))
  return(scale_by_mf["1",])
}

rownames(scale_by_mf_df) =scale.names
colnames(scale_by_mf_df) = paste0("MF", 1:4)

#pdf("output/Fig_2.1_scale_corrected_MF.pdf", width=15, height = 8, pointsize = 12)

par(mar=c(5,20,1,1))
barplot(t(scale_by_mf_df)[,c(9:1)], names.arg = rev(scale.names), col = viridis(4), las=1, horiz=T)
legend("bottomright", #inset=c(1,0),
       fill=viridis(4),
       legend=paste0("MF", 1:4))

#dev.off()


barplot(scale_by_mf_df, names.arg = paste0("MF", 1:4), col = viridis(9))


# scale_given_cnt= apply(scale_given_detected, MARGIN = 1,  FUN = function(x) (which(x)))
# scale_given_tb = table(unlist(scale_given_cnt))
# names(scale_given_tb) = choices_org
#
# scale_all_tb= c(Other = n_other_2.1, sort(scale_given_tb, decreasing = F))
#
# par(mar=c(5,20,1,1))
# barplot(scale_all_tb,  las=1, horiz=T, col = IPbeslightgreen, border =IPbeslightgreen, xlim = c(0, max(habitat_all_tb) *1.1))
# #dev.off()


### 2.2 - The application assesses the following habitats (multiple possible)
summary(s3_single$"2.2")
# write.xlsx(table(s3_single$"2.2"), file = "output/summary_s3_single_2.2_raw.xlsx")

# Multiple answers are separated by ','

wetland_org = c("Wetlands \\– peatslands, mires, bogs")
wetland_alt = "Wetlands"
wetland_org_v = as.character(s3_single$"2.2")
wetland_alt_v = wetland_org_v

wetland_alt_v = str_replace_all(wetland_alt_v, pattern = wetland_org, replacement = wetland_alt)

split_res_l_2.2 = vector("list", length = nrow(s3_single))

for (row_idx in 1:nrow(s3_single)) {

  split_tmp = str_trim(str_split(wetland_alt_v[row_idx], pattern = ",")[[1]])
  split_res_l_2.2[[row_idx]] = split_tmp
}

habitat_split_v = unlist(split_res_l_2.2)
habitat_split_v_fac = factor(habitat_split_v)

habitat_tb_sorted = sort(table(habitat_split_v_fac), decreasing = F)
barplot(habitat_tb_sorted, log="x", las=1, horiz=T, cex.names = 0.5)

habitat_tb_reduced = (table(habitat_split_v_fac))[which (table(habitat_split_v_fac)>5)]
barplot(sort(habitat_tb_reduced, F), horiz=T, las=1, cex.names=0.5)

# don't use escaping! as we don't use regexp-like functions below
habitat_given_answer = c( "Forests",
"Savannah",
"Deserts",
"Grasslands",
"Shrublands",
"Wetlands", #\\- peatlands, mires, bogs",
"Mountain habitats",
"Urban / Semi-urban",
"Cultivated areas",
"Aquaculture",
"Inland surface water and water bodies / freshwater",
"Coastal areas",
"Deep sea", "unclear", "Irrelevant"
)


# split list to data frame
split_res_2.2_df = plyr::ldply(split_res_l_2.2, rbind) # automatically decide how many columns should it have

split_res_2.2_df = sapply(split_res_2.2_df, FUN = function(x) as.character(x))

names(table(split_res_2.2_df))

# Extract Paper IDs of the choice_malformed

# Be aware of the NA values when counting
habitat_other_choice_cnt = apply(split_res_2.2_df, MARGIN = 1, FUN = function(x) length(which(!(x[!is.na(x)] %in% habitat_given_answer)))) # how many choices (non-NA) are not in the given well-formed answers

table(habitat_other_choice_cnt > 0 ) # 127 studies
cnt_otherchoice_habitat = length(which(habitat_other_choice_cnt > 0))
# studies including other choices
s3_single$paperID[habitat_other_choice_cnt > 0 ]


other_answers_habitat =  apply(split_res_2.2_df, MARGIN = 1, FUN = function(x) x[(which(!(x[!is.na(x)] %in% habitat_given_answer)))]) # how many choices (non-NA) are not in the given well-formed answers
other_answers_habitat = unlist(other_answers_habitat)

other_answers_habitat_tb = table(other_answers_habitat)
table(other_answers_habitat_tb)
other_answers_habitat_tb[other_answers_habitat_tb>2]

s3_single_with_other_habitat_df = s3_single[habitat_other_choice_cnt > 0, c("paperID", "2.2")]

head(s3_single_with_other_habitat_df)

# write.xlsx(s3_single_with_other_habitat_df, paste0("output/s3_single_with_other_habitat_n", cnt_otherchoice_habitat, ".xlsx"))



################ Read corrected data (from Sander)

habitat_corrected_df = read.xlsx("Corrected/s3_single_with_other_habitat_n127-corrSJ.xlsx", sheet = 1)
str(habitat_corrected_df)

# Identify other answers in the uncorrected data

habitat_other_v = wetland_alt_v
for (given_idx in 1:length(habitat_given_answer)) {

  habitat_other_v = str_replace_all(habitat_other_v, pattern = habitat_given_answer[given_idx], replacement = "")
}

habitat_other_v = str_trim(str_replace_all(habitat_other_v, pattern = "[,;]", replacement = "")) # warning: other answers without comma and semicolon
table(habitat_other_v)


habitat_other_wospace_v = str_replace_all(habitat_other_v, pattern = "[ ]", replacement = "") # remove whitespace
habitat_other_idx_2.2 = str_length(habitat_other_wospace_v) >= 1 # does it have still more characters?

wetland_alt_v[habitat_other_idx_2.2]
n_other_2.2 = length(which(habitat_other_idx_2.2)); n_other_2.2
# other_df_6.1= cbind(paperID= s3_single$paperID, OTHER_ANSWER_6_1 = wellbeing_indicator_other_v,ANSWER_RAW=as.character(s3_single$"6.1") )[other_idx_6.1,]
# write.xlsx(other_df_6.1, file = "output/6.1_otheranswers.xlsx")

# Count given and other answers individually

habitat_given_detected = sapply(habitat_given_answer, FUN = function(x) str_detect(wetland_alt_v, pattern = x))

habitat_given_detected_one_na = ifelse(habitat_given_detected, yes = 1, no = NA)

habitat_given_detected_df = data.frame(s3_single$paperID, s3_single$"2.2", habitat_given_detected_one_na, other=NA)
colnames(habitat_given_detected_df) = colnames(habitat_corrected_df) # Make sure two datasets are in the same order

# merge two datasets using paperID
habitat_given_detected_df[match(habitat_corrected_df$paperID, habitat_given_detected_df$paperID), ] = habitat_corrected_df

habitat_tb_final = colSums(habitat_given_detected_df[,-c(1:2)], na.rm = T)

barplot(habitat_tb_final, horiz=T, las=2)
names(habitat_tb_final)
habitat_tb_final[16] <- habitat_tb_final[16] + habitat_tb_final[14] # other + unclear
habitat.name.plot <- c("Forests",
                      "Savannah",
                      "Deserts",
                      "Grasslands",
                      "Shrublands",
                      "Wetlands",
                      "Mountain habitats",
                      "Urban / Semi-urban",
                      "Cultivated areas",
                      "Aquaculture",
                      "Inland surface water and water bodies / freshwater",
                       "Coastal areas",
                       "Deep sea",
                       "unclear",
                      "No habitat assessed",
                       "Other / Unclear")
#
# names(habitat_tb_final)[15] <- "No habitat assessed"
#
# names(habitat_tb_final[16]) <- "Other and unclear"
names(habitat_tb_final) <- habitat.name.plot
habitat_tb_final <-  habitat_tb_final[-(14)]


pdf("output/Q2.2_Habitat_bar_3Dec.pdf", width=15, height = 8, pointsize = 20)
 png(file="output/Q2.2_Habitat_bar_3Dec.png", width=25, height = 15, units= "cm", res = 400)
par(mar=c(5,20,1,1))
barplot(rev(habitat_tb_final), las=1, horiz=T, col = gray(0.7), xlim = c(0, max(habitat_tb_final) *1.1), xlab = "Number of applications", main = "The application assesses the following habitats ")
dev.off()

#pdf("output/Fig_habitat_corrected_30Oct.pdf", width=15, height = 8, pointsize = 12)
#png("output/Fig_habitat_corrected_30Oct.png", width=800, height = 600, pointsize = 12)

# par(mar=c(5,20,1,1))
# barplot(habitat_all_tb[c(8,1:7,9:15)],  las=1, horiz=T, col = IPbeslightgreen, border =IPbeslightgreen, xlim = c(0, max(habitat_all_tb) *1.1))
#dev.off()


### 2.7 - The application collected data with the following temporal frequency: (only one answer)
summary(s3_single$"2.7")
table(s3_single$"2.7")
barplot(table(s3_single$"2.7"))

pdf("output/Q2.7_Temporal Frequency_bar_3Dec.pdf", width=8, height = 8, pointsize = 20)
png("output/Q2.7_Temporal Frequency_bar_3Dec.png", width=15, height = 8, unit = "cm", res = 400)
barplot(table(s3_single$"2.7"), las = 1, col = gray(0.7), ylab = "Number of applications", xlab = "Temporal frequency")
dev.off()

pdf("output/Q2.7_Temporal Frequency_pie_3Dec.pdf", width=8, height = 8, pointsize = 20)
# png("output/Q2.7_Temporal Frequency_pie_3Dec.png", width=10, height = 8, unit = "cm", res = 400)
pie(table(s3_single$"2.7"), init.angle = 90, col = gray.colors(3, start = 0.9, end = 0.3), main = "Temporal frenquency")
dev.off()


### 2.8 - The application assesses values change over time:
summary(s3_single$"2.8")

pdf("output/Q2.8_overTime_pie_3Dec.pdf", width=10, height = 10, pointsize = 20)
# png("output/Q2.8_overTime_pie_3Dec.png", width=14, height = 10, unit= "cm", res = 400)
pie(table(s3_single$"2.8"), labels = c("No", "Yes"), main = "The application assesses values change over time", init.angle = 90, col = gray.colors(2, start = 0.9, end = 0.3))
dev.off()



### 2.9 The application assesses following temporal changes :
summary(s3_single$"2.9")
sort(table(s3_single$"2.9"))

split_res_l_2.9 = vector("list", length = nrow(s3_single))

for (row_idx in 1:nrow(s3_single)) {

  split_tmp = str_trim(str_split(as.character(s3_single$"2.9")[row_idx], pattern = ",")[[1]])
  split_res_l_2.9[[row_idx]] = split_tmp
}

time_split_v = unlist(split_res_l_2.9)
time_split_v_fac = factor(time_split_v)
time_tb_sorted = sort(table(time_split_v_fac), decreasing = F)

pdf("output/Q2.9_TemporalChange_bar_3Dec.pdf", width=12, height = 8, pointsize = 20)
png("output/Q2.9_TemporalChange_bar_3Dec.png", width=20, height = 15, unit = "cm", res = 400)
par(mar=c(5,20,5,5))
barplot(sort(time_tb_sorted, F)[-(9)], horiz=T, las=1, xlim = c(0, 100), col = gray(0.7), main = "Temporal Changes", xlab = "Number of applications")
dev.off()


### 2.10 There are multiple classifications of 'what is valued'. We want to know how these fit in the IPBES categories. The application assesses the following 'targets of valuation' regarding Nature Itself (multiple possible)
summary(s3_single$"2.10")
# Multiple answers are separated by ','
split_res_l_2.10 = vector("list", length = nrow(s3_single))

for (row_idx in 1:nrow(s3_single)) {

  split_tmp = str_trim(str_split(as.character(s3_single$"2.10")[row_idx], pattern = ",")[[1]])
  split_res_l_2.10[[row_idx]] = split_tmp
}

nature_split_v = unlist(split_res_l_2.10)
nature_split_v_fac = factor(nature_split_v)
nature_tb_sorted = sort(table(nature_split_v_fac), decreasing = F)
nature_tb_reduced = (table(nature_split_v_fac))[which (table(nature_split_v_fac)>1)]
barplot(sort(nature_tb_reduced, F), horiz=T, las=1, cex.names=0.5)

# split list to data frame
split_res_2.10_df = plyr::ldply(split_res_l_2.10, rbind) # automatically decide how many columns should it have

split_res_2.10_df = sapply(split_res_2.10_df, FUN = function(x) as.character(x))

nature_given_answers = c(
  "Individual organisms \\(e.g. the sacred village tree\\)",
  "Biophysical assemblages",
  "Biophysical processes",
  "Biodiversity",
  "Maintenance of options",
  "none"
)


# Extract Paper IDs of the choice_malformed

# Be aware of the NA values when counting
nature_other_choice_cnt =  apply(split_res_2.10_df, MARGIN = 1, FUN = function(x) length(which(!(x[!is.na(x)] %in% nature_given_answers)))) # how many choices (non-NA) are not in the given well-formed answers
# length(idx1)
table(nature_other_choice_cnt > 0 ) # 20 studies
cnt_otherchoice_nature = length(which(nature_other_choice_cnt > 0))
# studies including other choices
s3_single$paperID[nature_other_choice_cnt > 0 ]
s3_single_with_other_nature_df = s3_single[nature_other_choice_cnt > 0, c("paperID", "2.10")]

#write.xlsx(s3_single_with_other_nature_df, paste0("output/s3_single_with_other_nature_n", cnt_otherchoice_nature, ".xlsx"))


# Identify other answers in the uncorrected data

NCP_nature_other_v = as.character(s3_single$"2.10")

for (given_idx in 1:length(nature_given_answers)) {
  NCP_nature_other_v = str_replace_all(NCP_nature_other_v, pattern = nature_given_answers[given_idx], replacement = "")
}

NCP_nature_other_v = str_trim(str_replace_all(NCP_nature_other_v, pattern = "[,;]", replacement = "")) # warning: other answers without comma and semicolon
table(NCP_nature_other_v)

NCP_nature_other_wospace_v = str_replace_all(NCP_nature_other_v, pattern = "[ ]", replacement = "") # remove whitespace
NCP_nature_other_idx_2.10 = str_length(NCP_nature_other_wospace_v) >= 1 # does it have still more characters?

NCP_nature_other_v[NCP_nature_other_idx_2.10]
n_other_2.10 = length(which(NCP_nature_other_idx_2.10)); n_other_2.10
# other_df_6.1= cbind(paperID= s3_single$paperID, OTHER_ANSWER_6_1 = wellbeing_indicator_other_v,ANSWER_RAW=as.character(s3_single$"6.1") )[other_idx_6.1,]
# write.xlsx(other_df_6.1, file = "output/6.1_otheranswers.xlsx")

# Count given and other answers individually

NCP_nature_given_detected = sapply(nature_given_answers, FUN = function(x) str_detect(as.character(s3_single$"2.10"), pattern = x))

NCP_nature_given_detected_one_na = ifelse(NCP_nature_given_detected, yes = 1, no = NA)

NCP_nature_given_detected_df = data.frame(s3_single$paperID, s3_single$"2.10", NCP_nature_given_detected_one_na, other=NA)
NCP_nature_given_detected_df<- NCP_nature_given_detected_df[-c(8:9)]

colnames(NCP_nature_given_detected_df)

NCP_nature_given_detected_df[is.na(NCP_nature_given_detected_df)] = 0

colSums(NCP_nature_given_detected_df[-c(1:2)])


MF_cols = paste0("MF", 1:4, ".key")
s3_single_MF = s3_single[,c("paperID", MF_cols)]


# Sum all the nature given answers
NCP_nature_sum_v =rowSums(NCP_nature_given_detected_df[, 3:7], na.rm=T)

# Tabulate by MF
NCP_nature_by_mf_df = sapply(MF_cols, FUN = function(mf_colname) tapply(NCP_nature_sum_v, INDEX = s3_single_MF[,mf_colname], FUN = sum, na.rm=T))
NCP_nature_by_mf_df = NCP_nature_by_mf_df["1", ]

names(NCP_nature_by_mf_df) = c("MF1", "MF2", "MF3", "MF4")

barplot(NCP_nature_by_mf_df)

pdf("output/Q2.10_Nature_pie_3Dec.pdf", width=15, height = 15, pointsize = 20)
pie(NCP_nature_by_mf_df, col = viridis(4), main = "NCP Nature", init.angle = 90, labels = NA)
dev.off()


# dim(NCP_nature_given_detected_df)
#
# #
# NCP_nature_given_detected_df[match(CorrectedOtherTF_df$paperID, NCP_nature_given_detected_df$s3_single.paperID), 3:8] = CorrectedOtherTF_df[, c(3:8)]

#
# NCP_list[match(NCP_nature_corrected_df$paperID, NCP_nature_full$paperID), ] = NCP_nature_corrected_df
#
# # merge two datasets using paperID
# NCP_nature_full[match(NCP_nature_corrected_df$paperID, NCP_nature_full$paperID), ] = NCP_nature_corrected_df
#
# NCP_nature_full_final = colSums(NCP_nature_full[,-c(1:2)], na.rm = T)
#
# barplot(NCP_nature_full_final, horiz=T, las=2)
#
# #pdf("output/Fig_habitat_corrected_25Oct.pdf", width=15, height = 8, pointsize = 12)
# #png("output/Fig_habitat_corrected_25Oct.png", width=800, height = 600, pointsize = 12)
#
# par(mar=c(5,20,1,1))
# barplot(NCP_nature_full_final,  las=1, horiz=T, col = IPbeslightgreen, border =IPbeslightgreen, xlim = c(0, max(NCP_nature_full_final) *1.1))
# #dev.off()
#


### 2.11 There are multiple classifications of 'what is valued'. We want to know how these fit in the IPBES categories. The application assesses the following 'targets of valuation' regarding REgulating Nature Contributions To People (multiple possible)
summary(s3_single$"2.11")


# Multiple answers are separated by ','
flow_org = c("Regulation of freshwater quantity, flow and timing \\(\\!includes water provision\\!\\)", "Formation, protection and decontamination of soils and sediments")

flow_alt = c("Regulation of freshwater quantity", "Soil formation")
flow_org_v = as.character(s3_single$"2.11")
flow_alt_v = flow_org_v


# Replace well-defined options to simple format
for (o_idx in 1:length(flow_org)) {
  flow_alt_v = str_replace_all(flow_alt_v, pattern = flow_org[o_idx], replacement = flow_alt[o_idx])
}


split_res_l_2.11 = vector("list", length = nrow(s3_single))
for (row_idx in 1:nrow(s3_single)) {

  split_tmp = str_trim(str_split(flow_alt_v[row_idx], pattern = ",")[[1]])
  split_res_l_2.11[[row_idx]] = split_tmp
}

regul_split_v = unlist(split_res_l_2.11)
regul_split_v_fac = factor(regul_split_v)
regul_tb_sorted = sort(table(regul_split_v_fac), decreasing = F)
regul_tb_reduced = (table(regul_split_v_fac))[which (table(regul_split_v_fac)>2)]
barplot(sort(regul_tb_reduced, F), horiz=T, las=1, cex.names=0.5)

split_res_2.11_df = plyr::ldply(split_res_l_2.11, rbind) # automatically decide how many columns should it have

split_res_2.11_df = sapply(split_res_2.11_df, FUN = function(x) as.character(x))


regul_given_answer = c(
  "Maintenance of options",
  "Habitat creation and maintenance",
  "Pollination and dispersal of seeds and other propagules",
  "Regulation of air quality",
  "Regulation of climate",
  "Regulation of ocean acidification",
  "Regulation of freshwater quantity", #Regulation of freshwater quantity, flow and timing (!includes water provision!),
  "Regulation of freshwater and coastal water quality",
  "Soil formation", #Formation, protection and decontamination of soils and sediments
  "Regulation of hazards and extreme events",
  "None"
)


# Be aware of the NA values when counting
regul_other_choice_cnt =  apply(split_res_2.11_df, MARGIN = 1, FUN = function(x) length(which(!(x[!is.na(x)] %in% regul_given_answer)))) # how many choices (non-NA) are not in the given well-formed answers
# length(idx1)
table(regul_other_choice_cnt > 0 ) # 46 studies
cnt_otherchoice_regul = length(which(regul_other_choice_cnt > 0))
# studies including other choices
s3_single$paperID[regul_other_choice_cnt > 0 ]
s3_single_with_other_regul_df = s3_single[regul_other_choice_cnt > 0, c("paperID", "2.11")]


# write.xlsx(s3_single_with_other_regul_df, paste0("output/s3_single_with_other_regul_n", cnt_otherchoice_regul, ".xlsx"))




# Identify other answers in the uncorrected data

NCP_regul_other_v = flow_alt_v

for (given_idx in 1:length(regul_given_answer)) {
  NCP_regul_other_v = str_replace_all(NCP_regul_other_v, pattern = regul_given_answer[given_idx], replacement = "")
}

NCP_regul_other_v = str_trim(str_replace_all(NCP_regul_other_v, pattern = "[,;]", replacement = "")) # warning: other answers without comma and semicolon
table(NCP_regul_other_v)

NCP_regul_other_wospace_v = str_replace_all(NCP_regul_other_v, pattern = "[ ]", replacement = "") # remove whitespace
NCP_regul_other_idx_2.11 = str_length(NCP_regul_other_wospace_v) >= 1 # does it have still more characters?

NCP_regul_other_v[NCP_regul_other_idx_2.11]
n_other_2.11 = length(which(NCP_regul_other_idx_2.11)); n_other_2.11

# Count given and other answers individually

NCP_regul_given_detected = sapply(regul_given_answer, FUN = function(x) str_detect(flow_alt_v, pattern = x))

NCP_regul_given_detected_one_na = ifelse(NCP_regul_given_detected, yes = 1, no = NA)

NCP_regul_given_detected_df = data.frame(s3_single$paperID, s3_single$"2.11", NCP_regul_given_detected_one_na, other=NA)
NCP_regul_given_detected_df <- NCP_regul_given_detected_df[-c(13:14)]

colnames(NCP_regul_given_detected_df)

NCP_regul_given_detected_df[is.na(NCP_regul_given_detected_df)] = 0

colSums(NCP_regul_given_detected_df[-c(1:2)])

## create a full matrix

# colnames(NCP_nature_corrected_df)

ncol(NCP_regul_given_detected_df)
# nrow(NCP_nature_given_detected_df) ; nrow(NCP_regul_given_detected_df)

# NCP_list <- matrix(NA, ncol = ncol(NCP_nature_corrected_df)-12, nrow = nrow(NCP_regul_given_detected_df))
#
# colnames(NCP_list[c(1:8)]) ) <- colnames(NCP_nature_corrected_df)[c(1:8)]
#
# NCP_nature_full <- cbind(NCP_nature_given_detected_df, NCP_list)
# NCP_nature_full[is.na(NCP_nature_full)] = 0
#
# dim(NCP_nature_full)
# colnames(NCP_nature_full[,c(7:16)])
#

MF_cols = paste0("MF", 1:4, ".key")
s3_single_MF = s3_single[,c("paperID", MF_cols)]

# Sum all the nature given answers
NCP_regul_sum_v =rowSums(NCP_regul_given_detected_df[, 3:12], na.rm=T)

# Tabulate by MF
NCP_regul_by_mf_df = sapply(MF_cols, FUN = function(mf_colname) tapply(NCP_regul_sum_v, INDEX = s3_single_MF[,mf_colname], FUN = sum, na.rm=T))
NCP_regul_by_mf_df = NCP_regul_by_mf_df["1", ]

names(NCP_regul_by_mf_df) = c("MF1", "MF2", "MF3", "MF4")

barplot(NCP_regul_by_mf_df)

pdf("output/Q2.11_Regulating_Pie_3Dec.pdf", width=10, height = 10, pointsize = 20)
pie(NCP_regul_by_mf_df, col = viridis(4), main = "Regulating Nature Contribution to People", init.angle = 90, labels = NA)
dev.off()

# pie(NCP_nature_by_mf_df, col = viridis(4), main = "NCP Nature")





### 2.12 There are multiple classifications of 'what is valued'. We want to know how these fit in the IPBES categories. The application assesses the following 'targets of valuation' regarding Material Nature Contributions To People (multiple possible)
summary(s3_single$"2.12")

medi_org = c("Medicinal, biochemical and genetic resources")
medi_alt = "Medicinal resources"
medi_org_v = as.character(s3_single$"2.12")
medi_alt_v = medi_org_v

medi_alt_v = str_replace_all(medi_alt_v, pattern = medi_org, replacement = medi_alt)

# Multiple answers are separated by ','
split_res_l_2.12 = vector("list", length = nrow(s3_single))
for (row_idx in 1:nrow(s3_single))  {

  split_tmp = str_trim(str_split(medi_alt_v[row_idx], pattern = ",")[[1]])
  split_res_l_2.12[[row_idx]] = split_tmp
}

material_split_v = unlist(split_res_l_2.12)
material_split_v_fac = factor(material_split_v)
material_tb_sorted = sort(table(material_split_v_fac), decreasing = F)
material_tb_reduced = (table(material_split_v_fac))[which (table(material_split_v_fac)>1)]
barplot(sort(material_tb_reduced, F), horiz=T, las=1, cex.names=0.5)


split_res_2.12_df = plyr::ldply(split_res_l_2.12, rbind) # automatically decide how many columns should it have

split_res_2.12_df = sapply(split_res_2.12_df, FUN = function(x) as.character(x))

material_given_answer = c(
  "Energy",
  "Food and feed",
  "Materials",
  "Medicinal resources",
  "None"
)


# Be aware of the NA values when counting
material_other_choice_cnt =  apply(split_res_2.12_df, MARGIN = 1, FUN = function(x) length(which(!(x[!is.na(x)] %in% material_given_answer)))) # how many choices (non-NA) are not in the given well-formed answers
# length(idx1)
table(material_other_choice_cnt > 0 ) # 25 studies
cnt_otherchoice_material = length(which(material_other_choice_cnt > 0))
# studies including other choices
s3_single$paperID[material_other_choice_cnt > 0 ]
s3_single_with_other_material_df = s3_single[material_other_choice_cnt > 0, c("paperID", "2.12")]


#write.xlsx(s3_single_with_other_material_df, paste0("output/s3_single_with_other_material_n", cnt_otherchoice_material, ".xlsx"))


NCP_material_other_v = medi_alt_v

for (given_idx in 1:length(material_given_answer)) {
  NCP_material_other_v = str_replace_all(NCP_material_other_v, pattern = material_given_answer[given_idx], replacement = "")
}

NCP_material_other_v = str_trim(str_replace_all(NCP_material_other_v, pattern = "[,;]", replacement = "")) # warning: other answers without comma and semicolon
table(NCP_material_other_v)

NCP_material_other_wospace_v = str_replace_all(NCP_material_other_v, pattern = "[ ]", replacement = "") # remove whitespace
NCP_material_other_idx_2.12 = str_length(NCP_material_other_wospace_v) >= 1 # does it have still more characters?

NCP_material_other_v[NCP_material_other_idx_2.12]
n_other_2.12 = length(which(NCP_material_other_idx_2.12)); n_other_2.12

# Count given and other answers individually

NCP_material_given_detected = sapply(material_given_answer, FUN = function(x) str_detect(medi_alt_v, pattern = x))

NCP_material_given_detected_one_na = ifelse(NCP_material_given_detected, yes = 1, no = NA)

NCP_material_given_detected_df = data.frame(s3_single$paperID, s3_single$"2.12", NCP_material_given_detected_one_na, other=NA)
NCP_material_given_detected_df <- NCP_material_given_detected_df[-c(7:8)]

colnames(NCP_material_given_detected_df)

NCP_material_given_detected_df[is.na(NCP_material_given_detected_df)] = 0

colSums(NCP_material_given_detected_df[-c(1:2)])


MF_cols = paste0("MF", 1:4, ".key")
s3_single_MF = s3_single[,c("paperID", MF_cols)]

# Sum all the nature given answers
NCP_material_sum_v =rowSums(NCP_material_given_detected_df[, 3:6], na.rm=T)

# Tabulate by MF
NCP_material_by_mf_df = sapply(MF_cols, FUN = function(mf_colname) tapply(NCP_material_sum_v, INDEX = s3_single_MF[,mf_colname], FUN = sum, na.rm=T))
NCP_material_by_mf_df = NCP_material_by_mf_df["1", ]

names(NCP_material_by_mf_df) = c("MF1", "MF2", "MF3", "MF4")

barplot(NCP_material_by_mf_df)

pdf("output/Q2.12_Material_pie_3Dec.pdf", width=10, height = 10, pointsize = 20)
pie(NCP_material_by_mf_df, col = viridis(4), main = "Material Nature Contribution to People", init.angle = 90, labels = NA)
dev.off()


### 2.13 There are multiple classifications of 'what is valued'. We want to know how these fit in the IPBES categories. The application assesses the following 'targets of valuation' regarding non-material Nature Contributions To People (multiple possible)
summary(s3_single$"2.13")
# Multiple answers are separated by ','
split_res_l_2.13 = vector("list", length = nrow(s3_single))

for (row_idx in 1:nrow(s3_single))  {

  split_tmp = str_trim(str_split(s3_single$"2.13"[row_idx], pattern = ",")[[1]])
  split_res_l_2.13[[row_idx]] = split_tmp
}

non_material_split_v = unlist(split_res_l_2.13)
non_material_split_v_fac = factor(non_material_split_v)
non_material_tb_sorted = sort(table(non_material_split_v_fac), decreasing = F)
non_material_tb_reduced = (table(non_material_split_v_fac))[which (table(non_material_split_v_fac)>1)]
barplot(sort(non_material_tb_reduced, F), horiz=T, las=1, cex.names=0.5)


split_res_2.13_df = plyr::ldply(split_res_l_2.13, rbind) # automatically decide how many columns should it have

split_res_2.13_df = sapply(split_res_2.13_df, FUN = function(x) as.character(x))

non_material_given_answer = c(
  "Learning and inspiration",
  "Physical and psychological experiences",
  "Supporting identities",
  "None"
)


# Be aware of the NA values when counting
non_material_other_choice_cnt =  apply(split_res_2.13_df, MARGIN = 1, FUN = function(x) length(which(!(x[!is.na(x)] %in% non_material_given_answer)))) # how many choices (non-NA) are not in the given well-formed answers
# length(idx1)
table(non_material_other_choice_cnt > 0 ) # 22 studies
cnt_otherchoice_non_material = length(which(non_material_other_choice_cnt > 0))
# studies including other choices
s3_single$paperID[non_material_other_choice_cnt > 0 ]
s3_single_with_other_non_material_df = s3_single[non_material_other_choice_cnt > 0, c("paperID", "2.13")]


#write.xlsx(s3_single_with_other_non_material_df, paste0("output/s3_single_with_other_non_material_n", cnt_otherchoice_non_material, ".xlsx"))


NCP_non_material_other_v = as.character(s3_single$"2.13")

for (given_idx in 1:length(non_material_given_answer)) {
  NCP_non_material_other_v = str_replace_all(NCP_non_material_other_v, pattern = non_material_given_answer[given_idx], replacement = "")
}

NCP_non_material_other_v = str_trim(str_replace_all(NCP_non_material_other_v, pattern = "[,;]", replacement = "")) # warning: other answers without comma and semicolon
table(NCP_non_material_other_v)

NCP_non_material_other_wospace_v = str_replace_all(NCP_non_material_other_v, pattern = "[ ]", replacement = "") # remove whitespace
NCP_non_material_other_idx_2.13 = str_length(NCP_non_material_other_wospace_v) >= 1 # does it have still more characters?

NCP_non_material_other_v[NCP_non_material_other_idx_2.13]
n_other_2.13 = length(which(NCP_non_material_other_idx_2.13)); n_other_2.13

# Count given and other answers individually

NCP_non_material_given_detected = sapply(non_material_given_answer, FUN = function(x) str_detect(as.character(s3_single$"2.13"), pattern = x))

NCP_non_material_given_detected_one_na = ifelse(NCP_non_material_given_detected, yes = 1, no = NA)

NCP_non_material_given_detected_df = data.frame(s3_single$paperID, s3_single$"2.13", NCP_non_material_given_detected_one_na, other=NA)
colnames(NCP_non_material_given_detected_df)
NCP_non_material_given_detected_df <- NCP_non_material_given_detected_df[-c(6:7)]

colnames(NCP_non_material_given_detected_df)

NCP_non_material_given_detected_df[is.na(NCP_non_material_given_detected_df)] = 0

colSums(NCP_non_material_given_detected_df[-c(1:2)])


MF_cols = paste0("MF", 1:4, ".key")
s3_single_MF = s3_single[,c("paperID", MF_cols)]

# Sum all the nature given answers
NCP_non_material_sum_v =rowSums(NCP_non_material_given_detected_df[, 3:5], na.rm=T)

# Tabulate by MF
NCP_non_material_by_mf_df = sapply(MF_cols, FUN = function(mf_colname) tapply(NCP_non_material_sum_v, INDEX = s3_single_MF[,mf_colname], FUN = sum, na.rm=T))
NCP_non_material_by_mf_df = NCP_non_material_by_mf_df["1", ]

names(NCP_non_material_by_mf_df) = c("MF1", "MF2", "MF3", "MF4")

barplot(NCP_non_material_by_mf_df)

 # pdf("output/Fig_non_material_MF_12Nov.pdf", width=10, height = 10, pointsize = 12)
pie(NCP_non_material_by_mf_df, col = viridis(4), main = "Non Material Nature Contribution to People", init.angle = 90, labels = NA)
 # dev.off()




### 2.14 There are multiple classifications of 'what is valued'. We want to know how these fit in the IPBES categories. The application assesses the following 'targets of valuation' regarding People's quality of life (multiple possible)
summary(s3_single$"2.14")
# Multiple answers are separated by ','
split_res_l_2.14 = vector("list", length = nrow(s3_single))

for (row_idx in 1:nrow(s3_single))  {

  split_tmp = str_trim(str_split(s3_single$"2.14"[row_idx], pattern = ",")[[1]])
  split_res_l_2.14[[row_idx]] = split_tmp
}

QoL_split_v = unlist(split_res_l_2.14)
QoL_split_v_fac = factor(QoL_split_v)
QoL_tb_sorted = sort(table(QoL_split_v_fac), decreasing = F)
QoL_tb_reduced = (table(QoL_split_v_fac))[which (table(QoL_split_v_fac)>1)]
barplot(sort(QoL_tb_reduced, F), horiz=T, las=1, cex.names=0.5)

split_res_2.14_df = plyr::ldply(split_res_l_2.14, rbind) # automatically decide how many columns should it have

split_res_2.14_df = sapply(split_res_2.14_df, FUN = function(x) as.character(x))

QoL_given_answer = c(
  "Living well in harmony with nature",
  "Identity and Autonomy",
  "Spirituality and Religions",
  "Art and Cultural heritage",
  "Sustainability and Resilience",
  "Diversity and Options",
  "Governance and Justice",
  "Health and Wellbeing",
  "Education and Knowledge",
  "Good social relations",
  "Security and Livelihoods",
  "None"
)


# Be aware of the NA values when counting
QoL_other_choice_cnt =  apply(split_res_2.14_df, MARGIN = 1, FUN = function(x) length(which(!(x[!is.na(x)] %in% QoL_given_answer)))) # how many choices (non-NA) are not in the given well-formed answers
# length(idx1)
table(QoL_other_choice_cnt > 0 ) # 29 studies
cnt_otherchoice_QoL = length(which(QoL_other_choice_cnt > 0))
# studies including other choices
s3_single$paperID[QoL_other_choice_cnt > 0 ]
s3_single_with_other_QoL_df = s3_single[QoL_other_choice_cnt > 0, c("paperID", "2.14")]


#write.xlsx(s3_single_with_other_QoL_df, paste0("output/s3_single_with_other_QoL_n", cnt_otherchoice_QoL, ".xlsx"))

NCP_QoL_other_v = as.character(s3_single$"2.14")

for (given_idx in 1:length(QoL_given_answer)) {
  NCP_QoL_other_v = str_replace_all(NCP_QoL_other_v, pattern = QoL_given_answer[given_idx], replacement = "")
}

NCP_QoL_other_v = str_trim(str_replace_all(NCP_QoL_other_v, pattern = "[,;]", replacement = "")) # warning: other answers without comma and semicolon
table(NCP_QoL_other_v)

NCP_QoL_other_wospace_v = str_replace_all(NCP_QoL_other_v, pattern = "[ ]", replacement = "") # remove whitespace
NCP_QoL_other_idx_2.14 = str_length(NCP_QoL_other_wospace_v) >= 1 # does it have still more characters?

NCP_QoL_other_v[NCP_QoL_other_idx_2.14]
n_other_2.14 = length(which(NCP_QoL_other_idx_2.14)); n_other_2.14

# Count given and other answers individually

NCP_QoL_given_detected = sapply(QoL_given_answer, FUN = function(x) str_detect(as.character(s3_single$"2.14"), pattern = x))

NCP_QoL_given_detected_one_na = ifelse(NCP_QoL_given_detected, yes = 1, no = NA)

NCP_QoL_given_detected_df = data.frame(s3_single$paperID, s3_single$"2.14", NCP_QoL_given_detected_one_na, other=NA)
colnames(NCP_QoL_given_detected_df)
NCP_QoL_given_detected_df <- NCP_QoL_given_detected_df[-c(14:15)]

colnames(NCP_QoL_given_detected_df)

NCP_QoL_given_detected_df[is.na(NCP_QoL_given_detected_df)] = 0

colSums(NCP_QoL_given_detected_df[-c(1:2)])


MF_cols = paste0("MF", 1:4, ".key")
s3_single_MF = s3_single[,c("paperID", MF_cols)]

# Sum all the nature given answers
NCP_QoL_sum_v =rowSums(NCP_QoL_given_detected_df[, 3:13], na.rm=T)

# Tabulate by MF
NCP_QoL_by_mf_df = sapply(MF_cols, FUN = function(mf_colname) tapply(NCP_QoL_sum_v, INDEX = s3_single_MF[,mf_colname], FUN = sum, na.rm=T))
NCP_QoL_by_mf_df = NCP_QoL_by_mf_df["1", ]

names(NCP_QoL_by_mf_df) = c("MF1", "MF2", "MF3", "MF4")

barplot(NCP_QoL_by_mf_df)

 # pdf("output/Fig_QoL_MF_12Nov.pdf", width=10, height = 10, pointsize = 12)
pie(NCP_QoL_by_mf_df, col = viridis(4), main = "People's quality of life", init.angle = 90, labels = NA)
 # dev.off()



################ ASSIGN overall None
non_list = list(split_res_2.10_df, split_res_2.11_df, split_res_2.12_df, split_res_2.13_df, split_res_2.14_df)

## replace "None" to NA

non_list = lapply(non_list, FUN = function(df_in){
  df_in[df_in=="None"] =NA
  return(df_in)
})

df_in = non_list[[1]]
ES_cnt_df = sapply(non_list, FUN = function(df_in){
  return(rowSums(!is.na(df_in)))
})

colnames(ES_cnt_df) = c("Nature", "Regulation", "Material", "Non-material", "Quality of Life")


# how many ES were studied in each ES category in an application?
boxplot(ES_cnt_df)

ES_cnt_tb_l = (apply(ES_cnt_df, MARGIN = 2,  table))

par(mfrow=c(2,3))
par(mar=c(4,4,4,4))
pie(ES_cnt_tb_l[[1]], main = "Nature")
pie(ES_cnt_tb_l[[2]], main = "Regulation")
pie(ES_cnt_tb_l[[3]], main = "Material")
pie(ES_cnt_tb_l[[4]], main = "Non-material")
pie(ES_cnt_tb_l[[5]], main = "Quality of Life")

dev.off()
# how many ES were studied in each application?
boxplot(rowSums(ES_cnt_df))

pdf("output/Q2.10-14.howmanyNCPperApplication_bar.pdf", width= 20, height = 10, pointsize = 20)
# png("output/Q2.10-14.howmanyNCPperApplication_bar.png", width= 25, height = 15, unit = "cm", res = 400)
barplot(table(rowSums(ES_cnt_df)), las = 1, ylim = c(0, 350), main = "How many NCP (value type) were studied per application?", ylab = "Number of applications", xlab = "Value types per application")
dev.off()

# were there studies with no ES category studied?
table(rowSums(ES_cnt_df) == 0 ) # no

# How many ES categries were studied in each application?
ES_category_cnt_v = rowSums(ES_cnt_df > 0)
boxplot(ES_category_cnt_v)
ES_category_cnt_tb =(table(ES_category_cnt_v))

pdf("output/Q2.10-14.howmanyNCPCategoriesperApp_pie.pdf", width=10, height = 10, pointsize = 20)
# png("output/Q2.10-14.howmanyNCPCategoriesperApp_pie.png", width=13, height = 10, unit = "cm", res = 400)
pie(ES_category_cnt_tb, main = "Number of Value type categories per application",  init.angle = 90, col = gray.colors(5, start = 0.9, end = 0.3))
dev.off()

##### Add up them all
All_paperIds = c(NCP_nature_given_detected_df$s3_single.paperID, NCP_regul_given_detected_df$s3_single.paperID, NCP_material_given_detected_df$s3_single.paperID, NCP_non_material_given_detected_df$s3_single.paperID,  NCP_QoL_given_detected_df$s3_single.paperID)

length(All_paperIds)
length(unique(All_paperIds))
All_paperIds = unique(All_paperIds)



NCP_nature_corrected_df = read.xlsx("Corrected/s3_single_with_other_nature_n20-corrSJ.xlsx", sheet = 1)
dim(NCP_nature_corrected_df); colnames(NCP_nature_corrected_df)

colnames(NCP_nature_corrected_df)[c(1:7)] # Make sure two datasets are in the same order

str(NCP_nature_given_detected_df)

NCP_list <- matrix(NA, ncol = ncol(NCP_nature_corrected_df)-6, nrow = nrow(NCP_nature_given_detected_df))
colnames(NCP_list) <- colnames(NCP_nature_corrected_df)[-c(1:6)]

NCP_nature_full <- cbind(NCP_nature_given_detected_df, NCP_list)
NCP_nature_full[is.na(NCP_nature_full)] = 0

colnames(NCP_nature_full)[7] = "Maintenance.of.options.org"

dim(NCP_nature_full)

colnames(NCP_nature_full)
colnames(NCP_nature_given_detected_df) # 2.11
colnames(NCP_regul_given_detected_df) # 2.11
colnames(NCP_material_given_detected_df) # 2.12
colnames(NCP_non_material_given_detected_df) # 2.13
colnames(NCP_QoL_given_detected_df) # 2.14

NCP_nature_full[,c(8:17)][match(NCP_regul_given_detected_df$s3_single.paperID, NCP_nature_full$s3_single.paperID),  ] = NCP_regul_given_detected_df[,-c(1:2)]

NCP_nature_full[,c(18:21)][match(NCP_material_given_detected_df$s3_single.paperID, NCP_nature_full$s3_single.paperID),  ] = NCP_material_given_detected_df[,-c(1:2)]

NCP_nature_full[,c(22:24)][match(NCP_non_material_given_detected_df$s3_single.paperID, NCP_nature_full$s3_single.paperID), ] = NCP_non_material_given_detected_df[,-c(1:2)]

NCP_nature_full[,c(25:35)][match(NCP_QoL_given_detected_df$s3_single.paperID, NCP_nature_full$s3_single.paperID),  ] = NCP_QoL_given_detected_df[,-c(1:2)]

# merge duplicated colummns
NCP_nature_full$Maintenance.of.options = NCP_nature_full$Maintenance.of.options.org + NCP_nature_full$Maintenance.of.options

NCP_nature_full = subset(NCP_nature_full, select= -Maintenance.of.options.org)


colnames(NCP_nature_full)

# Check the col sum of the integrated dataset
colSums(NCP_nature_full[-c(1:2)])
colSums(NCP_nature_given_detected_df[-c(1:2)])
colSums(NCP_regul_given_detected_df[-c(1:2)])
colSums(NCP_material_given_detected_df[-c(1:2)])
colSums(NCP_non_material_given_detected_df[-c(1:2)])
colSums(NCP_QoL_given_detected_df[-c(1:2)])



################ Read corrected data (from Sander)

NCP_nature_corrected_df = read.xlsx("Corrected/s3_single_with_other_nature_n20-corrSJ.xlsx", sheet = 1)
NCP_material_corrected_df = read.xlsx("Corrected/s3_single_with_other_material_n25_corrSJ.xlsx", sheet = 1)
NCP_non_material_corrected_df = read.xlsx("Corrected/s3_single_with_other_non_material_n22_corrSJ.xlsx", sheet = 1)
NCP_regul_corrected_df = read.xlsx("Corrected/s3_single_with_other_regul_n46-corrSJ.xlsx", sheet = 1)
NCP_QoL_corrected_df = read.xlsx("Corrected/s3_single_with_other_QoL_n29_corrSJ.xlsx", sheet = 1)


colnames(NCP_nature_corrected_df)
colnames(NCP_material_corrected_df)
colnames(NCP_non_material_corrected_df)
colnames(NCP_regul_corrected_df)
colnames(NCP_QoL_corrected_df)
dim(NCP_nature_corrected_df)
dim(NCP_material_corrected_df)
dim(NCP_non_material_corrected_df)
dim(NCP_regul_corrected_df)
dim(NCP_QoL_corrected_df)

# NCP_other_l = list(NCP_nature_corrected_df, NCP_material_corrected_df, NCP_non_material_corrected_df, NCP_regul_corrected_df, NCP_QoL_corrected_df)


Allanswers_paperIds = c(NCP_nature_given_detected_df$paperID, NCP_material_corrected_df$paperID, NCP_non_material_corrected_df$paperID, NCP_regul_corrected_df$paperID, NCP_QoL_corrected_df$paperID)

# length(CorrectedOther_paperIds)
# length(unique(CorrectedOther_paperIds))



# First prepare a data frame which can store all the five data frames
CorrectedOther_paperIds = c(NCP_nature_corrected_df$paperID, NCP_material_corrected_df$paperID, NCP_non_material_corrected_df$paperID, NCP_regul_corrected_df$paperID, NCP_QoL_corrected_df$paperID)

# length(CorrectedOther_paperIds)
# length(unique(CorrectedOther_paperIds))
CorrectedOther_paperIds = unique(CorrectedOther_paperIds)

CorrectedOther_df = data.frame(matrix(nrow = length(CorrectedOther_paperIds), ncol = ncol(NCP_nature_corrected_df)))
colnames(CorrectedOther_df) = colnames(NCP_nature_corrected_df)

# Fill all values by zero
CorrectedOther_df[,-c(1:2)] = 0
CorrectedOther_df$paperID =CorrectedOther_paperIds

NCP_nature_corrected_df[is.na(NCP_nature_corrected_df)] = 0
NCP_material_corrected_df[is.na(NCP_material_corrected_df)] = 0
NCP_non_material_corrected_df[is.na(NCP_non_material_corrected_df)] = 0
NCP_regul_corrected_df[is.na(NCP_regul_corrected_df)] = 0
NCP_QoL_corrected_df[is.na(NCP_QoL_corrected_df)] = 0

# add up.. there are duplicated entries.
CorrectedOther_df[match( NCP_nature_corrected_df$paperID, CorrectedOther_df$paperID), -c(1:2)] = CorrectedOther_df[match( NCP_nature_corrected_df$paperID, CorrectedOther_df$paperID), -c(1:2)] + NCP_nature_corrected_df[, -c(1:2)]

CorrectedOther_df[match( NCP_material_corrected_df$paperID, CorrectedOther_df$paperID), -c(1:2)] = CorrectedOther_df[match( NCP_material_corrected_df$paperID, CorrectedOther_df$paperID), -c(1:2)] + NCP_material_corrected_df[, -c(1:2)]

CorrectedOther_df[match( NCP_non_material_corrected_df$paperID, CorrectedOther_df$paperID), -c(1:2)] = CorrectedOther_df[match( NCP_non_material_corrected_df$paperID, CorrectedOther_df$paperID), -c(1:2)] + NCP_non_material_corrected_df[, -c(1:2)]

CorrectedOther_df[match( NCP_regul_corrected_df$paperID, CorrectedOther_df$paperID), -c(1:2)] = CorrectedOther_df[match( NCP_regul_corrected_df$paperID, CorrectedOther_df$paperID), -c(1:2)] + NCP_regul_corrected_df[, -c(1:2)]

CorrectedOther_df[match( NCP_QoL_corrected_df$paperID, CorrectedOther_df$paperID), -c(1:2)] = CorrectedOther_df[match( NCP_QoL_corrected_df$paperID, CorrectedOther_df$paperID), -c(1:2)]+ NCP_QoL_corrected_df[, -c(1:2)]

# assertion fails as there are multiple entries
# sum(CorrectedOther_df[,-c(1:2)], na.rm=T)
#
# sum(NCP_QoL_corrected_df[,-c(1:2)], na.rm=T) +
#   sum(NCP_nature_corrected_df[,-c(1:2)], na.rm=T) +
#   sum(NCP_material_corrected_df[,-c(1:2)], na.rm=T) +
#   sum(NCP_non_material_corrected_df[,-c(1:2)], na.rm=T) +
#   sum(NCP_regul_corrected_df[,-c(1:2)], na.rm=T)

# we can ignore it as we only cares if it is > 0
CorrectedOtherTF_df = cbind(CorrectedOther_df[, 1:2], CorrectedOther_df[, -c(1:2)] > 0)

corr_cnt = sapply(CorrectedOtherTF_df[, 3:34], FUN = function(x) length(which(x)))

barplot(corr_cnt, horiz=T, las=1)


## Correct the NCP_nature_full with the corrected answer by Sander

NCP_nature_full[match(CorrectedOther_df$paperID, NCP_nature_full$s3_single.paperID), ] = CorrectedOther_df
# NCP_nature_full$s3_single.paperID[CorrectedOther_df$paperID]
names(NCP_nature_full) <- names(CorrectedOther_df)


NCP_short_names <- c(nature_given_answers[-6], regul_given_answer[-c(1, 11)], material_given_answer[-5], non_material_given_answer[-4], QoL_given_answer[-12])
NCP_short_names[1] <- "Individual organisms"
length(NCP_short_names)




pdf("output/Q2.10-14_FullNCP_8Dec.pdf", width=16, height = 18, pointsize = 20)
# png("output/Q2.10-14_FullNCP_8Dect.png", width=25, height = 23, unit = "cm", res = 400)
par(mar = c(5, 25, 5, 5))
barplot(rev(colSums(NCP_nature_full[-c(1:2)])), horiz = T, las = 1, col= c(rep("peachpuff", 11), rep("blue", 3), rep("skyblue2", 4), rep("lightblue1", 9), rep("palegreen3", 5)), border = c(rep("peachpuff", 11), rep("blue", 3), rep("skyblue2", 4), rep("lightblue1", 9), rep("palegreen3", 5)), names.arg  = rev(NCP_short_names), xlab = "Number of applications")
dev.off()





### Levelplot


combinedNCP_by_mf_df = foreach (idx = 1:32, .combine = "rbind") %do% {
  print(NCP_short_names[idx])
  combinedNCP_by_mf = sapply(MF_cols, FUN = function(mf_colname) by(NCP_nature_full[ ,idx+2], INDICES = s3_single_MF[,mf_colname], FUN = sum, na.rm=T))
  return(combinedNCP_by_mf["1",])
}


rownames(combinedNCP_by_mf_df) = NCP_short_names
colnames(combinedNCP_by_mf_df) = paste0("MF", 1:4)

barplot(combinedNCP_by_mf_df)


combinedNCP_by_mf_rela1_df = apply(combinedNCP_by_mf_df, MARGIN = 1, FUN = function(x) x / sum(x, na.rm=T))

# dim(combinedNCP_by_mf_rela1_df)
# colSums(combinedNCP_by_mf_rela1_df)


# pdf("output/Fig_FullNCP_MF_rowRatio12Nov.pdf", width=16, height = 18, pointsize = 20)
levelplot(combinedNCP_by_mf_rela1_df, col.regions = rev(viridis(31)), main = "MF (%) per NCP", ylab="", xlab="")
# dev.off()

combinedNCP_by_mf_rela2_df = apply(combinedNCP_by_mf_df, MARGIN = 2, FUN = function(x) x / sum(x, na.rm=T))

dim(combinedNCP_by_mf_rela2_df)
colSums(combinedNCP_by_mf_rela2_df)

# pdf("output/Fig_FullNCP_MF_ColRatio12Nov.pdf", width=16, height = 18, pointsize = 20)
levelplot(t(combinedNCP_by_mf_rela2_df), col.regions = rev(viridis(31)), main = "MF dist.", ylab="", xlab="")
# dev.off()


###



### 2.15 The application assesses multiple values and brings thme together into an 'overall value' or importance by: (check all that apply)
summary(s3_single$"2.15")
# Multiple answers are separated by ','
split_res_l_2.15 = vector("list", length = nrow(s3_single))

for (row_idx in 1:nrow(s3_single))  {

  split_tmp = str_trim(str_split(s3_single$"2.15"[row_idx], pattern = ",")[[1]])
  split_res_l_2.15[[row_idx]] = split_tmp
}

value_split_v = unlist(split_res_l_2.15)
value_split_v_fac = factor(value_split_v)
value_tb_sorted = sort(table(value_split_v_fac), decreasing = F)
value_tb_reduced = (table(value_split_v_fac))[which (table(value_split_v_fac)>1)]
value_tb_reduced_temporary = as.numeric(value_tb_reduced)

short.names.multivalues = c(
  "Bringing them together and assign weights to individual value targets \nbased on a group discussion about acceptable weights given to different values",
"Bringing them together and assigning weights to individual value targets \nbased on researcher defined (or even) weights",                     "Bringing them together by converting all values to a common unit",
  "irrelevant: application does not attempt to bring the different values together",                                                         "Keeping the results from the study of the separate value and \nuse the results as a basis for a group discussion.",                            "The application asks respondents or participants to rank options \nincluding multiple values and infer the weight of individual components from their ranking.",
"The application assesses multiple values as bundles and \ndo not attempt to weigh individual value targets",                                  "Other/unclear" #"unclear: method of bringing values together is not explained"
)
# names(value_tb_reduced_temporary) = names(value_tb_reduced)
names(value_tb_reduced_temporary)= short.names.multivalues
value_tb_reduced_temporary["other"]=17
value_tb_reduced_temporary[8] <- value_tb_reduced_temporary[8] + value_tb_reduced_temporary[9]
# names(value_tb_reduced_temporary[8]) = "unclear/other"
value_tb_reduced_temporary[-9]


pdf("output/Q2.15_MultiValues_bar_8Dec.pdf", width=16, height = 10, pointsize = 20)
# png("output/Q2.15_MultiValues_bar_8Dec.png", width=25, height = 15, unit = "cm", res= 400)
par(mar=c(5, 25, 5, 5))
barplot(rev(value_tb_reduced_temporary[-c(4,9)]), horiz=T, las=1, cex.names = 0.7, col = gray(0.7), xlab = "Number of applications", xlim =c(0, 300))
dev.off()


multivalues_sum <- c(sum(value_tb_reduced[-c(4)]), value_tb_reduced[4])
pdf("output/Q2.15_Multivalues_pie_8Dec.pdf", width=10, height = 10, pointsize = 20)
# png("output/Q2.15_Multivalues_pie_8Dec.png", width=15, height = 15, unit = "cm", res = 400)
pie(multivalues_sum,  col = gray.colors(2, start = 0.7, end = 0.3), labels = NA, init.angle = 90, border = F, main = "overall values")
legend("bottomright", c('It is assessed', 'Not assessed'), fill = gray.colors(2, start = 0.7, end = 0.3), bty = "n", border = F, horiz = T, cex = 1.0)
dev.off()





### 2.16 - The application mentions interests and conflicts
summary(s3_single$"2.16")
table(s3_single$"2.16")
par(mar=c(5, 20, 5, 5))
barplot(table(s3_single$"2.16"), horiz = T, las = 1, cex.names = 0.5)

summary.2.16 <- table(s3_single$"2.16")
short.names.comflict = c(
   "conflict mentioned", # : mentioning of verbal, political or even physical conflict between interest groups over the assessed values",
   "no different interests or conflicts mentioned",                                                                                             "non-conflictual differences in interests", #:  mentioning of various interests to be considered but NO mentioning of actual verbal or physical harm.",
    "unclear"
)

long.names.conflict = c(
  "conflict mentioned: mentioning of verbal, political or \neven physical conflict between interest groups over the assessed values",
  "no different interests or conflicts mentioned",                                                                                             "non-conflictual differences in interests: mentioning of various interests to be considered \nbut NO mentioning of actual verbal or physical harm.",
  "unclear"
)


names(summary.2.16) <- short.names.comflict

pdf("output/Q2.16_InterestConflict_pie_8Dec.pdf", width=14, height = 10, pointsize = 18)
# png("output/Q2.16_InterestConflict_pie_8Dec.png", width=30, height = 30, unit = "cm", res = 400)
pie(summary.2.16, init.angle = 90, col = gray.colors(4, start = 0.2, end = 0.9), border = F) #col = c(IPbeslightgreen, "red", IPbesdarkgreen, "white")
dev.off()



#### Topic 3: Application descriptors

## 3.1 Elicitation process: Through what process were values collected?
summary(s3_single$"3.1")
table(s3_single$"3.1")


# Multiple answers are separated by ','
elicitation_org = c("Individual responses face-to-face, phone or similar  responses",
                    "Obtaining information by observing biological aspects in real-time \\(e.g. recording bee visitation of flowers, counting birds, measuring biomass of trees\\)",
                    "Obtaining information by collecting measurement data over a period of time \\(e.g. weather data, sediments, nutrients etc. \\)",
                    "Obtaining information about biophysical indicators using secondary data \\(expert estimates, land use maps, satellite images, species Atlas data etc\\)"
                    )

elicitation_alt = c("Individual responses personal",
                    "Obtaining information by observing biological aspects in real-time",
                    "Obtaining information by collecting measurement data over a period of time",
                    "Obtaining information about biophysical indicators using secondary data")


elicitation_org_v = as.character(s3_single$"3.1")
elicitation_alt_v = elicitation_org_v


# Replace well-defined options to simple format
for (o_idx in 1:length(elicitation_org)) {
  elicitation_alt_v = str_replace_all(elicitation_alt_v, pattern = elicitation_org[o_idx], replacement = elicitation_alt[o_idx])
}

split_res_l_3.1 = vector("list", length = nrow(s3_single))

for (row_idx in 1:nrow(s3_single)) {

  split_tmp = str_trim(str_split(elicitation_alt_v[row_idx], pattern = ",")[[1]])
  split_res_l_3.1[[row_idx]] = split_tmp
}

elicitation_split_v = unlist(split_res_l_3.1)
elicitation_split_v_fac = factor(elicitation_split_v)
elici_tb_sorted = sort(table(elicitation_split_v_fac), decreasing = F)
barplot(elici_tb_sorted, horiz = T, las =1) # until here, everything is fine.. Then, it doesn't find the shorted answers..


split_res_3.1_df = plyr::ldply(split_res_l_3.1, rbind) # automatically decide how many columns should it have

split_res_3.1_df = sapply(split_res_3.1_df, FUN = function(x) as.character(x))



# don't do escaping
elicit_given_answer = c(
  "Individuals written responses to questions",
  "Individual responses following a group discussion",
  "Individual responses personal", # face-to-face, phone or similar responses",
  "Group responses to the valuation question",
  "Obtaining information from transactions in markets",
  "Obtaining information from documents (policy/legal/historical texts)",
  "Observing individual practices/behaviors",
  "Observing group practices/behaviors",
  "Obtaining information by observing biological aspects in real-time", # (e.g. recording bee visitation of flowers, counting birds, measuring biomass of trees)
  "Obtaining information by collecting measurement data over a period of time", # (e.g. weather data, sediments, nutrients etc. )
  "Obtaining information about biophysical indicators using secondary data", # (expert estimates, land use maps, satellite images, species Atlas data etc)
  "unclear"
)


# # we can escape input characters for being used in Stringr functions
# escapeForStringR(elicit_given_answer)

# Be aware of the NA values when counting
elicit_other_choice_cnt = apply(split_res_3.1_df, MARGIN = 1, FUN = function(x) length(which(!(x[!is.na(x)] %in% elicit_given_answer)))) # how many choices (non-NA) are not in the given well-formed answers
# length(elicit_other_choice_cnt)

table(elicit_other_choice_cnt > 0 ) # 78 studies; it doesn't find the shortened answers TODO (solved?)
cnt_otherchoice_elicit = length(which(elicit_other_choice_cnt > 0))
# studies including other choices
s3_single$paperID[elicit_other_choice_cnt > 0 ]
s3_single_with_other_elicit_df = s3_single[elicit_other_choice_cnt > 0, c("paperID", "3.1")]

# write.xlsx(s3_single_with_other_elicit_df, paste0("output/s3_single_with_other_elicit_n", cnt_otherchoice_elicit, ".xlsx"))


# Identify other answers in the uncorrected data

elicit_other_v = elicitation_alt_v

for (given_idx in 1:length(elicit_given_answer)) {
  elicit_other_v = str_replace_all(elicit_other_v, pattern = escapeForStringR(elicit_given_answer[given_idx]), replacement = "") # escaped using escapeForStringR
}

elicit_other_v = str_trim(str_replace_all(elicit_other_v, pattern = "[,;]", replacement = "")) # warning: other answers without comma and semicolon
table(elicit_other_v)

eclit_other_wospace_v = str_replace_all(elicit_other_v, pattern = "[ ]", replacement = "") # remove whitespace
elicit_other_idx_3.1 = str_length(eclit_other_wospace_v) >= 1 # does it have still more characters?

elicit_other_v[elicit_other_idx_3.1]
n_other_3.1 = length(which(elicit_other_idx_3.1)); n_other_3.1

# Count given and other answers individually

NCP_elicit_given_detected = sapply(escapeForStringR(elicit_given_answer), FUN = function(x) str_detect(elicitation_alt_v, pattern = x))
dim(NCP_elicit_given_detected)


NCP_elicit_given_detected_one_na = ifelse(NCP_elicit_given_detected, yes = 1, no = NA)

NCP_elicit_given_detected_df = data.frame(s3_single$paperID, s3_single$"3.1", NCP_elicit_given_detected_one_na, other=NA)
NCP_elicit_given_detected_df <- NCP_elicit_given_detected_df[-c(15)]

colnames(NCP_elicit_given_detected_df)

NCP_elicit_given_detected_df[is.na(NCP_elicit_given_detected_df)] = 0

NCP_elicit_given_detected_final <- colSums(NCP_elicit_given_detected_df[-c(1:2)])

names(NCP_elicit_given_detected_final) <- elicit_given_answer

NCP_elicit_given_detected_final["Other/unclear"] = cnt_otherchoice_elicit


NCP_elicit_given_detected_final[12]
NCP_elicit_given_detected_final[13] = NCP_elicit_given_detected_final[12] + NCP_elicit_given_detected_final[13]
# names(NCP_elicit_given_detected_final[13]) = "Other/unclear"

pdf("output/Q3.1_Elicitation_bar_8Dec.pdf", width=16, height = 10, pointsize = 20)
# png("output/Q3.1_Elicitation_bar_8Dec.png", width=30, height = 15, unit = "cm", res = 400)
par(mar = c(5, 30, 5, 5))
barplot(rev(NCP_elicit_given_detected_final[-(12)]), horiz = T, las= 1, col = gray(0.7), xlab = "Number of applications")
dev.off()



## 3.2 The application uses or is based on data from the same spatiotemporal and socio-economic context (multiple answers possible in case of mixed/integrated methods)
summary(s3_single$"3.2")
table(s3_single$"3.2")

split_res_l_3.2 = vector("list", length = nrow(s3_single))

for (row_idx in 1:nrow(s3_single))  {

  split_tmp = str_trim(str_split(s3_single$"3.2"[row_idx], pattern = ",")[[1]])
  split_res_l_3.2[[row_idx]] = split_tmp
}

same_spatiotemporal_split_v = unlist(split_res_l_3.2)
same_spatiotemporal_split_v_fac = factor(same_spatiotemporal_split_v)
same_spatiotemporal_sorted = sort(table(same_spatiotemporal_split_v_fac), decreasing = F)

par(mar=c(5, 20, 5, 5))
barplot(sort(same_spatiotemporal_sorted, F), horiz=T, las=1, col = IPbeslightgreen, border = IPbeslightgreen, xlim = c(0, 1100))

split_res_3.2_df = plyr::ldply(split_res_l_3.2, rbind) # automatically decide how many columns should it have

split_res_3.2_df = sapply(split_res_3.2_df, FUN = function(x) as.character(x))

other_data_cnt =  apply(split_res_3.2_df, MARGIN = 1, FUN = function(x) length(which((x[!is.na(x)] %in% c("data transferred from other context(s)", "unclear"))))) # how many choices (non-NA) are not in the given well-formed answers
# length(idx1)
table(other_data_cnt == 1) # 148 (125 + 23)
cnt_otherdata = length(which(other_data_cnt == 1))
# studies including other choices
s3_single$paperID[other_data_cnt == 1]

s3_single_with_other_data_df = s3_single[other_data_cnt == 1, c("paperID", "3.2")]
#write.xlsx(s3_single_with_other_data_df, paste0("output/s3_single_with_other_data_n", cnt_otherdata, ".xlsx"))

barplot(same_spatiotemporal_sorted)


pdf("output/Q3.2_Datasource_pie_8Dec.pdf", width=15, height = 15, pointsize = 20)
# png("output/Q3.2_Datasource_pie_8Dec.png", width=15, height = 15, unit = "cm", res = 400)
pie(same_spatiotemporal_sorted, init.angle = 90, col = gray.colors(3, start= 0.2, end = 0.9)) #col = c("gray", IPbesdarkgreen, IPbeslightgreen),
dev.off()


## 3.3 How are values articulated in this application? (e.g. time spent; number of people visiting; stated or narrated importance; avoided damage costs; healthy life years; species number; rarity....) please state how the main results (from main methods) are represented in the paper. If multiple, make a list using semicolons
length(summary(s3_single$"3.3"))
summary(s3_single$"3.3")
as.character(s3_single$"3.3")

split_res_l_3.3 = vector("list", length = nrow(s3_single))

for (row_idx in 1:nrow(s3_single))  {

  split_tmp = str_trim(str_split(s3_single$"3.3"[row_idx], pattern = ";")[[1]])
  split_res_l_3.3[[row_idx]] = split_tmp
}

articulated_split_v = unlist(split_res_l_3.3)
articulated_split_v_fac = factor(articulated_split_v)
articulated_sorted = sort(table(articulated_split_v_fac), decreasing = F)
length(articulated_sorted)  # 2013

other_df_3.3= cbind(paperID= s3_single$paperID, ANSWER_RAW= as.character(s3_single$"3.3") )
# write.xlsx(other_df_3.3, file = "output/3.3_allanswers.xlsx") # does it make sense to check all?




## 3.4 This application presents values in following form (multiple answers possible in case of mixed/integrated methods)
summary(s3_single$"3.4")
length(summary(s3_single$"3.4"))
sort(summary(s3_single$"3.4"))

values_org = "artistic \\(e.g. music, paintings, drawings, poetry\\)"
values_alt = "artistic"
values_org_v = as.character(s3_single$"3.4")
values_alt_v = values_org_v
values_alt_v = str_replace_all(values_alt_v, pattern = values_org, replacement = values_alt)

values_org_v = as.character(s3_single$"3.4")
values_alt_v = values_org_v


# Replace well-defined options to simple format
for (o_idx in 1:length(values_org)) {
  values_alt_v = str_replace_all(values_alt_v, pattern = values_org[o_idx], replacement = values_alt[o_idx])
}

split_res_l_3.4 = vector("list", length = nrow(s3_single))

for (row_idx in 1:nrow(s3_single)) {

  split_tmp = str_trim(str_split(values_alt_v[row_idx], pattern = ",")[[1]])
  split_res_l_3.4[[row_idx]] = split_tmp
}

values_split_v = unlist(split_res_l_3.4)
values_split_v_fac = factor(values_split_v)
values_tb_sorted = sort(table(values_split_v_fac), decreasing = F)
barplot(values_tb_sorted, horiz = T, las =1) # until here, everything is fine.. Then, it doesn't find the shorted answers..


split_res_3.4_df = plyr::ldply(split_res_l_3.4, rbind) # automatically decide how many columns should it have

split_res_3.4_df = sapply(split_res_3.4_df, FUN = function(x) as.character(x))

values_given_answers = c(
  "artistic", # (e.g. music, paintings, drawings, poetry)
  "narrative (descriptions)",
  "categorical/nominal  (e.g. types)",
  "ordinal expression (ranked types e.g. worse - better / low -high / small - big)",
  "cardinal expression (in numbers)"
)


# Be aware of the NA values when counting
values_other_choice_cnt = apply(split_res_3.4_df, MARGIN = 1, FUN = function(x) length(which(!(x[!is.na(x)] %in% values_given_answers)))) # how many choices (non-NA) are not in the given well-formed answers
# length(elicit_other_choice_cnt)

table(values_other_choice_cnt > 0 ) # 59 studies
cnt_otherchoice_values = length(which(values_other_choice_cnt > 0)); cnt_otherchoice_values
# studies including other choices
s3_single$paperID[values_other_choice_cnt > 0 ]
s3_single_with_other_values_df = s3_single[values_other_choice_cnt > 0, c("paperID", "3.4")]

write.xlsx(s3_single_with_other_values_df, paste0("output/s3_single_with_other_values_n", cnt_otherchoice_values, ".xlsx"))


all_df_3.4= cbind(paperID= s3_single$paperID, ANSWER_RAW= as.character(s3_single$"3.4") )
write.xlsx(all_df_3.4, file = "output/3.4_allanswers.xlsx") # does it make sense to check all?



#
# escapeForStringR = function(x) {
#   gsub("([.|()\\^{}+$*?/-]|\\[|\\])", "\\\\\\1", x)
# }
#
# # we can escape input characters for being used in Stringr functions
# escapeForStringR(values_given_answers)

# Identify other answers in the uncorrected data

values_other_v = values_alt_v

for (given_idx in 1:length(values_given_answers)) {
  values_other_v = str_replace_all(values_other_v, pattern = escapeForStringR(values_given_answers[given_idx]), replacement = "") # escaped using escapeForStringR
}


values_other_v = str_trim(str_replace_all(values_other_v, pattern = "[,;]", replacement = "")) # warning: other answers without comma and semicolon
table(values_other_v)

values_other_wospace_v = str_replace_all(values_other_v, pattern = "[ ]", replacement = "") # remove whitespace
values_other_idx_3.4 = str_length(values_other_wospace_v) >= 1 # does it have still more characters?

values_other_v[values_other_idx_3.4]
n_other_3.4 = length(which(values_other_idx_3.4)); n_other_3.4


# Count given and other answers individually

values_given_detected = sapply(escapeForStringR(values_given_answers), FUN = function(x) str_detect(values_alt_v, pattern = x))
dim(values_given_detected)

values_given_detected_one_na = ifelse(values_given_detected, yes = 1, no = NA)

values_given_detected_df = data.frame(s3_single$paperID, s3_single$"3.4", values_given_detected_one_na, other=NA)
# NCP_elicit_given_detected_df <- values_given_detected_df[-c(15)]

colnames(values_given_detected_df)

values_given_detected_df[is.na(values_given_detected_df)] = 0

values_given_detected_df_final <- colSums(values_given_detected_df[-c(1:2)])

names(values_given_detected_df_final) <- values_given_answers
values_given_detected_df_final <- values_given_detected_df_final[-(6)]

names(values_given_detected_df_final)[4] <- "ordinal expression \n(ranked types e.g. worse - better / low -high / small - big)"

values_given_detected_df_final["other"] <- cnt_otherchoice_values



pdf("output/Q3.4_ValueForm_bar_8Dec.pdf", width=16, height = 10, pointsize = 20)
# png("output/Q3.4_ValueForm_bar_8Dec.png", width=23, height = 10, unit = "cm", res = 400)
par(mar = c(5, 25, 5, 5))
barplot(rev(values_given_detected_df_final), horiz = T, las= 1, col = gray(0.7), xlab = "Number of applications")
dev.off()



pdf("output/Q3.4_ValueForm_pie_8Dec.pdf", width=23, height = 15, pointsize = 20)
# png("output/Q3.4_ValueForm_pie_8Dec.png", width=28, height = 15, units = "cm", res = 400)
 # par(mar = c(2, 18, 2, 11))
pie(values_given_detected_df_final, init.angle = 90, col = gray.colors(6, start = 0.3, end = 0.9), border = F) #col = brewer.Greens(length(values_given_detected_df_final))
# legend("bottomright", names(values_given_detected_df_final), fill = gray.colors(6, start = 0.3, end = 0.9), bty = "n", border = F, horiz = F) # inset=c(-0.5,-0.5),

dev.off()


## 3.5 Aggregation: The application aggregateds different staekholders' values into an 'overall value or importance' by:

summary(s3_single$"3.5")
length(summary(s3_single$"3.5")) # 38
sort(summary(s3_single$"3.5"))

stakeholder_org = "Simple, "
stakeholder_alt = "Simple-"
stakeholder_org_v = as.character(s3_single$"3.5")
stakeholder_alt_v = stakeholder_org_v
stakeholder_alt_v = str_replace_all(stakeholder_alt_v, pattern = stakeholder_org, replacement = stakeholder_alt)

stakeholder_org_v = as.character(s3_single$"3.5")
stakeholder_alt_v = stakeholder_org_v


# Replace well-defined options to simple format
for (o_idx in 1:length(stakeholder_org)) {
  stakeholder_alt_v = str_replace_all(stakeholder_alt_v, pattern = stakeholder_org[o_idx], replacement = stakeholder_alt[o_idx])
}

split_res_l_3.5 = vector("list", length = nrow(s3_single))

for (row_idx in 1:nrow(s3_single)) {

  split_tmp = str_trim(str_split(stakeholder_alt_v[row_idx], pattern = ",")[[1]])
  split_res_l_3.5[[row_idx]] = split_tmp
}

stakeholder_split_v = unlist(split_res_l_3.5)
stakeholder_split_v_fac = factor(stakeholder_split_v)
stakeholder_tb_sorted = sort(table(stakeholder_split_v_fac), decreasing = F)
barplot(stakeholder_tb_sorted, horiz = T, las =1) # until here, everything is fine.. Then, it doesn't find the shorted answers..


split_res_3.5_df = plyr::ldply(split_res_l_3.5, rbind) # automatically decide how many columns should it have

split_res_3.5_df = sapply(split_res_3.5_df, FUN = function(x) as.character(x))


stakeholder_given_answer = c(
  "irrelevant: the method is not used to aggregate stakeholder's values",
  "Simple-non-weighted aggregation (averages or summations) of participants(respondents) values",
  "Simple-non-weighted aggregation (averages or summations) to a higher social scale (see 2.6)",
  "Unclear: method of aggregation is not explained.",
  "Weighted aggregation by numerical weighting to a higher social scale (see 2.6)", # should be replaced earlier than the shorter one
  "Weighted aggregation by numerical weighting",
  "Weighted aggregation by a group process"
)


# Be aware of the NA values when counting
stakeholder_other_choice_cnt = apply(split_res_3.5_df, MARGIN = 1, FUN = function(x) length(which(!(x[!is.na(x)] %in% stakeholder_given_answer)))) # how many choices (non-NA) are not in the given well-formed answers
# length(elicit_other_choice_cnt)

table(stakeholder_other_choice_cnt > 0 ) # 38 studies
cnt_otherchoice_stakeholder = length(which(stakeholder_other_choice_cnt > 0)); cnt_otherchoice_stakeholder
# studies including other choices
s3_single$paperID[stakeholder_other_choice_cnt > 0 ]
s3_single_with_other_stakeholder_df = s3_single[stakeholder_other_choice_cnt > 0, c("paperID", "3.5")]

# write.xlsx(s3_single_with_other_stakeholder_df, paste0("output/s3_single_with_other_stakeholder_n", cnt_otherchoice_stakeholder, ".xlsx"))

#
# escapeForStringR = function(x) {
#   gsub("([.|()\\^{}+$*?/-]|\\[|\\])", "\\\\\\1", x)
# }
#
# # we can escape input characters for being used in Stringr functions
# escapeForStringR(stakeholder_given_answer)

# Identify other answers in the uncorrected data

stakeholder_other_v = stakeholder_alt_v

for (given_idx in 1:length(stakeholder_given_answer)) {
  stakeholder_other_v = str_replace_all(stakeholder_other_v, pattern = escapeForStringR(stakeholder_given_answer[given_idx]), replacement = "") # escaped using escapeForStringR
}


stakeholder_other_v = str_trim(str_replace_all(stakeholder_other_v, pattern = "[,;]", replacement = "")) # warning: other answers without comma and semicolon
table(stakeholder_other_v)

stakeholder_other_wospace_v = str_replace_all(stakeholder_other_v, pattern = "[ ]", replacement = "") # remove whitespace
stakeholder_other_idx_3.5 = str_length(stakeholder_other_wospace_v) >= 1 # does it have still more characters?

stakeholder_other_v[stakeholder_other_idx_3.5]
n_other_3.5 = length(which(stakeholder_other_idx_3.5)); n_other_3.5


# Count given and other answers individually

stakeholder_given_detected = sapply(escapeForStringR(stakeholder_given_answer), FUN = function(x) str_detect(stakeholder_alt_v, pattern = x))
dim(stakeholder_given_detected)

stakeholder_given_detected_one_na = ifelse(stakeholder_given_detected, yes = 1, no = NA)

stakeholder_given_detected_df = data.frame(s3_single$paperID, s3_single$"3.5", stakeholder_given_detected_one_na, other=NA)
# NCP_elicit_given_detected_df <- values_given_detected_df[-c(15)]

colnames(stakeholder_given_detected_df)

stakeholder_given_detected_df[is.na(stakeholder_given_detected_df)] = 0

stakeholder_given_detected_df_final <- colSums(stakeholder_given_detected_df[-c(1:2)])
stakeholder_given_detected_df_final[["other"]] <- cnt_otherchoice_stakeholder

stakeholder.name.short <- c("Irrelevant","Simple: non-weighted aggregation nof participants values", "Simple: non-weighted aggregation to a higher social scale", "Unclear", "Weighted aggregation by numerical weighting to a higher social scale", "Weighted aggregation by numerical weighting", "Weighted aggregation by a group process", "Other/unclear")


names(stakeholder_given_detected_df_final) <- stakeholder.name.short

# stakeholder_given_detected_df_final <- stakeholder_given_detected_df_final[-(8)]
stakeholder_given_detected_df_final["other"] <- cnt_otherchoice_stakeholder



stakeholder_sum <- c(sum(stakeholder_given_detected_df_final[-c(1)]), stakeholder_given_detected_df_final[1])


pdf("output/Q3.5_Stakeholder_pie_8Dec.pdf", width=10, height = 10, pointsize = 18)
# png("output/Q3.5_Stakeholder_pie_8Dec.png", width=13, height = 13, units = "cm", res = 400)

pie(stakeholder_sum,  col = gray.colors(2, start = 0.9, end = 0.2), labels = NA, init.angle = 90, border = F)
legend("bottom", c('It is assessed', 'Not assessed'), fill = gray.colors(2, start = 0.9, end = 0.2), bty = "n", border = F, horiz = T, cex = 1.0)
dev.off()



stakeholder_given_detected_df_final[8] <- stakeholder_given_detected_df_final[8] + stakeholder_given_detected_df_final[4]

pdf("output/Q3.5_Stakeholder_bar_8Dec.pdf", width=18, height = 8, pointsize = 20)
 # png("output/Q3.5_Stakeholder_bar_8Dec.png", width=30, height = 15, units = "cm", res = 400)

par(mar = c(5, 30, 5, 5))
barplot(rev(stakeholder_given_detected_df_final[-c(1, 4, 9)]), horiz = T, las= 1, col = gray(0.7), xlab = "Number of applications")
 dev.off()








names(table(stakeholder_alt_v))

# Identify other answers
split_res_l_3.5 = vector("list", length = nrow(s3_single))

stakeholder_other_v = stakeholder_alt_v

for (given_idx in 1:length(stakeholder_given_answer)) {

  # stakeholder_other_v = str_replace_all(stakeholder_other_v, pattern = stakeholder_given_answer[given_idx], replacement = LETTERS[given_idx])
  stakeholder_other_v = str_replace_all(stakeholder_other_v, pattern = escapeForStringR(stakeholder_given_answer[given_idx]), replacement = "")

}


 names(table(stakeholder_other_v))

stakeholder_other_v = str_trim(str_replace_all(stakeholder_other_v, pattern = "[,]", replacement = "")) # warning: other answers without comma and semicolon
names(table(stakeholder_other_v))



length(table(stakeholder_other_v))

stakeholder_other_wospace_v = str_replace_all(stakeholder_other_v, pattern = "[ ]", replacement = "") # remove whitespace
other_idx_3.5 = str_length(stakeholder_other_wospace_v) >= 1 # does it have still more characters?

stakeholder_other_v[other_idx_3.5]
n_other_3.5 = length(which(other_idx_3.5)); n_other_3.5
other_df_3.5= cbind(paperID= s3_single$paperID, OTHER_ANSWER_3_5 = stakeholder_other_v, ANSWER_RAW = as.character(s3_single$"3.5") )[other_idx_3.5,]
#write.xlsx(other_df_6.5, file = "output/6.5_otheranswers.xlsx")

# Count given and other answers individually





########-----Data Overview 5 - Valuation for Human Wellbeing-----########

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



wellbeing_corrected_df = read.xlsx("Corrected/6.1_otheranswers_MT_HL.xlsx", sheet = 1)
str(wellbeing_corrected_df)

# Identify other answers in the uncorrected data

wellbeing_other_v = wellbeing_indicator_alt_v

for (given_idx in 1:length(wellbeing_given_answer)) {

  wellbeing_other_v = str_replace_all(wellbeing_other_v, pattern = wellbeing_given_answer[given_idx], replacement = "")
}

wellbeing_other_v = str_trim(str_replace_all(wellbeing_other_v, pattern = "[,;]", replacement = "")) # warning: other answers without comma and semicolon
table(wellbeing_other_v)


wellbeing_other_wospace_v = str_replace_all(wellbeing_other_v, pattern = "[ ]", replacement = "") # remove whitespace
wellbeing_other_idx_6.1 = str_length(wellbeing_other_wospace_v) >= 1 # does it have still more characters?

wellbeing_indicator_alt_v[wellbeing_other_idx_6.1]
n_other_6.1 = length(which(wellbeing_other_idx_6.1)); n_other_6.1
# other_df_6.1= cbind(paperID= s3_single$paperID, OTHER_ANSWER_6_1 = wellbeing_indicator_other_v,ANSWER_RAW=as.character(s3_single$"6.1") )[other_idx_6.1,]
# write.xlsx(other_df_6.1, file = "output/6.1_otheranswers.xlsx")

# Count given and other answers individually

wellbeing_given_detected = sapply(wellbeing_given_answer, FUN = function(x) str_detect(wellbeing_indicator_alt_v, pattern = x))

wellbeing_given_detected_one_na = ifelse(wellbeing_given_detected, yes = 1, no = NA)

wellbeing_given_detected_df = data.frame(s3_single$paperID, s3_single$"6.1", wellbeing_given_detected_one_na, other=NA)
colnames(wellbeing_given_detected_df) = colnames(wellbeing_corrected_df) # Make sure two datasets are in the same order

# merge two datasets using paperID

wellbeing_given_detected_df$Livelihood.dependence.on.management.of.land <- as.numeric(wellbeing_given_detected_df$Livelihood.dependence.on.management.of.land)
wellbeing_given_detected_df$Livelihood.dependence.on.access.to.natural.resources   <- as.numeric(wellbeing_given_detected_df$Livelihood.dependence.on.access.to.natural.resources)

 wellbeing_given_detected_df$paperID <- as.numeric(wellbeing_given_detected_df$paperID)
 wellbeing_corrected_df$paperID <- as.numeric(wellbeing_corrected_df$paperID)
 wellbeing_corrected_df$Livelihood.dependence.on.access.to.natural.resources <- as.numeric(wellbeing_corrected_df$Livelihood.dependence.on.access.to.natural.resources)

wellbeing_given_detected_df[,2] = as.character(wellbeing_given_detected_df[,2]) # factor to character
wellbeing_given_detected_df[match(wellbeing_corrected_df$paperID, wellbeing_given_detected_df$paperID), ] = wellbeing_corrected_df


wellbeing_tb_final = colSums(wellbeing_given_detected_df[,-c(1:2)], na.rm = T)

barplot(wellbeing_tb_final, horiz=T, las=2)
names(wellbeing_tb_final)
wellbeing_tb_final_plot <- c(
    "Livelihood dependence on access to natural resources",                                                                                      "Livelihood dependence on management of land",                                                                                               "(Loss of) profits from natural resources",                                                                                                  "Physical health outcomes linked to nature & biodiversity",                                                                                 "Mental health outcomes linked to nature & biodiversity",                                                                                   "Subjective well-being linked to nature & biodiversity",
  "Change in utility of individuals linked to changes in nature & biodiversity",                                                                "A composite indicator (combining different aspects \nof well-being into one, such as Human Development Index, \nBetter Life Index etc.) linked to nature & biodiversity",
  "Well-being indicator is assessed but not linked to nature & biodiversity",                                                                 "Application.does.not.assess.human.wellbeing.with.one.of.these.indicators",                                                                  "other"

)
#
# names(habitat_tb_final)[15] <- "No habitat assessed"
#
# names(habitat_tb_final[16]) <- "Other and unclear"
names(wellbeing_tb_final) <- wellbeing_tb_final_plot

# wellbeing_tb_final <-  wellbeing_tb_final[-(10)]


# pdf("output/Fig_6.1_corrected_12Nov.pdf", width=15, height = 8, pointsize = 18)
pdf("output/Q6.1_Wellbeing_bar_8Dec.pdf", width=18, height = 13, pointsize = 20)
# png("output/Q6.1_Wellbeing_bar_8Dec.png", width=30, height = 20, units = "cm", res = 400)

par(mar=c(5,30,1,1))
barplot(rev(wellbeing_tb_final[-(10)]), las=1, horiz=T, col = gray(0.7), xlim = c(0, 150), xlab = "Number of applications")
dev.off()


wellbeing_sum <- c(sum(wellbeing_tb_final[-c(10)]), wellbeing_tb_final[10])


pdf("output/Q6.1_Wellbeing_pie_8Dec.pdf", width=10, height = 10, pointsize = 18)
# png("output/Q6.1_Wellbeing_pie_8Dec.png", width=20, height = 20, units = "cm", res = 400)
pie(wellbeing_sum,  col = gray.colors(2, start = 0.9, end = 0.2), labels = NA, init.angle = 90, border = F)
legend("bottom", c('It is assessed', 'Not assessed'), fill = gray.colors(2, start = 0.9, end = 0.2), bty = "n", border = F, horiz = T, cex = 1.2)
dev.off()




MF_cols = paste0("MF", 1:4, ".key")
s3_single_MF = s3_single[,c("paperID", MF_cols)]


wellbeing_by_mf_df = foreach (wellbeing_idx = 1:10, .combine = "rbind") %do% {
  print(wellbeing_given_answer[wellbeing_idx])
  wellbeing_by_mf = sapply(MF_cols, FUN = function(mf_colname) by(wellbeing_given_detected_df[,wellbeing_idx +2 ], INDICES = s3_single_MF[,mf_colname], FUN = sum, na.rm=T))
  print(colSums(wellbeing_by_mf))
  return(wellbeing_by_mf["1",])
}


rownames(wellbeing_by_mf_df) =wellbeing_tb_final_plot[-(11)]
colnames(wellbeing_by_mf_df) = paste0("MF", 1:4)

#pdf("output/Fig_2.1_scale_corrected_MF.pdf", width=15, height = 8, pointsize = 12)

par(mar=c(5,20,1,1))
barplot(t(wellbeing_by_mf_df[-10, ]), horiz = T, col = viridis(4)) # ("Application.does.not.assess.human.wellbeing.with.one.of.these.indicators")
legend("bottomright", #inset=c(1,0),
       fill=viridis(4),
       legend=paste0("MF", 1:4))



# pdf("output/Fig_6.1_wellbeing_MF.pdf", width=8, height = 8, pointsize = 18)
barplot(colSums(wellbeing_by_mf_df[-10, ]), las = 1, col = IPbesdarkgreen, border = IPbesdarkgreen, ylab = "Number of applications", ylim = c(0,300))
# dev.off()



wellbeing_by_mf_perc = colSums(wellbeing_by_mf_df[-10, ]) / sum( colSums(wellbeing_by_mf_df[-10, ]) )
barplot(wellbeing_by_mf_perc * 100 , las=1, ylab= "MF (%)", border=IPbesdarkgreen, horiz=F, ylim=c(0,100))



# df_tmp =wellbeing_by_mf_df[-10, ]
# df_tmp_rela1 = apply(df_tmp, MARGIN = 2, FUN = function(x) x / sum(x, na.rm=T))
# barplot(df_tmp_rela, las = 1, col = IPbesdarkgreen, border = IPbesdarkgreen, ylab = "Number of applications")





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






preference_corrected_df = read.xlsx("Corrected/6.2_otheranswers_MT_HL.xlsx", sheet = 1)
preference_corrected_df = preference_corrected_df[!is.na(preference_corrected_df$paperID),]
str(preference_corrected_df)

preference_given_detected_one_na = ifelse(prefernece_given_detected, yes = 1, no = NA)
preference_given_detected_df = data.frame(s3_single$paperID, s3_single$"6.2", preference_given_detected_one_na, other=NA)
colnames(preference_given_detected_df) = colnames(preference_corrected_df) # Make sure two datasets are in the same order

# merge two datasets using paperID

preference_given_detected_df$other <- as.numeric(preference_given_detected_df$other)
preference_corrected_df$paperID <- as.numeric(preference_corrected_df$paperID)
preference_corrected_df$`Hypothetical.willingness.to.give.up.resources.(monetary.or.other.forms).to.maintain.(or.increase).aspects.of.nature.and.biodiversity` <- as.numeric(preference_corrected_df$`Hypothetical.willingness.to.give.up.resources.(monetary.or.other.forms).to.maintain.(or.increase).aspects.of.nature.and.biodiversity`)

# wellbeing_given_detected_df[,2] = as.character(wellbeing_given_detected_df[,2]) # factor to character

preference_given_detected_df[match(preference_corrected_df$paperID, preference_given_detected_df$paperID), -2] = preference_corrected_df[,-2]


preference_tb_final = colSums(preference_given_detected_df[,-c(1:2)], na.rm = T)

barplot(preference_tb_final, horiz=T, las=2)
names(preference_tb_final)
preference_tb_final_plot <- c(
"Hypothetical willingness to give up resources \nto maintain aspects of nature and biodiversity",
"Hypothetical compensation to give up access \nto nature or management rights of natural areas",                                             "Scores of relative importance to people \nof nature's contributions to people",
"Spending or expenditure to maintain \naspects of nature and biodiversity",
"Dialogues with communities about the importance \nof different aspects of nature and biodiversity",                                         "Application does not assess preferences of humans to nature",                                                                             "other"
)
#

names(preference_tb_final) <- preference_tb_final_plot

# wellbeing_tb_final <-  wellbeing_tb_final[-(10)]


#pdf("output/Fig_6.2_corrected_12Nov.pdf", width=15, height = 8, pointsize = 18)
pdf("output/Q6.2_Preference_bar_8Dec.pdf", width=25, height = 10, pointsize = 20)
# png("output/Q6.2_Preference_bar_8Dec.png", width=25, height = 10, units= "cm", res = 400)
par(mar=c(5,20,1,1))
barplot(rev(preference_tb_final[-c(6)]), las=1, horiz=T, col = gray(0.7), xlim = c(0, 250), xlab = "Number of applications")
dev.off()


preference_sum <- c(sum(preference_tb_final[-c(6)]), preference_tb_final[6])


pdf("output/Q6.2_Preference_pie_8Dec.pdf", width=10, height = 10, pointsize = 18)
# png("output/Q6.2_Preference_pie_8Dec.png", width=20, height = 20, units = "cm", res = 400)
pie(preference_sum,  col = gray.colors(2, start = 0.9, end = 0.2), labels = NA, init.angle = 90, border = F)
legend("bottom", c('It is assessed', 'Not assessed'), fill = gray.colors(2, start = 0.9, end = 0.2), bty = "n", border = F, horiz = T, cex= 1.2)
dev.off()


preference_by_mf_df = foreach (idx = 1:7, .combine = "rbind") %do% {
  print(preference_tb_final_plot[idx])
  preference_by_mf = sapply(MF_cols, FUN = function(mf_colname) by(preference_given_detected_df[,idx +2 ], INDICES = s3_single_MF[,mf_colname], FUN = sum, na.rm=T))
  return(preference_by_mf["1",])
}


rownames(preference_by_mf_df) = preference_tb_final_plot
colnames(preference_by_mf_df) = paste0("MF", 1:4)

barplot(preference_by_mf_df)


par(mar=c(5,20,1,1))
barplot(t(preference_by_mf_df[-10, ]), horiz = T, col = viridis(4)) # ("Application.does.not.assess.human.wellbeing.with.one.of.these.indicators")
legend("bottomright", #inset=c(1,0),
       fill=viridis(4),
       legend=paste0("MF", 1:4))



#pdf("output/Fig_6.2_preference_MF.pdf", width=8, height = 8, pointsize = 18)
barplot(colSums(preference_by_mf_df[-6, ]), las = 1, col = IPbesdarkgreen, border = IPbesdarkgreen, ylab = "Number of applications", ylim = c(0,500))
#dev.off()




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





cost_corrected_df = read.xlsx("Corrected/6.3_otheranswers_MT_HL.xlsx", sheet = 1)
cost_corrected_df = cost_corrected_df[!is.na(cost_corrected_df$paperID),]
str(cost_corrected_df)

cost_given_detected_one_na = ifelse(cost_given_detected, yes = 1, no = NA)
cost_given_detected_df = data.frame(s3_single$paperID, s3_single$"6.3", cost_given_detected_one_na, other=NA)
colnames(cost_given_detected_df) = colnames(cost_corrected_df) # Make sure two datasets are in the same order

# merge two datasets using paperID

cost_given_detected_df$other <- as.numeric(cost_given_detected_df$other)
cost_corrected_df$paperID <- as.numeric(cost_corrected_df$paperID)
cost_corrected_df$`What.has.actually.been.spent.in.the.past.to.protect.nature.and.biodiversity.or.maintain.nature’s.contribution.to.people.[This.only.applies.if.the.investment.was.done.in.the.past]`<- as.numeric(cost_corrected_df$`What.has.actually.been.spent.in.the.past.to.protect.nature.and.biodiversity.or.maintain.nature’s.contribution.to.people.[This.only.applies.if.the.investment.was.done.in.the.past]`)

 # wellbeing_given_detected_df[,2] = as.character(wellbeing_given_detected_df[,2]) # factor to character

cost_given_detected_df[match(cost_corrected_df$paperID, cost_given_detected_df$paperID), -2] = cost_corrected_df[,-2]


cost_tb_final = colSums(cost_given_detected_df[,-c(1:2)], na.rm = T)

barplot(cost_tb_final, horiz=T, las=2)
names(cost_tb_final)
cost_tb_final_plot <- c(
"What has actually been spent in the past to protect \nnature and biodiversity or maintain nature's contribution to people",
 "The costs and the benefits from past projects to protect \nnature and biodiversity or maintain nature's contribution to people",
"What it would cost to protect nature and biodiversity or \nmaintain nature's contribution to people",
"What it would cost to protect nature and biodiversity or \nmaintain nature's contribution to people in the most effective way",
"The costs and the benefits from potential or hypothetical \nfuture projects or policies to protect nature and biodiversity \nor maintain nature's contribution to people",
"Application does not assess cost to protect nature & biodiversity for its own sake or to maintain nature’s contributions to people",      "other"
)
#

names(cost_tb_final) <- cost_tb_final_plot


pdf("output/Q6.3_Cost_bar_8Dec.pdf", width=15, height = 8, pointsize = 20)
# png("output/Q6.3_Cost_bar_8Dec.png", width=25, height = 15, units = "cm", res = 400)
par(mar=c(5,28,1,1))
barplot(rev(cost_tb_final[-c(6)]), las=1, horiz=T, col = gray(0.7), xlim = c(0, 150), xlab = "Number of applications")
dev.off()


cost_sum <- c(sum(cost_tb_final[-c(6)]), cost_tb_final[6])


pdf("output/Q6.3_Cost_pie_8Dec.pdf", width=10, height = 10, pointsize = 18)
# png("output/Q6.3_Cost_pie_8Dec.png", width=20, height = 20, units = "cm", res = 400)

pie(cost_sum,  col = gray.colors(2, start = 0.9, end = 0.2), labels = NA, init.angle = 90, border = F)
legend("bottom", c('It is assessed', 'Not assessed'), fill = gray.colors(2, start = 0.9, end = 0.2), bty = "n", border = F, horiz = T, cex= 1.3)
dev.off()


cost_by_mf_df = foreach (idx = 1:7, .combine = "rbind") %do% {
  print(cost_tb_final_plot[idx])
  cost_by_mf = sapply(MF_cols, FUN = function(mf_colname) by(cost_given_detected_df[,idx +2 ], INDICES = s3_single_MF[,mf_colname], FUN = sum, na.rm=T))
  return(cost_by_mf["1",])
}


rownames(cost_by_mf_df) = cost_tb_final_plot
colnames(cost_by_mf_df) = paste0("MF", 1:4)

barplot(cost_by_mf_df)


par(mar=c(5,20,1,1))
barplot(t(cost_by_mf_df[-6, ]), horiz = T, col = viridis(4)) # ("Application.does.not.assess.human.wellbeing.with.one.of.these.indicators")
legend("bottomright", #inset=c(1,0),
       fill=viridis(4),
       legend=paste0("MF", 1:4))




#pdf("output/Fig_6.3_cost_MF.pdf", width=8, height = 8, pointsize = 18)
barplot(colSums(cost_by_mf_df[-6, ]), las = 1, col = IPbesdarkgreen, border = IPbesdarkgreen, ylab = "Number of applications", ylim = c(0,200))
#dev.off()







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


socio_corrected_df = read.xlsx("Corrected/Kopi af 6.5_otheranswers_MT.xlsx", sheet = 1)
socio_corrected_df = socio_corrected_df[!is.na(socio_corrected_df$paperID),]
str(socio_corrected_df)

socio_given_detected_one_na = ifelse(socio_given_detected, yes = 1, no = NA)
socio_given_detected_df = data.frame(s3_single$paperID, s3_single$"6.5", socio_given_detected_one_na, other=NA)
colnames(socio_given_detected_df) = colnames(socio_corrected_df) # Make sure two datasets are in the same order

# merge two datasets using paperID

socio_given_detected_df$other <- as.numeric(socio_given_detected_df$other)
socio_corrected_df$paperID <- as.numeric(socio_corrected_df$paperID)

# wellbeing_given_detected_df[,2] = as.character(wellbeing_given_detected_df[,2]) # factor to character

socio_given_detected_df[match(socio_corrected_df$paperID, socio_given_detected_df$paperID), -2] = socio_corrected_df[,-2]

socio_tb_final = colSums(socio_given_detected_df[,-c(1:2)], na.rm = T)

barplot(socio_tb_final, horiz=T, las=2)
names(socio_tb_final)
socio_tb_final_plot <- c(
  "Yes, analysed for different income groups",
  "Yes, analysed for different age groups",
  "Yes, analysed for different levels of education",
  "Yes, analysed for different gender",
  "Yes, analysed for different stakeholder groups",
  "No, difference in the well-being indicator is assessed \nbut not attributed to different socio-demographic groups",
  "No, difference in the well-being indicator \nby socio-demographic group is not evaluated",
  "irrelevant",
  "other"
)
#

names(socio_tb_final) <- socio_tb_final_plot
barplot(socio_tb_final, horiz = T, las = 1)

# pdf("output/Fig_6.5_corrected_12Nov.pdf", width=15, height = 8, pointsize = 18)
pdf("output/Q6.5_SocioDemo_bar_8Dec.pdf", width=15, height = 8, pointsize = 20)
# png("output/Q6.5_SocioDemo_bar_8Dec.png", width=25, height = 10, units = "cm", res = 400)

par(mar=c(5,28,1,1))
barplot(rev(socio_tb_final[-c(7,8)]), las=1, horiz=T, col = gray(0.7), xlim = c(0, 150), xlab = "Number of applications")
dev.off()


socio_sum <- c(sum(socio_tb_final[-c(7,8)]), socio_tb_final[7], socio_tb_final[8])


pdf("output/Q6.5_SocioDemo_pie_8Dec.pdf", width=10, height = 10, pointsize = 18)
# png("output/Q6.5_SocioDemo_pie_8Dec.png", width=20, height = 20, units = "cm", res = 400)

pie(socio_sum,  col = gray.colors(3, start = 0.9, end = 0.2), labels = NA, init.angle = 90, border = F)
legend("bottom", c('It is assessed', 'Not assessed', 'Irrelevant'), fill = gray.colors(3, start = 0.9, end = 0.2), bty = "n", border = F, horiz = T, cex = 1.3)
dev.off()


socio_by_mf_df = foreach (idx = 1:9, .combine = "rbind") %do% {
  print(socio_tb_final_plot[idx])
  socio_by_mf = sapply(MF_cols, FUN = function(mf_colname) by(socio_given_detected_df[,idx +2 ], INDICES = s3_single_MF[,mf_colname], FUN = sum, na.rm=T))
  return(socio_by_mf["1",])
}


rownames(socio_by_mf_df) = socio_tb_final_plot
colnames(socio_by_mf_df) = paste0("MF", 1:4)

barplot(socio_by_mf_df)


par(mar=c(5,20,1,1))
barplot(t(socio_by_mf_df[-8, ]), horiz = T, col = viridis(4)) # ("Application.does.not.assess.human.wellbeing.with.one.of.these.indicators")
legend("bottomright", #inset=c(1,0),
       fill=viridis(4),
       legend=paste0("MF", 1:4))



pdf("output/Fig_6.5_cost_MF.pdf", width=8, height = 8, pointsize = 18)
barplot(colSums(socio_by_mf_df[-c(7,8), ]), las = 1, col = IPbesdarkgreen, border = IPbesdarkgreen, ylab = "Number of applications", ylim = c(0,550))
dev.off()








#pdf("output/Topic6_combine_25Oct.pdf", width=15, height = 20, pointsize = 12)

par(mfrow=c(4,1), mar=c(5,25,1,1))
barplot(wellbeing_all_tb, las=1, horiz=T, cex.names = 0.8, col = IPbeslightgreen, border =IPbeslightgreen, xlim = c(0, max(wellbeing_all_tb) *1.1), main = "6.1 Well-being")
barplot(preference_all_tb, las=1, horiz=T, cex.names = 0.8, col = IPbeslightgreen, border =IPbeslightgreen, xlim = c(0, max(preference_all_tb) *1.1), main= "6.2 Preference")
barplot(cost_all_tb, las=1, horiz=T, cex.names = 0.8, col = IPbeslightgreen, border =IPbeslightgreen, xlim = c(0, max(cost_all_tb) *1.1), main = "6.3 Cost")
barplot(socio_all_tb, las=1, horiz=T, cex.names = 0.8, col = IPbeslightgreen, border =IPbeslightgreen, xlim = c(0, max(socio_all_tb) *1.1), main = "6.5 Socio-demographic groups")
#dev.off()






