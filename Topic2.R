library(stringr)
# library(doSNOW) # foreach()
library(openxlsx)

source("Starting.R")

############Define a color scheme#####################
IPbesdarkgreen <- rgb(92/255, 102/255, 93/255) #5c665d
IPbeslightgreen <- rgb(181/255, 212/255, 141/255) #b5d48d
colfunc <- colorRampPalette(c(rgb(92/255, 102/255, 93/255), rgb(181/255, 212/255, 141/255)))


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
 library(plyr)
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

s3_single_with_other_choices_df = s3_single[other_choice_cnt > 0, c("paperID", "2.1")]
write.xlsx(s3_single_with_other_choices_df, paste0("output/s3_single_with_other_choices_n", cnt_otherchoice, ".xlsx"))


 split_cnt_v = sapply(split_res_l, FUN = length)
stopifnot(max(split_cnt_v) == ncol(split_res_df))

table(split_cnt_v)
# split_cnt_v
#    1    2    3    4    5
# 1042   86   10    1    1
barplot(table(split_cnt_v), las= 1)


# Max 5 items
str(split_res_df)
head(split_res_df)
tail(split_res_df)
tail(s3_single$"2.1")
table(split_res_df)


s3_single_2.1_comparison_df = cbind(split_res_df, RawData_2.1 = as.character(s3_single$"2.1"))
colnames(s3_single_2.1_comparison_df)[1:5] = paste0("Scale_Split_", 1:5)

library(openxlsx)
write.xlsx(s3_single_2.1_comparison_df, file = "output/s3_single_2.1_for_comparison.xlsx")



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
scale_given_detected_df[match(scale_corrected_df$paperID, scale_given_detected_df$s3_single.paperID), ] = scale_corrected_df

scale_tb_final = colSums(scale_given_detected_df[,-c(1:2)], na.rm = T)

scale.names = c(choices_org, "other")
scale.names[1] <- "local"
barplot(scale_tb_final, horiz=T, las=2, names.arg = scale.names, las = 1 )
#TODO: Check the correction again..

#
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

habitat_given_answer = c( "Forests",
"Savannah",
"Deserts",
"Grasslands",
"Shrublands",
"Wetlands", #\\- peatlands, mires, bogs",
"Mountain habitats",
"Urban \\/ Semi-urban",
"Cultivated areas",
"Aquaculture",
"Inland surface water and water bodies \\/ freshwater",
"Coastal areas",
"Deep sea", "unclear", "Irrelevant"
)


# split list to data frame
split_res_2.2_df = plyr::ldply(split_res_l_2.2, rbind) # automatically decide how many columns should it have

split_res_2.2_df = sapply(split_res_2.2_df, FUN = function(x) as.character(x))


# Extract Paper IDs of the choice_malformed

# Be aware of the NA values when counting
habitat_other_choice_cnt =  apply(split_res_2.2_df, MARGIN = 1, FUN = function(x) length(which(!(x[!is.na(x)] %in% habitat_given_answer)))) # how many choices (non-NA) are not in the given well-formed answers
# length(idx1)
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

write.xlsx(s3_single_with_other_habitat_df, paste0("output/s3_single_with_other_habitat_n", cnt_otherchoice_habitat, ".xlsx"))



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

## TODO: habitat_given_tb vs. habitat_tb_final(still 'other' category exists)



habitat_given_cnt= apply(habitat_given_detected, MARGIN = 1,  FUN = function(x) (which(x)))
habitat_given_tb = table(unlist(habitat_given_cnt))
names(habitat_given_tb)= habitat_given_answer

names(habitat_given_tb)[15] <- "No habitat assessed"

barplot(sort(habitat_given_tb)[c()],  las=1, horiz=T, col = IPbeslightgreen, border =IPbeslightgreen, xlim = c(0, max(habitat_all_tb) *1.1))



# habitat_all_tb[1] <- habitat_all_tb[1] + habitat_all_tb[6] # 127 + 55
# names(habitat_all_tb)[9] <- "No habitat assessed"
# names(habitat_all_tb)[1] <- "Other/unclear"
# habitat_all_tb <- habitat_all_tb[-(6)]

#pdf("output/Fig_habitat_corrected_30Oct.pdf", width=15, height = 8, pointsize = 12)
#png("output/Fig_habitat_corrected_30Oct.png", width=800, height = 600, pointsize = 12)

par(mar=c(5,20,1,1))
barplot(habitat_all_tb[c(8,1:7,9:15)],  las=1, horiz=T, col = IPbeslightgreen, border =IPbeslightgreen, xlim = c(0, max(habitat_all_tb) *1.1))
#dev.off()


### 2.7 - The application collected data with the following temporal frequency: (only one answer)
summary(s3_single$"2.7")
table(s3_single$"2.7")
barplot(table(s3_single$"2.7"))

### 2.8 - The application assesses values change over time:
summary(s3_single$"2.8")
pie(table(s3_single$"2.8"))


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
nrow(NCP_nature_given_detected_df) ; nrow(NCP_regul_given_detected_df)

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
par(mfrow=c(2,2))
pie(ES_cnt_tb_l[[1]], main = "Nature")
pie(ES_cnt_tb_l[[2]], main = "Regulation")
pie(ES_cnt_tb_l[[3]], main = "Material")
pie(ES_cnt_tb_l[[4]], main = "Non-material")
pie(ES_cnt_tb_l[[5]], main = "Quality of Life")

dev.off()
# how many ES were studied in each application?
boxplot(rowSums(ES_cnt_df))

barplot(table(rowSums(ES_cnt_df)), main = "How many ES were studied per application? ")

# were there studies with no ES category studied?
table(rowSums(ES_cnt_df) == 0 ) # no

# How many ES categries were studied in each application?
ES_category_cnt_v = rowSums(ES_cnt_df > 0)
boxplot(ES_category_cnt_v)
ES_category_cnt_tb =(table(ES_category_cnt_v))

pie(ES_category_cnt_tb, main = "# of ES categories per application")


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

# NCP_nature_full[match(CorrectedOther_df$paperID, NCP_nature_full$s3_single.paperID,)] = CorrectedOther_df

# NCP_nature_full$s3_single.paperID[CorrectedOther_df$paperID]

names(NCP_nature_full) <- names(CorrectedOther_df)
combined.data <- rbind(NCP_nature_full, CorrectedOther_df)
nrow(combined.data)


NCP_short_names <- c(nature_given_answers[-6], regul_given_answer[-c(1, 11)], material_given_answer[-5], non_material_given_answer[-4], QoL_given_answer[-12])
NCP_short_names[1] <- "Individual organisms"
length(NCP_short_names)

 pdf("output/Fig_FullNCP_30Oct.pdf", width=16, height = 18, pointsize = 12)
# png("output/Fig_FullNCP_30Oct.png", width=800, height = 1000, pointsize = 12)
par(mar = c(5, 25, 5, 5))
barplot(colSums(combined.data[-c(1:2)]), horiz = T, las = 1, col= c(rep("palegreen3", 5), rep("lightblue1", 9), rep("skyblue2", 4), rep("blue", 3), rep("peachpuff", 11)), border = c(rep("palegreen3", 5), rep("lightblue1", 9), rep("skyblue3", 4), rep("blue", 3), rep("peachpuff", 11)), names.arg  = NCP_short_names)
dev.off()

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
par(mar=c(5, 20, 5, 5))
barplot(sort(value_tb_reduced, F), horiz=T, las=1, cex.names=0.5)





### 2.16 - The application mentions interests and conflicts
summary(s3_single$"2.16")
table(s3_single$"2.16")
par(mar=c(5, 20, 5, 5))
barplot(table(s3_single$"2.16"), horiz = T, las = 1, cex.names = 0.5)


## TODO:
#### Topic 3: Application descriptors

## 3.1 Elicitation process: Through what process were values collected?

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
barplot(sort(same_spatiotemporal_sorted, F), horiz=T, las=1, cex.names=0.5)

# pdf("output/Fig_3.2_26Oct.pdf", width=12, height = 8, pointsize = 12)
pie(same_spatiotemporal_sorted)
# dev.off()


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
write.xlsx(other_df_6.4, file = "output/3.3_allanswers.xlsx") # does it make sense to check all?

## 3.4 This application presents values in following form (multiple answers possible in case of mixed/integrated methods)
summary(s3_single$"3.4")
length(summary(s3_single$"3.4"))
sort(summary(s3_single$"3.4"))

values_org = "artistic \\(e.g. music, paintings, drawings, poetry\\)"
values_alt = "artistic"
values_org_v = as.character(s3_single$"3.4")
values_alt_v = values_org_v
values_alt_v = str_replace_all(values_alt_v, pattern = values_org, replacement = values_alt)

values_given_answers = c(
  "artistic",# (e.g. music, paintings, drawings, poetry)
  "narrative \\(descriptions\\)",
  "categorical\\/nominal  \\(e.g. types\\)",
  "ordinal expression \\(ranked types e.g. worse \\- better \\/ low \\-high \\/ small \\- big\\)",
  "cardinal expression \\(in numbers\\)"
)

help(stringr)


split_res_l_3.4 = vector("list", length = nrow(s3_single))

values_other_v = values_alt_v


for (given_idx in 1:length(values_given_answers)) {

  values_other_v = str_replace_all(values_other_v, pattern = values_given_answers[given_idx], replacement = "")
}
names(table(values_other_v))

values_other_v = str_trim(str_replace_all(values_other_v, pattern = "[,]", replacement = "")) # warning: other answers without comma and semicolon
table(values_other_v)
names(table(values_other_v))

length(table(values_other_v))

values_other_wospace_v = str_replace_all(values_other_v, pattern = "[ ]", replacement = "") # remove whitespace
other_idx_3.4 = str_length(values_other_wospace_v) >= 1 # does it have still more characters?

values_other_v[other_idx_3.4]
n_other_3.4 = length(which(other_idx_3.4)); n_other_3.4
other_df_3.4= cbind(paperID= s3_single$paperID, OTHER_ANSWER_3_4 = values_other_v, ANSWER_RAW = as.character(s3_single$"3.4") )[other_idx_3.4,]



## 3.5 Aggregation: The application aggregateds different staekholders' values into an 'overall value or importance' by:

summary(s3_single$"3.5")
length(summary(s3_single$"3.5")) # 38
sort(summary(s3_single$"3.5"))

stakeholder_org = "Simple, "
stakeholder_alt = "Simple-"
stakeholder_org_v = as.character(s3_single$"3.5")
stakeholder_alt_v = stakeholder_org_v
stakeholder_alt_v = str_replace_all(stakeholder_alt_v, pattern = stakeholder_org, replacement = stakeholder_alt)

names(table(stakeholder_alt_v))

stakeholder_given_answer = c(
  "irrelevant: the method is not used to aggregate stakeholder's values",
  "Simple-non-weighted aggregation \\(averages or summations\\) of participants\\(respondents\\) values",
  "Simple-non-weighted aggregation \\(averages or summations\\) to a higher social scale \\(see 2.6\\)",
  "Unclear: method of aggregation is not explained.",
  "Weighted aggregation by numerical weighting to a higher social scale \\(see 2.6\\)", # should be replaced earlier than the shorter one
  "Weighted aggregation by numerical weighting",
  "Weighted aggregation by a group process"
)

# Identify other answers
split_res_l_3.5 = vector("list", length = nrow(s3_single))

stakeholder_other_v = stakeholder_alt_v

for (given_idx in 1:length(stakeholder_given_answer)) {

  # stakeholder_other_v = str_replace_all(stakeholder_other_v, pattern = stakeholder_given_answer[given_idx], replacement = LETTERS[given_idx])
  stakeholder_other_v = str_replace_all(stakeholder_other_v, pattern = stakeholder_given_answer[given_idx], replacement = "")

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

