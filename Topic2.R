library(stringr)
# library(doSNOW) # foreach()
library(openxlsx)

source("Starting.R")

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
"cross-national or cross-region Indigenous territories/jurisdictions/lands",
"cross-national or cross-region protected areas",
"national, federal",
"regional as multi-national continental",
"global",
"Option 9"
)
choices_alt = c("O1_Local", "O2_District", "O3_Administrative", "O4_CrossNationalIndigenous", "O5_CrossNationalProtected", "O6_National", "O7_Continental", "O8_Global", "O9_Other", "O10")


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

  split_tmp = str_trim(str_split(s3_single$"2.10"[row_idx], pattern = ",")[[1]])
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
  "Individual organisms (e.g. the sacred village tree)",
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


write.xlsx(s3_single_with_other_nature_df, paste0("output/s3_single_with_other_nature_n", cnt_otherchoice_nature, ".xlsx"))



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


write.xlsx(s3_single_with_other_regul_df, paste0("output/s3_single_with_other_regul_n", cnt_otherchoice_regul, ".xlsx"))



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


write.xlsx(s3_single_with_other_material_df, paste0("output/s3_single_with_other_material_n", cnt_otherchoice_material, ".xlsx"))




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


write.xlsx(s3_single_with_other_non_material_df, paste0("output/s3_single_with_other_non_material_n", cnt_otherchoice_non_material, ".xlsx"))



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


write.xlsx(s3_single_with_other_QoL_df, paste0("output/s3_single_with_other_QoL_n", cnt_otherchoice_QoL, ".xlsx"))



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


