source("Starting.R")

######  Topic 2 - Context of application

### 2.1- The application addresses the following spatial scales (multiple possible)
### Their multiple answers are separated by ','

summary(s3_single$"2.1")
# how many are 'multiple scales' ?
levels(s3_single$"2.1")

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

library(stringr)
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

split_res_l = foreach (row_idx = 1:nrow(s3_single)) %do% {

  split_tmp = str_trim(str_split(choice_alt_v[row_idx], pattern = ",")[[1]])
  return(split_tmp)
}

choice_split_v = unlist(split_res_l)
choice_split_v_fac = factor(choice_split_v)

choice_tb_sorted = sort(table(choice_split_v_fac), decreasing = F)

barplot(choice_tb_sorted, log="x", las=1, horiz=T, cex.names = 0.5)

# choice_ordreded = names(choice_tb_sorted)

choice_tb_reduced = (table(choice_split_v_fac))[which (table(choice_split_v_fac)>2)]

barplot(sort(choice_tb_reduced, F), horiz=T, las=1, cex.names=0.5)

split_res_l

library(plyr)
split_res_df = plyr::ldply(split_res_l, rbind)

split_res_df = sapply(split_res_df, FUN = function(x) as.character(x))

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
# Multiple answers are separated by ','

wetland_org = "Wetlands \\- peatlands, mires, bogs"
wetland_alt = "Wetlands"
wetland_org_v = as.character(s3_single$"2.2")
wetland_alt_v = wetland_org_v

wetland_alt_v = str_replace_all(wetland_alt_v, pattern = wetland_org, replacement = wetland_alt)

split_res_l_2.2 = foreach (row_idx = 1:nrow(s3_single)) %do% {

  split_tmp = str_trim(str_split(wetland_alt_v[row_idx], pattern = ",")[[1]])
  return(split_tmp)
}

habitat_split_v = unlist(split_res_l_2.2)
habitat_split_v_fac = factor(habitat_split_v)

habitat_tb_sorted = sort(table(habitat_split_v_fac), decreasing = F)

barplot(habitat_tb_sorted, log="x", las=1, horiz=T, cex.names = 0.5)

habitat_tb_reduced = (table(habitat_split_v_fac))[which (table(habitat_split_v_fac)>5)]

barplot(sort(habitat_tb_reduced, F), horiz=T, las=1, cex.names=0.5)

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
split_res_l_2.10 = foreach (row_idx = 1:nrow(s3_single)) %do% {

  split_tmp = str_trim(str_split(s3_single$"2.10"[row_idx], pattern = ",")[[1]])
  return(split_tmp)
}

nature_split_v = unlist(split_res_l_2.10)
nature_split_v_fac = factor(nature_split_v)
nature_tb_sorted = sort(table(nature_split_v_fac), decreasing = F)
nature_tb_reduced = (table(nature_split_v_fac))[which (table(nature_split_v_fac)>1)]
barplot(sort(nature_tb_reduced, F), horiz=T, las=1, cex.names=0.5)


### 2.11 There are multiple classifications of 'what is valued'. We want to know how these fit in the IPBES categories. The application assesses the following 'targets of valuation' regarding REgulating Nature Contributions To People (multiple possible)
summary(s3_single$"2.11")
# Multiple answers are separated by ','
flow_org = "Regulation of freshwater quantity, flow and timing \\(\\!includes water provision\\!\\)"
flow_alt = "Regulation of freshwater quantity"
flow_org_v = as.character(s3_single$"2.11")
flow_alt_v = flow_org_v

flow_alt_v = str_replace_all(flow_alt_v, pattern = flow_org, replacement = flow_alt)

split_res_l_2.11 = foreach (row_idx = 1:nrow(s3_single)) %do% {

  split_tmp = str_trim(str_split(flow_alt_v[row_idx], pattern = ",")[[1]])
  return(split_tmp)
}

regul_split_v = unlist(split_res_l_2.11)
regul_split_v_fac = factor(regul_split_v)
regul_tb_sorted = sort(table(regul_split_v_fac), decreasing = F)
regul_tb_reduced = (table(regul_split_v_fac))[which (table(regul_split_v_fac)>2)]
barplot(sort(regul_tb_reduced, F), horiz=T, las=1, cex.names=0.5)



### 2.12 There are multiple classifications of 'what is valued'. We want to know how these fit in the IPBES categories. The application assesses the following 'targets of valuation' regarding Material Nature Contributions To People (multiple possible)
summary(s3_single$"2.12")
# Multiple answers are separated by ','
split_res_l_2.12 = foreach (row_idx = 1:nrow(s3_single)) %do% {

  split_tmp = str_trim(str_split(s3_single$"2.12"[row_idx], pattern = ",")[[1]])
  return(split_tmp)
}

material_split_v = unlist(split_res_l_2.12)
material_split_v_fac = factor(material_split_v)
material_tb_sorted = sort(table(material_split_v_fac), decreasing = F)
material_tb_reduced = (table(material_split_v_fac))[which (table(material_split_v_fac)>1)]
barplot(sort(material_tb_reduced, F), horiz=T, las=1, cex.names=0.5)



### 2.13 There are multiple classifications of 'what is valued'. We want to know how these fit in the IPBES categories. The application assesses the following 'targets of valuation' regarding non-material Nature Contributions To People (multiple possible)
summary(s3_single$"2.13")
# Multiple answers are separated by ','
split_res_l_2.13 = foreach (row_idx = 1:nrow(s3_single)) %do% {

  split_tmp = str_trim(str_split(s3_single$"2.13"[row_idx], pattern = ",")[[1]])
  return(split_tmp)
}

non_material_split_v = unlist(split_res_l_2.13)
non_material_split_v_fac = factor(non_material_split_v)
non_material_tb_sorted = sort(table(non_material_split_v_fac), decreasing = F)
non_material_tb_reduced = (table(non_material_split_v_fac))[which (table(non_material_split_v_fac)>1)]
barplot(sort(non_material_tb_reduced, F), horiz=T, las=1, cex.names=0.5)


### 2.14 There are multiple classifications of 'what is valued'. We want to know how these fit in the IPBES categories. The application assesses the following 'targets of valuation' regarding People's quality of life (multiple possible)
summary(s3_single$"2.14")
# Multiple answers are separated by ','
split_res_l_2.14 = foreach (row_idx = 1:nrow(s3_single)) %do% {

  split_tmp = str_trim(str_split(s3_single$"2.14"[row_idx], pattern = ",")[[1]])
  return(split_tmp)
}

QoL_split_v = unlist(split_res_l_2.14)
QoL_split_v_fac = factor(QoL_split_v)
QoL_tb_sorted = sort(table(QoL_split_v_fac), decreasing = F)
QoL_tb_reduced = (table(QoL_split_v_fac))[which (table(QoL_split_v_fac)>1)]
barplot(sort(QoL_tb_reduced, F), horiz=T, las=1, cex.names=0.5)



### 2.15 The application assesses multiple values and brings thme together into an 'overall value' or importance by: (check all that apply)
summary(s3_single$"2.15")
# Multiple answers are separated by ','
split_res_l_2.15 = foreach (row_idx = 1:nrow(s3_single)) %do% {

  split_tmp = str_trim(str_split(s3_single$"2.15"[row_idx], pattern = ",")[[1]])
  return(split_tmp)
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




