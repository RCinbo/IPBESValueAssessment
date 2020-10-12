source("Starting.R")

## Topic - Context of application
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

split_res_l = foreach (row_idx = 1:nrow(s3_single)) %do% {

  split_tmp = str_trim(str_split(choice_alt_v[row_idx], pattern = ",")[[1]])
  return(split_tmp)
}

choice_split_v = unlist(split_res_l)
choice_split_v_fac = factor(choice_split_v)
barplot(sort(table(choice_split_v_fac), F), log="x", las=1, horiz=T, cex.names = 0.5)

split_res_l

library(plyr)
split_res_df = plyr::ldply(split_res_l, rbind)

split_res_df = sapply(split_res_df, FUN = function(x) as.character(x))

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
