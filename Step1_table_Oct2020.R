library(openxlsx)
# library(readxl)
library(abind)
library(stringr)
library(RColorBrewer)
library(countrycode)
#library(tidyverse)
library(lattice)

brewer.YlGnBu <- colorRampPalette(brewer.pal(9, "YlGnBu"), interpolate = "spline")
brewer.BuPu <- colorRampPalette(brewer.pal(9, "BuPu"), interpolate = "spline")

# setwd("~/Google Drive/Values assessment/R-IPBES/")

paste_noNA <- function(x,sep=", ") {
  gsub(", " ,sep, toString(x[!is.na(x) & x!="" & x!="NA"] ) ) }

##########################################################################################
##########################################################################################

data.final <- read.xlsx("~/Google Drive/Values assessment/R-IPBES/IDSWAP_ 425 AND_16June20DG_copy.xlsx", colNames=T)
#data.final <- read.xlsx("./data/IDSWAP_ 425 AND_16June20DG_copy.xlsx", colNames=T)#Raïsa added this line to read the data locally





colnames(data.final)
final.country <- data.final[, c("TSU.ID_MERGED", "Region_TI_AB_DE_ID", "PY", "DI", "TS18", "TS19", paste0("TS", c(20:27)))] # "Subregion_TI_AB_DE_ID",
final.country[32,]
nrow(final.country)

### Expand the region
geocountry_l_final = lapply(final.country$Region_TI_AB_DE_ID,  FUN = function(geocountry) unique(strsplit(geocountry, ",")[[1]]))

final.country$TSU.ID_MERGED = as.integer(final.country$TSU.ID_MERGED) # real number to integer

TSU.ID_MERGED_StrLength_range = range(sapply(final.country$TSU.ID_MERGED, FUN = str_length))
substr_length_max = TSU.ID_MERGED_StrLength_range[2]

TSU.ID_MERGED_Padded = as.character(formatC(final.country$TSU.ID_MERGED,  flag = 0, width = substr_length_max))

#plot(as.numeric(TSU.ID_MERGED_Padded), final.country$TSU.ID_MERGED)

names(geocountry_l_final) = TSU.ID_MERGED_Padded
head(geocountry_l_final)

geocountry_names_fi = unique(unlist(geocountry_l_final))
table(sapply(geocountry_l_final, length))
sum(table(sapply(geocountry_l_final, length)))
# which(sapply(geocountry_l_final, length) == 4)

# geocountry_l_final[86]

table(unlist(geocountry_l_final))

geocountry_unlisted_fi_beforeunlist = (sapply(geocountry_l_final, FUN = function(x) {if (length(x)==0) {return("")} else {return(unlist(x))}}))
length(geocountry_unlisted_fi_beforeunlist)
geocountry_unlisted_fi = unlist(geocountry_unlisted_fi_beforeunlist)
names(geocountry_unlisted_fi)
length(geocountry_unlisted_fi) # 86066
# 72568 + 5968*2 + 455*3 + 48*4 + 5

# unlist(geocountry_unlisted_fi_beforeunlist[[1]])

geocountry_unlisted_fi_id = substr(names(geocountry_unlisted_fi), start = 1, stop = substr_length_max)
head(geocountry_unlisted_fi_id)

final.country$TSU.ID_MERGED_Padded = TSU.ID_MERGED_Padded

geocountry_expanded_df_fi = data.frame(TSU.ID_MERGED_Padded = geocountry_unlisted_fi_id, GEO_country_single = geocountry_unlisted_fi)
nrow(geocountry_expanded_df_fi) #86066

data_merged_fi = merge(final.country, geocountry_expanded_df_fi, by = "TSU.ID_MERGED_Padded", all= T )

data_merged_fi[which(data_merged_fi$GEO_country_single == "NA"), ]$GEO_country_single = NA

data_merged_fi$GEO_country_single = factor(as.character(data_merged_fi$GEO_country_single)) # to remove text "NA" from the levels


head(data_merged_fi)
str(data_merged_fi)
nrow(data_merged_fi) #86066

head(data_merged_fi)

write.xlsx(data_merged_fi, "data_country_merged.xlsx")
# data_merged$PY.f = factor(data_merged$PY)
# data_merged[1:10, ]

# GEO_country_single_tb = table(data_merged$GEO_country_single)
# names(GEO_country_single_tb)

# Method Family 1: TS20
# Method Family 2: TS21
# Method Family 3: TS22
# Method Family 4: TS23

# PY  (1981 ~ 2020)

# Method General
# Method A: TS18
# Method B: TS19


# method.subset.tem <- subset(final.country, TS20 == 1)
yr_start = 2010
yr_end= 2020


data_MF_year <- subset(final.country, PY >= yr_start & PY <= yr_end)
nrow(data_MF_year) #[1] 56650

############################################################################################
#### Figure
############################################################################################
head(data_merged_fi)
Decade = cut(data_merged_fi$PY, breaks = seq(1980, 2020, 10), include.lowest = F)
levels(Decade)
levels(Decade) = paste0("D", as.integer(seq(1980, 2020, 10)))

data_merged_fi$Decade = factor(as.character(Decade))

IPBES.region.decade.tsu <- table(data_merged_fi$Decade, data_merged_fi$GEO_country_single) # NA... not recognized..
barplot(IPBES.region.decade.tsu)

# Method Family 1: TS20
# Method Family 2: TS21
# Method Family 3: TS22
# Method Family 4: TS23

method_by_year_by = by(data_merged_fi[, paste0("TS2", 0:3)], INDICES = data_merged_fi$PY, FUN = rbind)
method_by_year_df = do.call("rbind", lapply(method_by_year_by, FUN = colSums))


method_by_decade_by = by(data_merged_fi[, paste0("TS2", 0:3)], INDICES = data_merged_fi$Decade, FUN = rbind)

t1= colSums(method_by_decade_by$D1980)
t2 = colSums(method_by_decade_by$D1990)
t3 = colSums(method_by_decade_by$D2000)
t4 = colSums(method_by_decade_by$D2010)

dt_plot_method_by_decade = cbind(t1, t2, t3, t4)

library(viridis)
col_method = viridis(4)
decade_names = c("1980", "1990", "2000", "2010")
method_names = paste0("Method Family", 1:4)

#pdf("Figures/FIG_IPBESmethod_by_decade.pdf", width = 12, height = 8)
par(mfrow=c(1,1))
barplot(dt_plot_method_by_decade, names = decade_names , col = col_method)
legend("topleft", legend =method_names, col = col_method, pch = 15, bty = "n")
#dev.off()

#pdf("Figures/FIG_IPBESmethod_by_year.pdf", width = 12, height = 8)

barplot(t(method_by_year_df), names = rownames(method_by_year_df) , col = col_method, ylim=c(0, max(rowSums(method_by_year_df))*1.2))
legend("topleft", legend =method_names, col = col_method, pch = 15, bty = "n")
#dev.off()


#
# TS20  TS21  TS22  TS23
# 34870  6359  4932  4079

###### Pie chart
method_by_region_by = by(data_merged_fi[, paste0("TS2", 0:3)], INDICES = data_merged_fi$GEO_country_single, FUN = rbind)
method_by_region_df = do.call("rbind", lapply(method_by_region_by, FUN = colSums))


#pdf("Figures/FIG_IPBESmethod_by_region.pdf", width = 18, height = 12, pointsize = 18)

radius = rowSums(method_by_region_df)
radius  = radius / sum(radius) * 3

colnames(method_by_region_df) = method_names

par(mfrow=c(2,2), mar = c(5,5,5,5))
for (i in 1:4) {
  pie(method_by_region_df[i,], radius = radius[i], col = col_method, main = paste0(rownames(method_by_region_df)[i], " (n=",  rowSums(method_by_region_df)[i], ")"))
}
# legend("topleft", legend =method_names, col = col_method, pch = 15, bty = "n")
dev.off()


# png("Figures/FIG_IPBESmethod_by_region.png", width = 700, height = 600, pointsize = 17, bg = NA)
#pdf("Figures/FIG_IPBESmethod_by_region_transparent.pdf",width = 18, height = 12, pointsize = 18,  bg = "transparent")

radius = rowSums(method_by_region_df)
radius  = radius / sum(radius) * 3.5

colnames(method_by_region_df) = method_names

par(mfrow=c(2,2), mar = c(5,5,5,5), bg= NA)
for (i in 1:4) {
  pie(method_by_region_df[i,], radius = radius[i], col = col_method,labels = "", main = paste0(rownames(method_by_region_df)[i], " (n=",  rowSums(method_by_region_df)[i], ")"))
}
# legend("topleft", legend =method_names, col = col_method, pch = 15, bty = "n")
dev.off()

#pdf("Figures/FIG_MethodFamily_Legend.pdf", width = 4, height = 4, pointsize = 18,  bg = "transparent")
par(mfrow=c(1,1), mar =c (1,1,1,1))
plot(NULL ,xaxt='n',yaxt='n',bty='n',ylab='',xlab='', xlim=0:1, ylim=0:1)
legend("topleft", legend =method_names, col = col_method, pch = 15, bty = "n")
dev.off()



###### Number of method families applied per valuation studies
# Method Family 1: TS20
# Method Family 2: TS21
# Method Family 3: TS22
# Method Family 4: TS23

method_code = paste0("TS", 20:23)
MF_count <- data.final[,method_code]

nrow(MF_count)
sum(table(rowSums(MF_count)))
#     0     1     2     3     4
# 34081 43422  1463    73     1
MF_notZero <- table(rowSums(MF_count))[-1]
pie(MF_notZero)
dev.off(); barplot(MF_notZero)



