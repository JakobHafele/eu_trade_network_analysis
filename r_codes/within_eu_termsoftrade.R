# calc inner european terms of trade

rm(list=ls())

library(data.table)
library(readr)
library(IndexNumR)
library(tidyr)
library(dplyr)
library(haven)
library(countrycode)



# set up empty dataframe for results ---------
within_eu_trade_bal <- data.frame(exporter = character(), year = double(), export_value_agg = double(), ex_qnt_agg = double(), import_value_agg = double(), im_qnt_agg = double())


# get within eu trade bal for each country ---------

for (y in c(2000:2014)) {
print(y)
year_analyzed <- y

# read data --------

# read countriecode dict
oec_countrycodes <- fread("data/wtn/country_names.tsv")
oec_countrycodes[,"id_3char"] <- toupper(oec_countrycodes$id_3char) #capitals 4 matching with other countrycodes

# read product dict
product_dict <- fread("data/products_sitc_rev2.tsv")


hvrd_complexity_data <- fread("data/hvrd_product_complexity_data.csv")
hvrd_complexity_data <- hvrd_complexity_data[,-("V1")]
hvrd_complexity_data <- na.omit(hvrd_complexity_data)

# raw_filename <- paste("C:/uni/forschung/data/wtn/raw/raw", year_analyzed, ".dta", sep="")
# raw_data <- read_dta(raw_filename)
# raw_data$import_value <- NULL #drop import_value
# data_v1 <- raw_data #[!(raw_data$export_value=="0"), ] #drop all 0 exports, depreciated um später keine probleme zu erzeugen

raw_data <- read_dta(paste("data/wtn/raw/H0_", year_analyzed, ".dta", sep=""))
raw_data$import_value <- NULL #drop import_value
data_v1 <- raw_data

# merge and clean Data ---------
# countrycodes + get product sitc ids right in complexity dataset
product_dict <- as.data.frame(product_dict)
oec_countrycodes <- as.data.frame(oec_countrycodes)
data_v1 <- data_v1[!(data_v1$exporter %in% c("ANS", "VDR", "SXM") | data_v1$importer %in% c("ANS", "VDR", "SXM")), ]
data_v1$exporter <- countrycode(data_v1$exporter, "id_3char", "id", custom_dict = oec_countrycodes)
data_v1$importer <- countrycode(data_v1$importer, "id_3char", "id", custom_dict = oec_countrycodes)

#  add complexity
hvrd_complexity_data$year <- as.numeric(as.character(hvrd_complexity_data$year))
hvrd_complexity_data$commoditycode <- as.character(hvrd_complexity_data$commoditycode)
data_v2 <- left_join (data_v1, hvrd_complexity_data, by=c('year', 'commoditycode') ) #erste table der wo die daten angesetzt werden.
data_v2 <- data_v1

# calc innereurope tot

data_v3 <- data_v2

# merge rest of the world
data_v3$exporter [-grep("^eu", data_v3$exporter)] <- "world"
data_v3$importer [-grep("^eu", data_v3$importer)] <- "world"
data_v3$exporter [grep("eualb|euand|eubih|euisl|eumda|eumkd|eumne|eunor|eurus|eusrb|euukr|euvat|eusmr|eufro|eusun|euyug|eugib|eublr|euche", data_v3$exporter)]  <- "world"
data_v3$importer [grep("eualb|euand|eubih|euisl|eumda|eumkd|eumne|eunor|eurus|eusrb|euukr|euvat|eusmr|eufro|eusun|euyug|eugib|eublr|euche", data_v3$importer)]  <- "world"

data_v4 <- data_v3 %>% 
  group_by(exporter, importer, commoditycode, year) %>%
  summarize(export_value = sum(export_value), quantity = sum(quantity)) %>%
  ungroup()
data_v5 <- data_v4[!(data_v4$exporter==data_v4$importer),] # remove loops


data_v6 <- data_v5 %>%
    group_by(exporter, importer, year) %>%
    summarise(export_value_agg = sum(export_value, na.rm=TRUE), qnt_agg = sum(quantity, na.rm=TRUE)) %>%
    ungroup()

# drop world

data_v7 <- data_v6[!data_v6$exporter == "world" & !data_v6$importer == "world",]

# create trade stats

data_exports <- data_v7 %>%
  group_by(exporter, year) %>%
  summarise(export_value_agg = sum(export_value_agg, na.rm=TRUE), ex_qnt_agg = sum(qnt_agg, na.rm=TRUE)) %>%
  ungroup()

data_imports <- data_v7 %>%
  group_by(importer, year) %>%
  summarise(import_value_agg = sum(export_value_agg, na.rm=TRUE), im_qnt_agg = sum(qnt_agg, na.rm=TRUE)) %>%
  ungroup()

data_agg <- left_join(data_exports, data_imports, by = c("exporter" = "importer", "year" = "year"))

# save data
within_eu_trade_bal <- rbind(within_eu_trade_bal, data_agg)


}

# save / load
write_csv(within_eu_trade_bal, "C:/uni/forschung/WTN/eu_trade_network/output/within_eu_trade_bal_nw.csv")
within_eu_trade_bal <- read_csv("C:/uni/forschung/WTN/eu_trade_network/output/within_eu_trade_bal_nw.csv")



tot_dat <- within_eu_trade_bal
tot_dat$countryid <- as.numeric(as.factor(tot_dat$exporter))
tot_dat$yearid <- as.numeric(as.factor(tot_dat$year))


tot_results <- as.data.frame(c(2000:2014))
colnames(tot_results) <- c("year")

country_cons <- c("euaut", "eubel", "eubgr", "eucze", "eudeu", "eudnk", "euesp", "euest", "eufin", "eufra",
                  "eugbr", "eugrc", "euhrv", "euhun", "euirl", "euita", "eultu", "eulux", "eulva", "eumlt",
                  "eunld", "eupol", "euprt", "eurou", "eusvk", "eusvn", "euswe")
for (country in country_cons){
calc <- data.frame(matrix(nrow = 15))
calc$ex_pi <- priceIndex(tot_dat[tot_dat$exporter == country, ], pvar="export_value_agg", qvar="ex_qnt_agg", pervar="yearid", indexMethod="paasche",  prodID="countryid")
calc$im_pi <- priceIndex(tot_dat[tot_dat$exporter == country, ], pvar="import_value_agg", qvar="im_qnt_agg", pervar="yearid", indexMethod="paasche",  prodID="countryid")
calc$tot <- calc$ex_pi / calc$im_pi
calc$year <- c(2000:2014)
setnames(calc, "tot", country)
subset <- calc[,4:5]
tot_results <- merge(tot_results,subset, by = c("year"))
}




