#### build correspondence tables: isic (= classification @ wiot) to hs92 (= classification @ hvrd complexity index); and Exiobase 2 to hs92  ####
# isic -> cpc2 -> hs7 -> hs0 ; exio -> cpa02 -> hs7 -> hs0
# cpc2 codes only up to 4xxxx, everything >= 5 is construction and services
#
# # read files
# isic_strucutre <- fread("data/wtn/correspondence/wiod/ISIC_Rev_4_english_structure.csv") #  (https://unstats.un.org/unsd/cr/registry/isic-4.asp)
# isic4_cpc2 <- fread("data/wtn/correspondence/wiod/isic-cpc-hs-hs/ISIC4-CPC2.csv") # (https://unstats.un.org/unsd/cr/registry/isic-4.asp)
# cpc2_hs7 <- fread("data/wtn/correspondence/wiod/isic-cpc-hs-hs/CPCv2_HS07.csv") # (https://unstats.un.org/unsd/cr/registry/cpc-2.asp)
# hs7_hs92 <- fread("data/wtn/correspondence/wiod/isic-cpc-hs-hs/hs07-hs92.csv") # (https://unstats.un.org/unsd/trade/classifications/correspondence-tables.asp)
#
# # for wiot
# corresp_wiot <- isic4_cpc2[,!"CPC2part"] # drop info about if its 1:n/n:1/n:n correspondence (see https://unstats.un.org/unsd/trade/classifications/corr-notes/HS2017%20conversion%20to%20earlier%20HS%20versions%20and%20other%20classifications.pdf)
# corresp_wiot <- left_join (corresp_wiot, cpc2_hs7, by=c('CPC2code'='CPC2Code') ) # erste table der wo die daten angesetzt werden.
# corresp_wiot$HS07Code <- gsub('[.]', '', corresp_wiot$HS07Code) # remove "." from hs codes.
# corresp_wiot$HS07Code <- as.numeric(corresp_wiot$HS07Code)
# corresp_wiot <- na.omit(corresp_wiot)
# corresp_wiot <- left_join (corresp_wiot, hs7_hs92, by=c('HS07Code'='HS 2007'))
# corresp_wiot <- subset(corresp_wiot, select = -c(CPC2Part, HS07Part))
# 
# write.csv(corresp_wiot, file = "data/io tables/correspondence/wiod/isic-cpc-hs-hs/isic4_cpc2_hs07_hs92.csv")
