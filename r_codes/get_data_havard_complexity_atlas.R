rm(list=ls())
library(haven)
library(data.table)

#years_availlable <- 1962:2015

###################################################################################
##### DOWNLOAD DATA ###############################################################
###################################################################################
#save_path <- "/home/knallbunt/uni/forschung/data/wtn/raw"
#base_url <- "https://intl-atlas-downloads.s3.amazonaws.com/CCPY/S2_final_"
#file_ending <- ".dta"
#year <- years_availlable[1]
#complete_name <- paste(base_url, year, file_ending, sep = "")
#for (y in 1:length(years_availlable)){
#   complete_name <- paste(base_url, years_availlable[y], file_ending, sep = "")
#   name_ <- paste(save_path, years_availlable[y], ".dta", sep = "")
#   print(name_)
#   download.file(url = complete_name, destfile = name_)
# }
########################################################################################


######################################################################################################
#### TRANSFORM TO SINGLE DATA SET ####################################################################
######################################################################################################
# save_path <- "/home/knallbunt/uni/forschung/data/wtn/raw/raw"
# year <- years_availlable[1]
# data_frame <- list()
# for (y in 1:length(years_availlable)){
#   t_y <- years_availlable[y]
#   print(t_y)
#  file_import <- paste(save_path, t_y, ".dta", sep = "")
#  data_frame[[as.character(paste("Y_", t_y, sep = ""))]] <- as.data.table(read_dta(file_import))
# }
 
 save_path_full <- "/home/knallbunt/uni/forschung/data/wtn/full_atlas.csv"
# full_frame <- rbindlist(data_frame)
# fwrite(full_frame, file=save_path_full, na = "NA")
######################################################################################################

# read in data
atlas_data <- fread(save_path_full, verbose=TRUE, na.strings = "NA", 
                    colClasses = c("numeric", "character", "character", 
                                   "character", "numeric", "numeric"))