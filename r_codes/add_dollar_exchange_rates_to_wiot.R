wiot_complexity_data <- readRDS("data/wtn/wiot/wiot_complexity_panel_w_i.Rda")
exchange_oecd <- read_csv("data/exchangerates_oecd_2000-2014.csv")


wiot_complexity_data_2 <- left_join(wiot_complexity_data, exchange_oecd[, c("Value", "TIME", "LOCATION")], by = c("Country" = "LOCATION", "Year" = "TIME"))
setnames(wiot_complexity_data_2, "Value", "national_curr_dollar_exchange")
leftout_countries <- wiot_complexity_data_2[is.na(wiot_complexity_data_2$national_curr_dollar_exchange), "Country"]
leftout_countries <- unique(leftout_countries) # only taiwan has missing data

# erase "." as thousands seperator
wiot_complexity_data_2$COMP <- gsub("[.]", "", as.numeric(as.character(wiot_complexity_data_2$COMP)))
wiot_complexity_data_2$CAP <- gsub("[.]", "", as.numeric(as.character(wiot_complexity_data_2$CAP)))
wiot_complexity_data_2$K <- gsub("[.]", "", as.numeric(as.character(wiot_complexity_data_2$K)))
wiot_complexity_data_2$LAB <- gsub("[.]", "", as.numeric(as.character(wiot_complexity_data_2$LAB)))
wiot_complexity_data_2$VA.y <- gsub("[.]", "", as.numeric(as.character(wiot_complexity_data_2$VA.y)))
  
# calc dollar prices
wiot_complexity_data_2[, "COMP_Dollar"] <- as.numeric(as.character(wiot_complexity_data_2$COMP)) / as.numeric(as.character(wiot_complexity_data_2$national_curr_dollar_exchange))
wiot_complexity_data_2[, "CAP_Dollar"] <- as.numeric(as.character(wiot_complexity_data_2$CAP)) / as.numeric(as.character(wiot_complexity_data_2$national_curr_dollar_exchange))
wiot_complexity_data_2[, "K_Dollar"] <- as.numeric(as.character(wiot_complexity_data_2$K)) / as.numeric(as.character(wiot_complexity_data_2$national_curr_dollar_exchange))
wiot_complexity_data_2[, "LAB_Dollar"] <- as.numeric(as.character(wiot_complexity_data_2$LAB)) / as.numeric(as.character(wiot_complexity_data_2$national_curr_dollar_exchange))
wiot_complexity_data_2[, "VA_from_national_prices(VA.y)_Dollar"] <- as.numeric(as.character(wiot_complexity_data_2$VA.y)) / as.numeric(as.character(wiot_complexity_data_2$national_curr_dollar_exchange))

wiot_complexity_data_2 <- wiot_complexity_data_2 %>%
  set_variable_labels("II" = "Intermediate inputs at current purchasers' prices (in millions of national currency)",
                      "COMP_Dollar" = "Compensation of employees (in millions of Dollar)",
                      "CAP_Dollar" = "Capital compensation (in millions of Dollar)" ,
                      "K_Dollar" = "capital compensation (in millions of Dollar)",
                      "LAB_Dollar" = "Labour compensation (in millions of Dollar)",
                      "VA_$"="Million Dollar", 
                      "CAP"="Capital compensation (in millions of national currency)",
                      "LAB"="Labour compensation (in millions of national currency)",
                      "K"="capital compensation (in millions of national currency)",
                      "COMP"="Compensation of employees (in millions of national currency)",
                      "H_EMPE"="Total hours worked by employees (millions)",
                      "EMPE"="Number of employees (thousands)",
                      "EMP"="Number of persons engaged (thousands)",
                      "VA.y"="Gross value added at current basic prices (in millions of national currency)",
                      "GO"="Gross output by industry at current basic prices (in millions of national currency)"
  )

saveRDS(wiot_complexity_data_2, "/home/knallbunt/uni/forschung/data/wtn/wiot/wiot_complexity_panel_w_i_dollar.Rda")
