rm(list=ls())

library(readr)
library(dplyr)
library(countrycode)
library(tidyr)
library(igraph)
library(ggplot2)
library(moments)
library(data.table)
library(haven)
library(labelled)
library(directlabels)
library(scales)
library(tidygraph)
library(assortnet)
library(tnet)
library(rowr)
library(xtable)
library(rgexf)




source("functions_etn.R")
source("analyze_network_function.R")

# marco data to be added as vertex attributes -- needs to be changed in function add_vertex_attr() as well
vars_to_be_added <- c("population", 
                      "Adjusted_wage_share",
                      "GDP_per_hour_worked_curr_prices",
                      "GDP_Dollar_curr",
                      "GDP_growth",
                      "complexity_rank_HH",
                      "complexity_harv"
                      )
# Core, Periphery clusters (if adjusted, needs to be adjusted at "# create clustered graphs", result merging, adding complexity lvls to lists, applying network_analysis function, creating empty dataframes)
Core <- "eudeu|eufra|euita|eugbr|euaut|eudnk|euswe|eufin|eunld|eubel|eulux|euirl"
Periphery <- "euprt|euesp|eugrc|eumlt|eucyp"
Semiperiphery <- "euhun|eucze|eusvn|eusvk|eupol"
East <- "eubgr|eurou|euhrv"
Baltics <- "euest|eulva|eultu"

# cutoffvalue for binary graph (currently used only for nominal assortativity with eci)
cutoffvalue <- 0.5

# cluster <- list(Core=c("eudeu", "eufra", "eugbr", "euaut", "eudnk", "euswe", "eufin", "eunld", "eubel", "eulux"), 
#                 Periphery=c("euprt", "euesp", "eugrc", "eumlt"), 
#                 Semiperiphery=c("euhun", "eucze", "eusvn", "eusvk", "eupol"), 
#                 East=c("eubgr", "eurou", "euhrv"), 
#                 Baltics=c("euest", "eulva", "eultu"))

##### Set up empty data frames and lists #####
measures_local <- list()
correlations_global <- data.frame(year=double(), cor.degree_strength=double(), cor.clust_degree=double(), cor.clust_stren=double(), complexity_lvl=double())
network_macro_cor <- data.frame(year_table=double(), Var1=character(), Var2=character(), correlation=double(), complexity_lvl=double())
characteristics_global <- data.frame(year=double(), complexity_lvl=double(), network.type=character(), density=double(), reciprocity=double(), dist.mean=double(), deg.mean=double(), dist.sd=double(), dist.kurt=double(), dist.skew=double(), global_clust=double(), edge.density=double(), assort_ECI=double(), assort_GDP_cur=double(), assort_GDP_hour=double(), assort_pop=double(), assort_Wage_share=double(), assort_stren=double(), assort_ECI_stren=double(), assort_ECI_cutoff=double(), assort_closeness=double())
characteristics_global_core <- data.frame(year=double(), complexity_lvl=double(), network.type=character(), density=double(), reciprocity=double(), dist.mean=double(), deg.mean=double(), dist.mean.ph=double(), dist.sd=double(), dist.kurt=double(), dist.skew=double(), global_clust=double(), edge.density=double())
characteristics_global_periphery <- data.frame(year=double(), complexity_lvl=double(), network.type=character(), density=double(), reciprocity=double(), dist.mean=double(), deg.mean=double(), dist.mean.ph=double(), dist.sd=double(), dist.kurt=double(), dist.skew=double(), global_clust=double(), edge.density=double())
characteristics_global_semip <- data.frame(year=double(), complexity_lvl=double(), network.type=character(), density=double(), reciprocity=double(), dist.mean=double(), deg.mean=double(), dist.mean.ph=double(), dist.sd=double(), dist.kurt=double(), dist.skew=double(), global_clust=double(), edge.density=double())
characteristics_global_east <- data.frame(year=double(), complexity_lvl=double(), network.type=character(), density=double(), reciprocity=double(), dist.mean=double(), deg.mean=double(), dist.mean.ph=double(), dist.sd=double(), dist.kurt=double(), dist.skew=double(), global_clust=double(), edge.density=double())
characteristics_global_baltics <- data.frame(year=double(), complexity_lvl=double(), network.type=character(), density=double(), reciprocity=double(), dist.mean=double(), deg.mean=double(), dist.mean.ph=double(), dist.sd=double(), dist.kurt=double(), dist.skew=double(), global_clust=double(), edge.density=double())
trade_stats <- data.frame(Year=double(), Country=character(), EX_Total=double(), EX_c1=double(), EX_c2=double(), EX_c3=double(), EX_c4=double(), EX_c5=double(), EX_c6=double(), EX_c7=double(), EX_c8=double(), EX_c9=double(), EX_c10=double(), IM_Total=double(), IM_c1=double(), IM_c2=double(), IM_c3=double(), IM_c4=double(), IM_c5=double(), IM_c6=double(), IM_c7=double(), IM_c8=double(), IM_c9=double(), IM_c10=double(), BAL_c1=double(), BAL_c2=double(), BAL_c3=double(), BAL_c4=double(), BAL_c5=double(), BAL_c6=double(), BAL_c7=double(), BAL_c8=double(), BAL_c9=double(), BAL_c10=double())    
trade_stats_cp <- data.frame(year=double(),exporter=character(), importer=character(), export_value_agg=double(), complexity=double())
# rich_club_results <- list()

for (y in 1965:2014){
  print(y)
  year_analyzed <- y
  # year_analyzed <- 2015

  
#### read data ####
# read countriecode dict
oec_countrycodes <- fread("data/wtn/country_names.tsv")
oec_countrycodes[,"id_3char"] <- toupper(oec_countrycodes$id_3char) #capitals 4 matching with other countrycodes

# read product dict
product_dict <- fread("data/products_sitc_rev2.tsv")

# read complexity ranking
hvrd_complexity_data <- fread("data/hvrd_product_complexity_data.csv")
hvrd_complexity_data <- hvrd_complexity_data[,-("V1")]
hvrd_complexity_data <- na.omit(hvrd_complexity_data)

# read datafile
raw_filename <- paste("data/wtn/raw/raw", year_analyzed, ".dta", sep="")
raw_data <- read_dta(raw_filename)
raw_data$import_value <- NULL #drop import_value
data_v1 <- raw_data #[!(raw_data$export_value=="0"), ] #drop all 0 exports, depreciated um später keine probleme zu erzeugen

# Import the dataset with macro properties of countries
macro_data <- read_csv("data/Macro_data_trade-v26.csv") 
macro_data_wb <- read_csv("data/gdp_worldbank_clean.csv")


#### merge and clean Data ####
# countrycodes + get product sitc ids right in complexity dataset
product_dict <- as.data.frame(product_dict)
oec_countrycodes <- as.data.frame(oec_countrycodes)
data_v1 <- data_v1[!(data_v1$exporter %in% c("ANS", "VDR", "SXM") | data_v1$importer %in% c("ANS", "VDR", "SXM")), ]
data_v1$exporter <- countrycode(data_v1$exporter, "id_3char", "id", custom_dict = oec_countrycodes)
data_v1$importer <- countrycode(data_v1$importer, "id_3char", "id", custom_dict = oec_countrycodes)

#  add complexity
data_v2 <- left_join (data_v1, hvrd_complexity_data, by=c('year', 'commoditycode') ) #erste table der wo die daten angesetzt werden.

# clean worldbank gdp data
# macro_data_wb_1 <- macro_data_wb[, -c(1:2)]
# macro_data_wb_2 <- melt(macro_data_wb_1, c("Country Name", "Country Code"))
# colnames(macro_data_wb_2) <- c("Country_Name", "Country_Code", "Year", "GDP_Dollar_curr")
# macro_data_wb_2$Year <- substring(macro_data_wb_2$Year, 1, 4)
# macro_data_wb_3 <- macro_data_wb_2
# macro_data_wb_3[macro_data_wb_3 == ".."] <- NA
# macro_data_wb_4 <- macro_data_wb_3[!is.na(macro_data_wb_3$Country_Name),]
# write_csv(macro_data_wb_4, "data/gdp_worldbank_clean.csv")

# add countrycode to wb macro gdp data
oec_countrycodes_wb <- oec_countrycodes
oec_countrycodes_wb[172, 3] <- "Slovak Republic"
macro_data_wb_matching <- left_join(macro_data_wb, oec_countrycodes_wb, by = c("Country_Name"="name"))


# build macrodata subsets fitting for networks
#  add countrycodes to macrodata for joining with european country network
macro_data_1 <- left_join (macro_data, oec_countrycodes, by = c("Country" = "name"))

# add gdp from worldbank
macro_data_1 <- left_join (macro_data_1, macro_data_wb_matching[, c("Year", "id", "GDP_Dollar_curr")], by = c("Year", "id"))

#  sum up values for Core Periphery categories
macro_data_2 <- macro_data_1[, c("population", "GDP_Dollar_curr", "Year", "Country")]
macro_data_2$Country <- countrycode(macro_data_2$Country, "name", "id", custom_dict = oec_countrycodes)
macro_data_2$Country [grep(Core, macro_data_2$Country)] <- "Core"
macro_data_2$Country [grep(Periphery, macro_data_2$Country)] <- "Periphery"
macro_data_2$Country [grep(Semiperiphery, macro_data_2$Country)] <- "Semiperiphery"
macro_data_2$Country [grep(East, macro_data_2$Country)] <- "East"
macro_data_2$Country [grep(Baltics, macro_data_2$Country)] <- "Baltics"
macro_data_2 <- macro_data_2[grep("Core|Periphery|Semiperiphery|East|Baltics", macro_data_2$Country),]
macro_data_cp <- macro_data_2 %>% 
  group_by(Country, Year) %>%
  summarize_at(c("population", "GDP_Dollar_curr"), sum, na.rm=TRUE) %>%
  ungroup()
macro_data_cp[macro_data_cp == 0] <- NA # NAs instead of zeros created from sum function
macro_data_cp_ya <- macro_data_cp[macro_data_cp$Year==year_analyzed,]




############################## create Networks 4 analysis ###############################

#  create eu trade network with usa, china and world 
etn <- data_v2
etn$exporter [-grep("^eu|aschn|nausa|ascyp", etn$exporter)] <- "world"
etn$importer [-grep("^eu|aschn|nausa|ascyp", etn$importer)] <- "world"
# eu only
etn$exporter [grep("eualb|euand|eubih|euisl|eumda|eumkd|eumne|eunor|eurus|eusrb|euukr|euvat|eusmr|eufro|eusun|euyug|eugib|eublr|euche", etn$exporter)]  <- "world"
etn$importer [grep("eualb|euand|eubih|euisl|eumda|eumkd|eumne|eunor|eurus|eusrb|euukr|euvat|eusmr|eufro|eusun|euyug|eugib|eublr|euche", etn$importer)]  <- "world"

etn <- etn %>% 
  group_by(exporter, importer, commoditycode, pci, year) %>%
  summarize(export_value = sum(export_value)) %>%
  ungroup()
etn_w_loops <- etn
etn<-etn[!(etn$exporter==etn$importer),] # remove loops

#### add Core Periphery clusters ####

etn_cp <- etn
etn_cp$exporter [grep(Core, etn_cp$exporter)] <- "Core"
etn_cp$importer [grep(Core, etn_cp$importer)] <- "Core"
etn_cp$exporter [grep(Periphery, etn_cp$exporter)] <- "Periphery"
etn_cp$importer [grep(Periphery, etn_cp$importer)] <- "Periphery"
etn_cp$exporter [grep(Semiperiphery, etn_cp$exporter)] <- "Semiperiphery"
etn_cp$importer [grep(Semiperiphery, etn_cp$importer)] <- "Semiperiphery"
etn_cp$exporter [grep(East, etn_cp$exporter)] <- "East"
etn_cp$importer [grep(East, etn_cp$importer)] <- "East"
etn_cp$exporter [grep(Baltics, etn_cp$exporter)] <- "Baltics"
etn_cp$importer [grep(Baltics, etn_cp$importer)] <- "Baltics"
etn_cp$exporter [-grep("Core|Periphery|Semiperiphery|East|Baltics|aschn|nausa", etn_cp$exporter)] <- "world" # somehow doesnt work!
etn_cp$importer [-grep("Core|Periphery|Semiperiphery|East|Baltics|aschn|nausa", etn_cp$importer)] <- "world"
etn_cp <- etn_cp %>% 
  group_by(exporter, importer, commoditycode, pci, year) %>%
  summarize(export_value = sum(export_value)) %>%
  ungroup()
etn_cp <- etn_cp[!(etn_cp$exporter==etn_cp$importer),] # remove loops
# delete aschncp u nausacp + world + rest 
etn_cp <- etn_cp[(etn_cp$exporter %in% c("Core", "Periphery", "Semiperiphery", "East", "Baltics", "nausa", "aschn", "world") & etn_cp$importer %in% c("Core", "Periphery", "Semiperiphery", "East", "Baltics", "nausa", "aschn", "world")),]
etn_cp[etn_cp=="aschn"] <- "aschn_cp"
etn_cp[etn_cp=="nausa"] <- "nausa_cp"
etn_cp[etn_cp=="world"] <- "world_cp"
# Core - Periphery bind
etn <- bind_rows(etn, etn_cp)



#  create etn w/o products
etn_no_prod <- no_prod(etn)
setDT(etn_no_prod)


#  create several networks with complexity thresholds
thresholds <- quantile(hvrd_complexity_data$pci, c(.1, .2, .3, .4, .5, .6, .7, .8, .9))  #creating thresholds at 20% 40% etc. of the distribution of all pci values since 1965
thresholds <- data.frame(as.list(thresholds))
etn_1 <- etn[etn$pci < thresholds$X10., ]
etn_2 <- etn[etn$pci >= thresholds$X10. & etn$pci < thresholds$X20., ]
etn_3 <- etn[etn$pci >= thresholds$X20. & etn$pci < thresholds$X30., ]
etn_4 <- etn[etn$pci >= thresholds$X30. & etn$pci < thresholds$X40., ]
etn_5 <- etn[etn$pci >= thresholds$X40. & etn$pci < thresholds$X50., ]
etn_6 <- etn[etn$pci >= thresholds$X50. & etn$pci < thresholds$X60., ]
etn_7 <- etn[etn$pci >= thresholds$X60. & etn$pci < thresholds$X70., ]
etn_8 <- etn[etn$pci >= thresholds$X70. & etn$pci < thresholds$X80., ]
etn_9 <- etn[etn$pci >= thresholds$X80. & etn$pci < thresholds$X90., ]
etn_10 <- etn[etn$pci >= thresholds$X90., ]

#  creat thresholded networks w/o products
etn_thresholds_list <- list(etn_1, etn_2, etn_3, etn_4, etn_5, etn_6, etn_7, etn_8, etn_9, etn_10) #creates list with all dfs
etn_thresholds_list <- lapply(etn_thresholds_list, no_prod) #apply no prod function to all dfs in list
etn_thresholds_list_1 <- lapply(etn_thresholds_list, na.omit)








#### create graphs 4 analysis ####
# european trade network: all complexity lvls; eu only; countries
# etn_no_prod_pop <- left_join(etn_no_prod, macro_data_1[macro_data_1$Year==year_analyzed, c("id", "population")], by = c("exporter"="id"))

etn_graph_allcomplexity <- build_graph("^eu", etn_no_prod) # 1: choose which countries to grep; 2: input network

# european trade network: complexity lvled networks; eu only; countries
etn_graph_1 <- build_graph("^eu", etn_thresholds_list_1[[1]])
etn_graph_2 <- build_graph("^eu", etn_thresholds_list_1[[2]])
etn_graph_3 <- build_graph("^eu", etn_thresholds_list_1[[3]])
etn_graph_4 <- build_graph("^eu", etn_thresholds_list_1[[4]])
etn_graph_5 <- build_graph("^eu", etn_thresholds_list_1[[5]])
etn_graph_6 <- build_graph("^eu", etn_thresholds_list_1[[6]])
etn_graph_7 <- build_graph("^eu", etn_thresholds_list_1[[7]])
etn_graph_8 <- build_graph("^eu", etn_thresholds_list_1[[8]])
etn_graph_9 <- build_graph("^eu", etn_thresholds_list_1[[9]])
etn_graph_10 <- build_graph("^eu", etn_thresholds_list_1[[10]])

graph_list_etn <- list(etn_graph_1,
                   etn_graph_2,
                   etn_graph_3,
                   etn_graph_4,
                   etn_graph_5,
                   etn_graph_6,
                   etn_graph_7,
                   etn_graph_8,
                   etn_graph_9,
                   etn_graph_10,
                   etn_graph_allcomplexity)

# create directed binary graphs
graph_list_etn_adj <- lapply(graph_list_etn, get.adjacency)
graph_list_etn_b <- lapply(graph_list_etn_adj, graph_from_adjacency_matrix, diag = FALSE)

# create undirected adj (for assortnet), with edge strength summed up
graph_list_etn_undirect <- lapply(graph_list_etn, as.undirected, mode = c("collapse"), edge.attr.comb=list(Weight="sum"))
graph_list_etn_adj_undirect <- lapply(graph_list_etn_undirect, as_adj, attr="Weight")

# create binary graph with cutoffvalue (assigned at top)
graph_list_etn_cutoff <- list(list(), list(), list (), list(), list(), list(), list (), list(), list(), list(), list ())
for (i in 1:11){
graph_list_etn_cutoff[[i]] <- delete.edges(graph_list_etn[[i]], which(E(graph_list_etn[[i]])$Weight < quantile(E(graph_list_etn[[i]])$Weight, c(cutoffvalue)) ))
}
graph_list_etn_cutoff_adj <-lapply(graph_list_etn_cutoff, get.adjacency)
graph_list_etn_cutoff_b <- lapply(graph_list_etn_cutoff_adj, graph_from_adjacency_matrix, diag = FALSE)




# european trade network: all complexity lvls; eu only; Core, Periphery
countries <- c("Core|Periphery|Semiperiphery|Baltics|East")
setDF(etn_no_prod)
etn_graph_cp_allcomplexity <- build_graph_2(countries, etn_no_prod) # takes 1: countries to grab and 2: input network. also adds (edge)weight as exports/population of exporting cluster and weight_abs as exports


# european trade network: complexity lvled networks; eu only
etn_graph_cp_1 <- build_graph_2(countries, etn_thresholds_list_1[[1]])
etn_graph_cp_2 <- build_graph_2(countries, etn_thresholds_list_1[[2]])
etn_graph_cp_3 <- build_graph_2(countries, etn_thresholds_list_1[[3]])
etn_graph_cp_4 <- build_graph_2(countries, etn_thresholds_list_1[[4]])
etn_graph_cp_5 <- build_graph_2(countries, etn_thresholds_list_1[[5]])
etn_graph_cp_6 <- build_graph_2(countries, etn_thresholds_list_1[[6]])
etn_graph_cp_7 <- build_graph_2(countries, etn_thresholds_list_1[[7]])
etn_graph_cp_8 <- build_graph_2(countries, etn_thresholds_list_1[[8]])
etn_graph_cp_9 <- build_graph_2(countries, etn_thresholds_list_1[[9]])
etn_graph_cp_10 <- build_graph_2(countries, etn_thresholds_list_1[[10]])

graph_list_cp <- list(etn_graph_cp_1,
                       etn_graph_cp_2,
                       etn_graph_cp_3,
                       etn_graph_cp_4,
                       etn_graph_cp_5,
                       etn_graph_cp_6,
                       etn_graph_cp_7,
                       etn_graph_cp_8,
                       etn_graph_cp_9,
                       etn_graph_cp_10,
                       etn_graph_cp_allcomplexity)


# add vertex attributes
countries <- unique(etn_no_prod$exporter)

# for eu only network with countries
data_added_etn <- as.data.frame(countries[grep("^eu", countries)])
setnames(data_added_etn, "countries[grep(\"^eu\", countries)]", "countries")
data_added_etn <- left_join(data_added_etn, macro_data_1[macro_data_1$Year == year_analyzed, c(vars_to_be_added, "id")], by = c("countries" = "id"))
data_added_etn[, "complexity_rank_ECI"] <- rank(data_added_etn$complexity_harv) # create rank from hvrd complexity data (more data than hh ranks). needed for assortativity function
data_added_etn[is.na(data_added_etn$complexity_harv), "complexity_rank_ECI"] <- NA

# apply to all etn graphs
graph_list_etn <- lapply(graph_list_etn, add_vertex_attr)
graph_list_etn_b <- lapply(graph_list_etn_b, add_vertex_attr)
graph_list_etn_cutoff_b <- lapply(graph_list_etn_cutoff_b, add_vertex_attr)


# create attribute which contains standardized [0-1] and (range-1) *(-1) weights for betweeness centr.
for (i in 1:11){
E(graph_list_etn[[i]])$betw_score <- (1- range01(E(graph_list_etn[[i]])$Weight) + 0.0000000001) * 100
}

  

# add attributes to cp_network
graph_list_cp <- lapply(graph_list_cp, add_pop_GDP_vertex)


# create clustered graphs (after add attribute part so that clustered networks have attributes too)
graph_list_etn_core <- list()
for(i in 1:11){
  graph_list_etn_core[[i]] <- delete.vertices(graph_list_etn[[i]], 
                                              V(graph_list_etn[[i]])[
                                                !grepl(Core, V(graph_list_etn[[i]])$name) ])
}

graph_list_etn_periphery <- list(list(), list(), list (), list(), list(), list(), list (), list(), list(), list(), list ())
for(i in 1:11){
  graph_list_etn_periphery[[i]] <- delete.vertices(graph_list_etn[[i]], 
                                                   V(graph_list_etn[[i]])[
                                                     !grepl(Periphery, V(graph_list_etn[[i]])$name) ])
}

graph_list_etn_baltics<- list(list(), list(), list (), list(), list(), list(), list (), list(), list(), list(), list ())
for(i in 1:11){
  graph_list_etn_baltics[[i]] <- delete.vertices(graph_list_etn[[i]], 
                                                 V(graph_list_etn[[i]])[
                                                   !grepl(Baltics, V(graph_list_etn[[i]])$name) ])
}

graph_list_etn_semip<- list(list(), list(), list (), list(), list(), list(), list (), list(), list(), list(), list ())
for(i in 1:11){
  graph_list_etn_semip[[i]] <- delete.vertices(graph_list_etn[[i]], 
                                               V(graph_list_etn[[i]])[
                                                 !grepl(Semiperiphery, V(graph_list_etn[[i]])$name) ])
}

graph_list_etn_east<- list(list(), list(), list (), list(), list(), list(), list (), list(), list(), list(), list ())
for(i in 1:11){
  graph_list_etn_east[[i]] <- delete.vertices(graph_list_etn[[i]], 
                                              V(graph_list_etn[[i]])[
                                                !grepl(East, V(graph_list_etn[[i]])$name) ])
}







################################# end of network creation section ####################



################################# Stats ######################

#### Trade stats #####
#  ex, im and trade balances of countries (agg) on different complexity lvls #


etn_trade_stats <- export_stats(etn_no_prod) # add total export stats
for (i in 1:10){
  # join export stats for thresholded networks (this method becaus lapply doesnt work with diiffering row lenghts)
    etn_trade_stats <- left_join(etn_trade_stats, export_stats(etn_thresholds_list_1[[i]][, c("exporter", "export_value_agg")]), by = c("exporter"))
}
etn_trade_stats <- left_join(etn_trade_stats, import_stats(etn_no_prod), by = c("exporter" = "importer")) # join total import stats
for (i in 1:10){
  # join import stats
  etn_trade_stats <- left_join(etn_trade_stats, import_stats(etn_thresholds_list_1[[i]][, c("importer", "export_value_agg")]), by = c("exporter" = "importer"))
}

colnames(etn_trade_stats)[2:12] <- c("EX_Total", "EX_c1", "EX_c2", "EX_c3", "EX_c4", "EX_c5", "EX_c6", "EX_c7", "EX_c8", "EX_c9", "EX_c10")
colnames(etn_trade_stats)[13:23] <- c("IM_Total", "IM_c1", "IM_c2", "IM_c3", "IM_c4", "IM_c5", "IM_c6", "IM_c7", "IM_c8", "IM_c9", "IM_c10")
for( i in 1:10) {
  etn_trade_stats[paste("BAL_c",i, sep="")] <- etn_trade_stats[paste("EX_c",i,sep="")] - etn_trade_stats[paste("IM_c",i,sep="")]
} # trade balance for complexity lvls: BAL_ci=EX_ci-IM_ci
etn_trade_stats[, "Year"] <- year_analyzed
setnames(etn_trade_stats, "exporter", "Country")

# trade balance between cores and peripheries over time
countries <- c("Core", "Periphery", "Semiperiphery", "Baltics", "East")
core_peri_bal <- etn_no_prod[etn_no_prod$exporter %in% countries & etn_no_prod$importer %in% countries,]
core_peri_bal[, "year"] <- year_analyzed
core_peri_bal[, "complexity"] <- 11

for (i in 1:10){
core_peri_bal_pre <- etn_thresholds_list_1[[i]][etn_thresholds_list_1[[i]]$exporter %in% countries & etn_thresholds_list_1[[i]]$importer %in% countries,]
core_peri_bal_pre[, "year"] <- year_analyzed
core_peri_bal_pre[, "complexity"] <- i
core_peri_bal <- rbind(core_peri_bal, core_peri_bal_pre)
}

# save results
trade_stats <- rbind(trade_stats, etn_trade_stats)
trade_stats_cp <- rbind(trade_stats_cp, core_peri_bal)

##### Network stats #####


# Analyze the weighted networks
  results <- lapply(graph_list_etn, analyze_network, year_analyzed)
  results_core <- lapply(graph_list_etn_core, analyze_network_clust, year_analyzed)
 # results[[1]][["global_characteristics_core"]] <- results_core[[1]]
  results_periphery <- lapply(graph_list_etn_periphery, analyze_network_clust, year_analyzed)
  if (vcount(graph_list_etn_semip[[11]]) > 0) {
    results_semip <- lapply(graph_list_etn_semip, analyze_network_clust, year_analyzed)
  }
  if (vcount(graph_list_etn_baltics[[11]]) > 0) {
    results_baltics <- lapply(graph_list_etn_baltics, analyze_network_clust, year_analyzed)
  }
  if (vcount(graph_list_etn_east[[11]]) > 0) {
  results_east <- lapply(graph_list_etn_east, analyze_network_clust, year_analyzed)
  }
  
  
# assortnet assortativity (weighted, undirected taking eci and weights into account, see cran)
for (i in 1:11){
  ECI <- V(graph_list_etn[[i]])$ECI_rank
  ECI[is.na(ECI)] <- 0
  results[[i]][["global_characteristics"]][["assort_ECI_streng"]] <- unlist(
    assortment.continuous(
      graph_list_etn_adj_undirect[[i]], ECI, weighted = TRUE)
    )[["r"]]
}

# assortnet assort with closeness
  for (i in 1:11){
    closen <- V(graph_list_etn[[i]])$closeness
    closen[is.na(closen)] <- 0
    results[[i]][["global_characteristics"]][["assort_closeness"]] <- unlist(
      assortment.continuous(
        graph_list_etn_adj_undirect[[i]], closen, weighted = TRUE)
    )[["r"]]
  }
  


# eci assort with cutoff
for(i in 1:11){
results[[i]][["global_characteristics"]][["assort_ECI_cutoff"]] <- assortativity_nominal(
  induced.subgraph(graph_list_etn_cutoff_b[[i]], which(!is.na(V(graph_list_etn_cutoff_b[[i]])$ECI_rank))), 
  types=na.omit(V(graph_list_etn_cutoff_b[[i]])$ECI_rank)
 )
}

  # add info about complexity thresholds
  for (i in 1:11){
    results[[i]][["global_characteristics"]]["complexity_lvl"] <- paste(i)
    results[[i]][["global_correlations"]]["complexity_lvl"] <- paste(i)
    results[[i]][["network.macro.cors"]]["complexity_lvl"] <- paste(i)
    results[[i]][["local_measures"]]["complexity_lvl"] <- paste(i)
    results_core[[i]]["complexity_lvl"] <- paste(i)
    results_periphery[[i]]["complexity_lvl"] <- paste(i)
  }
  if (vcount(graph_list_etn_semip[[11]]) > 0){
  for (i in 1:11){
  results_semip[[i]]["complexity_lvl"] <- paste(i)
  }}
  if (vcount(graph_list_etn_baltics[[11]]) > 0){
  for (i in 1:11){
  results_baltics[[i]]["complexity_lvl"] <- paste(i)
  }}
  if (vcount(graph_list_etn_east[[11]]) > 0){
  for (i in 1:11){
  results_east[[i]]["complexity_lvl"] <- paste(i) 
  }} # seperate loops in case clusters dont exist yet

  # Save the results
  for (i in 1:11) {
  measures_local[[as.character(year_analyzed)]][[paste("complexity_", i , sep="")]] <- results[[i]][["local_measures"]]
  correlations_global<- rbind(correlations_global, results[[i]][["global_correlations"]])
  network_macro_cor <- rbind(network_macro_cor, results[[i]][["network.macro.cors"]])
  characteristics_global <- rbind(characteristics_global, results[[i]][["global_characteristics"]])
  characteristics_global_core <- rbind(characteristics_global_core, results_core[[i]])
  characteristics_global_periphery <- rbind(characteristics_global_periphery, results_periphery[[i]])
  } 
  if (vcount(graph_list_etn_semip[[11]]) > 0){
  for (i in 1:11) {
  characteristics_global_semip <- rbind(characteristics_global_semip, results_semip[[i]])
  }}
  if (vcount(graph_list_etn_east[[11]]) > 0){
  for (i in 1:11) {
  characteristics_global_east <- rbind(characteristics_global_east, results_east[[i]])
  }}
  if (vcount(graph_list_etn_baltics[[11]]) > 0){
  for (i in 1:11) {
  characteristics_global_baltics <- rbind(characteristics_global_baltics, results_baltics[[i]])
  }}

######## core detection alogrythms -------
  # # rich club (tnet, weighted)
  # for (i in 1:11){
  # rich_club_coeff_2 <- weighted_richclub_w(as.tnet(cbind(as_edgelist(graph_list_etn[[i]], names = FALSE), E(graph_list_etn[[i]])$Weight)),
  #                                        rich="s", reshuffle="weights", directed = TRUE)
  # # plot(rich_club_coeff_2[,c("x","y")], type="b", log="x", xlim=c(min(rich_club_coeff_2[,"x"]), max(rich_club_coeff_2[,"x"])), ylim=c(0,max(rich_club_coeff_2[,"y"])+0.5), xaxs = "i", yaxs = "i", ylab="Weighted Rich-club Effect", xlab="Prominence (degree greater than)")
  # # lines(rich_club_coeff_2[,"x"], rep(1, nrow(rich_club_coeff_2)))
  # rich_club_results_pre <- data.frame(mean(rich_club_coeff_2$y), sd(rich_club_coeff_2$y), min(rich_club_coeff_2$y) >= 1)
  # rich_club_results[[paste(year_analyzed)]][[paste(i)]] <- rich_club_results_pre
  # }
  
######## PLOTS for each year ###########
  
#   # Core Periphery network
#   # exports per head
#   for (i in 1:11){
#   V(graph_list_cp[[i]])$GDP[is.na(V(graph_list_cp[[i]])$GDP)] <- as.numeric("0") # set gdp na's to 0's for plotting
#   png(filename=paste("output/network_plots/cp_networks_2/cp_network_ex_per_head",year_analyzed,"_c",i,".png", sep=""), width=1500, height=1000)
#   plot(graph_list_cp[[i]],
#        main=paste("EU Core-Periphery Network -", year_analyzed, "- Complexity Level:", i, "- Edges: Exports per Head, Vertex-Size: GDP per Head, Vertex Label: Population", sep = " "),
#        vertex.size=(rescale(V(graph_list_cp[[i]])$GDP, to = c(0, 1)) * 12000000) / V(graph_list_cp[[i]])$population,
#        weights = rescale(E(graph_list_cp[[i]])$Weight, to = c(1, 20)) ,
#        edge.arrow.size=2 ,
#        edge.width=rescale(E(graph_list_cp[[i]])$Weight, to = c(1, 20)) ,
#        layout=layout_as_star(graph_list_cp[[i]], center=V(graph_list_cp[[i]])["Core"]),
#        rescale=T,
#        edge.curved=0.5,
# #      edge.color="#457cb4",
#        vertex.label=paste(V(graph_list_cp[[i]])$name, V(graph_list_cp[[i]])$population, sep="\n"),
#        vertex.label.color="black",
# #       vertex.lebel.font="bold",
#        vertex.label.cex=2,
#        vertex.color="#457cb4",
#        vertex.frame.color="black"
#        )
#   dev.off()
#   }
# 
#   # exports absolute
#   for (i in 1:11){
#   V(graph_list_cp[[i]])$GDP[is.na(V(graph_list_cp[[i]])$GDP)] <- as.numeric("0") # set gdp na's to 0's for plotting
#   png(filename=paste("output/network_plots/cp_networks_2/cp_network_",year_analyzed,"_c",i,".png", sep=""), width=1500, height=1000)
#   print(plot(graph_list_cp[[i]],
#        main=paste("EU Core-Periphery Network -", year_analyzed, "- Complexity Level:", i, "- Edges: Exports, Vertex-Size: GDP per Head, Vertex Label: Population", sep = " "),
#        vertex.size=(rescale(V(graph_list_cp[[i]])$GDP, to = c(0, 1)) * 12000000) / V(graph_list_cp[[i]])$population,
#        edge.arrow.size=2,
#        edge.width=rescale(E(graph_list_cp[[i]])$Weight_abs, to = c(1, 20)),
#        layout=layout_as_star(graph_list_cp[[i]], center=V(graph_list_cp[[i]])["Core"]),
#        rescale=T,
#        edge.curved=0.5,
# #       edge.color="black",
#        vertex.label=paste(V(graph_list_cp[[i]])$name, V(graph_list_cp[[i]])$population, sep="\n"),
#        vertex.label.color="black",
# #       vertex.label.font="bold",
#        vertex.label.cex=2,
#        vertex.color="#457cb4",
#        vertex.frame.color="black"
#        ))
#   dev.off()
#   }

}
###### end of loop over years ##########

#### makje some more correlations between node characteristics -------
years_correl <- as.character(1966:2014)

correls <- rownames_to_column(as.data.frame(measures_local[["1965"]][["complexity_11"]][["strengths"]]))
correls <- cbind(correls, as.data.frame(measures_local[["1965"]][["complexity_11"]][["local.clustering.wei"]]))
for (i in years_correl){
  correls_calc <- rownames_to_column(as.data.frame(measures_local[[i]][["complexity_11"]][["strengths"]]))
  correls_calc <- cbind(correls_calc, as.data.frame(measures_local[[i]][["complexity_11"]][["local.clustering.wei"]]))
  correls <- left_join(correls, correls_calc, by= c("rowname"))
}
years <- c(1965:2014)
namesvector <- as.character()
for (i in years){
  namesvector <- c(namesvector, paste("strength_", i, sep=""), paste(paste("clust_", i, sep="")))
}
colnames(correls) <- c("Country", namesvector)
correls_2 <- correls[ , order(names(correls))]
correls_3_clust <- gather(correls_2[, 1:51], Year, local_clustering, 1:50)
correls_3_stren <- gather(correls_2[, 51:101], Year, strength, 2:51)
correls_3_clust$Year <- gsub("[^0-9\\.]", "", correls_3_clust$Year)
correls_3_stren$Year <- gsub("[^0-9\\.]", "", correls_3_stren$Year)
correls_4 <- left_join(correls_3_stren,correls_3_clust, by=c("Year", "Country"))

closeness_cor <- data.frame(Country = character(), Year = double(), closeness = double())
years <- as.character(years)
for (i in years){
cc <- rownames_to_column(as.data.frame(measures_local[[i]][["complexity_11"]][["closeness_tnet"]]))
cc[, "Year"] <- i 
colnames(cc) <- c("Country", "closeness", "Year")
closeness_cor <- rbind(closeness_cor, cc)
}

correls_5 <- left_join(correls_4, closeness_cor, by=c("Year", "Country"))
correls_5$Year <- as.numeric(correls_5$Year)
correls_6 <- left_join(correls_5, macro_data_1[, c("Year", "id", "GDP_Dollar_curr")], by=c("Year" = "Year", "Country" = "id"))
correls_6 <- left_join(correls_6, macro_data_1[, c("Year", "id", "complexity_rank_HH")], by=c("Year" = "Year", "Country" = "id"))
correls_6 <- left_join(correls_6, macro_data_1[, c("Year", "id", "complexity_harv")], by=c("Year" = "Year", "Country" = "id"))
correls_6 <- left_join(correls_6, macro_data_1[, c("Year", "id", "R_and_D_investment")], by=c("Year" = "Year", "Country" = "id"))
correls_6 <- left_join(correls_6, macro_data_1[, c("Year", "id", "total_debt_percGDP")], by=c("Year" = "Year", "Country" = "id"))
correls_6 <- left_join(correls_6, macro_data_1[, c("Year", "id", "unemp_harmo")], by=c("Year" = "Year", "Country" = "id"))

correls_res <- correls_6 %>% 
  group_by(Year) %>%
  summarize(GDP_closeness=cor(GDP_Dollar_curr, closeness, use = "pairwise.complete.obs", method = c("spearman")))

correls_res <- left_join(correls_res,
                         correls_6 %>% 
                           group_by(Year) %>%
                           summarize(complexity_rank_HH_closeness=cor(complexity_rank_HH, closeness, use = "pairwise.complete.obs", method = c("spearman"))),
                         by=c("Year"))

correls_res <- left_join(correls_res,
                         correls_6 %>% 
                           group_by(Year) %>%
                           summarize(R_and_D_investment_closeness=cor(R_and_D_investment, closeness, use = "pairwise.complete.obs", method = c("spearman"))),
                         by=c("Year"))

correls_res <- left_join(correls_res,
                         correls_6 %>% 
                           group_by(Year) %>%
                           summarize(total_debt_percGDP_closeness=cor(total_debt_percGDP, closeness, use = "pairwise.complete.obs", method = c("spearman"))),
                         by=c("Year"))

correls_res <- left_join(correls_res,
                         correls_6 %>% 
                           group_by(Year) %>%
                           summarize(unemp_harmo_closeness=cor(unemp_harmo, closeness, use = "pairwise.complete.obs", method = c("spearman"))),
                         by=c("Year"))

correls_res <- left_join(correls_res,
                         correls_6 %>% 
                           group_by(Year) %>%
                           summarize(GDP_strength=cor(GDP_Dollar_curr, strength, use = "pairwise.complete.obs", method = c("spearman"))),
                         by=c("Year"))

correls_res <- left_join(correls_res,
                         correls_6 %>% 
                           group_by(Year) %>%
                           summarize(closeness_strength=cor(closeness, strength, use = "pairwise.complete.obs", method = c("spearman"))),
                         by=c("Year"))


node_correls <- correls_res





######### Save results as R objects -----------
objects_to_be_saved <- c("trade_stats", "trade_stats_cp", "characteristics_global", "correlations_global", "network_macro_cor", "measures_local", "characteristics_global_core", "characteristics_global_periphery", "characteristics_global_semip", "characteristics_global_east", "characteristics_global_baltics", "node_correls")
save(list = objects_to_be_saved, file = "output/etn_analysis_results_all_years_2018_06_07.Rdata")
# rich club (needs to be activated in code above)
save(rich_club_results, file = "output/rich_club_results.Rdata")




# Print Clusters to table
Cluster_c <- unique(etn$exporter[grep(Core, etn$exporter)])
Cluster_p <- unique(etn$exporter[grep(Periphery, etn$exporter)])
Cluster_sp <- unique(etn$exporter[grep(Semiperiphery, etn$exporter)])
Cluster_e <- unique(etn$exporter[grep(East, etn$exporter)])
Cluster_b <- unique(etn$exporter[grep(Baltics, etn$exporter)])

Cluster_list <- list(Cluster_c, Cluster_p, Cluster_sp, Cluster_e, Cluster_b)
Cluster_list <- lapply(Cluster_list, countrycode, "id", "name", custom_dict = oec_countrycodes)
Cluster <- cbind.fill(Cluster_list[[1]], Cluster_list[2], Cluster_list[3], Cluster_list[4], Cluster_list[5], fill = NA)
colnames(Cluster) <- c("Core", "Periphery", "Semiperiphery", "East", "Baltics")

print(xtable(Cluster, type = "latex"), file = "output/Clusters.tex", include.rownames=FALSE)





 #### PLOTS #####
#### plot thresholded trade balance stats for dif countries ####
options(scipen=10000) # show full numbers at y axis
div <- 1000000 # millions @ y axis
core2_peri <- c("eugrc", "eudeu", "eufra", "euesp", "euprt", "eusvn", "euhun", "euita", "eusvk")
peri <- c("eugrc", "euesp", "euprt", "eusvn", "euhun", "eusvk")
core_peri <- c("Core", "Periphery", "Periphery_no_esp", "Semiperiphery", "East", "Baltics")

# plot_stats("BAL", core2_peri, "core2") # function arguments: which flows (BAL,EX,IM); which countries; ending of datafile name
# plot_stats("EX", core2_peri, "core2")
# plot_stats("IM", core2_peri, "core2")
# plot_stats("BAL", peri, "peri")
# plot_stats("EX", peri, "peri")
# plot_stats("IM", peri, "peri")
plot_stats("BAL", core_peri, "core_peri")
plot_stats("EX", core_peri, "core_peri")
plot_stats("IM", core_peri, "core_peri")


# eu trade network export to gephi as igraph plotting is limited
etn_plot <- graph_list_etn[[10]]
V(etn_plot)$cluster[grep(Core, V(etn_plot)$name)] <-  1
V(etn_plot)$cluster[grep(Semiperiphery, V(etn_plot)$name)] <-  2
V(etn_plot)$cluster[grep(Periphery, V(etn_plot)$name)] <-  3
V(etn_plot)$cluster[grep(Baltics, V(etn_plot)$name)] <-  4
V(etn_plot)$cluster[grep(East, V(etn_plot)$name)] <-  5
V(etn_plot)$name <- countrycode(V(etn_plot)$name, "id", "name", custom_dict = oec_countrycodes)
E(etn_plot)$width <- range01(E(etn_plot)$weight) * 20
E(etn_plot)$weight <- range01(E(etn_plot)$weight) + 0.00000000000000001



saveAsGEXF(etn_plot, "etn_gephi_1988.gexf")


# cp trade network export to gephi as igraph plotting is limited
for (i in c(1,3,5,7,9,10)){
etn_plot <- graph_list_cp[[i]]
E(etn_plot)$weight <- E(etn_plot)$weight / 1000000
E(etn_plot)$width <- E(etn_plot)$weight
# E(etn_plot)$width <- range01(E(etn_plot)$weight) * 20
# E(etn_plot)$weight <- range01(E(etn_plot)$weight) + 0.00000000000000001

saveAsGEXF_cp(etn_plot, paste("cp_gephi_", i, ".gexf", sep=""))
}

for (i in c(1,3,5,7,9,10)){
pdf(file=paste("output/network_plots/cp_plot_2010_c",i,".pdf", sep=""), width=5, height=5)
plot(graph_list_cp[[i]],
     vertex.size=(rescale(V(graph_list_cp[[i]])$GDP, to = c(0, 1)) * 12000000) / V(graph_list_cp[[i]])$population,
     edge.arrow.size=0.5,
     edge.width=rescale(E(graph_list_cp[[i]])$Weight_abs, to = c(1, 20)) / 2,
     layout=layout_as_star(graph_list_cp[[i]], center=V(graph_list_cp[[i]])["Core"]),
     rescale=T,
     edge.curved=0.5,
     edge.color="darkgrey",
     # vertex.label=paste(V(graph_list_cp[[i]])$name, V(graph_list_cp[[i]])$population, sep="\n"),
     vertex.label.color="black",
      #       vertex.label.font="bold",
     vertex.label.cex=1.5,
     vertex.color="darkgrey",
     vertex.frame.color="black")
dev.off()
}



# #       edge.color="black",
#        vertex.label=paste(V(graph_list_cp[[i]])$name, V(graph_list_cp[[i]])$population, sep="\n"),
#        vertex.label.color="black",
# #       vertex.label.font="bold",
#        vertex.label.cex=2,
#        vertex.color="#457cb4",
#        vertex.frame.color="black"
#        )

# #  create igraph 4 etn w/o prod
# # 4 testing
# etn_no_prod_1 <- etn_no_prod[(importer=="eudeu" | importer=="aschn") & (exporter=="eudeu" | exporter=="aschn"),]
# etn_no_prod_1 <- as.matrix(etn_no_prod_1)
# etn_no_prod_graph <- graph.edgelist(etn_no_prod_1[,1:2], directed = TRUE)
# E(etn_no_prod_graph)$weight=as.numeric(etn_no_prod_1[,3]) #add weights
# etn_no_prod_no_world_graph <- delete_vertices(etn_no_prod_graph, "world") #  remove world
# 
# 
# etn_no_prod <- as.matrix(etn_no_prod)
# etn_no_prod_graph <- graph.edgelist(etn_no_prod[,1:2], directed = TRUE)
# E(etn_no_prod_graph)$weight=as.numeric(etn_no_prod[,3]) #add weights
# etn_no_prod_no_world_graph <- delete_vertices(etn_no_prod_graph, "world") #  remove world
# 
# #  create igraphs 4 thresholded networks
# etn_0_2_matrix <- as.matrix(etn_0_2)
# etn_0_2_graph <- graph.edgelist(etn_0_2_matrix[,1:2])
# E(etn_0_2_graph)$weight = as.numeric(etn_0_2_matrix[,3]) #add weights
# etn_0_2_graph_no_world <- delete_vertices(etn_0_2_graph, "world")
# 
# etn_2_4_matrix <- as.matrix(etn_2_2)
# etn_2_4_graph <- graph.edgelist(etn_2_4_matrix[,1:2])
# E(etn_2_4_graph)$weight = as.numeric(etn_2_4_matrix[,3]) #add weights
# etn_2_4_graph_no_world <- delete_vertices(etn_2_4_graph, "world")
# 
# etn_4_6_matrix <- as.matrix(etn_4_6)
# etn_4_6_graph <- graph.edgelist(etn_4_6_matrix[,1:2])
# E(etn_4_6_graph)$weight = as.numeric(etn_4_6_matrix[,3]) #add weights
# etn_4_6_graph_no_world <- delete_vertices(etn_4_6_graph, "world")
# 
# etn_6_8_matrix <- as.matrix(etn_6_8)
# etn_6_8_graph <- graph.edgelist(etn_6_8_matrix[,1:2])
# E(etn_6_8_graph)$weight = as.numeric(etn_6_8_matrix[,3]) #add weights
# etn_6_8_graph_no_world <- delete_vertices(etn_6_8_graph, "world")
# 
# etn_8_9_matrix <- as.matrix(etn_8_9)
# etn_8_9_graph <- graph.edgelist(etn_8_9_matrix[,1:2], directed = TRUE)
# E(etn_8_9_graph)$weight <- as.numeric(etn_8_9_matrix[,3]) #add weights
# etn_8_9_graph_no_world <- delete_vertices(etn_8_9_graph, "world")
#
# etn_9_1_matrix <- as.matrix(etn_9_1)
# etn_9_1_graph <- graph.edgelist(etn_9_1_matrix[,1:2], directed = TRUE)
# E(etn_9_1_graph)$weight <- as.numeric(etn_9_1_matrix[,3]) #add weights
# etn_9_1_graph_no_world <- delete_vertices(etn_9_1_graph, "world")
# 
# 
# # ego graphs w/o edges between neighbours
ego <- "euesp"
for (i in 1:10)
  {
  ego_level <- as.data.frame(etn_thresholds_list_1[[i]]) # 1 for 0-10%, 2 for 10-20% ...
  etn_ego_edgelist <- ego_level[(ego_level$importer== ego | ego_level$exporter == ego),]
  etn_ego_edgelist["edgecolor"] <- 1
  etn_ego_edgelist$edgecolor[(etn_ego_edgelist$exporter==ego)] <- 2
  etn_ego_matrix <- as.matrix(etn_ego_edgelist)
  etn_ego_graph <- graph.edgelist(etn_ego_matrix[,1:2], directed = TRUE)
  E(etn_ego_graph)$weight <- as.numeric(etn_ego_matrix[,3]) #add weights
  E(etn_ego_graph)$color <- ifelse(etn_ego_edgelist$edgecolor==1, "#FF9933", "#0066CC")
  etn_ego_graph <- delete_vertices(etn_ego_graph, "world")

  # plot for ego
  plot_of <- etn_ego_graph
  png(filename=paste("output/ego_networks/",ego,"_",year_analyzed,"_c",i,".png", sep=""), width=1500, height=1000)
  plot(plot_of, vertex.size=degree(plot_of)*0.7, edge.arrow.size=0.7, edge.width=E(plot_of)$weight*0.00000001, layout=layout_as_star(etn_ego_graph, center = V(etn_ego_graph)[ego]), rescale=T, edge.curved=0.5)
  dev.off()
}

