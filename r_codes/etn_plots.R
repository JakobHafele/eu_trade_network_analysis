# This file imports the R objects created by the etn_analysis_hvd.R file and creates the figures
rm(list=ls())

library(dplyr)
library(countrycode)
library(tidyr)
library(igraph)
library(ggplot2)
library(readr)
library(scales)
library(cowplot)
library(xtable)
library(tibble)
library(data.table)
library(timeSeries)
library(directlabels)

output_path_figures <- "output/" # Specify relative output path for the figures
output_path_tables <- "output/tables/" # Specify relative output path for the  tables

load("output/etn_analysis_results_all_years_2018_06_06.Rdata") # Import the results created by file WTN-analysis.R
load("output/rich_club_results.Rdata")

char_global_w <- characteristics_global  # Global characteristics for the network

years <- c("1965", "1970", "1980", "1990", "2000", "2010", "2014") # Specify the years to be considered
years <- as.character(c(1965:2014))


Core <- "eudeu|eufra|euita|eugbr|euaut|eudnk|euswe|eufin|eunld|eubel|eulux|euirl"
Periphery <- "euprt|euesp|eugrc|eumlt|eucyp"
Semiperiphery <- "euhun|eucze|eusvn|eusvk|eupol"
East <- "eubgr|eurou|euhrv"
Baltics <- "euest|eulva|eultu"

macro_data <- read_csv("data/Macro_data_trade-v26.csv") 
oec_countrycodes <- fread("data/wtn/country_names.tsv")
oec_countrycodes <- as.data.frame(oec_countrycodes)


# pop data for per head calcs -------

pop_data <- macro_data[, c(1,2,99)]

pop_data$Country <- countrycode(pop_data$Country, "name", "id", custom_dict = oec_countrycodes)
pop_data_cp <- pop_data
pop_data_cp$Country [grep(Core, pop_data_cp$Country)] <- "Cores"
pop_data_cp$Country [grep(Periphery, pop_data_cp$Country)] <- "Peripheries"
pop_data_cp$Country [grep(Semiperiphery, pop_data_cp$Country)] <- "Semiperipheries"
pop_data_cp$Country [grep(East, pop_data_cp$Country)] <- "East"
pop_data_cp$Country [grep(Baltics, pop_data_cp$Country)] <- "Baltics"
pop_data_cp$Country [-grep("Cores|Peripheries|Semiperipheries|East|Baltics", pop_data_cp$Country)] <- "erase"
pop_data_cp <- pop_data_cp[!pop_data_cp$Country == "erase",]
pop_data_cp_1 <- pop_data_cp %>% 
  group_by(Country, Year) %>%
  summarize_at(c("population"), sum, na.rm=TRUE) %>%
  ungroup()
  

# weighted cluster stats -----------
char_global_baltics_o <- characteristics_global_baltics[characteristics_global_baltics$complexity_lvl == 11,]
char_global_baltics_o[["group"]] <- "Baltics"

char_global_core_o <- characteristics_global_core[characteristics_global_core$complexity_lvl == 11,]
char_global_core_o[["group"]] <- "Cores"

char_global_periphery_o <- characteristics_global_periphery[characteristics_global_periphery$complexity_lvl == 11,]
char_global_periphery_o[["group"]] <- "Peripheries"

char_global_east_o <- characteristics_global_east[characteristics_global_east$complexity_lvl == 11,]
char_global_east_o[["group"]] <- "East"

char_global_semip_o <- characteristics_global_semip[characteristics_global_semip$complexity_lvl == 11,]
char_global_semip_o[["group"]] <- "Semiperipheries"

char_global_clusters <- rbind(char_global_core_o, char_global_periphery_o, char_global_semip_o, char_global_baltics_o, char_global_east_o)



char_global_clusters$year <- as.numeric(as.character(char_global_clusters$year))
char_global_clusters <- left_join(char_global_clusters, pop_data_cp_1, by = c("group" = "Country", "year" = "Year"))


# check if imports are responsible for east rise (plot below at c/p strength plots)
trade_stats_2 <- trade_stats[, c("Country","EX_Total", "IM_Total", "Year")]
trade_stats_2[,"exratio"] <- trade_stats_2[, 2] / ( trade_stats_2[, 2] + trade_stats_2[, 3] )
trade_stats_2$Year <- as.numeric(as.character(trade_stats_2$Year))
trade_stats_2$Country <- gsub("Core", "Cores", trade_stats_2$Country)
trade_stats_2$Country <- gsub("Periphery", "Peripheries", trade_stats_2$Country)
trade_stats_2$Country <- gsub("Semiperiphery", "Semiperipheries", trade_stats_2$Country)
char_global_clusters <- left_join(char_global_clusters, trade_stats_2[, -c(2,3)], by = c("year" = "Year", "group" = "Country") )



# strength growth (plot below at c/p strength plots)
char_global_clusters[, "stren.growth"] <- with(char_global_clusters, ave(dist.mean, group, 
                                                       FUN=function(x) c(NA, diff(x)/x[-length(x)]) ))
# char_global_clusters$stren.growth[grep("Inf", char_global_clusters$stren.growth)] <- 0
# char_global_clusters[102,15] <- NA
# char_global_clusters[152,15] <- NA

# strength between clusters
trade_core_peris <- trade_stats_cp[trade_stats_cp$importer == "Core" & trade_stats_cp$exporter == "Periphery" | 
                                     trade_stats_cp$importer == "Periphery" & trade_stats_cp$exporter == "Core" ,] %>%
  group_by(year) %>%
  summarize_at(c("export_value_agg"), sum)
trade_core_peris[, "exporter"] <- "Core - Periphery"
trade_core_peris <- rbind(trade_core_peris, trade_stats_cp[
  trade_stats_cp$importer == "Periphery" & trade_stats_cp$exporter == "Periphery",
  c(1,3,4)])

trade_core_sp <- trade_stats_cp[trade_stats_cp$importer == "Core" & trade_stats_cp$exporter == "Semiperiphery" | 
                                  trade_stats_cp$importer == "Semiperiphery" & trade_stats_cp$exporter == "Core" ,] %>%
  group_by(year) %>%
  summarize_at(c("export_value_agg"), sum)
trade_core_sp[, "exporter"] <- "Core - Semiperiphery"
trade_core_sp <- rbind(trade_core_sp, trade_stats_cp[
  trade_stats_cp$importer == "Semiperiphery" & trade_stats_cp$exporter == "Semiperiphery",
  c(1,3,4)])

trade_core_east <- trade_stats_cp[trade_stats_cp$importer == "Core" & trade_stats_cp$exporter == "East" | 
                                  trade_stats_cp$importer == "East" & trade_stats_cp$exporter == "Core" ,] %>%
  group_by(year) %>%
  summarize_at(c("export_value_agg"), sum)
trade_core_east[, "exporter"] <- "Core - East"
trade_core_east <- rbind(trade_core_east, trade_stats_cp[
  trade_stats_cp$importer == "East" & trade_stats_cp$exporter == "East",
  c(1,3,4)])


trade_core_baltics <- trade_stats_cp[trade_stats_cp$importer == "Core" & trade_stats_cp$exporter == "Baltics" | 
                                     trade_stats_cp$importer == "Baltics" & trade_stats_cp$exporter == "Core" ,] %>%
  group_by(year) %>%
  summarize_at(c("export_value_agg"), sum)
trade_core_baltics[, "exporter"] <- "Core - Baltics"
trade_core_baltics <- rbind(trade_core_baltics, trade_stats_cp[
  trade_stats_cp$importer == "Baltics" & trade_stats_cp$exporter == "Baltics",
  c(1,3,4)])

trade_core_allps <- trade_stats_cp[!trade_stats_cp$importer == "Core" & !trade_stats_cp$exporter == "Core" ,] %>%
  group_by(year) %>%
  summarize_at(c("export_value_agg"), sum)
trade_core_allps[, "exporter"] <- "All peripheries"
calc <- trade_stats_cp[!trade_stats_cp$importer == trade_stats_cp$exporter &
                       ( trade_stats_cp$importer == "Core" | trade_stats_cp$exporter == "Core" ),]
calc_2 <- calc %>%
  group_by(exporter,year) %>%
  summarize_at(c("export_value_agg"), sum)

calc_3 <- calc %>%
  group_by(importer,year) %>%
  summarize_at(c("export_value_agg"), sum)
setnames(calc_3, "importer", "exporter")

calc_4 <- bind_rows(calc_2[calc_2$exporter=="Core",], calc_3[calc_3$exporter=="Core",])

calc_5 <- calc_4 %>%
  group_by(year) %>%
  summarize_at(c("export_value_agg"), sum)
calc_5[,"exporter"] <- "Core - all Peripheries"
  
trade_core_allps <- rbind(trade_core_allps, calc_5)






# betweeness ranks -----
betweeness_centrality <- rownames_to_column(as.data.frame(measures_local[["2014"]][["complexity_11"]][["RWBC_wei"]]))
for (y in years){
betweeness_centrality <- left_join(betweeness_centrality, rownames_to_column(as.data.frame(measures_local[[y]][["complexity_11"]][["RWBC_wei"]])), by = c("rowname" = "rowname"))
}
betweeness_centrality_1 <- betweeness_centrality[, -2]
colnames(betweeness_centrality_1) <- c("country", years)

write_csv(betweeness_centrality_1, "output/betweeness_centrality.csv")

# closness ranks
closeness_centrality <- rownames_to_column(as.data.frame(measures_local[["2014"]][["complexity_11"]][["closeness_tnet"]]))
for (y in years){
  closeness_centrality <- left_join(closeness_centrality, rownames_to_column(as.data.frame(measures_local[[y]][["complexity_11"]][["closeness_tnet"]])), by = c("rowname" = "rowname"))
}
closeness_centrality_1 <- closeness_centrality[, -2]
colnames(closeness_centrality_1) <- c("country", years)

# write_csv(closeness_centrality_1, "output/closeness_centrality.csv")
# closeness_centrality_1 <- read_csv("output/closeness_centrality.csv")



# ranked closeness
closeness_latex <- closeness_centrality_1[, c("country", "1970", "1990", "2010")]
ranked <- closeness_latex[order(closeness_latex$`1970`, decreasing=TRUE), c(1,2)]
ranked <- cbind(ranked, closeness_latex[order(closeness_latex$`1990`, decreasing=TRUE), c(1,3)])
ranked <- cbind(ranked, closeness_latex[order(closeness_latex$`2010`, decreasing=TRUE), c(1,4)])
rownames(ranked) <- c(1:nrow(ranked))

ranked_1 <- lapply(ranked[,c(1,3,5)], countrycode, "id", "name", custom_dict = oec_countrycodes) 
ranked_2 <- as.data.frame(do.call(rbind, ranked_1))
ranked_2 <- as.data.frame(t(ranked_2))
colnames(ranked_2) <- c("1970", "1990", "2010")
rownames(ranked_2) <- c(1:nrow(ranked_2))

# latex table
print(xtable(ranked_2[1:15,], type = "latex"), file = "output/closeness_centrality.tex")

# plot closeness density dist
density_closeness_plot_data <- gather(closeness_latex, Year, Closeness, 2:4)
density_closeness_plot <- ggplot(data = density_closeness_plot_data) +
  geom_density(aes(x = Closeness, group= Year, linetype= Year)) +
  scale_linetype_manual(values=c("dotted", "longdash", "solid")) +
  ylab ("Density")

kurtosis_closeness <- colKurtosis(closeness_centrality_1[,2:51], method = "moment")
kurtosis_closeness_plot_data <- data.frame(kurtosis_closeness)
kurtosis_closeness_plot_data[,"Year"] <- as.numeric(rownames(kurtosis_closeness_plot_data))

kurtosis_closeness_plot <- ggplot(data = kurtosis_closeness_plot_data) +
  geom_line(aes(x=Year, y = kurtosis_closeness, group=1)) +
  ylab ("Kurtosis") +
  xlab ("Year")

closness_kurt <- plot_grid(density_closeness_plot, kurtosis_closeness_plot, 
                           align='h', ncol=2)

ggsave(paste(output_path_figures, "density_closeness.pdf", sep=""), plot = closness_kurt, width = 8, height = 4)
 
  

# Degree and strength stats ----
# weighted for thresholded

char_global_w_t <- char_global_w[char_global_w$complexity_lvl %in% c(1:10),]

plot_dist_sd <- qplot(char_global_w_t$year, char_global_w_t$dist.sd,
                      colour = as.numeric(char_global_w_t$complexity_lvl),
                      main = "Sd", ylab = "Sd", xlab = "Year", geom = c("point")) +
                      scale_colour_continuous(name = "PCI", low = "#c0c0c0", high = "black")


plot_dist_kurt <- qplot(char_global_w_t$year, char_global_w_t$dist.kurt,
                        colour = as.numeric(char_global_w_t$complexity_lvl),
                        main = "Kurtosis", ylab = "Kurtosis", xlab = "Year", geom = c("point")) +
                        scale_colour_continuous(name = "PCI", low = "#c0c0c0", high = "black")

plot_dist_skew <- qplot(char_global_w_t$year, char_global_w_t$dist.skew,
                        colour = as.numeric(char_global_w_t$complexity_lvl),
                        main = "Skewness", ylab = "Skewness", xlab = "Year", geom = c("point")) +
                        scale_colour_continuous(name = "PCI", low = "#c0c0c0", high = "black")

plot_density <- qplot(char_global_w_t$year, char_global_w_t$density, 
                      colour = as.numeric(char_global_w_t$complexity_lvl),
                      main = "Density", ylab= "Density", xlab = "Year", geom = c("point")) +
                      scale_colour_continuous(name = "PCI", low = "#c0c0c0", high = "black")

plot_reciprocity <- qplot(char_global_w_t$year, char_global_w_t$reciprocity, 
                          colour = as.numeric(char_global_w_t$complexity_lvl),
                          main = "Reciprocity", ylab = "Reciprocity", xlab = "Year", geom = c("point"))  +
                          scale_colour_continuous(name = "PCI", low = "#c0c0c0", high = "black")

plot_dist_mean <- qplot(char_global_w_t$year, char_global_w_t$dist.mean, 
                        colour = as.numeric(char_global_w_t$complexity_lvl),
                        main = "Mean strength", ylab = "Mean strength", xlab = "Year", geom = c("point"))  +
                        scale_colour_continuous(name = "PCI", low = "#c0c0c0", high = "black")

plot_global_clust <- qplot(char_global_w_t$year, char_global_w_t$global_clust, 
                           colour = as.numeric(char_global_w_t$complexity_lvl),
                           main = "Global Clustering", ylab = "Clustering Coefficient", xlab = "Year", geom = c("point")) +
                           scale_colour_continuous(name = "PCI", low = "#c0c0c0", high = "black")

plot_edge_density <- qplot(char_global_w_t$year, char_global_w_t$edge.density, 
                           colour = as.numeric(char_global_w_t$complexity_lvl),
                           main = "Edge density", ylab = "Edge density", xlab = "Year", geom = c("point")) +
                           scale_colour_continuous(name = "PCI", low = "#c0c0c0", high = "black")

plot_assort_stren <- qplot(char_global_w_t$year, char_global_w_t$assort_streng, 
                           colour = as.numeric(char_global_w_t$complexity_lvl),
                           main = "Assortativity based on strength", ylab = "Assortativity (stren)", xlab = "Year", geom = c("point")) +
                           scale_colour_continuous(name = "PCI", low = "#c0c0c0", high = "black")


overview_plot <- plot_grid(plot_density, plot_reciprocity, 
                           plot_global_clust, plot_dist_mean, 
                           plot_dist_sd, plot_edge_density,
                           plot_assort_stren,
                           align='h', ncol=2)
ggsave(paste(output_path_figures, "stats-overview-thresholded-plot_w.pdf", sep=""), plot = overview_plot, width = 12, height = 12)



distribution_plot <- plot_grid(plot_dist_mean, plot_dist_sd,
                               plot_dist_kurt, plot_dist_skew,
                               align='h')
ggsave(paste(output_path_figures, "dist-overview-thresholded-plot_w.pdf", sep=""), plot = distribution_plot, width = 12, height = 8)

# weighted for overall

char_global_w_o <- char_global_w[char_global_w$complexity_lvl == 11, -10 ]

plot_dist_sd <- qplot(char_global_w_o$year, char_global_w_o$dist.sd, 
                      main = "Sd", ylab = "Sd", xlab = "Year", geom = c("point", "path"))
plot_dist_kurt <- qplot(char_global_w_o$year, char_global_w_o$dist.kurt, 
                        main = "Kurtosis", ylab = "Kurtosis", xlab = "Year", geom = c("point", "path"))
plot_dist_skew <- qplot(char_global_w_o$year, char_global_w_o$dist.skew, 
                        main = "Skewness", ylab = "Skewness", xlab = "Year", geom = c("point", "path"))

plot_density <- qplot(char_global_w_o$year, char_global_w_o$density, 
                      main = "Density", ylab= "Density", xlab = "Year", geom = c("point", "path"))
plot_reciprocity <- qplot(char_global_w_o$year, char_global_w_o$reciprocity, 
                          main = "Reciprocity", ylab = "Reciprocity", xlab = "Year", geom = c("point", "path"))
plot_dist_mean <- qplot(char_global_w_o$year, char_global_w_o$dist.mean, 
                        main = "Mean strength", ylab = "Mean strength", xlab = "Year", geom = c("point", "path"))
plot_global_clust <- qplot(char_global_w_o$year, char_global_w_o$global_clust, 
                           main = "Global Clustering", ylab = "Clustering Coefficient", xlab = "Year", geom = c("point", "path"))
plot_edge_density <- qplot(char_global_w_o$year, char_global_w_o$edge.density, 
                           main = "Edge density", ylab = "Edge density", xlab = "Year", geom = c("point", "path"))
plot_assort_ECI <- qplot(unique(char_global_w_o$year), char_global_w[char_global_w$complexity_lvl==11, "assort_ECI"], 
                         main = "Assortativity (directed + binary) based on Complexity Country Ranks", ylab = "Assortativity (ECI)", xlab = "Year", geom = c("point"))
plot_assort_ECI_stren <- qplot(unique(char_global_w_o$year), char_global_w[char_global_w$complexity_lvl==11, "assort_ECI_streng"], 
                               main = "Assortativity (undirected + strength) based on Complexity Country Ranks", ylab = "Assortativity (ECI)", xlab = "Year", geom = c("point"))
plot_assort_ECI_cutoff <- qplot(unique(char_global_w_o$year), char_global_w[char_global_w$complexity_lvl==11, "assort_ECI_cutoff"], 
                                main = "Assortativity (directed + binary(cutoff at 50% of dist)) based on Complexity Country Ranks", ylab = "Assortativity (ECI)", xlab = "Year", geom = c("point"))


overview_plot <- plot_grid(plot_density, plot_reciprocity, plot_global_clust,
                           plot_dist_mean, plot_dist_sd, plot_edge_density,
                           plot_assort_ECI, plot_assort_ECI_stren, plot_assort_ECI_cutoff,
                           align='h', ncol=2)
ggsave(paste(output_path_figures, "stats-overview-plot_w.pdf", sep=""), plot = overview_plot, width = 12, height = 16)

distribution_plot <- plot_grid(plot_dist_mean, plot_dist_sd,
                               plot_dist_kurt, plot_dist_skew,
                               align='h')
ggsave(paste(output_path_figures, "dist-overview-plot_w.pdf", sep=""), plot = distribution_plot, width = 12, height = 8)

# assort plot

plot_assort_ECI <- qplot(unique(char_global_w_o$year), char_global_w_o$assort_ECI, 
                         main = "Assortativity (directed + binary) based on ECI", ylab = "Assortativity", xlab = "Year", geom = c("point", "path"))
plot_assort_ECI_stren <- qplot(unique(char_global_w_o$year), char_global_w_o$assort_ECI_streng, 
                               main = "based on ECI (undirected + strength)", ylab = "Assortativity", xlab = "Year", geom = c("point", "path"))
plot_assort_ECI_cutoff <- qplot(unique(char_global_w_o$year), char_global_w_o$assort_ECI_cutoff, 
                                main = "based on ECI (directed + binary(cutoff at 50% of dist))", ylab = "Assortativity", xlab = "Year", geom = c("point", "path"))
plot_assort_GDP_cur <- qplot(unique(char_global_w_o$year), char_global_w_o$assort_GDP_cur, 
                                main = "based on GDP in current $", ylab = "Assortativity", xlab = "Year", geom = c("point", "path"))
plot_assort_wage_share <- qplot(unique(char_global_w_o$year), char_global_w_o$assort_Wage_share, 
                             main = "based on wage share", ylab = "Assortativity", xlab = "Year", geom = c("point", "path"))
plot_assort_streng <- qplot(unique(char_global_w_o$year), char_global_w_o$assort_streng, 
                                main = "based on strength", ylab = "Assortativity", xlab = "Year", geom = c("point", "path"))
plot_assort_closeness <- qplot(unique(char_global_w_o$year), char_global_w_o$assort_closeness, 
                               main = "based on closeness", ylab = "Assortativity", xlab = "Year", geom = c("point", "path"))
assort_plot <- plot_grid(plot_assort_ECI, plot_assort_ECI_stren,
                         plot_assort_ECI_cutoff, plot_assort_GDP_cur,
                         plot_assort_streng, plot_assort_closeness,
                         ncol=2, align='h')
ggsave(paste(output_path_figures, "assort-overview-plot_w.pdf", sep=""), plot = assort_plot, width = 10, height = 12)

cor.test(char_global_w_o$assort_closeness, char_global_w_o$assort_ECI_streng)

##############################################


# overall, allyears -----------
# strength overview plot 
plot_dist_mean_all <- ggplot(data = char_global_w_o) +
  geom_point(aes(year, dist.mean / 1000000000, group=1)) +
  geom_line(aes(year, dist.mean / 1000000000, group=1)) +
  ylab("Mean strength (billion $)") +
  xlab("Year") +
  ggtitle("Mean strength")


plot_dist_sd_all <- ggplot(data = char_global_w_o) +
  geom_point(aes(year, dist.sd / dist.mean, group=1)) +
  geom_line(aes(year, dist.sd / dist.mean, group=1)) +
  ylab("Coefficient of variation") +
  xlab("Year") +
  ggtitle("Coefficient of variation")  
 
plot_dist_kurt_all <- ggplot(data = char_global_w_o) +
  geom_point(aes(year, dist.kurt, group=1)) +
  geom_line(aes(year, dist.kurt, group=1)) +
  ylab("Kurtosis") +
  xlab("Year") +
  ggtitle("Kutosis") 

plot_dist_skew_all <- ggplot(data = char_global_w_o) +
  geom_point(aes(year, dist.skew, group=1)) +
  geom_line(aes(year, dist.skew, group=1)) +
  ylab("Skewness") +
  xlab("Year") +
  ggtitle("Skewness")

distribution_plot_all <- plot_grid(plot_dist_mean_all, plot_dist_sd_all,
                               plot_dist_kurt_all, plot_dist_skew_all,
                               align='h')
ggsave(paste(output_path_figures, "dist-overview-plot_w_all.pdf", sep=""), plot = distribution_plot_all, width = 12, height = 8)




# density and global clust plot all years
plot_density <- ggplot(data = char_global_w_o) +
  geom_point(aes(year, density, group=1)) +
  geom_line(aes(year, density, group=1)) +
  ylab("Density") +
  xlab("Year") +
  ggtitle("Density")

plot_global_clust <- ggplot(data = char_global_w_o) +
  geom_point(aes(year, global_clust, group=1)) +
  geom_line(aes(year, global_clust, group=1)) +
  ylab("Global Clustering") +
  xlab("Year") +
  ggtitle("Global Clustering Coefficient")

densclust_plot_all <- plot_grid(plot_density, plot_global_clust,
                                   align='h')

ggsave(paste(output_path_figures, "densclust_allyears.pdf", sep=""), plot = densclust_plot_all, width = 8, height = 4)




# weightes cluster strtenght plots
 
options(scipen=10000)

plot_dist_mean_cluster <- ggplot(data = char_global_clusters) +
  geom_line(aes(year, dist.mean / 1000000000, group = group)) +
  # scale_linetype_manual(values=c("solid",  "twodash", "longdash", "dotted", "dashed")) +
  geom_point(aes(year, dist.mean / 1000000000, group = group, shape = group)) +
  ylab("Mean strength (billion $)") +
  xlab("Year") +
  labs(shape="Cluster") +
  ggtitle("Mean strength")

plot_dist_mean_cluster_no_core <- ggplot(data = char_global_clusters[!char_global_clusters$group == "Cores",]) +
  geom_line(aes(year, dist.mean / 1000000000, group = group)) +
  # scale_linetype_manual(values=c("solid",  "twodash", "longdash", "dotted", "dashed")) +
  geom_point(aes(year, dist.mean / 1000000000, group = group, shape = group)) +
  ylab("Mean strength (billion $)") +
  xlab("Year") +
  labs(shape="Cluster") +
  theme(legend.position = "none") +
  ggtitle("Mean strength (without cores)")


plot_dist_mean_cluster_per_head <- ggplot(data = char_global_clusters) +
  geom_line(aes(year, (dist.mean / 1000000) / population, group = group)) +
  # scale_linetype_manual(values=c("solid",  "twodash", "longdash", "dotted", "dashed")) +
  geom_point(aes(year, (dist.mean / 1000000) / population, group = group, shape = group)) +
  ylab("Mean strength per head (million $)") +
  xlab("Year") +
  labs(shape="Cluster") +
  theme(legend.position = "none") +
  ggtitle("Mean strength per head")


plot_dist_mean_cluster_per_head_out <- ggplot(data = char_global_clusters) +
  geom_line(aes(year, ((dist.mean / 1000000) / population) * exratio, group = group)) +
  # scale_linetype_manual(values=c("solid",  "twodash", "longdash", "dotted", "dashed")) +
  geom_point(aes(year, ((dist.mean / 1000000) / population) * exratio, group = group, shape = group)) +
  ylab("Mean strength per head (million $)") +
  xlab("Year") +
  labs(shape="Cluster") +
  theme(legend.position = "none") +
  ggtitle("Mean out strength per head")

plot_dist_mean_cluster_growth <- ggplot(data = char_global_clusters) +
  geom_line(aes(year, stren.growth, group = group)) +
  # scale_linetype_manual(values=c("solid",  "twodash", "longdash", "dotted", "dashed")) +
  geom_point(aes(year, stren.growth, group = group, shape = group)) +
  ylab("Growth in %") +
  xlab("Year") +
  labs(shape="Cluster") +
  theme(legend.position = "none") +
  ggtitle("Growth of strength")

plot_dist_mean_cluster_growth_sp <- ggplot(data = char_global_clusters) +
  geom_line(aes(year, stren.growth, group = group)) +
  # scale_linetype_manual(values=c("solid",  "twodash", "longdash", "dotted", "dashed")) +
  geom_point(aes(year, stren.growth, group = group, shape = group)) +
  ylab("Growth in %") +
  xlab("Year") +
  coord_cartesian(ylim = c(-1, 2))  +
  labs(shape="Cluster") +
  theme(legend.position = "none") +
  ggtitle("Growth of strength")
  

cp_strength_plot <- plot_grid(plot_dist_mean_cluster, plot_dist_mean_cluster_no_core,
                              plot_dist_mean_cluster_per_head, plot_dist_mean_cluster_per_head_out,
                              plot_dist_mean_cluster_growth, plot_dist_mean_cluster_growth_sp,
                               align='h', ncol = 2)
ggsave(paste(output_path_figures, "cp-strength-plot_w_allyears.pdf", sep=""), plot = cp_strength_plot, width = 11, height = 12)


# strength between clusters
trade_cp_plot <- ggplot(data = trade_core_peris) +
  geom_line(aes(x=year, y=export_value_agg / 1000000000, group=exporter, linetype= exporter)) +
  ylab("Trade value in billion $") +
  xlab("Year") +
  labs(linetype="Trading partners")

trade_csp_plot <- ggplot(data = trade_core_sp) +
  geom_line(aes(x=year, y=export_value_agg / 1000000000, group=exporter, linetype= exporter)) +
  ylab("Trade value in billion $") +
  xlab("Year") +
  labs(linetype="Trading partners")

trade_ce_plot <- ggplot(data = trade_core_east) +
  geom_line(aes(x=year, y=export_value_agg / 1000000000, group=exporter, linetype= exporter)) +
  ylab("Trade value in billion $") +
  xlab("Year") +
  labs(linetype="Trading partners")

trade_cb_plot <- ggplot(data = trade_core_baltics) +
  geom_line(aes(x=year, y=export_value_agg / 1000000000, group=exporter, linetype= exporter)) +
  scale_linetype_manual(values=c("dashed", "solid")) +
  ylab("Trade value in billion $") +
  xlab("Year") +
  labs(linetype="Trading partners")

trade_caps_plot <- ggplot(data =   trade_core_allps) +
  geom_line(aes(x=year, y=export_value_agg / 1000000000, group=exporter, linetype= exporter)) +
  scale_linetype_manual(values=c("dashed", "solid")) +
  ylab("Trade value in billion $") +
  xlab("Year") +
  labs(linetype="Trading partners")

cp_between_strength_plot <- plot_grid(trade_cp_plot, trade_csp_plot,
                                      trade_ce_plot, trade_cb_plot,
                                      trade_caps_plot, 
                                      align='h', ncol = 2)

ggsave(paste(output_path_figures, "cp-between-strength-plot_w_allyears.pdf", sep=""), plot = cp_between_strength_plot, width = 11, height = 12)



 # Network stats correlations ----
#  thresholded
correlations_global_t <- correlations_global[correlations_global$complexity_lvl %in% c(1:10),]

plot_clustering_strength <- qplot(correlations_global_t$year, correlations_global_t$cor.clust_stren,
                                  colour = as.numeric(correlations_global_t$complexity_lvl),
                                  main = "Strength vs. clustering", ylab= "Correlation", xlab = "Year", geom = c("point"),
                                  ylim = c(-1,1)) +
                                  scale_colour_continuous(name = "PCI", low = "#c0c0c0", high = "black")

ggsave(paste(output_path_figures, "network-correl-thresholded-overview.pdf", sep=""), plot = plot_clustering_strength, width = 5, height = 4)

#  overall
correlations_global_o <- correlations_global[correlations_global$complexity_lvl == 11,]
plot_clustering_strength <- qplot(correlations_global_o$year, correlations_global_o$cor.clust_stren,
                                  main = "Strength vs. clustering", ylab= "Correlation", xlab = "Year", geom = c("point", "line"),
                                  ylim = c(-1,1))

ggsave(paste(output_path_figures, "network-correl-overview.pdf", sep=""), plot = plot_clustering_strength, width = 5, height = 4)



# Assortativity based on knn-----
# for overall network

measures_local_o <- list()
for (i in years){
measures_local_o[[paste(i)]] <- measures_local[[paste(i)]][["complexity_11"]]
} # subset results for complexity lvl 11 (all products summed up) for each year
  

# annd <- rep(NA, length(years))
# annd_95 <- rep(NA, length(years))
# annd_05 <- rep(NA, length(years))
anns <- rep(NA, length(years))
anns_95 <- rep(NA, length(years))
anns_05 <- rep(NA, length(years))

# annd_degree <- rep(NA, length(years))
# annd_degree_up <- rep(NA, length(years))
# annd_degree_low <- rep(NA, length(years))

anns_stren <- rep(NA, length(years))
anns_stren_up <- rep(NA, length(years))
anns_stren_low <- rep(NA, length(years))

for (y in 1:length(years)) {
  anns[y] <- mean(measures_local_o[[as.character(years[y])]]$wei_knn)
  anns_95[y] <- quantile(measures_local_o[[as.character(years[y])]]$wei_knn, 0.95)
  anns_05[y] <- quantile(measures_local_o[[as.character(years[y])]]$wei_knn, 0.05)
  anns_stren[y] <- cor.test(measures_local_o[[as.character(years[y])]]$wei_knn, 
                            measures_local_o[[as.character(years[y])]]$strengths)$estimate
  anns_stren_up[y] <- cor.test(measures_local_o[[as.character(years[y])]]$wei_knn, 
                               measures_local_o[[as.character(years[y])]]$strengths)$conf.int[1]
  anns_stren_low[y] <- cor.test(measures_local_o[[as.character(years[y])]]$wei_knn, 
                                measures_local_o[[as.character(years[y])]]$strengths)$conf.int[2]
}
assort_measures <- data.frame(Year=years,
                              ANNS=anns,
                              AS95=anns_95,
                              AS05=anns_05,
                              ANNS_STR=anns_stren,
                              ANNS_STR_up=anns_stren_up,
                              ANNS_STR_low=anns_stren_low)


anns_plot <- ggplot(data = assort_measures) + geom_line(aes(Year, ANNS), group=1) + geom_point(aes(Year, ANNS)) + 
  geom_line(aes(Year, AS95), linetype = "dashed", group=1) + 
  geom_line(aes(Year, AS05), linetype = "dashed", group=1) +
  ylab("Mean ANNS") +
  # ylim(c(0, 400)) +
  ggtitle("Av. neightest neighbour strength")
anns_plot

anns_strength_plot <- ggplot(data = assort_measures) + geom_line(aes(Year, ANNS_STR), group=1) + geom_point(aes(Year, ANNS_STR)) + 
  geom_line(aes(Year, ANNS_STR_up), linetype = "dashed", group=1) + 
  geom_line(aes(Year, ANNS_STR_low), linetype = "dashed", group=1) +
  ylim(c(-1, 1)) +
  ylab("Mean ANNS") +
  ggtitle("ANNS vs strength")
anns_strength_plot

assortativity_wei_plot <- plot_grid(anns_plot, anns_strength_plot,
                                    plot_assort_ECI, plot_assort_stren,
                                    align='h', ncol = 2)
ggsave(paste(output_path_figures, "assortativity_wei.pdf", sep=""), plot = assortativity_wei_plot, width = 12, height = 9)


# 2do: assort for thresholded networks ----

# anns <- rep(NA, length(years))
# anns_95 <- rep(NA, length(years))
# anns_05 <- rep(NA, length(years))
# 
# anns_stren <- rep(NA, length(years))
# anns_stren_up <- rep(NA, length(years))
# anns_stren_low <- rep(NA, length(years))
# 
# 
# for (y in 1:length(years)) {
#   anns[y] <- mean(measures_local[[as.character(years[y])]][[]]$wei_knn)
#   anns_95[y] <- quantile(measures_local_o[[as.character(years[y])]]$wei_knn, 0.95)
#   anns_05[y] <- quantile(measures_local_o[[as.character(years[y])]]$wei_knn, 0.05)
#   anns_stren[y] <- cor.test(measures_local_o[[as.character(years[y])]]$wei_knn, 
#                             measures_local_o[[as.character(years[y])]]$strengths)$estimate
#   anns_stren_up[y] <- cor.test(measures_local_o[[as.character(years[y])]]$wei_knn, 
#                                measures_local_o[[as.character(years[y])]]$strengths)$conf.int[1]
#   anns_stren_low[y] <- cor.test(measures_local_o[[as.character(years[y])]]$wei_knn, 
#                                 measures_local_o[[as.character(years[y])]]$strengths)$conf.int[2]
# }
# assort_measures <- data.frame(Year=years,
#                               ANNS=anns,
#                               AS95=anns_95,
#                               AS05=anns_05,
#                               ANNS_STR=anns_stren,
#                               ANNS_STR_up=anns_stren_up,
#                               ANNS_STR_low=anns_stren_low)
# 
# 
# anns_plot <- ggplot(data = assort_measures) + geom_line(aes(Year, ANNS), group=1) + geom_point(aes(Year, ANNS)) + 
#   geom_line(aes(Year, AS95), linetype = "dashed", group=1) + 
#   geom_line(aes(Year, AS05), linetype = "dashed", group=1) +
#   ylab("Mean ANNS") +
#   # ylim(c(0, 400)) +
#   ggtitle("Av. neightest neighbour strength")
# anns_plot
# 
# anns_strength_plot <- ggplot(data = assort_measures) + geom_line(aes(Year, ANNS_STR), group=1) + geom_point(aes(Year, ANNS_STR)) + 
#   geom_line(aes(Year, ANNS_STR_up), linetype = "dashed", group=1) + 
#   geom_line(aes(Year, ANNS_STR_low), linetype = "dashed", group=1) +
#   ylim(c(-1, 1)) +
#   ylab("Mean ANNS") +
#   ggtitle("ANNS vs strength")
# anns_strength_plot
# 
# assortativity_wei_plot <- plot_grid(anns_plot, anns_strength_plot,
#                                     align='h', ncol = 2)
# ggsave(paste(output_path_figures, "assortativity_wei.pdf", sep=""), plot = assortativity_wei_plot, width = 12, height = 4)




# Clustering ----


# average CC in wei over time
# expected CC in wei and deviation from observation# das ist density
avCC_wei <- rep(NA, length(years))
avCC_wei_up <- rep(NA, length(years))
avCC_wei_low <- rep(NA, length(years))
clust_strength <- rep(NA, length(years))
clust_strength_low <- rep(NA, length(years))
clust_strength_up <- rep(NA, length(years))

clustering_measures <- data.frame(Year=double(),
                                  av_clust_wei=double(),
                                  av_clust_wei_up=double(),
                                  av_clust_wei_low=double(),
                                  expct_clust_wei=double(),
                                  clust_strength=double(),
                                  clust_strength_low=double(),
                                  clust_strength_up=double(),
                                  complexity_lvl=double()) # setup empty df for results

for (i in c("1", "10", "11")){ # set which complexity lvls to be included
for (y in 1:length(years)) {
  # local.clustering.wei
  avCC_wei[y] <- t.test(measures_local[[as.character(years[y])]][[as.character(paste("complexity_",i,sep=""))]]$local.clustering.wei)$estimate
  
  avCC_wei_low[y] <- t.test(measures_local[[as.character(years[y])]][[as.character(paste("complexity_",i,sep=""))]]$local.clustering.wei)$conf.int[1]
  avCC_wei_up[y] <- t.test(measures_local[[as.character(years[y])]][[as.character(paste("complexity_",i,sep=""))]]$local.clustering.wei)$conf.int[2]
  # clustering vs strength
  clust_strength[y] <- cor.test(measures_local[[as.character(years[y])]][[as.character(paste("complexity_",i,sep=""))]]$local.clustering.wei,
                                measures_local[[as.character(years[y])]][[as.character(paste("complexity_",i,sep=""))]]$strengths)$estimate
  clust_strength_low[y] <- cor.test(measures_local[[as.character(years[y])]][[as.character(paste("complexity_",i,sep=""))]]$local.clustering.wei,
                                    measures_local[[as.character(years[y])]][[as.character(paste("complexity_",i,sep=""))]]$strengths)$conf.int[1]
  clust_strength_up[y] <- cor.test(measures_local[[as.character(years[y])]][[as.character(paste("complexity_",i,sep=""))]]$local.clustering.wei,
                                   measures_local[[as.character(years[y])]][[as.character(paste("complexity_",i,sep=""))]]$strengths)$conf.int[2]
}
clustering_measures <- rbind(clustering_measures,
                             data.frame(Year=years,
                                        av_clust_wei=avCC_wei,
                                        av_clust_wei_up=avCC_wei_up,
                                        av_clust_wei_low=avCC_wei_low,
                                        expct_clust_wei=char_global_w_o$density,
                                        clust_strength,
                                        clust_strength_low,
                                        clust_strength_up,
                                        complexity_lvl=paste(i)))
}

# jeweils clustering, up, low, und expected fuer bin und wei
# dann share of countries with excess clustering for bin und wei
clustering_plot_wei_exp <- ggplot(data = clustering_measures) + 
  geom_line(aes(Year, av_clust_wei, group=clustering_measures$complexity_lvl, colour = complexity_lvl)) +
  labs(colour = "Complexity lvl") +
  geom_line(aes(Year, expct_clust_wei, group=1), linetype = "dotted") +
  geom_point(aes(Year, expct_clust_wei, group=1), shape=1) +
  ylim(c(0, 1)) +
  ylab("Clustering Coefficient") +
  ggtitle("Clustering in the weighted ETN w/ expected clustering")
clustering_plot_wei_exp

clustering_plot_wei <- ggplot(data = clustering_measures) + 
  geom_line(aes(Year, av_clust_wei, group=clustering_measures$complexity_lvl, colour = complexity_lvl)) +
  labs(colour = "Complexity lvl") +
  geom_point(aes(Year, av_clust_wei, colour = complexity_lvl, group=clustering_measures$complexity_lvl)) + 
  geom_line(aes(Year, av_clust_wei_up, group=clustering_measures$complexity_lvl, colour = clustering_measures$complexity_lvl), linetype = "dashed") + 
  geom_line(aes(Year, av_clust_wei_low, group=clustering_measures$complexity_lvl, colour = clustering_measures$complexity_lvl), linetype = "dashed") +
  ylim(c(0.175, 0.25)) +
  ylab("Clustering Coefficient") +
  ggtitle("Clustering in the weighted ETN w/ expected clustering")
clustering_plot_wei



# correlation clustering coefficient vs. strength
clustering_strength <- ggplot(data = clustering_measures[clustering_measures$complexity_lvl==11,]) + 
  geom_line(aes(Year, clust_strength), group= 1) + geom_point(aes(Year, clust_strength)) + 
  geom_line(aes(Year, clust_strength_up, group= 1), linetype = "dashed") + 
  geom_line(aes(Year, clust_strength_low,group= 1), linetype = "dashed") +
  ylim(c(-1, 1)) +
  # ylab("Pearson coefficient") +
  ggtitle("Clustering vs strength")
clustering_strength

# er hatte noch scatter plot dabei
clustering_plot <- plot_grid(clustering_plot_wei, clustering_plot_wei_exp,
                             align='v', ncol=1)
ggsave(paste(output_path_figures, "clustering_abs.pdf", sep=""), plot = clustering_plot, width = 9, height = 12)

clustering_cors_plot <- plot_grid(clustering_strength,
                                  align='h', ncol = 1)
ggsave(paste(output_path_figures, "clustering_cor.pdf", sep=""), plot = clustering_cors_plot, width = 5, height = 4)




# Country stats correlations ----

gdp_closeness_plot <- ggplot(node_correls) +
  geom_line(aes(x=Year,y=GDP_closeness)) +
  ggtitle("GDP and closeness") +
  ylab("Correlation")

ECI_closeness_plot <- ggplot(node_correls) +
  geom_line(aes(x=Year,y=complexity_rank_HH_closeness)) +
  ggtitle("ECI and closeness") +
  ylab("Correlation")

RnD_closeness_plot <- ggplot(node_correls[node_correls$Year %in% c(1995:2014),]) +
  geom_line(aes(x=Year,y=R_and_D_investment_closeness)) +
  ggtitle("R&D and closeness") +
  ylab("Correlation")

GDP_strength_plot <- ggplot(node_correls) +
  geom_line(aes(x=Year,y=GDP_strength)) +
  ggtitle("GDP and strength") +
  ylab("Correlation")

closeness_stren_plot <- ggplot(node_correls) +
  geom_line(aes(x=Year,y=closeness_strength)) +
  ggtitle("Closeness and strength") +
  ylab("Correlation")

node_cors_plot <- plot_grid(gdp_closeness_plot, ECI_closeness_plot,
                            RnD_closeness_plot, GDP_strength_plot,
                            closeness_stren_plot,
                                  align='h', ncol = 2)
ggsave(paste(output_path_figures, "node_cor.pdf", sep=""), plot = node_cors_plot, width = 10, height = 12)



# Centrality ----
scientific_10 <- function(x) {
  parse(text=gsub("e", "%*%10^", scientific_format()(x)))
}


centra_1965_wei <- data.frame(rwbc=measures_local_o$`1965`$RWBC_wei, Year=rep(1965, length(measures_local_o$`1965`$RWBC_wei)))
centra_1980_wei <- data.frame(rwbc=measures_local_o$`1980`$RWBC_wei, Year=rep(1980, length(measures_local_o$`1980`$RWBC_wei)))
centra_1990_wei <- data.frame(rwbc=measures_local_o$`1990`$RWBC_wei, Year=rep(1990, length(measures_local_o$`1990`$RWBC_wei)))
centra_2000_wei <- data.frame(rwbc=measures_local_o$`2000`$RWBC_wei, Year=rep(2000, length(measures_local_o$`2000`$RWBC_wei)))
centra_2014_wei <- data.frame(rwbc=measures_local_o$`2014`$RWBC_wei, Year=rep(2014, length(measures_local_o$`2014`$RWBC_wei)))
centra_wei <- rbind(centra_1965_wei, centra_1980_wei, centra_1990_wei, centra_2000_wei, centra_2014_wei)
centra_wei$Year <- as.factor(centra_wei$Year)
betweeness_plot_wei <- ggplot(data=centra_wei, aes(x=rwbc, group=Year, colour=centra_wei$Year)) + 
  geom_density() +
  xlab("Betweeness cetrality") + ylab("Density") + 
  ggtitle("Betweeness centrality (weighted)") +
#  coord_cartesian(xlim = c(0, 4750), expand = FALSE) + 
  scale_y_continuous(label=scientific_10) +
  theme(legend.position = c(0.85, 0.85),
        axis.text.x = element_text(angle = 0, hjust = 1))
betweeness_plot_wei

eigen_1965 <- data.frame(evc=measures_local_o$`1965`$eigen.wei, Year=rep(1965, length(measures_local_o$`1965`$eigen.wei)))
eigen_1980 <- data.frame(evc=measures_local_o$`1980`$eigen.wei, Year=rep(1980, length(measures_local_o$`1980`$eigen.wei)))
eigen_1990 <- data.frame(evc=measures_local_o$`1990`$eigen.wei, Year=rep(1990, length(measures_local_o$`1990`$eigen.wei)))
eigen_2000 <- data.frame(evc=measures_local_o$`2000`$eigen.wei, Year=rep(2000, length(measures_local_o$`2000`$eigen.wei)))
eigen_2010 <- data.frame(evc=measures_local_o$`2010`$eigen.wei, Year=rep(2010, length(measures_local_o$`2010`$eigen.wei)))
eigen_2014 <- data.frame(evc=measures_local_o$`2014`$eigen.wei, Year=rep(2014, length(measures_local_o$`2014`$eigen.wei)))
eigen_frame <- rbind(eigen_1965, eigen_1980, eigen_1990, eigen_2000, eigen_2010, eigen_2014)
eigen_frame$Year <- as.factor(eigen_frame$Year)
eigenval_plot <- ggplot(data=eigen_frame, aes(x=evc, group=Year, colour=Year)) + 
  geom_density() +
  xlab("Eigenvector centrality") + ylab("Density") + 
  ggtitle("Eigenvector centrality (weighted)") +
  coord_cartesian(xlim = c(0, 1), expand = FALSE) + 
  theme(legend.position = c(0.85, 0.85),
        axis.text.x = element_text(angle = 0, hjust = 1))
eigenval_plot


centrality_plot <- plot_grid(betweeness_plot_wei,
                             eigenval_plot, 
                             align='h', ncol = 1)
ggsave(paste(output_path_figures, "centrality.pdf", sep=""), plot = centrality_plot, width = 8, height = 12)


# trade stats ---------
trade_clusters <- c("Core", "Periphery", "Semiperiphery", "East", "Baltics")
trade_stats_thresh_cp <- trade_stats[trade_stats$Country %in% trade_clusters,]
trade_stats_bal_cp <- trade_stats_thresh_cp[,c(1, 24:34)]
colnames(trade_stats_bal_cp) <- (c("Country", c(1:10), "Year"))
trade_stats_bal_cp_l <- gather(trade_stats_bal_cp, complexity_lvl, trade_bal,  2:11)
trade_stats_bal_cp_l$complexity_lvl <- as.numeric(trade_stats_bal_cp_l$complexity_lvl)

years_trade_plot <- c("1965", "1970", "1980", "1990", "2000", "2010", "2014")
for (i in years_trade_plot){
assign(paste("trade_stats_bal_",i,sep=""), ggplot(trade_stats_bal_cp_l[trade_stats_bal_cp_l$Year==i,]) +
  geom_line(aes(x=complexity_lvl, y=trade_bal / 1000000000, group=Country, linetype=Country)) +
  scale_x_continuous(expand = c(0.3, 0)) +
  geom_dl(aes(x=complexity_lvl, y=trade_bal / 1000000000, label = Country), method = list(dl.combine("first.bumpup", "last.bumpup"), cex = 0.8)) +
  scale_linetype_manual(values=c("solid", "twodash", "longdash", "dotted", "dashed")) +
  theme(legend.position = "none") +
  xlab("Complexity level") +
  ylab("Trade balance in billion $") +
  ggtitle(paste(i))
)
}

trade_stats_bal_plot_grid <- plot_grid(trade_stats_bal_1965, trade_stats_bal_1970,
                                       trade_stats_bal_1980, trade_stats_bal_1990,
                                       trade_stats_bal_2000,
                                       trade_stats_bal_2014,
                                       align='h', ncol = 2)
ggsave(paste(output_path_figures, "trade_stats/trade_stats_bal_plot_grid.pdf", sep=""), plot = trade_stats_bal_plot_grid, width = 11, height = 12)


# same same but different (no cores)

for (i in years_trade_plot){
  assign(paste("trade_stats_bal_nc_",i,sep=""), ggplot(trade_stats_bal_cp_l[trade_stats_bal_cp_l$Year==i & ! trade_stats_bal_cp_l$Country == "Core",]) +
           geom_line(aes(x=complexity_lvl, y=trade_bal / 1000000000, group=Country, linetype=Country)) +
           scale_x_continuous(expand = c(0.3, 0)) +
           geom_dl(aes(x=complexity_lvl, y=trade_bal / 1000000000, label = Country), method = list(dl.combine("first.bumpup", "last.bumpup"), cex = 0.8)) +
           scale_linetype_manual(values=c("twodash", "longdash", "solid", "dotted", "dashed")) +
           theme(legend.position = "none") +
           xlab("Complexity level") +
           ylab("Trade balance in billion $") +
           ggtitle(paste(i))
  )
}

trade_stats_bal_plot_grid_nc <- plot_grid(trade_stats_bal_nc_1965, trade_stats_bal_nc_1970,
                                       trade_stats_bal_nc_1980, trade_stats_bal_nc_1990,
                                       trade_stats_bal_nc_2000,
                                       trade_stats_bal_nc_2014,
                                       align='h', ncol = 2)
ggsave(paste(output_path_figures, "trade_stats/trade_stats_bal_plot_grid_nc.pdf", sep=""), plot = trade_stats_bal_plot_grid_nc, width = 11, height = 12)




# rich club ---------
rich_club_res_11 <- rich_club_results[["1970"]][["11"]]
rich_club_res_11 <- rbind(rich_club_res_11, rich_club_results[["1980"]][["11"]], rich_club_results[["1990"]][["11"]],
                          rich_club_results[["2000"]][["11"]], rich_club_results[["2010"]][["11"]])
rich_club_res_11[, "year"] <- c("1970", "1980", "1990", "2000", "2010")
colnames(rich_club_res_11) <- c("mean", "sd", "richclub", "year")

rich_club_plot_data <- gather(rich_club_res_11, stat, value,  1:2)

rich_club_plot <- ggplot(rich_club_plot_data) +
  geom_line(aes(x=year, y=value, group=stat, linetype=stat)) +
  ggtitle("Distribution of rich-club coefficients")

ggsave(paste(output_path_figures, "rich_club_c11.pdf", sep=""), plot = rich_club_plot, width = 5, height = 4)




# Table of core ----
# var_name <- c("RWBC.core.countries.bin", "RWBC.core.countries.wei",
#               "eigen.bin", "eigen.wei")
# output_names <- c("core-bin-betw", "core-wei-betw",
#                   "core-bin-eigen", "core-wei-eigen")
# 
# core_bin_table_between <- data.frame(Year=double(), 
#                                      Country=character(), 
#                                      Value=double(), 
#                                      Rank=double())
# 
# for (i in 1:length(var_name)){
#   tab <- data.frame(Year=double(), 
#                     Country=character(), 
#                     Value=double(), 
#                     Rank=double())
#   for (y in years){
#     if (var_name[i] %in% c("RWBC.core.countries.bin", "RWBC.core.countries.wei")){
#       vec_of_interest <- measures_local_o[[as.character(y)]][[var_name[i]]][[as.character(y)]]
#     } else{
#       vec_of_interest <- measures_local_o[[as.character(y)]][[var_name[i]]]
#     }
#     year_ind <- rep(y, length(vec_of_interest))
#     count_ind <- names(vec_of_interest)
#     vect <- unname(vec_of_interest)
#     rank_vec <- rank(-vect, ties.method = "min")
#     y_bin <- data.frame(Year=year_ind, Country=count_ind, Value=vect, Rank=rank_vec)
#     tab <- rbind(tab, y_bin)
#   }
#   years_cons <- c(1995, 2005, 2010, 2015)
#   tab_spread <- tab %>%
#     select(-Value) %>%
#     filter(Rank<=8, Year %in% years_cons) %>%
#     mutate(Country=countrycode(Country, "un", "country.name")) %>%  
#     spread(Year, Country) # %>%    select(-Rank)
#   tab_spread[tab_spread=="United Kingdom of Great Britain and Northern Ireland"]<-"UK"
#   tab_spread[tab_spread=="United States of America"]<-"USA"
#   tab_spread
#   write.table(tab_spread, 
#               row.names = FALSE,
#               sep = " & ", 
#               eol = "\\\\\ \n", # "\\\\\ \n \\midrule\n", 
#               quote = FALSE, 
#               na = " ",
#               file = paste(output_path_tables, output_names[i], ".tex", sep = ""))
# }

