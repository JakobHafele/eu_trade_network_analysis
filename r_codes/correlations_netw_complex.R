rm(list=ls())

library(countrycode)
library(tidyr)
library(data.table)
library(dplyr)
library(ggplot2)
library(ggpubr)
library(stargazer)
library(plm)
library(pcse)
library(foreign)
library(GenABEL)
library(cowplot)
library(readr)
library(tibble)
library(xtable)


source("correlations_netw_functions.R")

load("/home/knallbunt/uni/forschung/WTN/eu_trade_network/output/etn_analysis_results_all_years_2018_06_07.Rdata")

# make closeness dist
# centrality scores in diff networks ----------

# create table 
years <- as.numeric(c(1965:2014))

closeness <- rownames_to_column(as.data.frame(measures_local[["2014"]][["complexity_11"]][["closeness_tnet"]]))
for (y in years){
  closeness <- left_join(closeness, rownames_to_column(as.data.frame(measures_local[[paste(y)]][["complexity_11"]][["closeness_tnet"]])), by = c("rowname" = "rowname"))
}

closeness_1  <- closeness[, -2]
colnames(closeness_1) <- c("country", years)



for (i in 1:11){
  closeness <- rownames_to_column(as.data.frame(measures_local[["2014"]][[paste("complexity_",i,sep="")]][["closeness_tnet"]]))
  for (y in years){
    closeness <- left_join(closeness, rownames_to_column(as.data.frame(measures_local[[paste(y)]][[paste("complexity_",i,sep="")]][["closeness_tnet"]])), by = c("rowname" = "rowname"))
  }
  
  assign(paste("closeness_", i, sep=""), closeness[, -2])
  rm(closeness)
}

closeness_centralities <- list(closeness_1, closeness_2, closeness_3, closeness_4, closeness_5, closeness_6, closeness_7,
                               closeness_8, closeness_9, closeness_10, closeness_11)

closeness_centralities_1 <- lapply(closeness_centralities, "colnames<-", c("country", years))

# add complexity info as row
for (i in 1:11){
  closeness_centralities_1[[i]][["complexity_lvl"]] <- i 
}

# long format
closeness_centralities_long <-  lapply(closeness_centralities_1, gather, "year", "closeness", 2:51)

# to dataframe
closeness_long_df <- closeness_centralities_long[[1]]
closeness_long_df_1 <- closeness_long_df
for (i in 2:11){
  closeness_long_df_1 <- rbind(closeness_long_df_1, closeness_centralities_long[[i]])
}

# spread complexity lvls
closeness_long_df_2 <- spread(closeness_long_df_1, "complexity_lvl", "closeness")

write_csv(closeness_long_df_2, "output/closeness_complexity_lvls.csv")

# create pooled data

closeness_long_df_2_pooled <- closeness_long_df_2
closeness_long_df_2_pooled[,2] <- "pooled" 
closeness_long_df_3 <- rbind(closeness_long_df_2, closeness_long_df_2_pooled)

# cors (make yearwise lists, then calc cors)
closeness_cors <- closeness_long_df_3
closeness_cors[is.na(closeness_cors)] <- 0

dates <- c(1965:2014,"pooled")
yearwise_closeness <- list()
for(i in dates){
  yearwise_closeness[[i]] <- closeness_cors[closeness_cors$year==i,]
}




cors_closeness <- as.data.frame(c(1965:2014,"pooled"))
colnames(cors_closeness) <- "year"
rownames(cors_closeness) <- dates

# calc closness cor between 11 and other thresholds
for (i in dates){
  for (x_var in 1:10){
    cors_closeness[paste(i),paste(x_var)] <- cor(yearwise_closeness[[paste(i)]][[paste(x_var)]],yearwise_closeness[[paste(i)]][["11"]], method = c("spearman"))
  }}

write_csv(cors_closeness, "output/cors_closeness_complexity_11_and_others_2018_06_06.csv")

cors_closeness_long <- gather(cors_closeness, "Network overall and threshold", "Correlation", 2:11)
cors_closeness_long_1 <- cors_closeness_long[!cors_closeness_long$year=="pooled",]
cors_closeness_long_1$year <- as.numeric(as.character(cors_closeness_long_1$year))

# plot
plot_cor_closeness_networks <- ggplot(data = cors_closeness_long_1[cors_closeness_long_1$`Network overall and threshold`==c(1,2,9,10), ], aes(x = year)) +
  geom_line(aes(y = Correlation, group = `Network overall and threshold`, linetype = `Network overall and threshold`)) +
  labs(linetype='Network') 

ggsave("output/network_stats_complexity_cors/cor_closeness_networks_2018_06_06.pdf", plot = plot_cor_closeness_networks, width = 3, height = 2)


# closeness for each complexity lvl -> kurtosis of closeness dist! correl mit thresholds
# mean closeness for each year for ech thrshold
closeness_mean <- closeness_long_df_2[, -1] %>%
  group_by(year) %>%
  summarise_all(mean, na.rm=TRUE)
closeness_dist <- gather(closeness_mean, complexity_lvl, mean_closeness, 2:12)
closeness_dist$complexity_lvl <- as.numeric(closeness_dist$complexity_lvl)
closeness_dist$year <- as.numeric(closeness_dist$year)
closeness_mean_3 <- closeness_dist %>%
  group_by(year) %>%
  summarize(cor_closeness_mean = cor(complexity_lvl,mean_closeness))

closeness_kurt <- closeness_long_df_2[, -1] %>%
  group_by(year) %>%
  summarise_all(kurtosis, na.rm=TRUE)
closeness_kurt_2 <- gather(closeness_kurt, complexity_lvl, closeness_kurt, 2:12)
closeness_kurt_2$complexity_lvl <- as.numeric(closeness_kurt_2$complexity_lvl)
closeness_kurt_2$year <- as.numeric(closeness_kurt_2$year)

closeness_skew <- closeness_long_df_2[, -1] %>%
  group_by(year) %>%
  summarise_all(skewness, na.rm=TRUE)
closeness_skew_2 <- gather(closeness_skew, complexity_lvl, closeness_skew, 2:12)
closeness_skew_2$complexity_lvl <- as.numeric(closeness_skew_2$complexity_lvl)
closeness_skew_2$year <- as.numeric(closeness_skew_2$year)

closeness_maxima <- closeness_long_df_2[, -1] %>%
  group_by(year) %>%
  summarise_all(find_peaks) # m defines how many lower values on each side of datapoint are needed for it to be defined as peak




# peaks in density dist
years_peaks <- c(1965:2014)
# df for results
peaks <- data.frame(matrix(0, ncol = 11, nrow = length(years_peaks)))
colnames(peaks) <- c("year", c(1:10))
rownames(peaks) <- years_peaks
peaks$year <- years_peaks
# calc peaks for each year and complexity lvl

for (i in 1:10){
  for (y in years_peaks){
d <- density(closeness_long_df_2[closeness_long_df_2$year==paste(y), paste(i)], na.rm=TRUE)
peaks[paste(y), paste(i)] <- count(find_peaks(d$y, m=60)) # m defines how many values next to maxpoint must be lower in order for it to be counted
  }
}

closeness_peaks <- gather(peaks, complexity_lvl, closeness_density_peaks, 2:11)
closeness_peaks$complexity_lvl <- as.numeric(closeness_peaks$complexity_lvl)
closeness_peaks$year <- as.numeric(closeness_peaks$year)

# one examplary plot of dist

# fucking loop is not working -.-
plot_1 <- ggplot(closeness_long_df_2[closeness_long_df_2$year==2014, ]) +
  geom_density(aes(x = closeness_long_df_2[closeness_long_df_2$year==2014, "1"])) +
  xlab("closeness") +
  ggtitle("Complexity level 1")
plot_2 <- ggplot(closeness_long_df_2[closeness_long_df_2$year==2014, ]) +
  geom_density(aes(x = closeness_long_df_2[closeness_long_df_2$year==2014, "2"])) +
  xlab("closeness") +
  ggtitle("Complexity level 2")
plot_3 <- ggplot(closeness_long_df_2[closeness_long_df_2$year==2014, ]) +
  geom_density(aes(x = closeness_long_df_2[closeness_long_df_2$year==2014, "3"])) +
  xlab("closeness") +
  ggtitle("Complexity level 3")
plot_4 <- ggplot(closeness_long_df_2[closeness_long_df_2$year==2014, ]) +
  geom_density(aes(x = closeness_long_df_2[closeness_long_df_2$year==2014, "4"])) +
  xlab("closeness") +
  ggtitle("Complexity level 4")
plot_5 <- ggplot(closeness_long_df_2[closeness_long_df_2$year==2014, ]) +
  geom_density(aes(x = closeness_long_df_2[closeness_long_df_2$year==2014, "5"])) +
  xlab("closeness") +
  ggtitle("Complexity level 5")
plot_6 <- ggplot(closeness_long_df_2[closeness_long_df_2$year==2014, ]) +
  geom_density(aes(x = closeness_long_df_2[closeness_long_df_2$year==2014, "6"])) +
  xlab("closeness") +
  ggtitle("Complexity level 6")
plot_7 <- ggplot(closeness_long_df_2[closeness_long_df_2$year==2014, ]) +
  geom_density(aes(x = closeness_long_df_2[closeness_long_df_2$year==2014, "7"])) +
  xlab("closeness") +
  ggtitle("Complexity level 7")
plot_8 <- ggplot(closeness_long_df_2[closeness_long_df_2$year==2014, ]) +
  geom_density(aes(x = closeness_long_df_2[closeness_long_df_2$year==2014, "8"])) +
  xlab("closeness") +
  ggtitle("Complexity level 8")
plot_9 <- ggplot(closeness_long_df_2[closeness_long_df_2$year==2014, ]) +
  geom_density(aes(x = closeness_long_df_2[closeness_long_df_2$year==2014, "9"])) +
  xlab("closeness") +
  ggtitle("Complexity level 9")
plot_10 <- ggplot(closeness_long_df_2[closeness_long_df_2$year==2014, ]) +
  geom_density(aes(x = closeness_long_df_2[closeness_long_df_2$year==2014, "10"])) +
  xlab("closeness") +
  ggtitle("Complexity level 10")



density_closeness_complex <- plot_grid(plot_1, plot_2, plot_3, plot_4, plot_5,
                                      plot_6, plot_7, plot_8, plot_9, plot_10,
                           align='h', ncol=3)

ggsave("output/density_closeness_complexity.pdf", plot = density_closeness_complex, width = 9, height = 12)

# cor between closeness and gdp in less coimplex networks compared to more komplex networks -> 
# if it is lower: austerity makes no sense at all b/c if you are primarily exporting in less complex networks a country
# would get closer to core i nthat networks, but that doesnt help!


# tables of top 10
# 2014
for (i in c(1,2,9,10,11)){
  ranked <- closeness_centralities_1[[i]][,c(1,51)]
  ranked <- ranked[order(ranked$`2014`, decreasing=TRUE),]
  assign(paste("c_2014_", i, sep=""), ranked[1:10,1])
}

closeness_top10_2014 <- data.frame(c_2014_1, c_2014_2, c_2014_9, c_2014_10, c_2014_11)
colnames(closeness_top10_2014) <- c("Complexity 0% - 10%", "Complexity 10% - 20%", "Complexity 80% - 90%", "Complexity 90% - 100%", "Overall Network")

# translate countrycodes
closeness_top10_2014 <- lapply(closeness_top10_2014, countrycode, "id", "name", custom_dict = oec_countrycodes) 
closeness_top10_2014 <- as.data.frame(do.call(rbind, closeness_top10_2014))
closeness_top10_2014 <- as.data.frame(t(closeness_top10_2014))
rownames(closeness_top10_2014) <- c(1:10)
closeness_top10_2014 <- closeness_top10_2014[,c(1,4,5)]


# 1990
for (i in c(1,2,9,10,11)){
  ranked <- closeness_centralities_1[[i]][,c(1,27)]
  ranked <- ranked[order(ranked$`1990`, decreasing=TRUE),]
  assign(paste("c_1990_", i, sep=""), ranked[1:10,1])
}

closeness_top10_1990 <- data.frame(c_1990_1, c_1990_2, c_1990_9, c_1990_10, c_1990_11)
colnames(closeness_top10_1990) <- c("Complexity 0% - 10%", "Complexity 10% - 20%", "Complexity 80% - 90%", "Complexity 90% - 100%", "Overall Network")

# translate countrycodes
closeness_top10_1990 <- lapply(closeness_top10_1990, countrycode, "id", "name", custom_dict = oec_countrycodes)
closeness_top10_1990 <- as.data.frame(do.call(rbind, closeness_top10_1990))
closeness_top10_1990 <- as.data.frame(t(closeness_top10_1990))
rownames(closeness_top10_1990) <- c(1:10)
closeness_top10_1990 <- closeness_top10_1990[,c(1,4,5)]

# save
print(xtable(closeness_top10_1990, type = "latex"), file = "output/network_stats_complexity_cors/closeness_top10_1990.tex", include.rownames=FALSE)
print(xtable(closeness_top10_2014, type = "latex"), file = "output/network_stats_complexity_cors/closeness_top10_2014.tex", include.rownames=FALSE)







# facet plots for cors between thresholds and stats
work_data_1 <- characteristics_global[characteristics_global$complexity_lvl %in% c(1:10), ]
work_data_1$complexity_lvl <- as.numeric(work_data_1$complexity_lvl)
work_data_1 <- left_join(work_data_1, closeness_kurt_2, by=c("year", "complexity_lvl"))
work_data_1 <- left_join(work_data_1, closeness_skew_2, by=c("year", "complexity_lvl"))
work_data_1 <- left_join(work_data_1, closeness_dist, by=c("year", "complexity_lvl"))
work_data_1 <- left_join(work_data_1, closeness_peaks, by=c("year", "complexity_lvl"))
work_data_1$year <- as.character(work_data_1$year)
work_data_1$complexity_lvl <- as.numeric(work_data_1$complexity_lvl)
data_pooled <- work_data_1
data_pooled[,1] <- "pooled" 
work_data_2 <- rbind(work_data_1, data_pooled)


vars_list <- c("dist.mean", "dist.sd", "dist.kurt",
               "dist.skew", "global_clust", "edge.density", "assort_ECI", "assort_streng", "assort_ECI_streng",
               "assort_ECI_cutoff", "assort_GDP_cur", "assort_pop", "assort_Wage_share", "assort_GDP_hour", "closeness_kurt",
               "closeness_skew", "mean_closeness", "closeness_density_peaks")



title <- "Complexity and "
for (var_cons in vars_list){
  print(var_cons)
  plot_name <- paste0("output/facet_plots_network/new/complexity_", var_cons, ".pdf")
  make_facet_plot(var_cons, "complexity_lvl", title, ylog = FALSE)
  ggsave(filename = plot_name, height = 14, width = 14)
}

# rm(list=ls())




oec_countrycodes <- fread("/home/knallbunt/uni/forschung/data/wtn/country_names.tsv")
oec_countrycodes <- as.data.frame(oec_countrycodes)

# vars_list <- c("dist.mean", "dist.sd", "dist.kurt",
#                "dist.skew", "global_clust", "edge.density", "assort_ECI", "assort_streng", "assort_ECI_streng",
#                "assort_ECI_cutoff", "assort_GDP_cur", "assort_pop", "assort_Wage_share", "assort_GDP_hour")
# 
# # all years cor coefs ---------
# load("/home/knallbunt/uni/forschung/WTN/eu_trade_network/output/etn_analysis_results_all_years.Rdata") 
# work_data_ay <- characteristics_global[characteristics_global$complexity_lvl %in% c(1:10), ]
# work_data_ay$year <- as.character(work_data_ay$year)
# work_data_ay$complexity_lvl <- as.numeric(work_data_ay$complexity_lvl)
# data_pooled_ay <- work_data_ay
# data_pooled_ay[,1] <- "pooled" 
# work_data_ay_1 <- rbind(work_data_ay, data_pooled_ay)
work_data_ay_1 <- work_data_2

cors <- as.data.frame(c(1965:2014,"pooled"))
colnames(cors) <- "year"
dates <- c(1965:2014,"pooled")

work_data_ay_2 <- work_data_ay_1
work_data_ay_2[is.na(work_data_ay_2)] <- 0

yearwise <- list()
for(i in dates){
yearwise[[i]] <- work_data_ay_2[work_data_ay_2$year==i,]
}
rownames(cors) <- dates


for (i in dates){
  for (y_var in vars_list){
cors[paste(i),paste(y_var)] <- cor(yearwise[[paste(i)]][["complexity_lvl"]],yearwise[[paste(i)]][[y_var]], method = c("spearman"))
cors[paste(i),paste(y_var, "_p_value", sep="")] <- cor.test(yearwise[[paste(i)]][["complexity_lvl"]],yearwise[[paste(i)]][[y_var]], method = c("spearman"))$p.value 
}}

write_csv(cors, "output/network_stats_complexity_cors_2018_06_06.csv")

for (y_var in vars_list){
plot_cor <- ggplot(data = cors, aes_string (x = "year")) +
  geom_line(aes_string(y=y_var, group = 1))

plot_p <- ggplot(data = cors, aes_string (x = "year")) +
   geom_col(aes_string(y=paste(y_var, "_p_value", sep=""))) +
  coord_cartesian(ylim = c(0, 0.2)) +
  ylab("p")


assign(paste("merged_", y_var, sep=""), plot_grid(plot_cor, plot_p, align='v', ncol=1, rel_heights = c(2, 1)))
}

cors_p_vlaues <- plot_grid(merged_assort_ECI, merged_assort_ECI_cutoff, merged_assort_ECI_streng, merged_assort_GDP_cur,
                           merged_assort_GDP_hour, merged_assort_pop, merged_assort_streng, merged_assort_Wage_share,
                           merged_dist.kurt, merged_dist.mean, merged_dist.sd, merged_dist.skew, merged_edge.density,
                           merged_global_clust, merged_closeness_density_peaks, merged_closeness_kurt, merged_closeness_skew,
                           merged_mean_closeness, align = 'h', ncol = 4)

ggsave("output/network_stats_complexity_cors/cors_plots_2018_06_06.pdf", plot = cors_p_vlaues, width = 20, height = 25)

# cor between global clust and complexity --------
cors_years <- cors[-51,]
cors_years$year <- as.numeric(as.character(cors_years$year))
cor_complex_glob_clust <- ggplot(data = cors_years, aes_string (x = "year")) +
  geom_line(aes_string(y="global_clust", group = 1)) +
  xlab("Year") +
  ylab("Correlation Coefficient") +
  ggtitle("Correlation between global clustering\n and complexity thresholds")
  
  
ggsave("output/network_stats_complexity_cors/cor_complex_glob_clust.pdf", plot = cor_complex_glob_clust, width = 5, height = 4)


# cor between closesness and complexity -------
cor_complex_close_skew <- ggplot(data = cors_years, aes_string (x = "year")) +
  geom_line(aes_string(y="closeness_skew", group = 1)) +
  xlab("Year") +
  ylab("Correlation") +
  ggtitle("Skewness of closeness distribution\n and complexity thresholds")

cor_complex_close_kurt <- ggplot(data = cors_years, aes_string (x = "year")) +
  geom_line(aes_string(y="closeness_kurt", group = 1)) +
  xlab("Year") +
  ylab("Correlation") +
  ggtitle("Kurtosis of closeness distribution\n and complexity thresholds")

cor_complex_close_dens <- ggplot(data = cors_years, aes_string (x = "year")) +
  geom_line(aes_string(y="closeness_density_peaks", group = 1)) +
  xlab("Year") +
  ylab("Correlation") +
  ggtitle("Maxima in closeness density\n and complexity thresholds")

cor_complex_close_mean <- ggplot(data = cors_years, aes_string (x = "year")) +
  geom_line(aes_string(y="mean_closeness", group = 1)) +
  xlab("Year") +
  ylab("Correlation") +
  ggtitle("Mean of closeness distribution\n and complexity thresholds")

closeness_complexity_plot <- plot_grid(cor_complex_close_skew, cor_complex_close_kurt,
                                       cor_complex_close_dens, cor_complex_close_mean,
                                       align = 'h', ncol = 2)
ggsave("output/network_stats_complexity_cors/cor_complex_closeness.pdf", plot = closeness_complexity_plot, width = 8, height = 8)


# cor between strenght and complexity ------
cor_complex_stren_mean <- ggplot(data = cors_years, aes_string (x = "year")) +
  geom_line(aes_string(y="dist.mean", group = 1)) +
  xlab("Year") +
  ylab("Correlation") +
  ggtitle("Mean of strength distribution\n and complexity thresholds")

cor_complex_stren_skew <- ggplot(data = cors_years, aes_string (x = "year")) +
  geom_line(aes_string(y="dist.skew", group = 1)) +
  xlab("Year") +
  ylab("Correlation") +
  ggtitle("Skewness of strength distribution\n and complexity thresholds")

cor_complex_stren_kurt <- ggplot(data = cors_years, aes_string (x = "year")) +
  geom_line(aes_string(y="dist.kurt", group = 1)) +
  xlab("Year") +
  ylab("Correlation") +
  ggtitle("Kurtosis of strength distribution\n and complexity thresholds")

cor_complex_stren_sd <- ggplot(data = cors_years, aes_string (x = "year")) +
  geom_line(aes_string(y="dist.sd", group = 1)) +
  xlab("Year") +
  ylab("Correlation") +
  ggtitle("Standard deviation of strength mean\n and complexity thresholds")



stren_complexity_plot <- plot_grid(cor_complex_stren_mean, cor_complex_stren_skew,
                                   cor_complex_stren_kurt, cor_complex_stren_sd, 
                                       align = 'h', ncol = 2)
ggsave("output/network_stats_complexity_cors/cor_complex_stren.pdf", plot = stren_complexity_plot, width = 8, height = 8)






# strength gdp in diff networks?
strenght_gdp <- network_macro_cor[network_macro_cor$Var1=="strengh" & network_macro_cor$Var2=="GDP", ]

# cluster_wei gdp in diff networks?
#



# rich club cors -----
rich_years <- c("1970", "1980", "1990", "2000", "2010")
rich_res <- data.frame(mean=double(), sd=double(), rich=character(), year=double(), complexity=double())
for (y in rich_years){
for (i in 1:10){
 rich_res_pre <- rich_club_results[[paste(y)]][[paste(i)]]
 rich_res_pre[, "year"] <- y
 colnames(rich_res_pre) <- c("mean", "sd", "rich", "year")
 rich_res_pre[, "complexity"] <- i
 rich_res <- rbind(rich_res, rich_res_pre)
}}

rich_cors <- rich_res %>%
  group_by(year) %>%
  summarise(cor=cor(complexity,mean))

# -> eher no correl ... more data needed to figure out whats going on here


# trade balances betweens cores and ps ---------

View(trade_stats_cp)
trade_stats_core_peri <- trade_stats_cp[!trade_stats_cp$complexity == 11,]
trade_stats_core_peri_2 <- trade_stats_core_peri[trade_stats_core_peri$exporter == "Core" & trade_stats_core_peri$importer == "Periphery" |
                                                   trade_stats_core_peri$exporter == "Periphery" & trade_stats_core_peri$importer == "Core", ]
trade_stats_core_peri_3 <- trade_stats_core_peri_2[,-2]
trade_stats_core_peri_4 <- spread(trade_stats_core_peri_3, exporter, export_value_agg)
colnames(trade_stats_core_peri_4) <- c("year", "complexity", "ex", "im")
trade_stats_core_peri_4[, "bal"] <- trade_stats_core_peri_4[, "ex"] - trade_stats_core_peri_4[, "im"]

cors <- trade_stats_core_peri_4 %>%
  group_by(year) %>%
  summarise(cor=cor(bal,complexity, method="spearman"))
colnames(cors) <- c("year", "core_peri_cor")

# semis
trade_stats_core_peri <- trade_stats_cp[!trade_stats_cp$complexity == 11,]
trade_stats_core_peri_2 <- trade_stats_core_peri[trade_stats_core_peri$exporter == "Core" & trade_stats_core_peri$importer == "Semiperiphery" |
                                                   trade_stats_core_peri$exporter == "Semiperiphery" & trade_stats_core_peri$importer == "Core", ]
trade_stats_core_peri_3 <- trade_stats_core_peri_2[,-2]
trade_stats_core_peri_4 <- spread(trade_stats_core_peri_3, exporter, export_value_agg)
colnames(trade_stats_core_peri_4) <- c("year", "complexity", "ex", "im")
trade_stats_core_peri_4[, "bal"] <- trade_stats_core_peri_4[, "ex"] - trade_stats_core_peri_4[, "im"]

cors_semi <- trade_stats_core_peri_4 %>%
  group_by(year) %>%
  summarise(cor=cor(bal,complexity, method="spearman"))
colnames(cors_semi) <- c("year", "core_semiperi_cor")
cors <- left_join(cors,cors_semi, b=c("year"))

# baltics
trade_stats_core_peri <- trade_stats_cp[!trade_stats_cp$complexity == 11,]
trade_stats_core_peri_2 <- trade_stats_core_peri[trade_stats_core_peri$exporter == "Core" & trade_stats_core_peri$importer == "Baltics" |
                                                   trade_stats_core_peri$exporter == "Baltics" & trade_stats_core_peri$importer == "Core", ]
trade_stats_core_peri_3 <- trade_stats_core_peri_2[,-2]
trade_stats_core_peri_4 <- spread(trade_stats_core_peri_3, exporter, export_value_agg)
colnames(trade_stats_core_peri_4) <- c("year", "complexity", "im", "ex")
trade_stats_core_peri_4[, "bal"] <- trade_stats_core_peri_4[, "ex"] - trade_stats_core_peri_4[, "im"]

cors_baltics <- trade_stats_core_peri_4 %>%
  group_by(year) %>%
  summarise(cor=cor(bal,complexity, method="spearman"))
colnames(cors_baltics) <- c("year", "core_baltics_cor")
cors <- left_join(cors,cors_baltics, b=c("year"))

# east
trade_stats_core_peri <- trade_stats_cp[!trade_stats_cp$complexity == 11,]
trade_stats_core_peri_2 <- trade_stats_core_peri[trade_stats_core_peri$exporter == "Core" & trade_stats_core_peri$importer == "East" |
                                                   trade_stats_core_peri$exporter == "East" & trade_stats_core_peri$importer == "Core", ]
trade_stats_core_peri_3 <- trade_stats_core_peri_2[,-2]
trade_stats_core_peri_4 <- spread(trade_stats_core_peri_3, exporter, export_value_agg)
colnames(trade_stats_core_peri_4) <- c("year", "complexity", "ex", "im")
trade_stats_core_peri_4[, "bal"] <- trade_stats_core_peri_4[, "ex"] - trade_stats_core_peri_4[, "im"]

cors_baltics <- trade_stats_core_peri_4 %>%
  group_by(year) %>%
  summarise(cor=cor(bal,complexity, method="spearman"))
colnames(cors_baltics) <- c("year", "core_east_cor")
cors <- left_join(cors,cors_baltics, b=c("year"))

# plot
cors_plot_data <- cors
colnames(cors_plot_data) <- c("Year", "Periphery", "Semiperiphery", "Baltics", "East")
cors_plot_data_2 <- gather(cors_plot_data, Core_and , Correlation, 2:5)

cors_bals_plot <- ggplot(cors_plot_data_2) +
  geom_line(aes(x=Year, y=Correlation, group=Core_and, linetype=Core_and)) +
  scale_linetype_manual(values=c("dashed", "longdash", "solid", "dotted")) +
  ggtitle("Trade balance vs complexity level")

ggsave("output/network_stats_complexity_cors/cor_complex_bal.pdf", plot = cors_bals_plot, width = 6, height = 4)

# semi and baltics
trade_stats_core_peri <- trade_stats_cp[!trade_stats_cp$complexity == 11,]
trade_stats_core_peri_2 <- trade_stats_core_peri[trade_stats_core_peri$exporter == "Semiperiphery" & trade_stats_core_peri$importer == "Baltics" |
                                                   trade_stats_core_peri$exporter == "Baltics" & trade_stats_core_peri$importer == "Semiperiphery", ]
trade_stats_core_peri_3 <- trade_stats_core_peri_2[,-2]
trade_stats_core_peri_4 <- spread(trade_stats_core_peri_3, exporter, export_value_agg)
colnames(trade_stats_core_peri_4) <- c("year", "complexity", "ex", "im")
trade_stats_core_peri_4[, "bal"] <- trade_stats_core_peri_4[, "ex"] - trade_stats_core_peri_4[, "im"]

cors_semi_baltics <- trade_stats_core_peri_4 %>%
  group_by(year) %>%
  summarise(cor=cor(bal,complexity, method="spearman"))


