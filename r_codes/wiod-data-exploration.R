
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


source("wiod-functions.R")

raw_data <- readRDS("data/wtn/wiot/wiot_complexity_panel_w_i_dollar.Rda")

# work_data_1 <- raw_data %>%
#   select(-Name) %>%
#   rename(VA=`VA_$`,
#          VA_dollar=`VA_from_national_prices(VA.y)_Dollar`) %>%
#   mutate_at(4:length(names(.)), as.double)
# head(work_data_1)

work_data_1 <- raw_data

# General information - plots 

# Complexity of industries over time ---- only makes sense for unweighted industries
# industry_complexity <- ggplot(work_data_1, aes(x=Year, y=avg_weighted_PCI, color=IndustryCode)) + geom_line() +
#   scale_color_discrete(guide=guide_legend(ncol=8)) + 
#   theme(legend.position = "bottom",
#         panel.background = element_rect(fill = "white", size = 0.5, linetype = "solid"),
#         panel.border = element_blank(), axis.line = element_line(),
#         panel.grid.major = element_line(size = 0.5, linetype = 'solid', colour = "lightgrey"))
# 
# ggsave(filename = "output/indus_complexity_w_time.pdf", 
#        plot = industry_complexity,  
#        height = 14, width = 28)

# Industries and VA: uninteressant, da avg_complexity ueber industry berechnet

# Make facet plots on general relationship between complexity and other variables in various countries ----

vars_list <- c("VA", "CAP","COMP","EMP","EMPE","GO","GO_PI","GO_QI","H_EMPE","II","II_PI",
               "II_QI","K","LAB","VA.y","VA_PI","VA_QI","national_curr_dollar_exchange", 
               "COMP_Dollar","CAP_Dollar","K_Dollar","LAB_Dollar","VA_dollar", 
               "gini_solt_disp", "gini_solt_mkt", "GDP_total", "GDP_PC")


title_ranked <- "Weighted ranked Complexity and "
for (var_cons in vars_list){
  print(var_cons)
  plot_name <- paste0("output/facet_plots/complexity_w_rank_", var_cons, ".pdf")
  make_facet_plot(var_cons, "ranked_pci", title_ranked, ylog = TRUE)
  ggsave(filename = plot_name, height = 14, width = 14)
}


title_norm <- "Weighted ranked and normalized Complexity and "
for (var_cons in vars_list){
  print(var_cons)
  plot_name <- paste0("output/facet_plots/complexity_w_norm_", var_cons, ".pdf")
  make_facet_plot(var_cons, "normal_pci", title_norm, ylog = TRUE)
  ggsave(filename = plot_name, height = 14, width = 14)
}


# everything pooled
# for (i in vars_list){
# assign(paste("plot_", i, sep=""), ggplot(work_data_1, aes(x=ranked_pci, y=i)) + geom_point() + geom_smooth(method=lm))
# }

plot_CAP_Dollar <- ggplot(work_data_1, aes(x=ranked_pci, y=CAP_Dollar)) + geom_point() + geom_smooth(method=lm)
plot_VA_dollar <- ggplot(work_data_1, aes(x=ranked_pci, y=VA_dollar)) + geom_point() + geom_smooth(method=lm)
plot_LAB_Dollar <- ggplot(work_data_1, aes(x=ranked_pci, y=LAB_Dollar)) + geom_point() + geom_smooth(method=lm)
plot_COMP_Dollar <- ggplot(work_data_1, aes(x=ranked_pci, y=COMP_Dollar)) + geom_point() + geom_smooth(method=lm)
plot_H_EMPEr <- ggplot(work_data_1, aes(x=ranked_pci, y=H_EMPE)) + geom_point() + geom_smooth(method=lm)
plot_GO <- ggplot(work_data_1, aes(x=ranked_pci, y=GO)) + geom_point() + geom_smooth(method=lm)

overview_plot <- plot_grid(plot_CAP_Dollar, plot_VA_dollar, plot_LAB_Dollar,
                           plot_COMP_Dollar, plot_H_EMPE, plot_GO,
                           align='h')


ggsave("output/wiod-pooled-overview-plot.pdf", plot = overview_plot, width = 12, height = 8)


# avg complexity of industries (which are the most complex industries?)
avg_complexity_industries <- work_data_1 %>%
  group_by(IndustryCode) %>%
  summarize_at(c("avg_weighted_PCI"), mean, na.rm=TRUE) 
write_csv(avg_complexity_industries, "avg_industry_complexity_total.csv")  
 
