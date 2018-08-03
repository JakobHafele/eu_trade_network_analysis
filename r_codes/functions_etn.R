
# for ent_analysis_hvd

no_prod <- function(inputnetwork){
  #  summarizes all products into one export flow
    inputnetwork %>%
    group_by(exporter, importer) %>%
    summarise(export_value_agg = sum(export_value, na.rm=TRUE)) %>%
    ungroup()
}

export_stats <- function(input){ 
  # summarizes all exports by exporting country
  input %>%  
  group_by(exporter) %>%
  summarise(exports_agg = sum(export_value_agg, na.rm=TRUE)) %>%
  ungroup() #%>%
  #select(exports_agg)
}

import_stats <- function(input){
  input %>%  
    group_by(importer) %>%
    summarise(imports_agg = sum(export_value_agg, na.rm=TRUE)) %>%
    ungroup() #%>%
    #select(imports_agg)
}

plot_stats <- function(type, countries, name){
  etn_balance_long <- melt(etn_trade_stats[,c("exporter",colnames(etn_trade_stats)[grep(type,colnames(etn_trade_stats))])])
  etn_balance_plot <- subset(etn_balance_long, etn_balance_long[,"exporter"] %in% countries) ###choose countries 4 plot
  png(filename=paste("/home/knallbunt/uni/forschung/WTN/eu trade network/output/thresholded_trade_",type,"_",year_analyzed,"_",name,".png", sep=""), width=1500, height=1000)
  print(ggplot(etn_balance_plot, aes(x=variable, y=value/div, group=exporter)) + geom_line(aes(color=exporter), size=1) + geom_dl(aes(label=exporter),method="first.points") + geom_dl(aes(label=exporter),method="last.points") + scale_y_continuous(label=comma))
  dev.off()
}

build_graph <- function(countries, network){
  network_analyzed <- network[grep(countries, network[["exporter"]]),]
  network_analyzed <- network_analyzed[grep(countries, network_analyzed[["importer"]]),]
  network_analyzed_m <- as.matrix(network_analyzed)
  network_analyzed_g <- graph.edgelist(network_analyzed_m[,1:2], directed = TRUE)
  E(network_analyzed_g)$Weight <- as.numeric(network_analyzed_m[,3]) #add weights
  E(network_analyzed_g)$weight <- as.numeric(network_analyzed_m[,3])
  
  closeness_tnet <- as.data.frame(closeness_w(
    as.tnet(cbind(as_edgelist(network_analyzed_g, names = FALSE), E(network_analyzed_g)$Weight))))
  # sometimes one vertex has no closeness score and thus gets omitted. in this case adding names doesnt work, so solution:
  bugfix <- as.data.frame(c(1:length(V(network_analyzed_g)$name)))
  colnames(bugfix) <- "id"
  closeness_tnet <- left_join(bugfix, closeness_tnet, by= c("id" = "node") )
  closeness_tnet[is.na(closeness_tnet)] <- 0
  closeness_tnet <- closeness_tnet[,2]
  closeness_tnet <- setNames(closeness_tnet, V(network_analyzed_g)$name)
  
  V(network_analyzed_g)$closeness <- closeness_tnet * 1000 # 4 assort
  
  return(network_analyzed_g)
}



build_graph_2 <- function(countries, network){
  network_analyzed <- network[grep(countries, network[["exporter"]]),]
  network_analyzed <- network_analyzed[grep(countries, network_analyzed[["importer"]]),]
  network_analyzed <- left_join(network_analyzed, macro_data_cp_ya[, c("Country", "population")], by = c("exporter"="Country"))
  network_analyzed[, "weights"] <- network_analyzed[, 3] / network_analyzed[, 4]
  network_analyzed_m <- as.matrix(network_analyzed)
  network_analyzed_g <- graph_from_edgelist(network_analyzed_m[,1:2], directed = TRUE)
  E(network_analyzed_g)$Weight_abs <- as.numeric(network_analyzed_m[,3]) #add weights abs
  E(network_analyzed_g)$Weight <- as.numeric(network_analyzed_m[,5]) #add weights rel to pop
  E(network_analyzed_g)$weight <- as.numeric(network_analyzed_m[,5])
  return(network_analyzed_g)
}

# function to add attributes to vertices
add_vertex_attr <- function(graph){
 V(graph)$population <- data_added_etn[["population"]]
 V(graph)$Wage_share <- data_added_etn[["Adjusted_wage_share"]]
 V(graph)$GDP_curr_prices <- data_added_etn[["GDP_Dollar_curr"]]
 V(graph)$GDP_hour <- data_added_etn[["GDP_per_hour_worked_curr_prices"]]
 V(graph)$GDP_growth <- data_added_etn[["GDP_growth"]]
 V(graph)$ECI <- data_added_etn[["complexity_harv"]]
 V(graph)$ECI_rank <- data_added_etn[["complexity_rank_ECI"]]
 return(graph)
}

add_pop_GDP_vertex <- function(graph){
  V(graph)$population <- macro_data_cp_ya[["population"]][match(V(graph)$name, macro_data_cp_ya$Country)]
  V(graph)$GDP <- macro_data_cp_ya[["GDP_Dollar_curr"]][match(V(graph)$name, macro_data_cp_ya$Country)]
  return(graph)
}


range01 <- function(x){
  (x-min(x))/(max(x)-min(x))
  }


# for io_analysis_wiod

plot_va <- function(countries, name){
  wiot_valueadded_complexity_plot <- subset(wiot_valueadded_complexity_o, wiot_valueadded_complexity_o[,"Country"] %in% countries)
  png(filename=paste("/home/knallbunt/uni/forschung/WTN/eu trade network/output/valueadded_complexity",year_analyzed_wiot,"_",name,".png", sep=""), width=1500, height=1000)
  print(ggplot(wiot_valueadded_complexity_plot, aes(x=reorder(IndustryCode, avg_complexity), y=VA, group=Country)) + geom_line(aes(color=Country), size=1)) # + geom_dl(aes(label=country),method="first.points") + geom_dl(aes(label=country),method="last.points") # +  scale_y_continuous(label=comma)
  dev.off()
}

# for gephi

saveAsGEXF <- function(g, filepath="converted_graph.gexf")
{
  require(igraph)
  require(rgexf)
  
  # gexf nodes require two column data frame (id, label)
  # check if the input vertices has label already present
  # if not, just have the ids themselves as the label
    V(g)$label <- as.character(V(g)$name)
  
  # similarily if edges does not have weight, add default 1 weight
  if(is.null(E(g)$weight))
    E(g)$weight <- rep.int(1, ecount(g))
  
  nodes <- data.frame(cbind(V(g), V(g)$label))
  edges <- t(Vectorize(get.edge, vectorize.args='id')(g, 1:ecount(g)))
  
  # combine all node attributes into a matrix (and take care of & for xml)
  vAttrNames <- setdiff(list.vertex.attributes(g), "label") 
  nodesAtt <- data.frame(sapply(vAttrNames, function(attr) sub("&", "&",get.vertex.attribute(g, attr))))
  for (i in 2:9){
  nodesAtt[, i] <- as.numeric(as.character(nodesAtt[, i]))
  }
  
  # combine all edge attributes into a matrix (and take care of & for xml)
  eAttrNames <- setdiff(list.edge.attributes(g), "weight") 
  edgesAtt <- data.frame(sapply(eAttrNames, function(attr) sub("&", "&",get.edge.attribute(g, attr))))
  
  # combine all graph attributes into a meta-data
  graphAtt <- sapply(list.graph.attributes(g), function(attr) sub("&", "&",get.graph.attribute(g, attr)))
  
  # generate the gexf object
  output <- write.gexf(nodes, edges, 
                       edgesWeight=E(g)$weight,
                       edgesAtt = edgesAtt,
                       nodesAtt = nodesAtt,
                       meta=c(list(creator="Gopalakrishna Palem", description="igraph -> gexf converted file", keywords="igraph, gexf, R, rgexf"), graphAtt))
  
  print(output, filepath, replace=T)
}

saveAsGEXF_cp <- function(g, filepath="converted_graph.gexf")
{
  require(igraph)
  require(rgexf)
  
  # gexf nodes require two column data frame (id, label)
  # check if the input vertices has label already present
  # if not, just have the ids themselves as the label
  V(g)$label <- as.character(V(g)$name)
  
  # similarily if edges does not have weight, add default 1 weight
  if(is.null(E(g)$weight))
    E(g)$weight <- rep.int(1, ecount(g))
  
  nodes <- data.frame(cbind(V(g), V(g)$label))
  edges <- t(Vectorize(get.edge, vectorize.args='id')(g, 1:ecount(g)))
  
  # combine all node attributes into a matrix (and take care of & for xml)
  vAttrNames <- setdiff(list.vertex.attributes(g), "label") 
  nodesAtt <- data.frame(sapply(vAttrNames, function(attr) sub("&", "&",get.vertex.attribute(g, attr))))
  for (i in 2:3){
    nodesAtt[, i] <- as.numeric(as.character(nodesAtt[, i]))
  }
  
  # combine all edge attributes into a matrix (and take care of & for xml)
  eAttrNames <- setdiff(list.edge.attributes(g), "weight") 
  edgesAtt <- data.frame(sapply(eAttrNames, function(attr) sub("&", "&",get.edge.attribute(g, attr))))
  
  # combine all graph attributes into a meta-data
  graphAtt <- sapply(list.graph.attributes(g), function(attr) sub("&", "&",get.graph.attribute(g, attr)))
  
  # generate the gexf object
  output <- write.gexf(nodes, edges, 
                       edgesWeight=E(g)$weight,
                       edgesAtt = edgesAtt,
                       nodesAtt = nodesAtt,
                       meta=c(list(creator="Gopalakrishna Palem", description="igraph -> gexf converted file", keywords="igraph, gexf, R, rgexf"), graphAtt))
  
  print(output, filepath, replace=T)
}