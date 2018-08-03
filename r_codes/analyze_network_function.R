analyze_network <- function(graph_wei, # the weighted network to be analyzed
                            year_considered # the year of the network 
){ 
  require(igraph) # make sure igraph package is loaded
  results <- list() # create empty list for results to be stored
  
  # closeness -----
  closeness_centr <- closeness(graph_wei, weights = E(graph_wei)$betw_score)
  closeness_tnet <- as.data.frame(closeness_w(
    as.tnet(cbind(as_edgelist(graph_wei, names = FALSE), E(graph_wei)$Weight))))
  # sometimes one vertex has no closeness score and thus gets omitted. in this case adding names doesnt work, so solution:
  bugfix <- as.data.frame(c(1:length(V(graph_wei)$name)))
  colnames(bugfix) <- "id"
  closeness_tnet <- left_join(bugfix, closeness_tnet, by= c("id" = "node") )
  closeness_tnet[is.na(closeness_tnet)] <- 0
  closeness_tnet <- closeness_tnet[,2]
  closeness_tnet <- setNames(closeness_tnet, V(graph_wei)$name)



  # Global properties ----
  # print("Standard measures for the network")
  wei_density <- c(edge_density(graph_wei, loops=F))
  wei_reciprocity <- c(reciprocity(graph_wei))
  wei_glob_clust <- c(clustering_w(as.tnet(cbind(as_edgelist(graph_wei, names = FALSE), E(graph_wei)$Weight))))
  wei_local_clust <- c(transitivity(graph_wei, type="barrat"))
  wei_comp_rdn_netw <- igraph::edge_density(graph_wei)
  
  # print("Strength and Degree distribution for the network")
  degrees <- igraph::degree(graph_wei, mode = "all")
  deg.dist <- degree_distribution(graph_wei, cumulative=T, mode="all") 
  deg_mean <- c(mean(degrees))
  strengths <- igraph::strength(graph_wei, mode = "all")
  strength.dist <- degree_distribution(graph_wei, cumulative=T, mode="all") 
  wei_stren_mean <- c(mean(strengths))
  wei_stren_sd <- c(sd(strengths))
  wei_stren_kurt <- c(kurtosis(strengths))
  wei_stren_skew <- c(skewness(strengths))
  wei_ANN <- knn(graph_wei) # Assortativity (av neareast neighbor degree)
  # wei_assort_closeness <- assortativity_nominal(
  #   induced.subgraph(graph_wei, which(!is.na(V(graph_wei)$closeness))), 
  #   types=na.omit(V(graph_wei)$closeness)
  # ) # Assortativity based on closeness, nas in eci leads to ommited node
  wei_assort_eci <- assortativity_nominal(
    induced.subgraph(graph_wei, which(!is.na(V(graph_wei)$ECI_rank))), 
    types=na.omit(V(graph_wei)$ECI_rank)
  ) # Assortativity based on eci score, nas in eci leads to ommited node
  wei_assort_pop <- assortativity_nominal(
    induced.subgraph(graph_wei, which(!is.na(V(graph_wei)$population))), 
    types=na.omit(V(graph_wei)$population)
  )
  wei_assort_GDP_curr_prices <- assortativity_nominal(
    induced.subgraph(graph_wei, which(!is.na(V(graph_wei)$GDP_curr_prices))), 
    types=na.omit(V(graph_wei)$GDP_curr_prices/100000)
  )
  wei_assort_GDP_hour <- assortativity_nominal(
    induced.subgraph(graph_wei, which(!is.na(V(graph_wei)$GDP_hour))), 
    types=na.omit(V(graph_wei)$GDP_hour)
  )
  wei_assort_Wage_share <- assortativity_nominal(
    induced.subgraph(graph_wei, which(!is.na(V(graph_wei)$Wage_share))), 
    types=na.omit(V(graph_wei)$Wage_share)
  )

  wei_assort_stren <- assortativity(graph_wei, graph.strength(graph_wei)) # Assortativity based on strenght
 

   # Additional stuff?
  # bin_hs <- hub_score(baci_graph_bin, weights=NA)$vector
  # bin_as <- authority_score(baci_graph_bin, weights=NA)$vector
  # wei_hs <- hub_score(graph_wei, weights=NA)$vector
  # wei_as <- authority_score(graph_wei, weights=NA)$vector
  
  # print("Assemble global, single numbered measures")
  global_sn_wei <- data.frame(year=year_considered,
                              density = wei_density,
                              reciprocity = wei_reciprocity,
                              deg.mean = deg_mean,
                              dist.mean = wei_stren_mean,
                              dist.sd = wei_stren_sd,
                              dist.kurt = wei_stren_kurt,
                              dist.skew = wei_stren_skew,
                              global_clust = wei_glob_clust,
                              edge.density = wei_comp_rdn_netw,
                              assort_ECI = wei_assort_eci,
                              assort_pop = wei_assort_pop,
                              assort_GDP_cur = wei_assort_GDP_curr_prices,
                              assort_GDP_hour = wei_assort_GDP_hour,
                              assort_Wage_share = wei_assort_Wage_share,
                              assort_streng = wei_assort_stren)
  
  results[["global_characteristics"]] <- global_sn_wei
  

  # Correlations -----
  
  # print("Correlations among network characteristics")
  cor_deg_stren <- cor(degrees, strengths)
  # cor_clust_deg <- cor(bin_local_clust, degrees)
  cor_clust_stren <- cor(wei_local_clust, strengths)
  
  global_dis  <- data.frame(year=year_considered,
                            cor.degree_strength = cor_deg_stren,
                            # cor.clust_degree = cor_clust_deg,
                            cor.clust_stren = cor_clust_stren)
  results[["global_correlations"]] <- global_dis

  # print("Correlations between network and macro characteristics")
  # GDP
  # cor_deg_GDP <- cor(degrees, V(baci_graph_bin)$GDP, "pairwise.complete.obs")
  cor_stren_GDP <- cor(strengths, V(graph_wei)$GDP_curr_prices, "pairwise.complete.obs")
  # cor_clust_GDP_bin <- cor(bin_local_clust, V(baci_graph_bin)$GDP, "pairwise.complete.obs")
  cor_clust_GDP_wei <- cor(wei_local_clust, V(graph_wei)$GDP_curr_prices, "pairwise.complete.obs")

  # # Gini market
  # cor_deg_ginim_bin <- cor(degrees, V(baci_graph_bin)$gini.m, "pairwise.complete.obs")
  # cor_stren_ginim_wei <- cor(strengths, V(graph_wei)$gini.m, "pairwise.complete.obs")
  # cor_clust_ginim_bin <- cor(bin_local_clust, V(baci_graph_bin)$gini.m, "pairwise.complete.obs")
  # cor_clust_ginim_wei <- cor(wei_local_clust, V(graph_wei)$gini.m, "pairwise.complete.obs")
  # 
  # # Gini net
  # cor_deg_ginin_bin <- cor(degrees, V(baci_graph_bin)$gini.n, "pairwise.complete.obs")
  # cor_stren_ginin_wei <- cor(strengths, V(graph_wei)$gini.n, "pairwise.complete.obs")
  # cor_clust_ginin_bin <- cor(bin_local_clust, V(baci_graph_bin)$gini.n, "pairwise.complete.obs")
  # cor_clust_ginin_wei <- cor(wei_local_clust, V(graph_wei)$gini.n, "pairwise.complete.obs")
  # 
  # # Growth
  # cor_deg_growth_bin <- cor(degrees, V(baci_graph_bin)$growth, "pairwise.complete.obs")
  cor_stren_growth_wei <- cor(strengths, V(graph_wei)$GDP_growth, "pairwise.complete.obs")
  # cor_clust_growth_bin <- cor(bin_local_clust, V(baci_graph_bin)$growth, "pairwise.complete.obs")
  cor_clust_growth_wei <- cor(wei_local_clust, V(graph_wei)$GDP_growth, "pairwise.complete.obs")
  
  # wage_share
  cor_stren_wage_wei <- cor(strengths, V(graph_wei)$Wage_share, "pairwise.complete.obs")
  cor_clust_wage_wei <- cor(wei_local_clust, V(graph_wei)$Wage_share, "pairwise.complete.obs")
  
  # Productivity
  cor_stren_productivity_wei <- cor(strengths, V(graph_wei)$GDP_hour, "pairwise.complete.obs")
  cor_clust_productivity_wei <- cor(wei_local_clust, V(graph_wei)$GDP_hour, "pairwise.complete.obs")
  
  # print("Assemble correlations")
   correlation <- c(
  #   cor_deg_GDP,
    cor_stren_GDP,
  #   cor_clust_GDP_bin,
    cor_clust_GDP_wei,
  #   cor_deg_ginim_bin,
  #   cor_stren_ginim_wei,
  #   cor_clust_ginim_bin,
  #   cor_clust_ginim_wei,
  #   cor_deg_ginin_bin,
  #   cor_stren_ginin_wei,
  #   cor_clust_ginin_bin,
  #   cor_clust_ginin_wei,
  #   cor_deg_growth_bin,
    cor_stren_growth_wei,
  #   cor_clust_growth_bin,
    cor_clust_growth_wei,
  cor_stren_wage_wei,
  cor_clust_wage_wei,
  cor_stren_productivity_wei,
  cor_clust_productivity_wei
  )
  year_table <- rep(as.double(year_considered), length(correlation))
  network.var <- c("degree", "strengh", "cluster.bin", "cluster.wei")
  country.var <- c("GDP", "Gini.m", "Gini.n", "Growth")
  assembled.cors <- expand.grid(network.var,country.var)
  assembled.cors <- cbind(year_table, assembled.cors, correlation)
  results[["network.macro.cors"]] <- assembled.cors

  # print("Analyze betweeness centrality score")
  # RWBC.score.bin <- list()
  RWBC.score.wei <- list()
  rwbc_scores_wei <- igraph::betweenness(graph_wei, directed=T, weights=E(graph_wei)$betw_score, normalized=T)
  # rwbc_scores_bin <- igraph::betweenness(baci_graph_bin, directed=T)
  # top.5.perc.bin <- round(0.05 * length(rwbc_scores_bin)) # Top 5% are the core
  top.5.perc.wei <- round(0.05 * length(rwbc_scores_wei)) # Top 5% are the core
  # RWBC.score.bin[[as.character(year_considered)]] <- head(sort(rwbc_scores_bin, decreasing = TRUE), n = top.5.perc.bin)
  RWBC.score.wei[[as.character(year_considered)]] <- head(sort(rwbc_scores_wei, decreasing = TRUE), n = top.5.perc.wei)

  # print("Analyze eigenvector centrality score")
  eigenvec_centr_wei <- igraph::eigen_centrality(graph_wei, directed = TRUE)$vector
  # eigenvec_centr_bin <- igraph::eigen_centrality(baci_graph_bin, directed = TRUE)$vector
  # 
  
  # closeness centrality
 # moved to the top so it can be used for assort calc too

  
  # print("Local measures or distributions")
  dists.current.year <- list()
  # dists.current.year[["degrees"]] = degrees
  dists.current.year[["strengths"]] = strengths
  # dists.current.year[["local.clustering.bin"]] = bin_local_clust
  dists.current.year[["local.clustering.wei"]] = wei_local_clust
  # dists.current.year[["degree.dist"]] = deg.dist
  dists.current.year[["strength.dist"]] = strength.dist
  # dists.current.year[["RWBC_bin"]] = rwbc_scores_bin
  dists.current.year[["RWBC_wei"]] = rwbc_scores_wei
  dists.current.year[["wei_knn"]] <- wei_ANN$knn
  dists.current.year[["wei_knnk"]] <- wei_ANN$knnk
  # dists.current.year[["bin_knn"]] <- bin_ANN$knn
  # dists.current.year[["bin_knnk"]] <- bin_ANN$knnk
  # dists.current.year[["eigen.bin"]] <- eigenvec_centr_bin
  dists.current.year[["eigen.wei"]] <- eigenvec_centr_wei
  # dists.current.year[["RWBC.core.countries.bin"]] <- RWBC.score.bin
  dists.current.year[["RWBC.core.countries.wei"]] <- RWBC.score.wei
  dists.current.year[["closeness_igraph"]] <- closeness_centr
  dists.current.year[["closeness_tnet"]] <- closeness_tnet
  results[["local_measures"]] <- dists.current.year
  # local_measures[[as.character(year_considered)]] <- dists.current.year
  # print("Finished function 'analyze_network'")
 return(results)
} # End of function "analyze_network"


analyze_network_clust <- function(graph_clust, year_considered) {
  # global characteristics
  wei_density_clust <- c(edge_density(graph_clust, loops=F))
  wei_reciprocity_clust <- c(reciprocity(graph_clust))
  
  if (nrow(as_edgelist(graph_clust, names = FALSE)) > 2 ){
    wei_glob_clust <- c(clustering_w(as.tnet(cbind(as_edgelist(graph_clust, names = FALSE), E(graph_clust)$Weight))))
  } else {wei_glob_clust <- 0
    print("clust_w failed: less than 3 nodes")}
  
  wei_comp_rdn_netw_clust <- igraph::edge_density(graph_clust)
  strengths_clust <- igraph::strength(graph_clust, mode = "all", weights = E(graph_clust)$Weight) # x1995_strengths
  strengths_perhead <- strengths_clust / V(graph_clust)$population
  strength.dist_clust <- degree_distribution(graph_clust, cumulative=T, mode="all") 
  wei_stren_mean_clust <- c(mean(strengths_clust))
  wei_stren_ph_mean_clust <- c(mean(strengths_perhead, na.rm=TRUE))
  wei_stren_sd_clust <- c(sd(strengths_clust))
  wei_stren_kurt_clust <- c(kurtosis(strengths_clust))
  wei_stren_skew_clust <- c(skewness(strengths_clust))
  
  global_sn_wei_clust <- data.frame(year=year_considered,                    
                                      density = wei_density_clust,
                                      reciprocity = wei_reciprocity_clust, 
                                      dist.mean = wei_stren_mean_clust,
                                      dist.mean.ph = wei_stren_ph_mean_clust,
                                      dist.sd = wei_stren_sd_clust,
                                      dist.kurt = wei_stren_kurt_clust,
                                      dist.skew = wei_stren_skew_clust,
                                      global_clust = wei_glob_clust,
                                      edge.density = wei_comp_rdn_netw_clust)
  results <- global_sn_wei_clust
}