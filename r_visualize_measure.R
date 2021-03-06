rm(list=ls())

### loading packages
install.packages('RColorBrewer')
install.packages('corrplot')
install.packages('ggpubr')
library(corrplot)
if (!require("ForceAtlas2")) devtools::install_github("analyxcompany/ForceAtlas2")
library(RColorBrewer)
library(igraph)
library(dplyr)
library("ggplot2")
library("ForceAtlas2")
library("ggpubr")
setwd("/Users/sallyisa/Documents/school/social-network-analysis/")

#### basic visualizations of network structures, we ended up doing most of this in gephi #####

load_channel_graph <- function(metadata, rels, graph_name){
  df_meta <- read.csv(metadata, encoding='UTF-8') 
  head(df_meta)
  df_rel <- read.csv(rels)
  head(df_rel)
  df_rel <- data.frame(df_rel$node1, df_rel$node2)
  g <- graph.data.frame(df_rel)
  E(g) #information about edges
  V(g) #information vertices
  V(g)$name
  V(g)$subscribers=as.integer(df_meta$subscriberCount[match(V(g)$name,df_meta$label)]) # This code 
  V(g)$size=V(g)$subscribers^(1/5) 
  V(g)$size[is.na(V(g)$size)] <- 0
  V(g)$is_inappropriate=df_meta$is_inappropriate[match(V(g)$name,df_meta$label)] # join to metadata 
  coul = brewer.pal(2, "Set1")
  my_color=coul[as.numeric(as.factor(V(g)$is_inappropriate))]
  g$palette <- categorical_pal(length(unique(df_meta$is_inappropriate)))
  V(g)$color_code=df_meta$is_inappropriate[match(V(g)$name,df_meta$label)] # join to metadata 
  plot.igraph(g,
              main=graph_name,
              vertex.color = my_color,  
              edge.arrow.size = 0.1,layout=layout.forceatlas2(g, directed=TRUE, iterations = 100, 
                                                              linlog = FALSE, pos = NULL, nohubs = FALSE, 
                                                              k = 400, gravity=1, ks=0.1, ksmax=10, delta = 1,  
                                                              center=NULL, tolerance = 0.1, dim = 2,
                                                              plotstep=10, plotlabels=TRUE))
  legend("bottomleft", title='Is inappropriate:', title.col = 'black',
         legend=levels(as.factor(df_meta$is_inappropriate)), 
         pt.cex = 3, cex = 0.9,
         col=coul, fill=coul, text.col=coul)
  g
}


# visualize channels associated with inappropriate video network videos, did not use channel relationships
g <-load_channel_graph('annotated_channels_gdf_metadata.csv','annotated_channels_gdf_relations.csv', 'Annotated Inappropriate Channel Graph')


load_video_graph <- function(metadata,rels, graph_name){
  df_meta <- read.csv(metadata, encoding='UTF-8') 
  head(df_meta)
  df_rel <- read.csv(rels,encoding='UTF-8')
  #head(df_rel)
  df_rel <- data.frame(df_rel$node1, df_rel$node2)
  df_rel
  g <- graph.data.frame(df_rel)
  g
  E(g) #information about edges
  V(g) #information vertices
  # add node sizing - view count
  V(g)$views=as.integer(df_meta$viewCount.INT[match(V(g)$name,df_meta$label)]) # This code 
  # 
  V(g)$size=V(g)$views**(1/6) 
  V(g)$size[is.na(V(g)$size)] <- 0
  # add node coloring - inappropriate (yes or no)
  if("is_inappropriate" %in% colnames(df_meta))
  {
    g$palette <- categorical_pal(length(unique(df_meta$is_inappropriate)))
    V(g)$color_code=df_meta$is_inappropriate[match(V(g)$name,df_meta$label)] # join to metadata 
    plot.igraph(g,
                main=graph_name,
                vertex.color = df_meta$is_inappropriate, 
                vertex.label=df_meta$title_language, 
                edge.arrow.size= 0.1,
                layout=layout.fruchterman.reingold(g)
    )
    legend("topleft",legend=unique(df_meta$is_inappropriate),fill=g$palette)
  }
  else{
    plot.igraph(g,
                main=graph_name,
                edge.arrow.size= 0.5,
                vertex.color = df_meta$title_language, 
                vertex.label=df_meta$title_language, 
                layout=layout.forceatlas2(g, directed=TRUE, iterations = 100, 
                                          linlog = FALSE, pos = NULL, nohubs = FALSE, 
                                          k = 400, gravity=1, ks=0.1, ksmax=10, delta = 1,  
                                          center=NULL, tolerance = 0.1, dim = 2,
                                          plotstep=10, plotlabels=TRUE))
    
  }
  df_meta$title_language[match(V(g)$name,df_meta$label)] 
  g
}


# visualize graph with videos labelled by language, sized by view count, did not use
g_good <-load_video_graph('masha_and_shark_2019_05_14_metadata.csv','masha_and_shark_2019_05_14_relations.csv', 'Child-Friendly Video Network')

g_bad <-load_video_graph('annotated_videonet_seeds_elsa_spiderman_2019_08_18_metadata.csv',
                         'videonet_seeds_elsa_spiderman_2019_08_18_relations.csv',
                         'Inappropriate Video Network')


visualize_graphs <- function(metadata,rels, graph_name){
  df_meta <- read.csv(metadata, encoding='UTF-8') 
  df_rel <- read.csv(rels,encoding='UTF-8')
  df_rel <- data.frame(df_rel$node1, df_rel$node2)
  g <- graph.data.frame(df_rel)
  
  # size nodes according to indegree
  df.g <- data.frame(video = V(g)$name,
                     indegree_norm = degree(g, mode = "in", normalized = F))
  V(g)$indegree=df.g$indegree_norm[match(V(g)$name,df.g$video)]
  V(g)$size=log2(V(g)$indegree)
  V(g)$size[is.na(V(g)$size)] <- 0
  
  # add node coloring - channelid
  g$palette <- categorical_pal(length(unique(df_meta$channelId.VARCHAR)))
  V(g)$color_code <- df_meta$channelId.VARCHAR[match(V(g)$name,df_meta$label)]
  V(g)$color_code[is.na(V(g)$color_code)] <- 0
  
  plot.igraph(g,
              main=graph_name,
              vertex.color = V(g)$color_code, 
              edge.arrow.size= 0.2,
              vertex.label="",
              layout=layout.fruchterman.reingold(g)
  )
  g
}

g_good <-visualize_graphs('masha_and_shark_2019_05_14_metadata.csv','masha_and_shark_2019_05_14_relations.csv', 'Child-Friendly Video Network')

g_bad <-visualize_graphs('annotated_videonet_seeds_elsa_spiderman_2019_08_18_metadata.csv',
                         'videonet_seeds_elsa_spiderman_2019_08_18_relations.csv',
                         'Inappropriate Video Network')


#### node level and network measurement functions #####

construct_degree_results_df <- function(){
  # construct results df
  N <- 6 # total number of rows to preallocate
  data.frame(data_subset=rep("", N), measure=rep("", N),
             degree=rep(NA, N),
             indegree=rep(NA, N), 
             outdegree=rep(NA, N),
             degree_norm=rep(NA, N),
             indegree_norm=rep(NA, N), 
             outdegree_norm=rep(NA, N), 
             betweenness=rep(NA, N),
             betweenness_norm=rep(NA, N),
             closeness_norm=rep(NA,N), 
             coreness=rep(NA,N),
             stringsAsFactors=FALSE)
}


get_groups <- function(graph, df_meta){
  graph$title_language <- df_meta$title_language[match(V(graph)$name,df_meta$label)] 
  graph$is_inappropriate <- df_meta$is_inappropriate[match(V(graph)$name,df_meta$label)] 
  
  modularity <- cluster_infomap(graph)
  
  plot.igraph(graph,
              main="Cluster Infomaps: Inappropriate Network",
              vertex.size = 8,
              vertex.label= graph$is_inappropriate,
              vertex.label.color = "black",
              vertex.label.cex = 1,
              vertex.color = modularity$membership,
              edge.arrow.size = 0.1,
              layout=layout.fruchterman.reingold(graph)
  )
  
}

visualize_ggplot <- function(df.g, outname){
  # to visualize one measure/factor vs. another... did not end up using this.
  df.g$deg_group[df.g$is_inappropriate == 'YES'] <- "Is inappropriate"
  df.g$deg_group[df.g$is_inappropriate == 'NO'] <- "Is child-friendly"
  ggplot(df.g, aes(indegree_norm, outdegree_norm, size=closeness_norm, colour=deg_group)) + geom_point() + labs(x ="Normalized In-degree", y ="Normalized Out-degree")
  ggsave(paste(outname, "_norm_indegree_norm_outdegree.png"), plot =last_plot(),width =25, height =25, units ="cm",dpi =1000)
  ggplot(df.g, aes(indegree, outdegree),colour=deg_group) + geom_point() + labs(x ="In-degree", y ="Out-degree")
  ggsave(paste(outname, "_indegree_outdegree.png"), plot =last_plot(),width =25, height =25, units ="cm",dpi =1000)
  ggplot(df.g, aes(degree, closeness_norm),colour=deg_group) + geom_point() + labs(x ="degree", y ="closeness")
  ggsave(paste(outname, "_degree_closeness.png"), plot = last_plot(),width =25, height =25, units ="cm",dpi =1000)
}

get_correlation_matrix <- function(df.g, cf, norm, title){
  # get correlations
  df.g$view_count[is.na(df.g$view_count)] <- 0
  if (cf == FALSE){ # if network is inappropriate, you have to turn that col into a numeric value 
    df.g$is_inappropriate_num <- !as.integer(as.character(df.g$is_inappropriate)=="YES")
    df.g$is_inappropriate_num <- !as.integer(as.character(df.g$is_inappropriate)=="NO")
    df.g$is_inappropriate_num[is.na(df.g$is_inappropriate_num)] <- 0
    if (norm == TRUE){
      M <-cor(df.g[, c(2,3,4,9,10,13,14)])
    }
    else{
      M <-cor(df.g[, c(5,6,7,8,13,14)])
    }
  }
  else{
    # get correlations
    if (norm == TRUE){
      M <-cor(df.g[, c(2,3,4,9,12)])
    }
    else{
      M <-cor(df.g[, c(5,6,7,8,12)])
    }
  }
  corrplot(M, main=title, type="upper", order="hclust",
           col=brewer.pal(n=8, name="RdYlBu"), sig.level = 0.01)
  df.g
}


# todo, add viewcount to node level output
get_measures <- function(metadata, rels, outname, k){
  cat('Running measures for', outname, '...')
  ## loading the data
  df_meta <- read.csv(metadata)
  
  # get summary statistics of metadata
  outfile <- paste(outname, "statistics.txt")
  sink(file=outfile)
  print(summary(df_meta))
  sink()
  
  # load relations
  df <- read.csv(rels) 
  df <- data.frame(df$node1, df$node2) # remove index column
  ## creating a graph object
  g <- graph.data.frame(df, directed = T)
  g <- simplify(g, remove.multiple = T, remove.loops = T) 
  ### creating a data frame where columns represent variables and rows represent videos
  df.g <- data.frame(video = V(g)$name,
                     degree_norm = degree(g, mode = "all", normalized = T), #normalize degree
                     indegree_norm = degree(g, mode = "in", normalized = T), #normalized indegree
                     outdegree_norm = degree(g, mode = "out", normalized = T ), # normalized outdegree
                     degree = degree(g, mode = "all", normalized = F), # raw degree
                     indegree = degree(g, mode = "in", normalized = F ), # raw indegree
                     outdegree = degree(g, mode = "out", normalized = F ), # raw outdegree
                     betweenness = betweenness(g,  directed = T, normalized = F), # normalized betweenness
                     betweenness_norm = betweenness(g,  directed = T, normalized = T), # normalized betweenness
                     closeness_norm = closeness(g, mode = "all", normalized = T), # normalized closeness
                     coreness = coreness(g)
  ) 
  df.g$is_inappropriate = df_meta$is_inappropriate[match(df.g$video,df_meta$label)] # join graph to metadata: is_inappropriate
  df.g$view_count = df_meta$viewCount.INT[match(df.g$video,df_meta$label)] # join graph to metadata: number of views
  # ranking the videos by raw (non-normalized) indegree in descending order
  df.g <- df.g %>% arrange(-indegree)
  ### exporting the dataframe as a csv in your working directory
  # construct results dfs
  out_measures <- construct_degree_results_df()
  out_measures[1, ] <- list("aggregate", "mean", 
                            mean(df.g[['degree']]), 
                            mean(df.g[['indegree']]), 
                            mean(df.g[['outdegree']]), 
                            mean(df.g[['degree_norm']]), 
                            mean(df.g[['indegree_norm']]),
                            mean(df.g[['outdegree_norm']]),
                            mean(df.g[['betweenness']], na.rm=TRUE),
                            mean(df.g[['betweenness_norm']], na.rm=TRUE),
                            mean(df.g[['closeness_norm']]),
                            mean(df.g[['coreness']]))
  out_measures[2, ] <- list("aggregate", "median",
                            median(df.g[['degree']]), 
                            median(df.g[['indegree']]), 
                            median(df.g[['outdegree']]), 
                            median(df.g[['degree_norm']]), 
                            median(df.g[['indegree_norm']]),
                            median(df.g[['outdegree_norm']]),
                            median(df.g[['betweenness']], na.rm=TRUE),
                            median(df.g[['betweenness_norm']], na.rm=TRUE),
                            median(df.g[['closeness_norm']]),
                            median(df.g[['coreness']]))
  
  # print network measures
  cat('\nfull network measures:')
  cat('\nnetwork density:', graph.density(g))
  cat('\nnetwork degree centralization', centr_degree(g, normalized = T)$centralization)
  cat('\nnetwork in-degree centralization', centr_degree(g, normalized = T, mode='in')$centralization)
  cat('\nnetwork out-degree centralization', centr_degree(g, normalized = T, mode='out')$centralization)
  cat('\nnetwork betweenness centralization (normalized)', centr_betw(g, normalized = T)$centralization)
  cat('\nnetwork betweenness centralization', centr_betw(g, normalized = F)$centralization)
  cat('\nnetwork closeness centralization', centr_clo(g, normalized = T)$centralization)
  if("is_inappropriate" %in% colnames(df.g))
  {
    # separate inappropriate and child-friendly videos
    df_inapprop_centrality <- df.g %>% filter(is_inappropriate == 'YES')# %>% select(abbrev1, abbrev2)
    df_childfriendly_centrality <- df.g %>% filter(is_inappropriate == 'NO')# %>% select(abbrev1, abbrev2)
    # centrality measure to dataframe
    out_measures[3, ] <- list("child friendly videos", "mean", 
                              mean(df_childfriendly_centrality[['degree']]), 
                              mean(df_childfriendly_centrality[['indegree']]), 
                              mean(df_childfriendly_centrality[['outdegree']]), 
                              mean(df_childfriendly_centrality[['degree_norm']]), 
                              mean(df_childfriendly_centrality[['indegree_norm']]),
                              mean(df_childfriendly_centrality[['outdegree_norm']]),
                              mean(df_childfriendly_centrality[['betweenness']], na.rm=TRUE),
                              mean(df_childfriendly_centrality[['betweenness_norm']], na.rm=TRUE),
                              mean(df_childfriendly_centrality[['closeness_norm']]),
                              mean(df_childfriendly_centrality[['coreness']])
    )
    out_measures[4, ] <- list("child friendly videos", "median", 
                              median(df_childfriendly_centrality[['degree']]), 
                              median(df_childfriendly_centrality[['indegree']]), 
                              median(df_childfriendly_centrality[['outdegree']]), 
                              median(df_childfriendly_centrality[['degree_norm']]), 
                              median(df_childfriendly_centrality[['indegree_norm']]),
                              median(df_childfriendly_centrality[['outdegree_norm']]),
                              median(df_childfriendly_centrality[['betweenness']], na.rm=TRUE),
                              median(df_childfriendly_centrality[['betweenness_norm']], na.rm=TRUE),
                              median(df_childfriendly_centrality[['closeness_norm']]),
                              median(df_childfriendly_centrality[['coreness']])
    )
    out_measures[5, ] <- list("inappropriate videos", "mean", 
                              mean(df_inapprop_centrality[['degree']]), 
                              mean(df_inapprop_centrality[['indegree']]), 
                              mean(df_inapprop_centrality[['outdegree']]), 
                              mean(df_inapprop_centrality[['degree_norm']]), 
                              mean(df_inapprop_centrality[['indegree_norm']]),
                              mean(df_inapprop_centrality[['outdegree_norm']]),
                              mean(df_inapprop_centrality[['betweenness']], na.rm=TRUE),
                              mean(df_inapprop_centrality[['betweenness_norm']], na.rm=TRUE),
                              mean(df_inapprop_centrality[['closeness_norm']]),
                              mean(df_inapprop_centrality[['coreness']])
    )
    out_measures[6, ] <- list("inappropriate videos", "median", 
                              median(df_inapprop_centrality[['degree']]), 
                              median(df_inapprop_centrality[['indegree']]), 
                              median(df_inapprop_centrality[['outdegree']]), 
                              median(df_inapprop_centrality[['degree_norm']]), 
                              median(df_inapprop_centrality[['indegree_norm']]),
                              median(df_inapprop_centrality[['outdegree_norm']]),
                              median(df_inapprop_centrality[['betweenness']], na.rm=TRUE),
                              median(df_inapprop_centrality[['betweenness_norm']], na.rm=TRUE),
                              median(df_inapprop_centrality[['closeness_norm']]),
                              median(df_inapprop_centrality[['coreness']])
    )
    # visualize centrality measures in ggplot
    visualize_ggplot(df.g, outname)
    get_groups(g, df_meta)
  }
  else{
    #visualize_ggplot(df.g, outname)
  }
  cat('\nIn/out degree measures:\n')
  out_measures <- na.omit(out_measures) # return measure, drop null rows 
  print(out_measures, row.names = FALSE)
  # k_cores = get_kcores(g, k, outname)
  df.g
}

data_bad_measures <-get_measures('annotated_videonet_seeds_elsa_spiderman_2019_08_18_metadata.csv',
                                 'videonet_seeds_elsa_spiderman_2019_08_18_relations.csv', 
                                 'inappropriate_video_net',
                                 k=4) #todo, figure out k
df_bad <- get_correlation_matrix(data_bad_measures, cf=FALSE, norm=TRUE, 'Inappropriate Network')
df_bad <- get_correlation_matrix(data_bad_measures, cf=FALSE, norm=FALSE, 'Inappropriate Network')

data_bad_measures <- data_bad_measures %>% arrange(-betweenness_norm)  # sort by column name
write.csv(data_bad_measures, 'video_node_measures-inappropriate_network.csv')

data_good_measures <-get_measures('masha_and_shark_2019_05_14_metadata.csv',
                                  'masha_and_shark_2019_05_14_relations.csv', 
                                  'child_friendly_video_net',
                                  k=30)
get_correlation_matrix(data_good_measures, cf=TRUE, norm=TRUE, 'Child-Friendly Network')
df_good <- get_correlation_matrix(data_good_measures, cf=TRUE, norm=FALSE, 'Child-Friendly Network')

write.csv(data_good_measures, 'video_node_measures-child_friendly_network.csv')

# ggplot(data_good_measures, aes(indegree_norm, outdegree)) + geom_point() + labs(x ="Normalized In-degree", y ="Normalized Out-degree")
# ggsave(paste(outname, "_norm_indegree_norm_outdegree.png"), plot =last_plot(),width =25, height =25, units ="cm",dpi =1000)
