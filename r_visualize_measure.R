######################################################
########Creating your first network###################
######################################################

### loading packages
library(igraph)
library(dplyr)
install.packages("devtools")
if (!require("ForceAtlas2")) devtools::install_github("analyxcompany/ForceAtlas2")
library("ForceAtlas2")
setwd("/Users/sallyisa/Documents/school/social-network-analysis/")


load_channel_graph <- function(metadata,rels){
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
  g$palette <- categorical_pal(length(unique(df_meta$is_inappropriate)))
  V(g)$color_code=df_meta$is_inappropriate[match(V(g)$name,df_meta$label)] # join to metadata 
  plot.igraph(g,
              vertex.color = df_meta$is_inappropriate, 
              edge.arrow.size = 0.1,layout=layout.forceatlas2(g, directed=TRUE, iterations = 100, 
                                                              linlog = FALSE, pos = NULL, nohubs = FALSE, 
                                                              k = 400, gravity=1, ks=0.1, ksmax=10, delta = 1,  
                                                              center=NULL, tolerance = 0.1, dim = 2,
                                                              plotstep=10, plotlabels=TRUE))
  legend("topleft",legend=unique(df_meta$is_inappropriate),fill=g$palette)
  df_rel
  }


load_video_graph <- function(metadata,rels){
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
  V(g)$size=V(g)$views^(1/6) 
  V(g)$size[is.na(V(g)$size)] <- 0
  # add node coloring - inappropriate (yes or no)
  g$palette <- categorical_pal(length(unique(df_meta$is_inappropriate)))
  V(g)$color_code=df_meta$is_inappropriate[match(V(g)$name,df_meta$label)] # join to metadata 
  plot.igraph(g, vertex.color = df_meta$is_inappropriate, 
              vertex.label=df_meta$is_inappropriate, 
              edge.arrow.size= 0.1,
              layout=layout.forceatlas2(g, directed=TRUE, iterations = 100, 
                                        linlog = FALSE, pos = NULL, nohubs = FALSE, 
                                        k = 400, gravity=1, ks=0.1, ksmax=10, delta = 1,  
                                        center=NULL, tolerance = 0.1, dim = 2,
                                        plotstep=10, plotlabels=TRUE)
              )
  df_meta$title_language[match(V(g)$name,df_meta$label)] 
  g
}




g <-load_channel_graph('annotated_channels_gdf_metadata.csv','annotated_channels_gdf_relations.csv')
g
V(g)$size
df_good <-load_video_graph('masha_and_shark_2019_05_14_metadata.csv','masha_and_shark_2019_05_14_relations.csv')

df_bad <-load_video_graph('annotated_videonet_seeds_elsa_spiderman_2019_08_18_metadata.csv',
                          'videonet_seeds_elsa_spiderman_2019_08_18_relations.csv')
colnames(df_bad)
V(df_bad)$size
unique(df_bad$channelId.VARCHAR)

construct_degree_results_df <- function(){
  # construct results df
  N <- 6 # total number of rows to preallocate
  data.frame(data_subset=rep("", N), measure=rep("", N), indegree=rep(NA, N), outdegree=rep(NA, N),  # as many cols as you need
                          indegree_norm=rep(NA, N), outdegree_norm=rep(NA, N), stringsAsFactors=FALSE)
  }


get_cliques <- function(graph, df_meta){
  graph$title_language <- df_meta$title_language[match(V(graph)$name,df_meta$label)] 
  ### plotting the graph 
  plot(graph,
       vertex.size = 20,
       vertex.label=graph$title_language,
       vertex.label.color = "black",
       vertex.label.cex = 1,
       vertex.color = "white")
  
  ### identifying cliques
  cliques <- cliques(graph, min =5) #mininum clique size = 5 nodes
  
  ## how many cliques are there?
  length(cliques)
  
  ### examining clique members
  cliques
  
  ### clique size
  sapply(cliques(graph, min = 5), length) 
  
  ###  identifying the largest clique
  largest_cliques(graph) 
  
  ### adding grey collor to all of the nodes
  vcol <- rep("grey80", vcount(graph))
  
  ## adding orange colour to the largest clique
  vcol[unlist(largest_cliques(graph))] <- "orange"
  
  ### vizualising who is in the largest clique (orange) and who is not (grey)
  plot.igraph(graph,
       vertex.size = 20,
       vertex.label= graph$title_language,
       vertex.label.color = "black",
       vertex.label.cex = 1,
       vertex.color = vcol,
       )
  
  # for more info om vizualisation, see this blog http://kateto.net/networks-r-igraph
  
  ### to compute n-cliques and n-clans see the stack oveflow thread: https://stackoverflow.com/questions/40088150/find-n-cliques-in-igraph
  
}

get_measures <- function(metadata,rels,outname){
  cat('Running measures for', outname, '...')
  ## loading the data
  df_meta <- read.csv(metadata)
  df <- read.csv(rels) 
  df <- data.frame(df$node1, df$node2) # remove index column
  ## creating a graph object
  g <- graph.data.frame(df, directed = T)
  g <- simplify(g, remove.multiple = T, remove.loops = T) # use this function to remove multiples or self-ties 
  ### creating a data frame where columns represent variables and rows represent videos
  df.g <- data.frame(video = V(g)$name,
                     indegree_norm = degree(g, mode = "in", normalized = T), #normalized indegree
                     outdegree_norm = degree(g, mode = "out", normalized = T ), # normalized outdegree
                     indegree = degree(g, mode = "in", normalized = F ), # raw indegree
                     outdegree = degree(g, mode = "out", normalized = F ) # raw outdegree
                     ) 
  df.g$is_inappropriate = df_meta$is_inappropriate[match(df.g$video,df_meta$label)] # join to metadata 
  # ranking the videos by raw (non-normalized) indegree in descending order
  df.g <- df.g %>% arrange(-indegree)
  ### exporting the dataframe as a csv in your working directory
  write.csv(df.g, paste(outname, "_centrality.csv"))
  # construct results dfs
  out_measures <- construct_degree_results_df()
  out_measures[1, ] <- list("aggregate", "mean", mean(df.g[['indegree']]), 
                mean(df.g[['outdegree']]), 
                mean(df.g[['indegree_norm']]),
                mean(df.g[['outdegree_norm']]))
  out_measures[2, ] <- list("aggregate", "median", median(df.g[['indegree']]), 
                            median(df.g[['outdegree']]), 
                            median(df.g[['indegree_norm']]),
                            median(df.g[['outdegree_norm']]))
  
  # print network measures
  cat('\nfull network measures:')
  cat('network density:', graph.density(g))
  cat('\nnetwork degree centralization', centr_degree(g, normalized = T)$centralization)
  cat('\nnetwork betweenness centralization', centr_betw(g, normalized = T)$centralization)
  cat('\nnetwork closeness centralization', centr_clo(g, normalized = T)$centralization)
  get_cliques(g, df_meta)
  if("is_inappropriate" %in% colnames(df.g))
  {
  # separate inappropriate and child-friendly videos
  df_inapprop_centrality <- df.g %>% filter(is_inappropriate == 'YES')# %>% select(abbrev1, abbrev2)
  df_childfriendly_centrality <- df.g %>% filter(is_inappropriate == 'NO')# %>% select(abbrev1, abbrev2)
  # centrality measure to dataframe
  out_measures[3, ] <- list("child friendly videos", "mean", mean(df_childfriendly_centrality[['indegree']]), 
                            mean(df_childfriendly_centrality[['outdegree']]), 
                            mean(df_childfriendly_centrality[['indegree_norm']]),
                            mean(df_childfriendly_centrality[['outdegree_norm']]))
  out_measures[4, ] <- list("child friendly videos", "median", median(df_childfriendly_centrality[['indegree']]), 
                            median(df_childfriendly_centrality[['outdegree']]), 
                            median(df_childfriendly_centrality[['indegree_norm']]),
                            median(df_childfriendly_centrality[['outdegree_norm']]))
  out_measures[5, ] <- list("inappropriate videos", "mean", mean(df_inapprop_centrality[['indegree']]), 
                            mean(df_inapprop_centrality[['outdegree']]), 
                            mean(df_inapprop_centrality[['indegree_norm']]),
                            mean(df_inapprop_centrality[['outdegree_norm']]))
  out_measures[6, ] <- list("inappropriate videos", "median", median(df_inapprop_centrality[['indegree']]), 
                            median(df_inapprop_centrality[['outdegree']]), 
                            median(df_inapprop_centrality[['indegree_norm']]),
                            median(df_inapprop_centrality[['outdegree_norm']]))
  
  }
  cat('\nIn/out degree measures:\n')
  out_measures <- na.omit(out_measures) # return measure, drop null rows 
  print(out_measures, row.names = FALSE)
  df.g
}

data_bad_in_out_degree <-get_measures('annotated_videonet_seeds_elsa_spiderman_2019_08_18_metadata.csv',
                          'videonet_seeds_elsa_spiderman_2019_08_18_relations.csv', 'bad_video_network')
data_bad_in_out_degree <- data_bad_in_out_degree %>% arrange(-outdegree)  # sort by column name
data_bad_in_out_degree


data_good_in_out_degree <-get_measures('masha_and_shark_2019_05_14_metadata.csv','masha_and_shark_2019_05_14_relations.csv', 'good_video_network')
data_good_in_out_degree
