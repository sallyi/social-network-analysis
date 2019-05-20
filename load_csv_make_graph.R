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


get_centrality_measures <- function(metadata,rels,outname){
  ## loading the data
  df_meta <- read.csv(metadata)
  df_meta

  df <- read.csv(rels) 
  df <- data.frame(df$node1, df$node2) # remove index column
  ## creating a graph object
  g <- graph.data.frame(df, directed = T)
  g
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
  #todo split into two,  inappropriate and child-friendly, then average measures
  cat('\nmean indegree norm centrality:', mean(df.g[['indegree_norm']]))
  cat('\nmean outdegree norm centrality:', mean(df.g[['outdegree_norm']]))
  cat('\nmean indegree centrality:', mean(df.g[['indegree']]))
  cat('\nmean outdegree centrality:', mean(df.g[['outdegree']]))
  
  cat('\nmedian indegree norm centrality:', median(df.g[['indegree_norm']]))
  cat('\nmedian outdegree norm centrality:', median(df.g[['outdegree_norm']]))
  cat('\nmedian indegree centrality:', median(df.g[['indegree']]))
  cat('\nmedian outdegree centrality:', median(df.g[['outdegree']]))
  df.g

}

df_centrality <-get_centrality_measures('annotated_videonet_seeds_elsa_spiderman_2019_08_18_metadata.csv',
                          'videonet_seeds_elsa_spiderman_2019_08_18_relations.csv', 'bad_video_network')
df_centrality
df_inapprop_centrality <- df_centrality %>% filter(is_inappropriate == 'YES')# %>% select(abbrev1, abbrev2)
df_childfriendly_centrality <- df_centrality %>% filter(is_inappropriate == 'NO')# %>% select(abbrev1, abbrev2)
df_childfriendly_centrality
cat('inappropriate video mean indegree norm centrality:', mean(df_inapprop_centrality[['indegree_norm']]))
cat('inappropriate video mean outdegree norm centrality:', mean(df_inapprop_centrality[['outdegree_norm']]))
cat('inappropriate video mean indegree centrality:', mean(df_inapprop_centrality[['indegree']]))
cat('inappropriate video mean outdegree centrality:', mean(df_inapprop_centrality[['outdegree']]))
cat('child-friendly video mean indegree norm centrality:', mean(df_childfriendly_centrality[['indegree_norm']]))
cat('child-friendly video mean outdegree norm centrality:', mean(df_childfriendly_centrality[['outdegree_norm']]))
cat('child-friendly video mean indegree centrality:', mean(df_childfriendly_centrality[['indegree']]))
cat('child-friendly video mean outdegree centrality:', mean(df_childfriendly_centrality[['outdegree']]))

cat('inappropriate video median indegree norm centrality:', median(df_inapprop_centrality[['indegree_norm']]))
cat('inappropriate video median outdegree norm centrality:', median(df_inapprop_centrality[['outdegree_norm']]))
cat('inappropriate video median indegree centrality:', median(df_inapprop_centrality[['indegree']]))
cat('inappropriate video median outdegree centrality:', median(df_inapprop_centrality[['outdegree']]))
cat('child-friendly video median indegree norm centrality:', median(df_childfriendly_centrality[['indegree_norm']]))
cat('child-friendly video median outdegree norm centrality:', median(df_childfriendly_centrality[['outdegree_norm']]))
cat('child-friendly video median indegree centrality:', median(df_childfriendly_centrality[['indegree']]))
cat('child-friendly video median outdegree centrality:', median(df_childfriendly_centrality[['outdegree']]))

#           indegree, indegree_norm, outdegree, outdegree_norm
# agg mean
# cf mean
# inapprop mean

#                 indegree , indegree_norm, outdegree, outdegree_norm
# agg median
# cf median
# inapprop median


df_centrality <-get_centrality_measures('masha_and_shark_2019_05_14_metadata.csv','masha_and_shark_2019_05_14_relations.csv', 'good_video_network')

