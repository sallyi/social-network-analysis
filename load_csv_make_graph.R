######################################################
########Creating your first network###################
######################################################

### loading packages
library(igraph)
setwd("C:/Users/sally/Documents/Social Network Analysis/social-network-analysis/")

load_channel_graph <- function(metadata,rels){
  df_meta <- read.csv(metadata, encoding='UTF-8') 
  head(df_meta)
  df_rel <- read.csv(rels)
  head(df_rel)
  df_rel <- data.frame(df_rel$node1, df_rel$node2)
  df_rel
  g <- graph.data.frame(df_rel)
  g
  E(g) #information about edges
  V(g) #information vertices
  plot.igraph(g, edge.arrow.size = 0.2)
  V(g)$name
  V(g)$subscribers=as.integer(df_meta$subscriberCount[match(V(g)$name,df_meta$label)]) # This code 
  V(g)$size=V(g)$subscribers^(1/4) 
  V(g)$size[is.na(V(g)$size)] <- 0
  plot.igraph(g,edge.arrow.size = 0.1,layout=layout.fruchterman.reingold)
  g
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
  #plot.igraph(g, edge.arrow.size = 0.2)
  V(g)$name
  g$palette <- categorical_pal(length(df_meta$title_language))
  V(g)$language=df_meta$title_language[match(V(g)$name,df_meta$label)] # join to metadata 
  plot.igraph(g, vertex.color = df_meta$title_language, vertex.label=df_meta$title_language, edge.arrow.size= 0.1,layout=layout.fruchterman.reingold)
  g
  df_meta$title_language[match(V(g)$name,df_meta$label)] 
  g
}



g <-load_channel_graph('disney_channel_metadata.csv','disney_channel_relations.csv')
V(g)$size
g <-load_video_graph('videonet_baby_shark_2019_05_14_metadata.csv','videonet_baby_shark_2019_05_14_relations.csv')
g
V(g)$language

