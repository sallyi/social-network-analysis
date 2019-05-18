######################################################
########Creating your first network###################
######################################################

### loading packages
library(igraph)
setwd("/Users/sallyisa/Documents/school/social-network-analysis/")

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
              layout=layout.fruchterman.reingold
              )
  df_meta$title_language[match(V(g)$name,df_meta$label)] 
  df_meta
}



g <-load_channel_graph('disney_channel_metadata.csv','disney_channel_relations.csv')
V(g)$size
df_good <-load_video_graph('masha_and_shark_2019_05_14_metadata.csv','masha_and_shark_2019_05_14_relations.csv')

df_bad <-load_video_graph('annotated_videonet_seeds_elsa_spiderman_2019_08_18_metadata.csv',
                          'videonet_seeds_elsa_spiderman_2019_08_18_relations.csv')
colnames(df_bad)
V(df_bad)$size


### loading packages
library(igraph)

### Creating an edgelist
# defining nodes
V2 <- c("Bob", "John", "Lisa", "Andrew")
V1  <- c("Yev", "Yev", "Yev", "Yev")

#puting everything together in one a dataframe
df <- data.frame(V1, V2)
df

### formatting as a graph object
g <- graph.data.frame(df)
E(g) #information about edges
V(g) #information vertices

### plotting the network
plot.igraph(g)


### turning the graph object into an adjacency matrix
matrix <- as_adjacency_matrix(g)
matrix

### turning the matrix into a graph object
g1 <- graph_from_adjacency_matrix(matrix, mode = "undirected")

### plotting the graph object
plot.igraph(g1)


######################################################
########Creating your first network###################
######################################################

###Did the abovementioned individuals take courses at the departpment of Political Science at KU during their bachelor? 
V1 <- c("Bob", "John", "Yev")
V2 <- c("Bachelor", "Bachelor", "Bachelor")

### formatting as a dataframe edgelist
df.affil <- data.frame(V1, V2)
df.affil
 
### formatting as a graph object
g2 <- graph.data.frame(df.affil, directed = F)

##adding node attribute indicating the type of the node
V(g2)$type <- bipartite.mapping(g2)$type

## adding colors to show profile type (individual/event)
colors <- V(g2)$type

### Plotting bi-partite affiliation network
plot.igraph(g2, main="Bi-partite network", vertex.color = colors)

### Plotting one-mode affiliation network
plot.igraph(bipartite_projection(g2)$proj1,main="Affilitaton Network")

### Plotting one-mode affiliation-affiation network
plot.igraph(bipartite_projection(g2)$proj2,main="Affilitaton Network")



### Day 2 ###


### loading packages
library(dplyr)
library(igraph)

## loading the data
df <- read.csv("data/DIPCON_3.0_Dyads.csv") # data and documentation from: http://www.u.arizona.edu/~volgy/data.html

###
df1 <- df %>% filter(dipcon2010 > 0) %>% select(abbrev1, abbrev2)

## creating a graph object
g <- graph.data.frame(df1, directed = T)

### creating a data frame where columns represent variables and rows represent countires
df.g <- data.frame(country = V(g)$name,
                   indegree_norm = degree(g, mode = "in", normalized = T), #normalized indegree
                   indegree = degree(g, mode = "in", normalized = F )) # raw indegree

# ranking the countries by raw (non-normalized) indegree in descending order
df.g <- df.g %>% arrange(-indegree)

### examining the first 10 rows (top 10 countries ranked by (non-normalized) indegree)
head(df.g, 10)


### exporting th dataframe as a csv in your working directory
write.csv(df.g, "diplodata.txt")



