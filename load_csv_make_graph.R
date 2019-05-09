######################################################
########Creating your first network###################
######################################################

### loading packages
library(igraph)
setwd("C:/Users/sally/Documents/Social Network Analysis/social-network-analysis/")

load_graph <- function(metadata,rels){
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
  #V(net)$Sex=as.character(a$Sex[match(V(net)$name,a$Bird.ID)]) # This code 
  # says to create a vertex attribute called "Sex" by extracting the 
  #value of the column "Sex" in the attributes file when the Bird ID number matches the vertex name
  
}

g <-load_graph('disney_channel_metadata.csv','disney_channel_relations.csv')

g <-load_graph('disney_video_metadata.csv','disney_video_relations.csv')
g

load_graph_scale_nodes <- function(metadata,rels){
  df_meta <- read.csv(metadata, encoding='UTF-8') 
  head(df_meta)
  df_rel <- read.csv(rels)
  head(df_rel)
  df_rel <- data.frame(df_rel$node1, df_rel$node2)
  g <- graph.data.frame(df_rel)
  g
  E(g) #information about edges
  V(g) #information vertices
  plot.igraph(g, edge.arrow.size = 0.2)
  g
}

?graph.data.frame

library(igraph)
#import the sample_adjmatrix file:
dat=read.csv(file.choose(),header=TRUE,row.names=1,check.names=FALSE)
m=as.matrix(dat)
net=graph.adjacency(m,mode="undirected",weighted=NULL,diag=FALSE)
# show the names of the vertices you just imported:
V(net)$name

#the result will be:
#[1] "23732" "23778" "23824" "23871" "58009" "58098" "58256"
'''
### Creating an edgelist
# defining nodes
V1 <- c("Sally","Sally","Sally", "Sally","Sally")
V2  <- c("Julia", "Matilde", "Julia", "Patricija", "Mette")

#puting everything together in one a dataframe
df <- data.frame(V1, V2)
df

### formatting as a graph object
g_test <- graph.data.frame(df)
g_test
E(g_test) #information about edges
V(g_test) #information vertices
?E
### plotting the network
plot.igraph(g_test, edge.arrow.size = 0.7)


### turning the graph object into an adjacency matrix
matrix <- as_adjacency_matrix(g)
matrix

### turning the matrix into an undirected graph object
g1 <- graph_from_adjacency_matrix(matrix, mode = "undirected")

### plotting the graph object
plot.igraph(g1)


######################################################
########Creating your first network###################
######################################################

###Did the abovementioned individuals take courses at the departpment of Political Science at KU during their bachelor? 
V1 <- c("Bob", "John", "Yev", "Sally")
V2 <- c("Bachelor", "Bachelor", "Bachelor", "Bachelor")

### formatting as a dataframe edgelist
df.affil <- data.frame(V1, V2)
df.affil

### formatting as a graph object
g2 <- graph.data.frame(df.affil, directed = F)
g2
##adding node attribute indicating the type of the node
V(g2)$type <- bipartite.mapping(g2)$type # select the vertices, create variable $type
?bipartite.mapping
V(g2)$type

## adding colors to show profile type (individual/event)
colors <- V(g2)$type
colors
### Plotting bi-partite affiliation network
plot.igraph(g2, main="Bi-partite network", vertex.color = colors)

### Plotting one-mode affiliation network
plot.igraph(bipartite_projection(g2)$proj1,main="Affilitaton Network")

### Plotting one-mode affiliation-affiliation network
plot.igraph(bipartite_projection(g2)$proj2,main="Affilitaton Network")
'''
