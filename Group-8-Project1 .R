#loaded the library
library("sna")
library("igraph")
getwd()
setwd("C:/Users/ADMIN/AppData/Local/Temp/Rtmp6Thuy1/downloaded_packages")
getwd()
opinions<-read.table("soc-Epinions1_adj.tsv")
optab<-as.matrix(opinions)
v1<-optab[,1]
v2<-optab[,2]
v3<-optab[,3]
relations<-data.frame(from=v1,to=v2)
#plotted the graph
g<-graph_from_data_frame(relations,directed=TRUE)
plot(g,vertex.size=5,edge.arrow.size=0.1,vertex.label=NA)


first_100_vertices <- graph_from_data_frame(relations[1:100,])
plot(first_100_vertices,vertex.size=5,edge.arrow.size=.4,vertex.label=NA)

f_200<-relations[1:200,]
first_200_vertices <- graph_from_data_frame(relations[1:200,])
plot(first_200_vertices,vertex.size=4,edge.arrow.size=.4,vertex.label=NA)

first_500_vertices <- graph_from_data_frame(relations[1:500,])
plot(first_500_vertices,vertex.size=5,edge.arrow.size=.4,vertex.label=NA)



first_10000_vertices <- graph_from_data_frame(relations[1:10000,])
plot(first_10000_vertices,vertex.size=5,edge.arrow.size=.4,vertex.label=NA)

first_20000_vertices <- graph_from_data_frame(relations[1:20000,])
plot(first_20000_vertices,vertex.size=5,edge.arrow.size=.4,vertex.label=NA ,layout=layout.fruchterman.reingold)



layout <- layout_with_fr(first_100000_vertices, niter=800, area=25, repulserad=1.5)
first_100000_vertices <- graph_from_data_frame(relations[1:100000,])
plot(first_100000_vertices,vertex.size=5,edge.arrow.size=.4,vertex.label=NA,layout=layout)


last_100_g <- graph_from_data_frame(relations[(nrow(relations) - 99):nrow(relations), ])
plot(last_200_g,vertex.size=5,edge.arrow.size=.4,vertex.label=NA)

last_200_g <- graph_from_data_frame(relations[(nrow(relations) - 199):nrow(relations), ])
plot(last_200_g,vertex.size=5,edge.arrow.size=.4,vertex.label=NA)

last_500_g <- graph_from_data_frame(relations[(nrow(relations) -499):nrow(relations), ])
plot(last_500_g,vertex.size=5,edge.arrow.size=.4,vertex.label=NA)

last_1000_g <- graph_from_data_frame(relations[(nrow(relations) -999):nrow(relations), ])
plot(last_1000_g,vertex.size=5,edge.arrow.size=.4,vertex.label=NA)

last_10000_g <- graph_from_data_frame(relations[(nrow(relations) -9999):nrow(relations), ])
layout <- layout_with_fr(last_10000_g, niter=800, area=25, repulserad=1.5)
plot(last_10000_g,vertex.size=5,edge.arrow.size=.4,layout=layout, vertex.label=NA)

install.packages("sna")
library("sna")

V(g) # VERTICES AND EDGES
E(g)

Adjacency_graph<-igraph::get.adjacency(g)
Adjacency_graph


Desity_Graph<-gden(relations)
Desity_Graph

Edge_Density<-igraph::edge_density(g)
Edge_Density

igraph::edge_density(g,loops=TRUE)

Degree_of_Different_Vertices<-igraph::degree(g)
Degree_of_Different_Vertices

hist(Degree_of_Different_Vertices)

Adjacency_matrix<-as.matrix(get.adjacency(first_1000_vertices,sparse=FALSE))
geodesic_distances<-geodist(Adjacency_matrix)
geodesic_distances

Multiplication_of_matrix=Adjacency_matrix%% Adjacency_matrix
Multiplication_of_matrix

Graph_betweeness_Centrality<-igraph::centr_betw(g)
Graph_betweeness_Centrality

Graph_closeness_Centrality<-igraph::centr_clo(g)
Graph_closeness_Centrality


first_shortest_PATH_200_vertices<-igraph::shortest.paths(first_200_vertices)
first_shortest_PATH_200_vertices

first_shortest_PATH_1000_vertices=igraph::shortest.paths(first_1000_vertices)
first_shortest_PATH_1000_vertices


igraph::get.shortest.paths(g,7)

node<-c(50)
g_Adjaceny_Graph<-igraph::graph_from_adjacency_matrix(Adjacency.graph)
g_50clique=igraph::max_cliques(g_Adjaceny_Graph,min = NULL,max = NULL,subset = node)

Largest_cliques_g <-igraph::clique_num(g_Adjaceny_Graph)
largest.cliques_g

is.simple(g)
simplify_g<-simplify(g)
is.simple(simplify_g)
is.simple(g)



wc<-walktrap.community(g)

plot(wc,g,vertex.size=4,edge.arrow.size=0.1,vertex.label=NA,layout=layout.fruchterman.reingold)
Graph_v2<-g
E(Graph_v2)$weight<-rnorm(ecount(Graph_v2))
V(Graph_v2)$weight<-rnorm(vcount(Graph_v2))

Subgraph_Duplicate<-igraph::induced.subgraph(Graph_v2,which(V(Graph_v2)$weight > 2.2))
Subgraph_Duplicate<-delete_edges(Subgraph_Duplicate, E(Subgraph_Duplicate)[E(Subgraph_Duplicate)$weight <0])



plot(igraph::delete.vertices(Subgraph_Duplicate,igraph::degree(Subgraph_Duplicate)==0),edge.label=round(E(Subgraph_Duplicate)$weight,2),edge.arrow.size=0.1,vertex.label.cex=0.5,edge.label.cex=0.6,vertex.color="Pink",edge.color="Yellow",layout=layout.fruchterman.reingold)
wc_subgraph_duplicate<-walktrap.community(Subgraph_Duplicate)
plot(wc_first_2000_verticessubgraph_duplicate,g,vertex.size=4,vertex.label.cex=0.1,vertex.label=NA,edge.arrow.size=0.1,layout=layout.fruchterman.reingold)


wc<-walktrap.community(first_2000_vertices)
plot(wc,first_20000_vertices,vertex.size=4,edge.arrow.size=0.1,vertex.label=NA,layout=layout.fruchterman.reingold)


Alpha_Centrality_Subgraph_Duplicate=alpha.centrality(Subgraph_Duplicate)
sort(Alpha_Centrality_Subgraph_Duplicate,decreasing = TRUE)

#longest path
Subgraph_v3<-igraph::induced.subgraph(g,which(V(g)$membership==1))
V(Subgraph_v3)$degree=igraph::degree(Subgraph_v3)
res_dfs=dfs(Subgraph_v3,root=1,dist=TRUE)$distsh

Betweeness_value<-igraph::betweenness(g)
Central_most_value<-which.max(Betweeness_value)
Central_most_value

Central_Node<-which.max(igraph::degree(g,mode="all"))
Central_Node

largest_cliques(g)


Ego_of_graph<-igraph::ego(g)
Ego_of_graph

Power_Central_graph<-power_centrality(Subgraph_Duplicate,exponent=0.5)
sort(Power_Central_graph,decreasing=TRUE)



