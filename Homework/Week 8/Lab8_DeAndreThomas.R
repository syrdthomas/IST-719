########################################################
#
# Author: DeAndre Thomas
# Purpose: Week 8 Lab
#
# "Class Social Network Data (Structure and Cleaning)"
#
# uses LINKS-421-719Network.csv
#      NODES-421-719Network.csv
#
########################################################
library(igraph)

link.data<- read.csv("C:\\Users\\deand\\OneDrive\\Documents\\SyrU ADS\\IST719\\Lab 8\\links-421-719network.csv"
                     , header = TRUE
                     , stringsAsFactors = FALSE)
                  

node.data<- read.csv("C:\\Users\\deand\\OneDrive\\Documents\\SyrU ADS\\IST719\\Lab 8\\nodes-421-719network.csv"
                     , header = TRUE
                     , stringsAsFactors = FALSE)

View(node.data)
View(link.data)
dim(node.data)

colnames(link.data)
colnames(link.data)<- gsub("\\.", "", colnames(link.data))
link.data$X<- gsub(" |-", "", link.data$X)

cbind(link.data$X, colnames(link.data)[-1])

node.data$Name<- gsub(" |-", "", node.data$Name)
cbind(node.data$Name, link.data$X)

M<- as.matrix(link.data[ , -1])
M

rownames(M)<- colnames(M)
dim(M)

any(is.na(M))
M[is.na(M)]<- 0
M[M > 1]

g<- graph_from_adjacency_matrix(M)

##########################################################
#
# The graph object and first plot
#
##########################################################

g # attri: name(v/c) vertices, characters

vcount(g)
ecount(g)

plot.igraph(g) #social network plot (overplotting is present)

g<- simplify(g)
par(mar = c (0,0,0,0))
plot.igraph(g, edge.arrow.size = 0, edge.arrow.width = 0)

E(g)$arrow.size<- 0
E(g)$arrow.width<- 0

plot.igraph(g)
g

V(g)$color<- "gold"
V(g)$frame.color<- "white"
V(g)$label.color<- "black"
V(g)$size<- 5
E(g)$color<- "cadetblue"

plot.igraph(g)
?igraph.plotting()

E(g)$curved<- .4
plot.igraph(g)

#####################################################
#
# Visualizing Centrality and Centrality Measurement
#
##################################################

par(mar = c(3,10,1,1))
degree(g) #counts number of networks a node has
plot(degree(g)) #Distribution of degree
barplot(degree(g))
barplot(sort(degree(g)), horiz = T, las = 2)

V(g)$degree<- degree(g)
V(g)$deg.out<- degree(g, mode = "out")
V(g)$deg.in<- degree(g, mode = "in")

barplot(V(g)$deg.out,names.arg = V(g)$name
        , horiz = T, las = 2
        , main = "Most Friendly")

barplot(V(g)$deg.in,names.arg = V(g)$name
        , horiz = T, las = 2
        , main = "Most Centralized")

#g.bak<- g
#g<- as.undirected(g)
#g<- g.bak

V(g)$close<- closeness(g, normalized = T, mode = "all")
V(g)$bet<- betweenness(g, directed = FALSE)

library(plotrix)
my.palette<- colorRampPalette(
  c("steelblue1", "violet", "tomato", "red"))

V(g)$color<- rev(
  my.palette(200))[round(1 + rescale(V(g)$close, c(1, 199)), 0)]

V(g)$size<- 2 + rescale(V(g)$degree, c(0,13))
V(g)$label.cex<- .7 + rescale(V(g)$bet, c(0,1.25))

plot.igraph(g)

##################################################################
#
# Visualizing Social Network Structures
#
##################################################################


cbind(V(g)$name, node.data$Name)

V(g)$class<- node.data$Class
V(g)$color<- node.data$Country
V(g)$year<- node.data$year

g<- delete_vertices(g, "JoHunter")


V(g)$shape<- "circle"
V(g)$shape[V(g)$class == "wednesday"] <- "square"
V(g)$shape[V(g)$class == "Both"] <- "rectangle"

V(g)$color<- "gold"
V(g)$color[V(g)$Country == "India"] <- "springgreen4"
V(g)$color[V(g)$Country == "China"] <- "red"
V(g)$color[V(g)$Class == "both"] <- "purple"

V(g)$label.color<- "blue"
V(g)$label.color[V(g)$year == 1]<- "black"

plot.igraph(g)

fc<- cluster_fast_greedy(as.undirected(g)) #creating clusters
print(modularity(fc))

membership(fc)
V(g)$cluster<- membership(fc)
length(fc)
sizes(fc)

par(mar = c(0,0,0,0))
plot_dendrogram(fc, palette = rainbow(7))


#####################################################################
#
# Visualizing Social Network Structures
#       use ist719NetworkObjects.rda
#
#####################################################################

my.dir<- "C:\\Users\\deand\\OneDrive\\Documents\\SyrU ADS\\IST719\\Lab 8\\"
load(paste0(my.dir, "ist719networkobject.rda"))
par(mar = c(0,0,0,0))
plot.igraph(g)


l<- layout_in_circle(g)
V(g)$x<- l[,1]
V(g)$y<- l[,2]
plot.igraph(g)

l<- layout_with_fr(g)

E(g)$color<- "gray"
E(g)[from("LeelaDeshmukh")]$color<- "red"

l<- layout_as_star(g, center = "LeelaDeshmukh")
l<-layout_with_kk(g)

V(g)$x<- 0
V(g)$y<- 0
plot.igraph(g)
coord<- cbind(V(g)$x, V(g)$y)

iteration<- c(500, 100, 20, 10, 5, 3, 2, 1)
for (i in 1:length(iteration)){
  l<- layout_with_fr(g, coord = coord, dim = 2, niter = iteration[i])
  V(g)$x<- l[,1]
  V(g)$y<- l[,2]
  plot.igraph(g)
  mtext(paste("Layout FR:", iteration[i])
        , side = 3, line = 0, cex = 1.5, adj = 0)
}

l<- layout_with_gem(g)
l<- layout_with_dh(g)
l<- layout_on_grid(g)
# different layouts

# Bipartite Networks 

g

my.linked.list<- data.frame(person = V(g)$name, event = V(g)$country)
g<- graph_from_data_frame(my.linked.list, directed = FALSE)
g

V(g)$type<- FALSE
V(g)$type[V(g)$name %in% node.data$Name]<- TRUE

l<- layout_as_bipartite(g, types = V(g)$type)
V(g)$x<- l[ , 2]
V(g)$y<- l[ , 1]

par(mar = c(0,0,0,0))

V(g)$size<- 0

























