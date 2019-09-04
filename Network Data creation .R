

list <- read.csv(file="/Users/abhik/Desktop/Nodelist1.csv", header=T, as.is=T,stringsAsFactors = FALSE)
list<- list %>% rename(id = ID , label = Fullname, group= Frd_mkr)

# Creating the edge list common by Telephone Numbers 
a1 <- full_join(list,list, by="Tele")

a1 <- a1 %>% select(id.x, id.y,Tele) %>% filter (id.x != id.y & id.x > id.y) %>%
  rename(from = id.x, to=id.y)

# Creating the edge list commmon by Postcode

b1 <- full_join(list,list, by="Postcode")

b1 <- b1 %>% select(id.x, id.y, Postcode) %>% filter (id.x != id.y & id.x > id.y) %>%
  rename(from = id.x, to=id.y)

# Creating the edge list commmon by Surname

c1 <- full_join(list,list, by="Surname")

c1 <- c1 %>% select(id.x, id.y, Surname) %>% filter (id.x != id.y & id.x > id.y) %>%
  rename(from = id.x, to=id.y)

# Creating the edge list commmon by Employer

d1 <- full_join(list,list, by="Employer")

d1 <- d1 %>% select(id.x, id.y, Employer) %>% filter (id.x != id.y & id.x > id.y) %>%
  rename(from = id.x, to=id.y)

library(gtools)
test <- smartbind(a1,b1,c1,d1)
rownames(test) <- NULL



test <- group_by(test, from, to)
test <- summarise(test, post = max(Postcode), Tel=max(Tele), surname = max(Surname), emp=max(Employer))


test$weight <- with(test, (10 * !is.na(Tel)) + (1 * !is.na(post)) + (1 * !is.na(surname))+ (2 * !is.na(emp)))
rownames(test) <- NULL




net2 <- graph_from_data_frame(d=test, vertices=list, directed=F) 

class(net2)

E(net2)       # The edges of the "net" object
V(net2)       # The vertices of the "net" object
E(net2)$weight  # Edge attribute "type"
V(net2)$Surname # Vertex attribute "Surname"



library(visNetwork)
edge <- mutate(test, width = weight + 0.5)
visNetwork(list, edge) %>% 
  visIgraphLayout(layout = "layout_with_fr", physics = FALSE, smooth = FALSE) %>%
  visGroups(groupname = "Fraud", color = "red") %>%
  visGroups(groupname = "Non-Fraud", color = "lightblue") %>%
  visLegend()




eigen_centrality(net2, directed = FALSE, weights = test$weight)
# Community representation 
fc <- fastgreedy.community(net2)
V(net2)$community <- fc$membership

list1 <- data.frame(id = V(net2)$name, title = V(net2)$name, group = V(net2)$community)
list1 <- list1[order(list1$id, decreasing = F),]
edges <- get.data.frame(net2, what="edges")[1:2]

visNetwork(list1, edges) %>%
  visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE)

net.sym <- as.undirected(net2, mode= "collapse", edge.attr.comb=list(weight="sum", "ignore"))

cliques(net.sym) # list of cliques
sapply(cliques(net.sym), length) # clique sizes 
largest_cliques(net.sym) # cliques with max number of nodes

ceb <- cluster_edge_betweenness(net2); dendPlot(ceb, mode="hclust")
plot(ceb, net2)
length(ceb)
membership(ceb)

