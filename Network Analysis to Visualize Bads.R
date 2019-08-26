list1 <- read.csv(file="/Users/abhik/Desktop/Nodelist1.csv", header=T, as.is=T)
list1<- list1 %>% rename(id = ID , label = Fullname, group= Frd_mkr)

# Creating the edge list common by Telephone Numbers 
a <- inner_join(list1,list1, by="Tele")

a %>% select(ID.x, ID.y, Tele)

a <- a %>% select(ID.x, ID.y) %>% filter (ID.x != ID.y) %>% mutate(weight =10)

# Creating the edge list commmon by Postcode

b <- inner_join(list1,list1, by="Postcode")

b %>% select(ID.x, ID.y, Postcode)

b <- b %>% select(ID.x, ID.y) %>% filter (ID.x != ID.y) %>% mutate(weight =2)

# Creating the edge list commmon by Surname

c <- inner_join(list1,list1, by="Surname")

c %>% select(ID.x, ID.y, Surname)

c <- c %>% select(ID.x, ID.y) %>% filter (ID.x != ID.y) %>% mutate(weight =1)

# Creating the edge list commmon by Employer

d <- inner_join(list1,list1, by="Employer")

d %>% select(ID.x, ID.y, Employer)

d <- d %>% select(ID.x, ID.y) %>% filter (ID.x != ID.y) %>% mutate(weight =1)



combined <- rbind(a,b,c,d)
combined <- combined %>% rename(from = ID.x, to=ID.y)

nrow(combined); nrow(unique(combined[,c("from", "to")]))
combined1 <- aggregate(combined[,3], combined[,-3], sum)
combined2 <- combined1[order(combined1$from, combined1$to),]



colnames(combined2)[3] <- "weight"
rownames(combined2) <- NULL

net2 <- graph_from_data_frame(d=combined2, vertices=list1, directed=F) 
class(net2)

E(net2)       # The edges of the "net" object
V(net2)       # The vertices of the "net" object
E(net2)$weight  # Edge attribute "type"
V(net2)$Surname # Vertex attribute "media"

plot(net2, edge.arrow.size=.4)
net <- simplify(net2, remove.multiple = F, remove.loops = T) 
as_edgelist(net2, names=T)
as_adjacency_matrix(net2, attr="weight")

plot(net2, edge.arrow.size=.2, edge.curved=0,
     
     vertex.color="orange", vertex.frame.color="#555555",
     
     vertex.label=V(net2)$Forname, vertex.label.color="black",
     
     vertex.label.cex=.7)

library(visNetwork)
library(networkD3)
visNetwork(list1, combined2)

edges <- mutate(combined2, width = weight/2 + 0.5)
visNetwork(list1, edges) %>% 
  visIgraphLayout(layout = "layout_with_fr") %>%
  visGroups(groupname = "1", color = "red") %>%
  visGroups(groupname = "0", color = "lightblue") %>%
  visLegend()