list <- read.csv(file="/Users/abhik/Desktop/MyData.csv", header=T, as.is=T,stringsAsFactors = FALSE)  
list<- list %>% rename(id = ID , label = Fullname, group= Frd_mkr)
list <- list[,2:9]

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



test <- group_by(test, from, to) %>%
 summarise( Postcode = max(Postcode, na.rm = TRUE), Telephone=max(Tele, na.rm = TRUE), Surname = max(Surname, na.rm = TRUE), Employer=max(Employer, na.rm = TRUE))


test$weight <- with(test, (10 * !is.na(Telephone)) + (1 * !is.na(Postcode)) + (1 * !is.na(Surname))+ (2 * !is.na(Employer)))
rownames(test) <- NULL
test1<- test %>% select(-weight)

cols <- c("Postcode", "Telephone", "Surname", "Employer")

test1$title <- apply(test1[, cols], 1, function(x){
  paste(cols[which(!is.na(x))], collapse = ",")
})



# CLuster Edge Betweenness
net <- graph_from_data_frame(d=test, vertices=list, directed=F) 

class(net)
E(net)       # The edges of the "net" object
V(net)       # The vertices of the "net" object
E(net)$weight  # Edge attribute "type"
V(net)$Surname # Vertex attribute "Surname"
V(net)$Postcode # Vertex attribute "Postcode"
V(net)$label
V(net)$id



ceb <- cluster_edge_betweenness(net)
plot(ceb, net)
length(ceb)
membership(ceb)
V(net)$community <- ceb$membership  # this step requires a separate net for each community algo
list1 <- data.frame(id = V(net)$name, title = V(net)$name, group = V(net)$community)
list1 <- list1[order(list1$id, decreasing = F),]
list1 <- data.frame(id=V(net)$name, label=V(net)$label, group = V(net)$community)
edges1 <- get.data.frame(net, what="edges")[1:2]

visNetwork(list1, edges1) %>%
  visOptions(highlightNearest = list(enabled = TRUE, hover = TRUE), nodesIdSelection = TRUE, manipulation = TRUE, selectedBy = "group", clickToUse=TRUE) %>% 
  visExport()

#Creating Fraud Rates 

fraud_community1 <- inner_join(list1, list, by="id") %>% select(id, label.x, group.x, group.y) %>% 
  rename(label=label.x, Frd_mkr=group.y, community=group.x) 
fraud_rate1 <- group_by(fraud_community1,community) %>% summarise(total=n_distinct(label))
fraud_rate2 <- fraud_community1 %>% filter(Frd_mkr=="Fraud") %>% group_by(community) %>% summarise(frd=n_distinct(label))
fraud_rate3 <- left_join(fraud_rate1,fraud_rate2,by="community") %>% mutate(Fraud_rate=frd/total)%>%
  select(community,Fraud_rate) %>% arrange(desc(Fraud_rate)) %>% head(5)



#Cluster Label propagation 
net2 <- graph_from_data_frame(d=test, vertices=list, directed=F) 

clp <- cluster_label_prop(net2) 
plot(clp, net2)
length(clp)
membership(clp)
V(net2)$community <- clp$membership  # this step requires a separate net for each community algo
list2 <- data.frame(id = V(net2)$name, title = V(net2)$name, group = V(net2)$community)
list2 <- list2[order(list2$id, decreasing = F),]
list2 <- data.frame(id=V(net2)$name, label=V(net2)$label, group = V(net2)$community)
edges2 <- get.data.frame(net2, what="edges")[1:2]
visNetwork(list2, edges2) %>%
  visOptions(highlightNearest = list(enabled = TRUE, hover = TRUE), nodesIdSelection = TRUE, manipulation = TRUE, selectedBy = "group", clickToUse=TRUE) %>% 
  visExport()


# **********************************************************************************************************************************************************
# **********************************************************************************************************************************************************
# **********************************************************************************************************************************************************
# **********************************************************************************************************************************************************


fc <- fastgreedy.community(net)
plot(fc,net)
length(fc)
membership(fc)


V(net2)$community <- fc$membership

list1 <- data.frame(id = V(net2)$name, title = V(net2)$name, group = V(net2)$community)
list1 <- list1[order(list1$id, decreasing = F),]
edges <- get.data.frame(net2, what="edges")[1:2]

visNetwork(list1, edges) %>%
  visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE)

net.sym <- as.undirected(net2, mode= "collapse", edge.attr.comb=list(weight="sum", "ignore"))


