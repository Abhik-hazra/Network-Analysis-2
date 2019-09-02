
list <- read.csv(file="/Users/abhik/Desktop/Nodelist1.csv", header=T, as.is=T,stringsAsFactors = FALSE)
list<- list %>% rename(id = ID , label = Fullname, group= Frd_mkr)

# Creating the edge list common by Telephone Numbers 
a1 <- full_join(list,list, by="Tele")

a1 <- a1 %>% select(id.x, id.y) %>% filter (id.x != id.y & id.x > id.y) %>%
  rename(from = id.x, to=id.y) %>% mutate(weight=10.0, Link_by="Telephone")

# Creating the edge list commmon by Postcode

b1 <- full_join(list,list, by="Postcode")

b1 <- b1 %>% select(id.x, id.y) %>% filter (id.x != id.y & id.x > id.y) %>%
  rename(from = id.x, to=id.y)  %>% mutate(weight=1.0, Link_by="Postcode")

# Creating the edge list commmon by Surname

c1 <- full_join(list,list, by="Surname")

c1 <- c1 %>% select(id.x, id.y) %>% filter (id.x != id.y & id.x > id.y) %>%
  rename(from = id.x, to=id.y)  %>% mutate(weight=1.0, Link_by="Surname")

# Creating the edge list commmon by Employer

d1 <- full_join(list,list, by="Employer")

d1 <- d1 %>% select(id.x, id.y) %>% filter (id.x != id.y & id.x > id.y) %>%
  rename(from = id.x, to=id.y)  %>% mutate(weight=2.0, Link_by="Employer")

data_sh <- rbind (a1,b1,c1,d1)


library(visNetwork)
edge <- mutate(data_sh, width = weight + 0.5)
visNetwork(list, edge) %>% 
  visIgraphLayout(layout = "layout_with_fr", physics = FALSE, smooth = FALSE) %>%
  visGroups(groupname = "Fraud", color = "red") %>%
  visGroups(groupname = "Non-Fraud", color = "lightblue") %>%
  visLegend()




