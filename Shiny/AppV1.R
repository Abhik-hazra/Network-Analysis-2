list <- read.csv(file="/Users/abhik/Desktop/Nodelist1.csv", header=T, as.is=T,stringsAsFactors = FALSE)
library(gtools)
library(shiny)
library(visNetwork)
library(networkD3)
library(dplyr)
library(igraph)

ui <- fluidPage(
  titlePanel("Fraud Network"),
 
    sidebarPanel(" Our Inputs will be here"
    ),
  
    mainPanel(
      visNetworkOutput("network")
    )
  )


server <- function(input, output) {
  output$network <- renderVisNetwork({
    # minimal example
    
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
    
    
    test <- smartbind(a1,b1,c1,d1)
    rownames(test) <- NULL
    
    
    test <- group_by(test, from, to)
    test <- summarise(test, post = max(Postcode), Tel=max(Tele), surname = max(Surname), emp=max(Employer))
    
    
    test$weight <- with(test, (10 * !is.na(Tel)) + (1 * !is.na(post)) + (1 * !is.na(surname))+ (2 * !is.na(emp)))
    rownames(test) <- NULL
    
    # net2 <- simplify(net2, remove.multiple = F, remove.loops = T) 
    edge <- mutate(test, width = weight + 0.5)
    visNetwork(list, edge) %>% 
      visIgraphLayout(layout = "layout_with_fr", physics = FALSE, smooth = FALSE) %>%
      visGroups(groupname = "Fraud", color = "red") %>%
      visGroups(groupname = "Non-Fraud", color = "lightblue") %>%
      visLegend()
    
    
  })
}

shinyApp(ui = ui, server = server)