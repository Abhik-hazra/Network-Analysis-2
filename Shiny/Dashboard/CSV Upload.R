library(shiny)

# Define UI for data upload app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("New Applications"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Select a file ----
      fileInput("file1", "Choose CSV File",
                multiple = TRUE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv"))
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      # Output: Data file ----
      visNetworkOutput("new_Apps")
      )
    
  )
)

# Define server logic to read selected file ----
server <- function(input, output) {
  
  output$new_Apps <- renderVisNetwork({

    req(input$file1)
    
    base <- read.csv(file="/Users/abhik/Desktop/MyData.csv", header=T, as.is=T,stringsAsFactors = FALSE)  
    base<- base %>% rename(id = ID , label = Fullname, group= Frd_mkr)
    base <- base[,2:9]
    
    new_app <- read.csv(input$file1$datapath, header = TRUE,stringsAsFactors = FALSE, as.is=T)
    new_app<- new_app %>% rename(id = ID , label = Fullname, group= Frd_mkr)
    new_app <- new_app[,2:9]
    
    list<- rbind(base, new_app)
    
    
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
    
    
    # Creating the edge list common by Telephone Numbers 
    w1 <- full_join(list,list, by="Tele")
    w1 <- w1 %>% select(id.x, id.y,Tele) %>% filter (id.x != id.y & id.x > id.y) %>%
      rename(from = id.x, to=id.y)
    
    # Creating the edge list commmon by Postcode
    
    x1 <- full_join(list,list, by="Postcode")
    x1 <- x1 %>% select(id.x, id.y, Postcode) %>% filter (id.x != id.y & id.x > id.y) %>%
      rename(from = id.x, to=id.y)
    
    # Creating the edge list commmon by Surname
    
    y1 <- full_join(list,list, by="Surname")
    y1 <- y1 %>% select(id.x, id.y, Surname) %>% filter (id.x != id.y & id.x > id.y) %>%
      rename(from = id.x, to=id.y)
    
    # Creating the edge list commmon by Employer
    
    z1 <- full_join(list,list, by="Employer")
    z1 <- z1 %>% select(id.x, id.y, Employer) %>% filter (id.x != id.y & id.x > id.y) %>%
      rename(from = id.x, to=id.y)
    
    test <- smartbind(w1,x1,y1,z1)
    rownames(test) <- NULL
    
    test <- group_by(test, from, to) %>%
      summarise( Postcode = max(Postcode, na.rm = TRUE), Telephone=max(Tele, na.rm = TRUE), Surname = max(Surname, na.rm = TRUE), Employer=max(Employer, na.rm = TRUE))
    
    
    cols <- c("Postcode", "Telephone", "Surname", "Employer")
    
    test$title <- apply(test[, cols], 1, function(x){
      paste(cols[which(!is.na(x))], collapse = ",")
    })
    
    data_sh1<- left_join(data_sh, test, by = "from") %>% 
      filter(to.x==to.y ) %>% 
      select(from, to.x,  title, weight, Link_by) %>%
      rename(to = to.x)
    
    data_sh2 <-subset(data_sh1, Link_by %in% input$links)
    
    
    
    
    edge <- mutate(data_sh2, width = weight + 0.5)
    visNetwork(list, edge) %>% 
      visIgraphLayout(layout = "layout_nicely", physics = FALSE, smooth = FALSE) %>%
      visGroups(groupname = "Fraud", color = "red") %>%
      visGroups(groupname = "Non-Fraud", color = "lightblue") %>%
      visGroups(groupname = "Unknown", color = "blue") %>%
      visLegend() 
   

  })
  

}
# Run the app ----
shinyApp(ui, server)
