
library(visNetwork)
library(networkD3)
library(dplyr)
library(igraph)
library(gtools)
library(shinydashboardPlus)
library(shiny)
library(shinyjs)
## shinysky is to customize buttons
library(shinysky)
library(DT)
library(data.table)
library(lubridate)
library(shinyalert)
library(shinydashboard)

useShinyalert()
skin <- Sys.getenv("DASHBOARD_SKIN")
skin <- tolower(skin)
if (skin == "")
  skin <- "blue"


sidebar <- dashboardSidebar(
  sidebarSearchForm(label = "Search...", "searchText", "searchButton"),
  sidebarMenu(
    menuItem("Network Linkages", tabName = "dashboard", icon = icon("project-diagram")),
    menuItem("Community Detection", icon = icon("users"), tabName = "community", badgeColor = "green" ),

    menuItem("New Customer", icon = icon("user-check"), tabName = "customer_entry", badgeColor = "green" ),
    menuItem("References", icon = icon("book"),
             href = "https://github.com/rstudio/shinydashboard/blob/gh-pages/_apps/sidebar/app.R"
    )
  )
)

body <- dashboardBody(
  tabItems(
    tabItem("dashboard",
            fluidRow(
              box(title = "Fraud Network", status = "primary",
                  visNetworkOutput("network", height = "650px"),
                  height = 700, width = 12
              )
            ),
            
            # Boxes with solid headers
            fluidRow(
              box(
                title = "Select Attributes for linkages",
                width = 6, solidHeader = TRUE, status = "primary",
                checkboxGroupInput("links", label="Link By",
                                   choices = c("Telephone", "Employer", "Postcode", "Surname"),
                                   selected = c("Postcode","Surname"), inline = TRUE
                )
              ),
              
              box(
                title = "Number of Links", width = 6, solidHeader = TRUE, status = "primary",
                textOutput("no_of_link")
              )
             
 
            ),
            
            # Solid backgrounds
            fluidRow(
              box(
                title = "Title 4",
                width = 4,
                background = "black",
                "A box with a solid black background"
              ),
              box(
                title = "Title 5",
                width = 4,
                background = "light-blue",
                "A box with a solid light-blue background"
              ),
              box(
                title = "Title 6",
                width = 4,
                background = "maroon",
                "A box with a solid maroon background"
              )
              
            )
    ),
    tabItem("community",
            fluidRow(
              tabBox(
                height=700, width = 12,
                tabPanel("Community 1: Based on Edge Betweenness", 
                         visNetworkOutput("community1",height = "600px")
                         ),
                tabPanel("Community 2: Based on Label Propagation Algorithm", 
                         visNetworkOutput("community2",height = "600px")
                         )
                  
                )#,
             # box(width = 6, title = "Fraud Rates by Communities:1",background = "black",
              #    status = "warning", 
               #   dataTableOutput("community_frd_rate_1")),
            #  box(width = 6, title = "Fraud Rates by Communities:2",background = "black",
             #     status = "warning",
              #    dataTableOutput("community_frd_rate_2"))
              )
              
             
            ), 
    tabItem("customer_entry", 
            fluidRow(
              box(
                title = "Customer Base", width=12,
                tags$head(tags$style(HTML('
                            .modal-lg {
                            width: 1200px;
                            }
                            '))),
                helpText("Note: Remember to save any updates!"),
                br(),
                ### tags$head() is to customize the download button
                tags$head(tags$style(".butt{background-color:#230682;} .butt{color: #e6ebef;}")),
                downloadButton("Trich_csv", "Download in CSV", class="butt"),
                useShinyalert(), # Set up shinyalert
                uiOutput("MainBody_trich"),actionButton(inputId = "Updated_trich",label = "Save")
              )
              
            ))
  )
)


notifications <- dropdownMenu(type = "notifications", badgeStatus = "warning",
                              notificationItem(
                                text = "5 new users today",
                                icon("users")
                              ),
                              notificationItem(
                                text = "12 items delivered",
                                icon("truck"),
                                status = "success"
                              ),
                              notificationItem(
                                text = "Server load at 86%",
                                icon = icon("exclamation-triangle"),
                                status = "warning"
                              )
)



header <- dashboardHeaderPlus(enable_rightsidebar = FALSE,
  title = "Network Analysis", notifications
)

ui <- dashboardPagePlus(header, sidebar, body, skin = "green", rightsidebar = FALSE)

server <- function(input, output) {
  
  set.seed(122)
  histdata <- rnorm(500)
  
  # ************** Inserting the Data entry in this section **********************************************************************************************
  
  shiny_data <- read.csv(file="/Users/abhik/Desktop/MyData.csv", header=T, as.is=T,stringsAsFactors = FALSE)
  shiny_data <- shiny_data[,2:9]
  vals_trich<-reactiveValues()
  vals_trich$Data<-shiny_data
  
  
  #### MainBody_trich is the id of DT table
  output$MainBody_trich<-renderUI({
    
    fluidPage(
      hr(),
      column(6,offset = 6,
             HTML('<div class="btn-group" role="group" aria-label="Basic example" style = "padding:10px">'),
             ### tags$head() This is to change the color of "Add a new row" button
             tags$head(tags$style(".butt2{background-color:#231651;} .butt2{color: #e6ebef;}")),
             div(style="display:inline-block;width:30%;text-align: center;",actionButton(inputId = "Add_row_head",label = "Add", class="butt2") ),
             HTML('</div>') ),
      
      column(12,dataTableOutput("Main_table_trich")),
      tags$script("$(document).on('click', '#Main_table_trich button', function () {
                   Shiny.onInputChange('lastClickId',this.id);
                   Shiny.onInputChange('lastClick', Math.random()) });")
      
      
      
    ) 
  })
  
  #### render DataTable part ####
  output$Main_table_trich<-renderDataTable({
    DT=vals_trich$Data
    datatable(DT,selection = 'single',
              escape=F) })
  
  
  observeEvent(input$Add_row_head, {
    ### This is the pop up board for input a new row
    showModal(modalDialog(title = "Add a new row",
                          #  dateInput(paste0("Date_add", input$Add_row_head), "Date:", value = Sys.Date()),
                          textInput(paste0("Forname_add", input$Add_row_head), "Forname"),
                          textInput(paste0("Surname_add", input$Add_row_head), "Surname"),
                          textInput(paste0("Postcode_add", input$Add_row_head), "Postcode:"),
                          textInput(paste0("Employer_add", input$Add_row_head), "Employer:"),
                          textInput(paste0("Telephone_add", input$Add_row_head), "Telephone:"),
                          actionButton("go", "Add item"),
                          easyClose = TRUE, footer = NULL ))
    
  })
  ### Add a new row to DT  
  observeEvent(input$go, {
    new_row=data.frame(
      ID = paste0("S",as.character(nrow(vals_trich$Data)+1)),
      Forname=input[[paste0("Forname_add", input$Add_row_head)]],
      Surname=input[[paste0("Surname_add", input$Add_row_head)]],
      Fullname= paste(input[[paste0("Forname_add", input$Add_row_head)]], input[[paste0("Surname_add", input$Add_row_head)]], sep=" "),
      Postcode=input[[paste0("Postcode_add", input$Add_row_head)]],
      Employer = input[[paste0("Employer_add", input$Add_row_head)]],
      Tele= input[[paste0("Telephone_add", input$Add_row_head)]], 
      Frd_mkr = "Unknown"
    )
    vals_trich$Data<-rbind(vals_trich$Data,new_row )
    removeModal()
  })
  
  ### save to RDS part 
  observeEvent(input$Updated_trich,{
    write.csv(vals_trich$Data, file = "/Users/abhik/Desktop/MyData.csv")
    shinyalert(title = "Saved!", type = "success")
  })
  
  
  
  # ************** Inserting the Network Build in this section **********************************************************************************************
  
  output$network <- renderVisNetwork({
    list <- read.csv(file="/Users/abhik/Desktop/MyData.csv", header=T, as.is=T,stringsAsFactors = FALSE)  
    list<- list %>% rename(id = ID , label = Fullname, group= Frd_mkr)
    list <- list[,2:9]
    
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
  # **********************************************************************************************************************************************************
  output$no_of_link <- renderText({
    
    list <- read.csv(file="/Users/abhik/Desktop/Nodelist1.csv", header=T, as.is=T,stringsAsFactors = FALSE)
    list<- list %>% rename(id = ID , label = Fullname, group= Frd_mkr)
    
    # Creating the edge list common by Telephone Numbers 
    a1 <- full_join(list,list, by="Tele")
    a1 <- a1 %>% select(id.x, id.y) %>% filter (id.x != id.y & id.x > id.y) %>%
      rename(from = id.x, to=id.y) %>% mutate(weight=10.0, Link_by="Telephone")
    count_tele=nrow(unique(a1[,c("from", "to")]))
    
    # Creating the edge list commmon by Postcode
    b1 <- full_join(list,list, by="Postcode")
    b1 <- b1 %>% select(id.x, id.y) %>% filter (id.x != id.y & id.x > id.y) %>%
      rename(from = id.x, to=id.y)  %>% mutate(weight=1.0, Link_by="Postcode")
    count_post=nrow(unique(b1[,c("from", "to")]))
    
    # Creating the edge list commmon by Surname
    
    c1 <- full_join(list,list, by="Surname")
    
    c1 <- c1 %>% select(id.x, id.y) %>% filter (id.x != id.y & id.x > id.y) %>%
      rename(from = id.x, to=id.y)  %>% mutate(weight=1.0, Link_by="Surname")
    count_srnm=nrow(unique(c1[,c("from", "to")]))
    # Creating the edge list commmon by Employer
    
    d1 <- full_join(list,list, by="Employer")
    
    d1 <- d1 %>% select(id.x, id.y) %>% filter (id.x != id.y & id.x > id.y) %>%
      rename(from = id.x, to=id.y)  %>% mutate(weight=2.0, Link_by="Employer")
    count_emp=nrow(unique(d1[,c("from", "to")]))
    
    data_sh <- rbind (a1,b1,c1,d1)
    
    data_sh1 <-subset(data_sh, Link_by %in% input$links)
    data_sh2 <- count(data_sh1) %>% rename (`Total links` =n)
    tot_links=nrow(data_sh1[,c("from", "to")])
    unique_links=nrow(unique(data_sh1[,c("from", "to")]))
    
    paste("The number of Links: ", tot_links, "  and  ", "The number of unique Links: ",unique_links)
    
  }
  
  )
  # **********************************************************************************************************************************************************
  # ************** Inserting the Fraud Rate by Community in this section **********************************************************************************************
  
  
  
  # **********************************************************************************************************************************************************
  # ************** Inserting the Community Buildin this section **********************************************************************************************
  
  output$community1 <- renderVisNetwork({
    
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
      summarise( post = max(Postcode, na.rm = TRUE), Tel=max(Tele, na.rm = TRUE), surname = max(Surname, na.rm = TRUE), emp=max(Employer, na.rm = TRUE))
    
    test$weight <- with(test, (10 * !is.na(Tel)) + (1 * !is.na(post)) + (1 * !is.na(surname))+ (2 * !is.na(emp)))
    rownames(test) <- NULL
    
    # CLuster Edge Betweenness
    net <- graph_from_data_frame(d=test, vertices=list, directed=F) 
    ceb <- cluster_edge_betweenness(net)
    
    V(net)$community <- ceb$membership  # this step requires a separate net for each community algo
    list1 <- data.frame(id = V(net)$name, title = V(net)$name, group = V(net)$community)
    list1 <- list1[order(list1$id, decreasing = F),]
    list1 <- data.frame(id=V(net)$name, label=V(net)$label, group = V(net)$community)
    edges1 <- get.data.frame(net, what="edges")[1:2]
    
    visNetwork(list1, edges1) %>%
      visOptions(highlightNearest = list(enabled = TRUE, hover = TRUE), nodesIdSelection = TRUE, manipulation = TRUE, selectedBy = "group", clickToUse=TRUE) %>% 
      visExport()
    
    
  })
  
  output$community2 <- renderVisNetwork({
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
      summarise( post = max(Postcode, na.rm = TRUE), Tel=max(Tele, na.rm = TRUE), surname = max(Surname, na.rm = TRUE), emp=max(Employer, na.rm = TRUE))
    
    test$weight <- with(test, (10 * !is.na(Tel)) + (1 * !is.na(post)) + (1 * !is.na(surname))+ (2 * !is.na(emp)))
    rownames(test) <- NULL
    
    #Cluster Label propagation 
    net2 <- graph_from_data_frame(d=test, vertices=list, directed=F) 
    clp <- cluster_label_prop(net2) 
    
    V(net2)$community <- clp$membership  # this step requires a separate net for each community algo
    list2 <- data.frame(id = V(net2)$name, title = V(net2)$name, group = V(net2)$community)
    list2 <- list2[order(list2$id, decreasing = F),]
    list2 <- data.frame(id=V(net2)$name, label=V(net2)$label, group = V(net2)$community)
    edges2 <- get.data.frame(net2, what="edges")[1:2]
    
    visNetwork(list2, edges2) %>%
      visOptions(highlightNearest = list(enabled = TRUE, hover = TRUE), nodesIdSelection = TRUE, manipulation = TRUE, selectedBy = "group", clickToUse=TRUE) %>% 
      visExport()
  })
  
}

shinyApp(ui, server)


