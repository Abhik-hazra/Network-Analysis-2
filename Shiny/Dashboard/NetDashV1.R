
library(shinydashboard)

skin <- Sys.getenv("DASHBOARD_SKIN")
skin <- tolower(skin)
if (skin == "")
  skin <- "blue"


sidebar <- dashboardSidebar(
  sidebarSearchForm(label = "Search...", "searchText", "searchButton"),
  sidebarMenu(
    menuItem("Network Linkages", tabName = "dashboard", icon = icon("project-diagram")),
    menuItem("Community Detection", icon = icon("users"), tabName = "community", badgeColor = "green" ),
    menuItem("Charts", icon = icon("bar-chart-o"),
             menuSubItem("Chart sub-item 1", tabName = "subitem1"),
             menuSubItem("Chart sub-item 2", tabName = "subitem2")
    ),
    menuItem("New Customer", icon = icon("users"), tabName = "customer_entry", badgeColor = "green" ),
    menuItem("References", icon = icon("asterisk"),
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
                title = "Histogram control", width = 12, solidHeader = TRUE, #status = "primary",
                sliderInput("count", "Count", min = 1, max = 500, value = 120)
              ),
              box(
                title = "Appearance",
                width = 4, solidHeader = TRUE,
                radioButtons("fill", "Fill", # inline = TRUE,
                             c(None = "none", Blue = "blue", Black = "black", red = "red")
                )
              ),
              box(
                title = "Scatterplot control",
                width = 4, solidHeader = TRUE, status = "warning",
                selectInput("spread", "Spread",
                            choices = c("0%" = 0, "20%" = 20, "40%" = 40, "60%" = 60, "80%" = 80, "100%" = 100),
                            selected = "60"
                )
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
    tabItem("community"), 
    tabItem("customer_entry")
    
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



header <- dashboardHeader(
  title = "Network Analysis", notifications
)

ui <- dashboardPage(header, sidebar, body, skin = "green")

server <- function(input, output) {
  
  set.seed(122)
  histdata <- rnorm(500)
  
  # ************** Inserting the Network Build in this section **********************************************************************************************
  output$network <- renderVisNetwork({
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
  
  data_sh1 <-subset(data_sh, Link_by %in% input$links)
  
  
    edge <- mutate(data_sh1, width = weight + 0.5)
    visNetwork(list, edge) %>% 
      visIgraphLayout(layout = "layout_nicely", physics = FALSE, smooth = FALSE) %>%
      visGroups(groupname = "Fraud", color = "red") %>%
      visGroups(groupname = "Non-Fraud", color = "lightblue") %>%
      visLegend() 
    
  })
  
  # **********************************************************************************************************************************************************


}

shinyApp(ui, server)