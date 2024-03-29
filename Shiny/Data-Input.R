


library(shiny)
library(shinyjs)
## shinysky is to customize buttons
library(shinysky)
library(DT)
library(data.table)
library(lubridate)
library(shinyalert)
library(visNetwork)
library(networkD3)
library(dplyr)
library(igraph)
library(gtools)
shiny_data <- list[,3:4]

useShinyalert()


ui <- fluidPage(
  # Application title
  titlePanel("Customer Base"),
  ### This is to adjust the width of pop up "showmodal()" for DT modify table 
  
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



server <- function(input, output) {
  ### interactive dataset
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
             # tags$head(tags$style(".butt4{background-color:#4d1566;} .butt4{color: #e6ebef;}")),
             # div(style="display:inline-block;width:30%;text-align: center;",actionButton(inputId = "mod_row_head",label = "Edit", class="butt4") ),
             # tags$head(tags$style(".butt3{background-color:#590b25;} .butt3{color: #e6ebef;}")),
             # div(style="display:inline-block;width:30%;text-align: center;",actionButton(inputId = "Del_row_head",label = "Delete", class="butt3") ),
             ### Optional: a html button 
             # HTML('<input type="submit" name="Add_row_head" value="Add">'),
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
                          #  numericInput(paste0("Request_add", input$Add_row_head), "Request Number:",0),  
                          #  selectInput(paste0("Completed_add", input$Add_row_head), "Status:",choices=c("Yes", "On progress")),
                          #  textInput(paste0("Comments_add", input$Add_row_head), "Comments"), 
                          actionButton("go", "Add item"),
                          easyClose = TRUE, footer = NULL ))
    
  })
  ### Add a new row to DT  
  observeEvent(input$go, {
    new_row=data.frame(
      # Date=as.character( input[[paste0("Date_add", input$Add_row_head)]] ),
      Forname=input[[paste0("Forname_add", input$Add_row_head)]],
      Surname=input[[paste0("Surname_add", input$Add_row_head)]]
      # Request=input[[paste0("Request_add", input$Add_row_head)]],
      # Completed=input[[paste0("Completed_add", input$Add_row_head)]],
      #  Comments=input[[paste0("Comments_add", input$Add_row_head)]]
    )
    vals_trich$Data<-rbind(vals_trich$Data,new_row )
    removeModal()
  })
  
  ### save to RDS part 
  observeEvent(input$Updated_trich,{
    shiny_data <- vals_trich$Data
    shinyalert(title = "Saved!", type = "success")
  })
  
  
  
}


shinyApp(ui = ui, server = server)








