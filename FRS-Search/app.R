#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

#install.packages("shiny")
#install.packages("DBI")
#install.packages("dplyr")
#install.packages("dbplyr")
#install.packages("pool")
#install.packages("RMySQL")


library(shiny)
#library(pool)
#library(dplyr)
library(DBI)
library(DT) #JavaScript data tables

conn <- dbConnect(
  drv = RMySQL::MySQL(),
  dbname = "frs_facilities",
  host = "frs-db.ckmkzk29kimh.us-east-1.rds.amazonaws.com",
  username = Sys.getenv("aws_rds_db_user"),
  password = Sys.getenv("aws_rds_db_pass")
  )

rs <- dbSendQuery(conn, "SELECT * FROM frs_facilities LIMIT 5;")

dbFetch(rs)

dbClearResult(rs)
dbDisconnect(conn)

state_codes = c('-','AK','CA')

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   #titlePanel("FRS Search"),
   
   
   
   fluidRow(
     column(3,
            selectInput(inputId = 'state_code', label = 'State', choices = state_codes, selected = '-', multiple = FALSE)
     ),
     column(9,
            textInput(inputId = "county_text", label = "Find text in County")     
     )
   ),
   fluidRow(
     column(12,
            textInput(inputId = "match_text", label = "Find text in Name or Address")    
     )
     ),
   fluidRow(
     column(12,
            actionButton("fetch_button", "Query Database"),
            br(),
            tags$em("may take several seconds, will show blank table if no results")
     )
   ),
  hr(),
   
   # Sidebar with a slider input for number of bins 
   #sidebarLayout(
  #    sidebarPanel(
   #      selectInput(inputId = 'state_code', label = 'State', choices = state_codes, selected = '-', multiple = FALSE)
    #  ),
      
      # Show a plot of the generated distribution
  
  fluidRow(
    column(12,
           #tableOutput("tbl")
           DTOutput("tbl")
    )
  )

)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    
    
    returned_data <- eventReactive(input$fetch_button, {
      conn <- dbConnect(
        drv = RMySQL::MySQL(),
        dbname = "frs_facilities",
        host = "frs-db.ckmkzk29kimh.us-east-1.rds.amazonaws.com",
        username = Sys.getenv("aws_rds_db_user"),
        password = Sys.getenv("aws_rds_db_pass")
      )
      on.exit(dbDisconnect(conn), add = TRUE)
      if (input$state_code == '-') {
        print('No state selected')
      } else {
        text_to_match <- input$match_text
        county_text_to_match <- input$county_text
        
        results <- dbGetQuery(conn, paste0(
          "SELECT * FROM facility ", 
          "WHERE state = '",input$state_code,"' ",
          "AND county LIKE '%",county_text_to_match,"%' ",
          "AND (name LIKE '%",text_to_match,"%' OR address LIKE '%",text_to_match,"%') ",
          "LIMIT 2000;"))
        
        if (nrow(results) == 0) {
          results
        } else {
          results$frsurl <- paste('<a href="',results$frsurl,'" target="_blank">Facility Detail Report</a>')
          results
        }
      }
    })  
  
    output$tbl <- 
      renderDT({
        returned_data()#reactive expressions need to be invoked!
        },
        escape = FALSE,
        options = list(
          
          columnDefs = list(list(width = '600px', targets = c(2))),
          scrollX = TRUE
        )
      )
    
}

# Run the application 
shinyApp(ui = ui, server = server)

