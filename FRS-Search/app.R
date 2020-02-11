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
library(pool)
library(dplyr)
library(DBI)

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
     column(4, offset = 1,
           actionButton("fetch_button", "Query Database") 
     ),
     column(4,
        renderText("That")    
     )
   ),
  hr(),
   
   # Sidebar with a slider input for number of bins 
   #sidebarLayout(
  #    sidebarPanel(
   #      selectInput(inputId = 'state_code', label = 'State', choices = state_codes, selected = '-', multiple = FALSE)
    #  ),
      
      # Show a plot of the generated distribution
  mainPanel(
      tableOutput("tbl")
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
        dbGetQuery(conn, paste0(
          "SELECT * FROM frs_facilities WHERE state = '",input$state_code,"' LIMIT 200;"))
      }
    })  
  
    output$tbl <- 
      renderTable({
        returned_data()#reactive expressions need to be invoked!
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)

