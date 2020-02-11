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



#multiline commenting
'rs <- dbSendQuery(conn, "SELECT * FROM frs_facilities LIMIT 5;")

dbFetch(rs)

dbClearResult(rs)
dbDisconnect(conn)'

state_codes = c("-","AK","AL","AR","AS","AZ","CA","CO","CT","DC","DE","FL","FM","GA","GU","HI","IA","ID","IL","IN","KS","KY","LA","MA","MD","ME","MH","MI","MN","MO","MP","MS","MT","NC","ND","NE","NH","NJ","NM","NV","NY","OH","OK","OR","PA","PR","PW","RI","SC","SD","TN","TX","UT","VA","VI","VT","WA","WI","WV","WY")

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   h3("FRS Search"),
   p("Search 4+ million EPA FRS facility records."),
   
   
   
   fluidRow(
     column(3,
            selectInput(inputId = 'state_code', label = 'State', choices = state_codes, selected = '-', multiple = FALSE)
     ),
     column(9,
            textInput(inputId = "county_text", label = "County contains:")     
     )
   ),
   fluidRow(
     column(12,
            actionButton("fetch_button", "Query Database"),
            br(),
            tags$em("may take 10+ seconds, if no results then will show blank table with headers")
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
           DTOutput("tbl"),
           br(),
           br(),
           tags$em("In testing: results limited to 10,000.")
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
      dbSendQuery(conn, "SET  NAMES utf8")
      on.exit(dbDisconnect(conn), add = TRUE)
      if (input$state_code == '-') {
        print('No state selected')
      } else {
        
        county_text_to_match <- input$county_text
        
        results <- dbGetQuery(conn, paste0(
          "SELECT * FROM facility ", 
          "WHERE state = '",input$state_code,"' ",
          "AND county LIKE '%",county_text_to_match,"%' ",
          
          "LIMIT 10000;"), n = Inf)
          #"AND (name LIKE '%",text_to_match,"%' OR address LIKE '%",text_to_match,"%') ",
          #I removed that part of the query, letting the DT package do the text searching
        
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

