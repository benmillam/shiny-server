library(shiny)
library(DBI)
library(RMariaDB)
library(DT) #JavaScript data tables
library(shinycssloaders) #'loading' icon while query is fetched

state_codes = c("-","AK","AL","AR","AS","AZ","CA","CO","CT","DC","DE","FL","FM","GA","GU","HI","IA","ID","IL","IN","KS","KY","LA","MA","MD","ME","MH","MI","MN","MO","MP","MS","MT","NC","ND","NE","NH","NJ","NM","NV","NY","OH","OK","OR","PA","PR","PW","RI","SC","SD","TN","TX","UT","VA","VI","VT","WA","WI","WV","WY")

ui <- fluidPage(
   
   h3("FRS Search"),
   p("Search 4+ million EPA FRS facility records."),
   
   
   
   fluidRow(
     column(3,
            selectInput(inputId = 'state_code', label = 'State', choices = state_codes, selected = '-', multiple = FALSE)
     ),
     column(9,
            textInput(inputId = "county_text", label = "County contains")     
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
 
  tags$em(textOutput("warning"), style="color: red; margin: 20px;"),
 
  
  fluidRow(
    column(12,
           #tableOutput("tbl")
           withSpinner(DTOutput("tbl"), color="#0064bd"),
           br(),
           br(),
    )
  )

)

server <- function(input, output) {
  
    large_result_set_warning_threshold <- 100000
    
    returned_data <- eventReactive(input$fetch_button, {
      conn <- dbConnect(
        drv = RMariaDB::MariaDB(),
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
        state_code_to_match <- input$state_code
        county_text_to_match <- input$county_text
        
        dbi_result <- dbSendQuery(conn,
          "SELECT * FROM FrsFacility 
           WHERE state = ? AND county LIKE ?
           ;")
        
        dbBind(dbi_result, list(state_code_to_match, paste0("%",county_text_to_match,"%")))
        
        results <- dbFetch(dbi_result, n = Inf)
        
        if (RMariaDB::dbGetRowCount(dbi_result) > large_result_set_warning_threshold) {
          output$warning <- renderText("Heads up! Your query generated more than 100,000 results, performance may be slower than usual.")
        } 
        
        dbClearResult(dbi_result)
        
        #convert urls in database to clickable links for DT output
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

