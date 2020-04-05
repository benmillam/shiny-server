library(shiny)
library(DBI)
library(RMariaDB)
library(DT) #JavaScript data tables
library(shinycssloaders) #'loading' icon while query is fetched

state_codes = c("-","AK","AL","AR","AS","AZ","CA","CO","CT","DC","DE","FL","FM","GA","GU","HI","IA","ID","IL","IN","KS","KY","LA","MA","MD","ME","MH","MI","MN","MO","MP","MS","MT","NC","ND","NE","NH","NJ","NM","NV","NY","OH","OK","OR","PA","PR","PW","RI","SC","SD","TN","TX","UT","VA","VI","VT","WA","WI","WV","WY")

ui <- fluidPage(
   
   tags$head(tags$script(src = "populate_search.js")),
  
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
            tags$em("may take 10+ seconds, if no results then the table BELOW the purple table will show as a blank table with headers")
     )
   ),
   fluidRow(
     column(12,
            DTOutput("hifld_record")
     )
   ),
  hr(),
  
  actionButton("showtxt", "correction", onclick = "set_search('correction')"),
  actionButton("showtxt", "detention", onclick = "set_search('detention')"),
  actionButton("showtxt", "youth", onclick = "set_search('youth')"),
  actionButton("showtxt", "prison", onclick = "set_search('prison')"),
  actionButton("showtxt", "justice", onclick = "set_search('justice')"),
  actionButton("showtxt", "juvenile", onclick = "set_search('juvenile')"),
  actionButton("showtxt", "sheriff", onclick = "set_search('sheriff')"),
  actionButton("showtxt", "jail", onclick = "set_search('jail')"),
  actionButton("showtxt", "penitentiary", onclick = "set_search('penitentiary')"),
  actionButton("showtxt", "doc", onclick = "set_search('doc')"),
  actionButton("showtxt", "safety", onclick = "set_search('safety')"),

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

server <- function(input, output, session) {
    
    large_result_set_warning_threshold <- 100000
    
    observe({
      
      query <- parseQueryString(session$clientData$url_search)
      
      url_param_hifld_name = query[["hifld_name"]]
      if (is.null(url_param_hifld_name)){url_param_hifld_name = ""}
      
      url_param_hifld_address = query[["hifld_address"]]
      if (is.null(url_param_hifld_address)){url_param_hifld_address = ""}
      
      url_param_hifld_city = query[["hifld_city"]]
      if (is.null(url_param_hifld_city)){url_param_hifld_city = ""}
      
      url_param_hifld_zip = query[["hifld_zip"]]
      if (is.null(url_param_hifld_zip)){url_param_hifld_zip = ""}
      
      cat(url_param_hifld_name)
      cat(url_param_hifld_address)
      cat(url_param_hifld_city)
      cat(url_param_hifld_zip)
      
      #sloppy hack with setting a global var from within observe() but for now faster than learning the scoping here
      hifld_from_google_sheet <- data.frame(
        NAME = url_param_hifld_name, 
        ADDRESS = url_param_hifld_address, 
        CITY = url_param_hifld_city, 
        ZIP = url_param_hifld_zip
      )
      
      formatted_hifld_table <<- datatable(
        hifld_from_google_sheet,
        options = list(
          dom = 't',
          initComplete = JS("
              function(settings, json) {
              $(this.api().table().header()).css({
              'background-color': '#d1c5e8',
              'color': '#000'
            });
            }")
        )
      )
      
      url_param_state = query[["state_code"]]
      url_param_county = query[["county_text"]]
      
      if (!is.null(url_param_state) && (url_param_state %in% state_codes)) {
        updateTextInput(session, "state_code", value = url_param_state)
      } else {
        updateTextInput(session, "state_code", value = "-")
        output$warning <- renderText("Please select a state!")
      }
      
      if (!is.null(url_param_county)) {
        updateTextInput(session, "county_text", value = url_param_county)
      } else {
        updateTextInput(session, "county_text", value = "")
      }
      
      })
    
    
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
          "SELECT * FROM FrsFacilityB 
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
      
      
      
      output$hifld_record <- 
        renderDT(formatted_hifld_table)
}

# Run the application 
shinyApp(ui = ui, server = server)

