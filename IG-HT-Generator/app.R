library(shiny)

hashtags <- data.frame(
  hashtag = c(
    "#california",
    "#cali",
    "#goldenstate",
    "#caligrammers",
    "#californiaholics",
    "#californian",
    "#unlimitedcalifornia",
    "#californiadreaming",
    "#californiadreamin",
    "#californiaadventure",
    "#visitcalifornia",
    "#explorecalifornia",
    "#californiacoast",
    "#westcoast",
    "#calilife",
    "#californialiving",
    "#californialife",
    "#calivibes",
    "#californiavibes",
    "#westcoast_exposures",
    "#californiaphotographer",
    "#californiaphotography",
    "#californiahistory"
  ),
  theme = c(
    "general",
    "general",
    "general",
    "general",
    "general",
    "general",
    "general",
    "nostalgia, ‘dreamy’, otherwordly, etc.",
    "nostalgia, ‘dreamy’, otherwordly, etc.",
    "travel",
    "travel",
    "travel",
    "coast",
    "coast",
    "lifestyle",
    "lifestyle",
    "lifestyle",
    "lifestyle",
    "lifestyle",
    "photography",
    "photography",
    "photography",
    "history"
  ),
  stringsAsFactors = FALSE
)

themes <- unique(hashtags$theme)

ui <- fluidPage(
  tags$style(
    type = "text/css",
    "div textarea#post_text {width:100%} div.col-sm-12 div.shiny-input-container {width:100%}"
  ),
  h3("IG-HT-Generator"),
  p(
    "Selects one random hashtag from each chosen theme, up to 5 hashtags total."
  ),
  
  fluidRow(column(
    12,
    checkboxGroupInput(
      "theme_select",
      "theme",
      choices = themes,
      selected = "general"
    )
  )),
  
  fluidRow(column(
    12,
    textAreaInput("post_text", "Enter main post text:")
  )),
  
  fluidRow(column(
    12,
    verbatimTextOutput("chosen_hashtags")
  )),
  
  hr()
)


server <- function(input, output, session) {
  
  observe({
    output$chosen_themes <- renderText({
      icons <- paste(input$theme_select, collapse = ", ")
      paste("You chose", icons)
    })
    
    choose_tags <- function(chosen_themes, n) {
      chosen_tags <- character()
      
      #order themes at random, we'll be sampling one hashtag from each theme, in order, up to n hashtags
      #accounts for number of themes chosen <n, = n, >n, and ensuring one hashtag from each randomly chosen (ordered, up to n) theme
      chosen_themes <- chosen_themes[sample(1:n, size = n, replace = FALSE)]
      
      for (i in 1:n) {
        filtered_df <-
          subset(hashtags, subset = hashtags$theme %in% chosen_themes[i])
        print(filtered_df$hashtag)
        chosen_tags[i] <- sample(filtered_df$hashtag, size = 1)
      }
      
      return(chosen_tags)
      
    }
    
    n = 5
    
    if (length(input$theme_select) < n) {
      n = length(input$theme_select)
      
    }
    
    #hack to avoid crashing when no boxes selected... faster than deep dive
    if (length(input$theme_select) == 0) {
      chosen_themes <- c("general")
      n = 1
      chosen_hashtags <-
        choose_tags(chosen_themes = chosen_themes, n = n)
    } else {
      chosen_hashtags <-
        choose_tags(chosen_themes = unique(input$theme_select),
                    n = n)
    }
    
    output$chosen_hashtags <- renderText({
      chosen_hashtags <- paste(chosen_hashtags, collapse = " ")
      final_text <-
        paste0(input$post_text, "\n.\n.\n.\n.\n", chosen_hashtags)
      paste(final_text)
    })
    
  })
  
}

# Run the application
shinyApp(ui = ui, server = server)

