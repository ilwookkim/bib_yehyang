library(shiny)
library(jsonlite)
library(dplyr)
library(DT)

json_data <- readRDS(gzcon(url("https://github.com/ilwookkim/bib_yehyang/raw/main/bib.RDS")))

# json_data <- fromJSON("https://github.com/ilwookkim/bib_yehyang/raw/main/NKRV_ELB_reshape.json")
# json_data$bible <- json_data$bible[!sapply(json_data$bible, is.null)]


parse_verse_input <- function(input_str) {
  parts <- strsplit(input_str, "-")[[1]]
  chapter_verse <- strsplit(parts[1], ":")[[1]]
  chapter <- as.numeric(chapter_verse[1])
  start_verse <- as.numeric(chapter_verse[2])
  
  if (length(parts) > 1) {
    end_verse <- as.numeric(parts[2])
  } else {
    end_verse <- start_verse
  }
  
  return(list(chapter = chapter, start = start_verse, end = end_verse))
}

ui <- fluidPage(
  titlePanel("Bible Verse Viewer"),
  
  sidebarLayout(
    sidebarPanel(
      # First dropdown for book selection
      selectInput("book", 
                  "Select Book:", 
                  choices = c(json_data$bible$book_info$name_ko)),
      
      # Text input for verse selection
      textInput("verse",
                "Enter verse (e.g., 1:1 or 1:1-5):",
                value = "1:1"),
      
      actionButton("submit", "Show Verses")
    ),
    
    mainPanel(
      DTOutput("verseTable")
    )
  )
)

server <- function(input, output, session) {
  
  # Rest of the server logic remains the same
  parse_verse_input <- function(input_str) {
    parts <- strsplit(input_str, "-")[[1]]
    chapter_verse <- strsplit(parts[1], ":")[[1]]
    chapter <- as.numeric(chapter_verse[1])
    start_verse <- as.numeric(chapter_verse[2])
    
    if (length(parts) > 1) {
      end_verse <- as.numeric(parts[2])
    } else {
      end_verse <- start_verse
    }
    
    return(list(chapter = chapter, start = start_verse, end = end_verse))
  }
  
  output$verseTable <- renderDT({
    input$submit
    
    req(input$book, input$verse)
    
    # Find selected book
    selected_book <- json_data$bible$content[[which(json_data$bible$book_info$name_ko %in% input$book)]]
    
    
    
    # Parse verse input
    verse_info <- parse_verse_input(input$verse)
    
    # Get chapter content
    chapter_content <- selected_book$verses[[verse_info$chapter]]
    
    # Select verses
    if(!is.na(verse_info$start)){
      verses <- chapter_content[verse_info$start:verse_info$end,]
    } else {
      verses <- chapter_content
    }
    
    # Format output
    result_df <- t(data.frame(
      Reference_ko = paste0(input$book,input$verse),
      Verse_ko = paste(paste0(verses$verse,". ", verses$text_ko), collapse = " "),
      Reference_de = paste(json_data$bible$book_info$name_de[which(json_data$bible$book_info$name_ko %in% input$book)],input$verse),
      Verse_de = paste(paste0(verses$verse,". ", verses$text_de), collapse = " "),
      stringsAsFactors = FALSE
    ))
    
    # Return formatted table
    datatable(result_df, 
              options = list(
                pageLength = 25,
                dom = 't',
                ordering = FALSE
              ),
              rownames = FALSE,
              colnames = F) %>%
      formatStyle(columns = 1:2, 
                  fontSize = '14px',
                  fontFamily = 'Arial')
    
    
  })
}

shinyApp(ui = ui, server = server)