
library(shiny)
library(shinythemes)
library(jsonlite)
library(httr)
library(stringr)
library(tidyverse)
library(date)
library(lubridate)
library(dplyr)
library(readr)
library(rsconnect)

# All possible Point Values from Jeopardy
point_values <- c(100, 200, 300, 400, 500, 600, 800, 1000)

# Pre-selected ID's from jService API. These were used to call the data
ids <- c(
  5412, 11496, 11498, 11499, 11504, 11544, 11521, 7580, 11522, 11523, 11512, 11513, 11514,
  11515, 11516, 11517, 11501, 11502, 11503
)

# Initializing variables
clues <- list("")
clue_ids <- c()
answers <- c()
questions <- c()
values <- c()
category_ids <- c()
category_titles <- c()
airdates <- c()

# Loop to call data and store in respective variables (with formatting)
for (i in 1:length(ids)) {
  endpoint <- str_glue("http://jservice.io/api/clues/?category={id}", id = ids[i])
  r <- GET(endpoint, query = list(count = 100))
  stop_for_status(r)
  clues[[i]] <- as.data.frame(fromJSON(content(r, as = "text", encoding = "UTF-8"), flatten = TRUE))
  clue_ids <- append(clue_ids, clues[[i]]$id)
  answers <- append(answers, clues[[i]]$answer)
  questions <- append(questions, clues[[i]]$question)
  values <- append(values, clues[[i]]$value)
  category_ids <- append(category_ids, clues[[i]]$category_id)
  category_titles <- append(category_titles, clues[[i]]$category.title)
  airdates <- append(airdates, (str_extract(clues[[i]]$airdate, "\\d+-\\d+-\\d+")))
}

# Used in category input options
category_titles_unique <- unique(category_titles)

# Max and Min dates were used to bound the input Date Range
min_date <- min(airdates)
max_date <- max(airdates)

# Formatting for NA values
values[is.na(values)] <- 0

# Cleaning Data
clue_ids <- clue_ids[-1:-10]
answers <- answers[-1:-10]
questions <- questions[-1:-10]
values <- values[-1:-10]
category_ids <- category_ids[-1:-10]
airdates <- airdates[-1:-10]

# Final Data
data <- data.frame(clue_ids, answers, questions, values, category_ids, category_titles, airdates)

# UI section for shiny app
ui <- shinyUI(fluidPage(

  # This presents theme options. Can be changed in the app
  shinythemes::themeSelector(),
  titlePanel("Jeopardy Trivia"),
  sidebarLayout(
    sidebarPanel(

      # Input Point Value
      selectizeInput(
        inputId = "point_value", label = "Select Point Value",
        choices = c(
          "Select Point Value", point_values
        ),
        selected = "Select Point Value"
      ),

      # Input Category
      selectizeInput(
        inputId = "category_value", label = "Select Category",
        choices = c("Select Category", category_titles_unique)
      ),

      # Input Data Range
      dateRangeInput("date",
        "Select Date Range",
        start = "2001-03-14",
        end = "2011-07-27",
        min = "2001-03-14",
        max = "2011-07-27"
      ),

      # Input for Random and Test Mode. Default is FALSE or de-selected
      checkboxInput(inputId = "random", label = "Random Mode", value = FALSE),
      checkboxInput(inputId = "test", label = "Test Mode", value = FALSE)
    ),

    # Structure of main page
    mainPanel(
      textOutput("randText"),
      DT::dataTableOutput("out"),
      textOutput("testText"),
      dataTableOutput("random"),
      dataTableOutput("test"),
      textOutput("randAndTest"),

      # Source
      tags$a(href = "http://www.jservice.io/", "Source: JService API")
    )
  )
))

# Server section for shiny app

server <- function(input, output, session) {

  # This reactive function controls the data the appears at the beginning and
  # when Random and Test Mode are de-selected.
  search_reactive <- reactive({

    # Filtering data for inputted Point Value
    if (!is.null(input$point_value)) {
      if (input$point_value != "Select Point Value") {
        data <- data %>%
          filter(values == input$point_value)
      }
    }

    # Filtering data for inputted Category
    if (input$category_value != "Select Category") {
      data <- data %>%
        filter(category_titles == input$category_value)
    }

    # Filtering data for inputted Date Range
    if (!is.null(input$date)) {
      data <- data %>%
        filter(airdates <= input$date[2] & airdates >= input$date[1])
    }

    # Displaying filtered data
    data %>%
      select(questions, values, airdates, category_titles, answers)
  })

  # This reactive function controls the Test Mode
  test_reactive <- reactive({

    # Check to see if Test Mode is selected and Random Mode is not
    if (!is.null(input$test)) {
      if (input$random == FALSE) {
        test_question <- data %>%
          filter(values == sample(point_values, 1)) %>%
          sample_n(1) %>%
          select(questions, category_titles, answers)
      }
    }

    # Display data
    test_question
  })

  # This reactive function controls the Random Mode
  random_reactive <- reactive({

    # Check to see if Random Mode is selected and Test Mode is not
    if (!is.null(input$random)) {
      if (input$test == FALSE) {
        random_data <- data %>%
          filter(!is.na(questions), !is.na(answers), airdates == sample(unique(airdates), 1)) %>%
          select(questions, values, airdates, category_titles, answers) %>%
          arrange(category_titles)
      }
    }

    # Display data
    random_data
  })

  # This function is for describing Random Mode
  output$randText <- renderText({
    if (input$random == TRUE & input$test == FALSE) {
      "Results are randomly generated for a jeopardy episode of a particular day."
    }
  })

  # This function is connected to the timer for Test Mode
  EventTime <- eventReactive(input$test, {
    EventTime <- as.POSIXlt(Sys.time() + 31)
  })

  # This function describes Test Mode and displays a timer of 30 seconds
  output$testText <- renderText({
    if (input$test == TRUE & input$random == FALSE) {
      invalidateLater(1000, session)
      t <- EventTime()
      EventTime <- strptime((t - Sys.time()), "%S")
      paste(
        "Test Question. Time remaining: ",
        EventTime,
        "seconds. The answer will not displayed at the end of the timer."
      )
    }
  })

  # This function outputs the data for search_reactive
  output$out <- DT::renderDataTable({
    if (!input$test & !input$random) {
      data <- search_reactive()
      n <- nrow(data)

      set_data <- data %>%
        filter(!is.na(questions), !is.na(answers)) %>%
        sample_n(n) %>%
        select(questions, values, airdates, category_titles, answers)

      set_data
    }
  })

  # This function informs user to not select both Random and Test Mode
  output$randAndTest <- renderText({
    if (input$random == TRUE) {
      if (input$test == TRUE) {
        paste("No results. Please select either Random or Test Mode")
      }
    }
  })

  # This function outputs the Random Mode data
  output$random <- renderDataTable(
    if (input$random == TRUE & input$test == FALSE) {
      random_reactive()
    }
  )

  # This function outputs the Test Mode data
  output$test <- renderDataTable(
    if (input$test == TRUE & input$random == FALSE) {
      test_question <- test_reactive()
      test_question %>%
        select(questions, category_titles)
    }
  )
}

# Launching the App
shinyApp(ui = ui, server = server)