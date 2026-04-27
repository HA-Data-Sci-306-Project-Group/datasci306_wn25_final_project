library(shiny)
library(tidyverse)

imdb_app_data <- title_ratings_genre %>%
  filter(
    !is.na(startYear),
    !is.na(averageRating),
    !is.na(numVotes),
    !is.na(genre),
    !is.na(titleType)
  )

ui <- fluidPage(
  titlePanel("Interactive IMDb Explorer"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput(
        "genre",
        "Choose genre:",
        choices = sort(unique(imdb_app_data$genre)),
        selected = "Drama",
        multiple = TRUE
      ),
      
      sliderInput(
        "year_range",
        "Choose year range:",
        min = min(imdb_app_data$startYear),
        max = max(imdb_app_data$startYear),
        value = c(1980, max(imdb_app_data$startYear)),
        sep = ""
      ),
      
      sliderInput(
        "min_votes",
        "Minimum number of votes:",
        min = 0,
        max = as.numeric(quantile(imdb_app_data$numVotes, 0.95, na.rm = TRUE)),
        value = 100,
        step = 50
      ),
      
      checkboxInput(
        "log_votes",
        "Use log scale for votes",
        value = TRUE
      )
    ),
    
    mainPanel(
      plotOutput("rating_votes_plot"),
      plotOutput("ratings_by_year_plot"),
      tableOutput("top_titles_table")
    )
  )
)

server <- function(input, output) {
  
  filtered_data <- reactive({
    imdb_app_data %>%
      filter(
        genre %in% input$genre,
        startYear >= input$year_range[1],
        startYear <= input$year_range[2],
        numVotes >= input$min_votes
      )
  })
  
  output$rating_votes_plot <- renderPlot({
    p <- filtered_data() %>%
      ggplot(aes(x = averageRating, y = numVotes, color = genre)) +
      geom_point(alpha = 0.6) +
      labs(
        title = "IMDb Rating vs Number of Votes",
        x = "Average Rating",
        y = "Number of Votes"
      ) +
      theme_minimal()
    
    if (input$log_votes) {
      p <- p + scale_y_log10()
    }
    
    p
  })
  
  output$ratings_by_year_plot <- renderPlot({
    yearly <- filtered_data() %>%
      group_by(startYear, genre) %>%
      summarise(
        mean_rating = mean(averageRating, na.rm = TRUE),
        n_titles = n(),
        .groups = "drop"
      )
    
    ggplot(yearly, aes(x = startYear, y = mean_rating, color = genre)) +
      geom_line(linewidth = 1) +
      labs(
        title = "Average Rating by Year",
        x = "Start Year",
        y = "Mean Rating"
      ) +
      theme_minimal()
  })
  
  output$top_titles_table <- renderTable({
    filtered_data() %>%
      select(primaryTitle, titleType, startYear, genre, averageRating, numVotes) %>%
      arrange(desc(averageRating), desc(numVotes)) %>%
      distinct(primaryTitle, startYear, genre, .keep_all = TRUE) %>%
      head(25)
  })
}

shinyApp(ui = ui, server = server)