library(shiny)
library(tidyverse)

principal_app_data <- title_principals_ratings %>%
  filter(
    !is.na(category),
    !is.na(primaryTitle),
    !is.na(averageRating),
    !is.na(numVotes)
  )

ui <- fluidPage(
  titlePanel("IMDb Principals Explorer"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput(
        "category",
        "Select job category:",
        choices = sort(unique(principal_app_data$category)),
        selected = "actor",
        multiple = TRUE
      ),
      
      sliderInput(
        "min_rating",
        "Minimum IMDb rating:",
        min = 0,
        max = 10,
        value = 7,
        step = 0.1
      ),
      
      sliderInput(
        "min_votes",
        "Minimum number of votes:",
        min = 0,
        max = as.numeric(quantile(principal_app_data$numVotes, 0.95, na.rm = TRUE)),
        value = 100,
        step = 50
      )
    ),
    
    mainPanel(
      h3("Average Rating by Selected Job Category"),
      plotOutput("category_rating_plot"),
      
      h3("Titles Matching Selected Categories"),
      tableOutput("category_table")
    )
  )
)

server <- function(input, output) {
  
  filtered_principals <- reactive({
    principal_app_data %>%
      filter(
        category %in% input$category,
        averageRating >= input$min_rating,
        numVotes >= input$min_votes
      )
  })
  
  output$category_rating_plot <- renderPlot({
    filtered_principals() %>%
      group_by(category) %>%
      summarise(
        mean_rating = mean(averageRating, na.rm = TRUE),
        n_titles = n(),
        .groups = "drop"
      ) %>%
      ggplot(aes(x = reorder(category, mean_rating), y = mean_rating)) +
      geom_col() +
      coord_flip() +
      labs(
        x = "Job Category",
        y = "Mean IMDb Rating",
        title = "Mean Rating by Principal Job Category"
      ) +
      theme_minimal()
  })
  
  output$category_table <- renderTable({
    filtered_principals() %>%
      select(
        primaryName,
        category,
        primaryTitle,
        titleType,
        startYear,
        averageRating,
        numVotes
      ) %>%
      arrange(desc(averageRating), desc(numVotes)) %>%
      head(25)
  })
}

shinyApp(ui = ui, server = server)