library(shiny)
library(tidyverse)

# ---- Data & Helpers (do not modify) ----

movie_actors <- read_csv("movie_actors.csv", show_col_types = FALSE)

# Lookup tables for fast access
actor_to_movies <- movie_actors |>
  group_by(actor) |>
  summarise(movies = list(movie), .groups = "drop") |>
  deframe()

movie_to_actors <- movie_actors |>
  group_by(movie) |>
  summarise(actors = list(actor), .groups = "drop") |>
  deframe()

non_bacon_actors <- setdiff(sort(unique(movie_actors$actor)), "Kevin Bacon")

# Sampling weights: proportional to number of movies (favors recognizable actors)
actor_weights <- lengths(actor_to_movies)[non_bacon_actors]

# Helper: get movies for an actor, sorted by release year (extracted from the
# "Title (YYYY)" suffix)
get_movies <- function(actor_name) {
  m <- actor_to_movies[[actor_name]]
  years <- as.integer(str_extract(m, "(?<=\\()\\d{4}(?=\\)$)"))
  m[order(years, m)]
}

# Helper: get cast for a movie
get_cast <- function(movie_name) {
  sort(movie_to_actors[[movie_name]])
}

# ---- UI (do not modify) ----

ui <- fluidPage(
  titlePanel("Six Degrees of Kevin Bacon"),
  sidebarLayout(
    sidebarPanel(
      actionButton("new_game", "New Game"),
      hr(),
      textOutput("step_display"),
      hr(),
      radioButtons("selection", "Make a choice:", choices = c("Click 'New Game' to start")),
      actionButton("choose", "Select")
    ),
    mainPanel(
      h4("Your Path"),
      htmlOutput("path_display"),
      hr(),
      h3(textOutput("result_message"))
    )
  )
)

# ---- Server ----

server <- function(input, output, session) {
  
  # Game state — use these reactive values in your TODOs below.
  # - active: TRUE while a game is in progress
  # - phase: "actor" when the player is choosing an actor,
  #          "movie" when choosing a movie
  # - step: number of actors selected so far (0 to 6)
  # - path: character vector of the trail, alternating actor/movie names
  # - visited: character vector of actors already chosen (to exclude from casts)
  # - visited_movies: character vector of movies already chosen (to exclude
  #   from future movie option lists)
  # - won / lost: flags for game outcome
  game <- reactiveValues(
    active = FALSE,
    phase  = "actor",
    step   = 0L,
    path   = character(),
    visited = character(),
    visited_movies = character(),
    won    = FALSE,
    lost   = FALSE
  )
  
  # Step counter display (do not modify)
  output$step_display <- renderText({
    if (!game$active && !game$won && !game$lost) return("")
    paste0("Step ", game$step, " of 6")
  })
  
  # ============================================================
  # TODO 1: New Game button
  # ============================================================
  # When input$new_game is clicked:
  #   - Set game$active to TRUE, game$won and game$lost to FALSE
  #   - Reset game$step to 0, game$path, game$visited, and game$visited_movies
  #     to empty character()
  #   - Set game$phase to "actor"
  #   - Sample 5 actors from non_bacon_actors with probability proportional
  #     to their number of movies. Pass prob = actor_weights to sample().
  #   - Use updateRadioButtons() to set those actors as the choices for
  #     the "selection" input. The syntax is:
  #       updateRadioButtons(session, "selection", choices = your_vector)
  
  observeEvent(input$new_game, {
    game$active <- TRUE
    game$won <- FALSE
    game$lost <- FALSE
    
    game$step <- 0L
    game$path <- character()
    game$visited <- character()
    game$visited_movies <- character()
    
    game$phase <- "actor"
    
    sample_actors <- sample(non_bacon_actors, size = 5, prob = actor_weights)
    
    updateRadioButtons(session, "selection", choices = sample_actors)
  })
  
  
  # ============================================================
  # TODO 2: Select button (main game logic)
  # ============================================================
  # When input$choose is clicked:
  #   Use req(game$active, input$selection) to guard the handler.
  #
  #   If game$phase == "actor":
  #     - Add the chosen actor (input$selection) to game$path and game$visited
  #     - Increment game$step by 1
  #     - If the chosen actor is "Kevin Bacon": set game$won to TRUE,
  #       game$active to FALSE, and return()
  #     - If game$step >= 6: set game$lost to TRUE, game$active to FALSE,
  #       and return()
  #     - Otherwise: get the actor's movies using get_movies(), remove any
  #       already-visited movies with setdiff(movies, game$visited_movies),
  #       update the radio buttons with updateRadioButtons(), and set
  #       game$phase to "movie". (Note: get_movies() already returns movies
  #       sorted by release year, so the options appear in chronological order.)
  #
  #   If game$phase == "movie":
  #     - Add the chosen movie (input$selection) to game$path AND to
  #       game$visited_movies
  #     - Get the cast using get_cast()
  #     - Remove already-visited actors with setdiff(cast, game$visited)
  #     - Update the radio buttons with the filtered cast
  #     - Set game$phase to "actor"
  
  observeEvent(input$choose, {
    req(game$active, input$selection)
    
    if (identical(game$phase, "actor")) {
      chosen_actor <- input$selection
      
      game$path <- c(game$path, chosen_actor)
      game$visited <- c(game$visited, chosen_actor)
      game$step <- game$step + 1L
      
      if (identical(chosen_actor, "Kevin Bacon")) {
        game$won <- TRUE
        game$active <- FALSE
        return()
      }
      
      if (game$step >= 6L) {
        game$lost <- TRUE
        game$active <- FALSE
        return()
      }
      
      movies <- get_movies(chosen_actor)
      movies <- setdiff(movies, game$visited_movies)
      
      updateRadioButtons(session, "selection", choices = movies)
      game$phase <- "movie"
      
    } else if (identical(game$phase, "movie")) {
      chosen_movie <- input$selection
      
      game$path <- c(game$path, chosen_movie)
      game$visited_movies <- c(game$visited_movies, chosen_movie)
      
      cast <- get_cast(chosen_movie)
      cast <- setdiff(cast, game$visited)
      
      updateRadioButtons(session, "selection", choices = cast)
      game$phase <- "actor"
    }
  })
  
  # ============================================================
  # TODO 3: Path display
  # ============================================================
  # Render the path as formatted HTML using renderUI().
  #   - If game$path is empty, show: tags$p(tags$em("No moves yet."))
  #   - Otherwise, loop through game$path:
  #       - Odd-indexed entries are actors: wrap in tags$strong()
  #       - Even-indexed entries are movies: wrap in tags$em()
  #     Separate entries with arrows: HTML(" &rarr; ")
  #   - Combine everything with do.call(tagList, ...)
  output$path_display <- renderUI({
    if (length(game$path) == 0) return(tags$p(tags$em("No moves yet.")))
    
    pieces <- list()
    for (i in seq_along(game$path)) {
      entry <- game$path[[i]]
      
      node <- if (i %% 2 == 1) tags$strong(entry) else tags$em(entry)
      pieces <- c(pieces, list(node))
      
      if (i < length(game$path)) {
        pieces <- c(pieces, list(HTML(" &rarr; ")))
      }
    }
    
    do.call(tagList, pieces)
  })
  
  # ============================================================
  # TODO 4: Result message
  # ============================================================
  # Use renderText() to show:
  #   - If game$won: a message like "You found Kevin Bacon in X steps!"
  #   - If game$lost: "Game over! You didn't reach Kevin Bacon in 6 steps."
  #   - Otherwise: "" (empty string)
  output$result_message <- renderText({
    if (isTRUE(game$won)) {
      paste0("You found Kevin Bacon in ", game$step, " steps!")
    } else if (isTRUE(game$lost)) {
      "Game over! You didn't reach Kevin Bacon in 6 steps."
    } else {
      ""
    }
  })
  
}
shinyApp(ui, server)
