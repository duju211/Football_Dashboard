## app.R ##
library(shinydashboard)
library(elor)
library(tidyverse)
library(DT)
library(stringr)
library(lubridate)
token <- read_lines("token.txt")

# Read data ---------------------------------------------------------------
df_elos <- elo_day() %>% 
  left_join(df_elo_dict[,c("Club", "id")], by = "Club")

df_competitions <- read_competitions(token) 

# Definition of UI --------------------------------------------------------
ui <- dashboardPage(
  dashboardHeader(title = "Football Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem(
        "Select League", tabName = "select_league", selected = TRUE, 
        selectInput(
          "league", "Choose a league:", df_competitions$caption, 
          selected = "1. Bundesliga 2017/18"
        )), 
      menuItem(
        "Tables", tabName = "league_table", icon = icon("table")), 
      menuItem(
        "Results / Fixtures", tabName = "league_fixtures",
        icon = icon("futbol-o")), 
      menuItem(
        "Elos", tabName = "elos", icon = icon("balance-scale")
      )
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "league_table",
        fluidRow(
          column(
            12, dataTableOutput("league_table"))
        )
      ),
      tabItem(
        tabName = "league_fixtures",
        fluidRow(
          column(
            12, dataTableOutput("league_fixtures")
          )
        )
      ), 
      tabItem(
        tabName = "elos", 
        plotOutput("league_elos")
      )
    )
  )
)

# Server Logic ------------------------------------------------------------
server <- function(input, output) {
  # Reactive Values ---------------------------------------------------------
  id <- reactiveVal()
  league_table <- reactiveVal()
  league_table(tibble())
  
  observeEvent(input$league, {
    id(df_competitions$id[df_competitions$caption == input$league])
    league_table(
      read_competition_table(token, id()) %>% 
        mutate(
          picture = paste0("<img src='", crestURI, "' height='19'></img>")) %>% 
        select(
          picture, team, points, goals, goalsAgainst, goalDifference,
          playedGames))
  })
  
  output$league_table <- renderDataTable({
    datatable(
      league_table(), filter = "none", escape = FALSE, 
      options = list(
        pageLength = 50, 
        columnDefs = list(list(className = 'dt-center', targets = 2))))
  })
  
  output$league_fixtures <- renderDataTable({
    league_fixtures <- read_fixtures(token, id()) %>% 
      mutate(
        score = paste0(result.goalsHomeTeam, ":", result.goalsAwayTeam)) %>% 
      gather(key = "home_away", value = "team_id", homeTeamId, awayTeamId) %>% 
      mutate(home_away = str_replace(home_away, "TeamId", "Elo")) %>% 
      left_join(df_elos[,c("id", "Elo")], by = c("team_id" = "id")) %>% 
      select(-team_id) %>% 
      spread(home_away, Elo) %>% 
      mutate(
        prob = map2_dbl(homeElo, awayElo, possibly(prob_a_beats_b, 0.5)), 
        pred_result = case_when(
          prob > 0.5 ~ "home", 
          prob == 0.5 ~ "draw",
          TRUE ~ "away"), 
        pred_score = map2_chr(pred_result, prob, pred_score))
    
    datatable(
      filter = "top", extensions = "Buttons", options = list(
        dom = 'Bfrtip', pageLength = 20,
        buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
      ), 
      league_fixtures %>% 
        select(everything(), -contains("id"), -contains("result")))
  })
  
  output$league_elos <- renderPlot({
    elo_plot <- df_elo_dict %>%
      filter(Club %in% league_table()$team) %>% 
      left_join(df_elos) %>% 
      arrange(desc(Elo)) %>% 
      ggplot(aes(x = fct_inorder(Club), y = Elo)) +
        geom_col()
    
    return(elo_plot)
  })
}

shinyApp(ui, server)