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

df_elos_historic <- read_rds("elos.rds")

df_competitions <- read_competitions(token) %>% 
  mutate(
    Country = case_when(
      str_detect(caption, "Bundesliga") ~ "GER", 
      str_detect(caption, "Brasileiro") ~ "BRA",
      str_detect(caption, "(League)|(Championship)") ~ "ENG",
      str_detect(caption, "Eredivisie") ~ "NED", 
      str_detect(caption, "Ligue") ~ "FRA",
      str_detect(caption, "Serie") ~ "ITA", 
      str_detect(caption, "Division") ~ "ESP", 
      str_detect(caption, "\\sLiga") ~ "POR",
      TRUE ~ NA_character_), 
    Level = case_when(
      str_detect(caption, "(1\\s)|(1\\.)|(^Pr)|(\\sA)") ~ 1, 
      str_detect(caption, "(2\\s)|(2\\.)|(Championship)|(\\sB)") ~ 2, 
      str_detect(caption, "One") ~ 3,
      str_detect(caption, "Two") ~ 4,
      TRUE ~ 1))

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
        plotOutput("league_elos", height = "900px")
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
          picture = paste0("<img src='", crestURI, "' height='19'></img>"))) 
  })
  
  output$league_table <- renderDataTable({
    league_table() %>% 
      select(
        picture, team, points, goals, goalsAgainst, goalDifference,
        playedGames) %>% 
      datatable(
        filter = "none", escape = FALSE, 
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
    team_filter <- df_elos %>% 
      filter(
        Country == df_competitions$Country[df_competitions$id == id()], 
        Level == df_competitions$Level[df_competitions$id == id()]) %>% 
      select(Club)
    
    elo_plot <- team_filter %>% 
      left_join(df_elos_historic, by = "Club") %>% 
      mutate(Club = fct_inorder(Club)) %>% 
      filter(
        year(To) >= year(today() - dyears(2)), To < today()) %>% 
      ggplot(aes(x = To, y = Elo, color = Club)) +
        geom_line() +
        theme(legend.position = "bottom") +
        labs(title = "Last 2 years of Elo developement") +
        theme_bw()
  
    return(elo_plot)
  })
}

shinyApp(ui, server)