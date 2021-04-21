#' @title shiny-nhl-events
#' @author Jordan Navin, OTH Analytics
#' 04-11-2021

#---------------#
# imports ------#
#---------------#
library(ggplot2)
source("oth_gg_rink.R")
library(DBI)
source(".Renviron")
library(dplyr)
library(RColorBrewer)
library(plotly)
library(glue)
library(shiny)
library(h2o)
library(shinythemes)

#---------------#
# pre loading --#
#---------------#

# make a connection to the db
con <- DBI::dbConnect(RPostgres::Postgres(),
  dbname = "main",
  host = "oth-hockey.cjsjvaerjq8q.us-east-2.rds.amazonaws.com",
  user = OTH_DB_USER,
  password = OTH_DB_PW
)

# we are going to have to load this before the app starts
df_raw <- DBI::dbGetQuery(con, "SELECT * FROM game_events")

# rename tampa bay
df_raw[df_raw$Ev_Team == "T.B", "Ev_Team"] <- "TBL"

# fix coordinates to be on one side of the rink
df <- df_raw %>%
  mutate(
    xC = ifelse(xC < 0, -xC, xC),
    yC = ifelse(xC < 0, -yC, yC)
  )


#---------------#
# ui -----------#
#---------------#

ui <- fluidPage(
  theme = shinytheme("lumen"),
  titlePanel(HTML("shiny-nhl-events v0.1 <br> 2021 NHL season"),
    windowTitle = "shiny-nhl-events v0.1"
  ),
  sidebarLayout(
    position = "right",
    sidebarPanel(
      selectInput("event_type",
        label = "Event Type",
        choices = c(
          "SHOT",
          "GOAL",
          "HIT",
          "MISS",
          "BLOCK"
          # "GIVE",
          # "TAKE"
        ),
        selected = "GOAL"
      ),
      selectInput("team",
        label = "Team",
        choices = c(
          " ",
          "ANA",
          "ARI",
          "BOS",
          "BUF",
          "CAR",
          "CBJ",
          "CGY",
          "CHI",
          "COL",
          "DAL",
          "DET",
          "EDM",
          "FLA",
          "LAK",
          "MIN",
          "MTL",
          "NJD",
          "NSH",
          "NYI",
          "NYR",
          "OTT",
          "PHI",
          "PIT",
          "SJS",
          "STL",
          "TBL",
          "TOR",
          "VAN",
          "VGK",
          "WPG",
          "WSH"
        ),
        selected = " "
      ),
      selectInput("player",
        label = "Player",
        choices = " "
      ),
      # TODO: build period functionality
      # selectInput('period',
      #             label = 'Period',
      #             choices = c(" ",
      #                         1,
      #                         2,
      #                         3,
      #                         "OT"
      #                         ),
      #             selected = " "),
      tags$footer(
        div(
          img(src = "logo.png", height = 150, width = 150)
        ),
        HTML("Disclaimer: All data unofficial <br> All rights reserved <br>"),
        a(href = "https://github.com/onthehillanalytics", HTML("GitHub"))
      )
    ),
    mainPanel(plotlyOutput("main")),
  )
)
#----------------#
# server --------#
#----------------#
server <- function(input, output, session) {

  # add an observe here to update the selectInput of player choices when a team is selected
  observeEvent(input$team, {
    # do it only if we have a non blank team selected
    if (input$team != " ") {
      team_id <- DBI::dbGetQuery(
        con,
        glue("SELECT team_id FROM team_meta WHERE
                                    abbreviation = '{input$team}'")
      )
      # going to query the database to get a list of players for the team selected
      player_choices <- as.vector(DBI::dbGetQuery(
        con,
        glue("SELECT DISTINCT \"fullName\",
                                      \"player_id\"
                                      FROM player_meta WHERE
                                      \"currentTeam_id\" = {team_id}")
      ))
      # update choices with players
      updateSelectInput(session, "player",
        choices = rlang::prepend(player_choices$fullName, " ")
      )
    }
  })


  plot_df <- reactive({
    # if only event and no other parameters
    if (input$team == " ") {
      df[df$Event == input$event_type, ]
      # if team selected, filter on team
    } else if (input$team != " " && input$player == " ") {
      df[(df$Event == input$event_type) & (df$Ev_Team == input$team), ]
      # if we have a player and the event type is block, access correct id (p2)
    } else if (input$team != " " && input$player != " " &&
      input$event_type == "BLOCK") {
      player_id <- DBI::dbGetQuery(
        con,
        glue("SELECT
                      player_id FROM
                         player_meta WHERE \"fullName\" = '{input$player}'")
      )
      df[(df$Event == input$event_type) & (df$Ev_Team == input$team) &
        (df$p2_ID == as.character(player_id[[1]])), ]
      # if it's any other type of event, we use the other id (p1)
    } else if (input$team != " " && input$player != " " &&
      input$event_type != "BLOCK") {
      player_id <- DBI::dbGetQuery(
        con,
        glue("SELECT
                      player_id FROM
                         player_meta WHERE \"fullName\" = '{input$player}'")
      )
      df[(df$Event == input$event_type) & (df$Ev_Team == input$team) &
        (df$p1_ID == as.character(player_id[[1]])), ]
    }
  })

  # render the main plot
  output$main <- renderPlotly({
    ggplotly(ggplot(plot_df(), aes(x = xC, y = yC)) +
      geom_count(alpha = 0.8, show.legend = F, colour = "#b4ecb4") +
      gg_rink(), height = 800, width = 900)
  })
}

shinyApp(ui, server)
