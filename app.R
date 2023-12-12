# STAT 302 Final Project

# load package(s)
library(shiny)
library(shinyWidgets)
library(tidyverse)
library(skimr)
library(ggthemes)
library(DT)
library(ggalt)
library(htmltools)


# load data
WNBA_stats <- read.csv("data/wnba-player-stats.csv") %>% 
  janitor::clean_names() %>% 
  mutate(tm = recode(tm, 
                     "ATL" = "Atlanta Dream",
                     "HOU" = "Houston Comets",
                     "CHI" = "Chicago Sky",
                     "CON" = "Conneticut Sun",
                     "CHA" = "Charlotte Sting",
                     "DET" = "Detroit Shock",
                     "DAL" = "Dallas Wings",
                     "IND" = "Indiana Fever",
                     "LAS" = "Los Angeles Sparks",
                     "LVA" = "Las Vegas Aces",
                     "MIA" = "Miami Sol",
                     "MIN" = "Minnesota Lynx",
                     "NYL" = "New York Liberty",
                     "ORL" = "Orlando Miracle",
                     "PHO" = "Phoenix Mercury",
                     "POR" = "Portland Fire",
                     "SAC" = "Sacramento Monarchs",
                     "SAS" = "San Antonio Stars",
                     "SEA" = "Seattle Storm",
                     "TUL" = "Tulsa Shock",
                     "UTA" = "Utah Starzz",
                     "CLE" = "Cleveland Rockers", 
                     "WAS" = "Washington Mystics"
                     )
         )

# Define UI for application
ui <- fluidPage(
  
  # Creating an Orange Gradient as the Background
  setBackgroundColor(
    color = c("#fbbc92", "#f36f2e"),
    gradient = "linear",
    direction = "bottom"
  ),
  
    # Application title
  titlePanel("The GOATS of the WNBA"),

    # Sidebar where the user can select the year and the team
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "year", 
                    label = "Select Year:", 
                    choices = unique(WNBA_stats$year_id),
                    selected = "2008"
                    ),
      selectInput(inputId = "team", 
                    label = "Select Team:", 
                    choices = unique(WNBA_stats$tm), 
                    selected = "Houston Comets"),
      p("This app intends to highlight 5 female players in the WNBA per year and per team that have made significant strides in women's basketball."),
      p(em("* To avoid a blank plot, please view WNBA Team History to see when some teams were created and when some teams disbanded."))
            ),

    # Main Panel with different tabsets that show lollipop charts and a general player stats table
  mainPanel(
    tabsetPanel(
      tabPanel("WNBA Team History", 
               img(src = "https://upload.wikimedia.org/wikipedia/en/timeline/38lvsz4yvm24rvc850rr1cl34xfi552.png"), 
               tags$div(
                 style = "font-size: 18px; font-weight: bold; color: black;",
                 tags$p(),
                 "This image displays the membership timeline of the WNBA teams throughout history. Since some teams were created in certain years and other teams were disbanded in other years, it's important to understand the team history of the WNBA while interacting with this app.")
               ), 
      tabPanel("Top 5 Player Efficiency Ratings", 
               plotOutput("per_chart")
               ),
      tabPanel("5 WNBA Players that Play the Most", 
               plotOutput("mp_chart")
               ),
      tabPanel("Player Table", 
               DT::dataTableOutput(outputId = "wnba_player_table")
               ),
      tabPanel("Additional Information", 
               textOutput("project_info")
               )
      )
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # Explaining the Additional Information for my Project and Answering Relevant Questions
  output$project_info <- renderText({
    "
    I created this shiny app as a way for me to learn more about the history of the WNBA and some of the greatest players. 
    I chose to use widgets where users could select a year and a team in order to showcase the talents and skills of some of the most efficient players in the WNBA throughout the years per team. I wanted to tell an overall story that there are some truly phenomenal players in the WNBA, regardless of an overall smaller WNBA league (in comparison to the NBA). 
    For the PLayer Efficiency Rating Lollipop Chart, I wanted to highlight the top 5 players with the best player efficiency rating, indicating the most efficient and impactful players in terms of overall performance. With this chart, users are able to identify standout players who contribute significantly to their teams.
    For the Minutes Played Lollipop Chart, I wanted to highlight the top 5 players with the highest number of minutes played in an overall season. With this chart, users can identify the players who are heavily relied upon and consistently stay on the court for extended periods, indicating their endurance and importance to their teams.
    Overall, Players with the greatest player efficiency rating and high minutes played are likely key contributors to their teams' success, as they have more opportunities to make an impact and contribute to various aspects of the game. Thus, they are the GOATS of the WNBA, and it's time that society show some love and appreciation towards them!
    If I had more time, I would continue tweaking the aesthetics of my graphs and the overall app. But I am proud of the work I have contributed thus far!
    The dataset I used to complete this project was obtained from FiveThirtyEight Github Repo on WNBA-stats
    "
  })
  

  # Filter the player stats based on user input
  filter_stats <- reactive({
    WNBA_stats %>%
      filter(year_id == input$year, tm == input$team) %>%
      select("player", 
             "per", 
             "mp", 
             "ts_pct", 
             "thr_p_ar",
             "f_tr", 
             "orb_pct", 
             "trb_pct", 
             "ast_pct", 
             "stl_pct", 
             "ows", 
             "dws", 
             "ws") %>%
      rename("Player" = "player",
             "Player Efficiency Rating" = "per",
             "Minutes Played" = "mp", 
             "True Shooting %" = "ts_pct",
             "Three Point Attempt Rate" = "thr_p_ar",
             "Free Throw Rate" = "f_tr",
             "Offensive Rebound %"= "orb_pct",
             "Total Rebound %" = "trb_pct",
             "Assist %" = "ast_pct",
             "Steal %" = "stl_pct",
             "Offensive Wins Shared" = "ows",
             "Defensive Wins Shared" = "dws",
             "Total Wins Shared" = "ws") %>% 
      arrange(desc("Total Wins Shared"))
  })
  
  # Create the WNBA Player Stats Table
  output$wnba_player_table <- DT::renderDataTable({
    datatable(
      filter_stats(),
      options = list(
        scrollX = TRUE,
        scrollY = "400px",
        dom = 't',
        initComplete = JS(
          "function(settings, json) {",
          "$(this.api().table().header()).css({'background-color': '#fff', 'color': '#000'});",
          "}")
      )
    )
  })
  
  # Generate the lollipop chart of top 5 players by PER
  output$per_chart <- renderPlot({
    
    top_per_players <- head(filter_stats() %>% arrange(desc("Player Efficiency Rating")), 5)
    
    ggplot(top_per_players, aes(x = reorder(Player, `Player Efficiency Rating`), y = `Player Efficiency Rating`)) +
      geom_lollipop(point.colour = "#D84727",
                    point.size = 4) +
      geom_label(aes(label = round(`Player Efficiency Rating`)), hjust = -0.1, size = 5) +
      labs(x = "Player", 
           y = "Player Efficiency Rating (PER)", 
           title = "Top 5 WNBA Players by Player Efficiency Rating") +
      theme_fivethirtyeight() +
      theme(axis.text.x = element_text(size = 15),
            axis.title.x = element_blank(),
            axis.title.y = element_text(size = 14),
            title = element_blank(), 
            axis.text.y = element_text(size = 15))
  })
  
  output$mp_chart <- renderPlot({
    
    top_mp_players <- head(filter_stats() %>% arrange(desc(`Minutes Played`)), 5)
    
    ggplot(top_mp_players, aes(x = reorder(Player, `Minutes Played`), y = `Minutes Played`)) +
      geom_lollipop(point.colour = "#D84727",
                    point.size = 4) +
      geom_label(aes(label = `Minutes Played`), hjust = -0.1, size = 5) +
      labs(x = "Player", 
           y = "Minutes Played", 
           title = "Top 5 WNBA Players by Number of Minutes Played") +
      theme_fivethirtyeight() +
      theme(axis.text.x = element_text(size = 15),
            axis.title.x = element_blank(),
            axis.title.y = element_text(size = 14),
            title = element_blank(), 
            axis.text.y = element_text(size = 15))
  })
    
}
# Run the application 
shinyApp(ui = ui, server = server)
