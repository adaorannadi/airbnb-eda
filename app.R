# Load necessary libraries
library(shiny)
library(shinyWidgets)
library(tidyverse)
library(ggthemes)
library(DT)
library(sf)
library(ggrepel)

# Load data
shopping_behavior <- read_csv("shopping_behavior3.csv")
load("US_income.rda")

# Data preprocessing
US_income$location = US_income$name
cols_to_remove <- c("median_income","median_income_moe","population","area","popdens")
US_income <- US_income[, !names(US_income) %in% cols_to_remove]
shopping_behavior <- left_join(shopping_behavior, US_income, by = "location")

# User Interface (UI)
ui <- fluidPage(
  
  #set backgorund color
  setBackgroundColor(
    color = c("floralwhite")
  ),
  titlePanel("A Customer Demographics Visualization App"),
  
  tabsetPanel(
    # Tab for Customer Demographics Bar Chart
    tabPanel("Customer Demographics Bar Chart",
             sidebarLayout(
               sidebarPanel(
                 selectInput("y_variable", 
                             "Select Y-axis Variable", 
                             choices = c("Age Bins" = "age_bins",
                                         "Category" = "category",
                                         "Gender" = "gender", 
                                         "Frequency of Purchases" = "frequency_of_purchases", 
                                         "Payment Method" = "payment_method",
                                         "Season" = "season", 
                                         "Subscription Status" = "subscription_status"),
                             selected = "gender"
                 ),
                 radioButtons(
                   inputId = "fill_variable", 
                   label = "Select Fill/Legend Variable:",
                   choices = c("Age Bins" = "age_bins",
                               "Category" = "category",
                               "Gender" = "gender", 
                               "Frequency of Purchases" = "frequency_of_purchases", 
                               "Payment Method" = "payment_method",
                               "Season" = "season", 
                               "Subscription Status" = "subscription_status"),
                   selected = "age_bins" 
                 ),
                 p("This app generates a stacked bar chart of consumer demographic variable based on the total purchase amount in a year."),
                 p(em("*Don't select the same variable for each input."))
               ),
               mainPanel(
                 plotOutput("bar_plot")
               )
             )
    ), 
    
    # Tab for Customer Demographic Pie Chart
    
    tabPanel("Customer Demographics Pie Chart",
             sidebarLayout(
               sidebarPanel(
                 selectInput("fill_variable2", 
                             "Select Fill Variable", 
                             choices = c("Age Bins" = "age_bins",
                                         "Category" = "category",
                                         "Gender" = "gender", 
                                         "Frequency of Purchases" = "frequency_of_purchases", 
                                         "Payment Method" = "payment_method",
                                         "Season" = "season", 
                                         "Subscription Status" = "subscription_status"),
                             selected = "age_bins"),
                 p("This app generates a pie chart of consumer demographics based on the total purchase amount in a year.")
               ),
               mainPanel(
                 plotOutput("pie_chart")
               )
             )
    ),
 


  # Tab for generating a consumer demographics map
  tabPanel("Customer Demographics Map",
           sidebarLayout(
             sidebarPanel(
               radioButtons("demographic", 
                            "Select Demographic",
                            choices = c("Age Bins" = "age_bins",
                                        "Category" = "category",
                                        "Gender" = "gender", 
                                        "Frequency of Purchases" = "frequency_of_purchases", 
                                        "Payment Method" = "payment_method",
                                        "Season" = "season", 
                                        "Subscription Status" = "subscription_status")),
               selectInput("category_choice", 
                           "Select Category",
                           choices = NULL,
                           selected = NULL),
               p("This app generates a consumer demographics map based on the total purchase amount in a year and the selected input.")
             ),
             mainPanel(
               plotOutput("map_chart")
             )
           )
  ),
  
  # Tab for generating a demographic cross-tabulation
  tabPanel("Demographics Cross Tabulation",
           sidebarLayout(
             sidebarPanel(
               selectInput("y_variable2", 
                           label = "Select Y-axis Variable", 
                           choices = c("Age Bins" = "age_bins",
                                       "Category" = "category",
                                       "Gender" = "gender", 
                                       "Frequency of Purchases" = "frequency_of_purchases", 
                                       "Payment Method" = "payment_method",
                                       "Season" = "season", 
                                       "Subscription Status" = "subscription_status"),
                           selected = "gender"
               ),
               selectInput(
                 inputId = "fill_variable3", 
                 label = "Select Fill/Legend Variable:",
                 choices = c("Age Bins" = "age_bins",
                             "Category" = "category",
                             "Gender" = "gender", 
                             "Frequency of Purchases" = "frequency_of_purchases", 
                             "Payment Method" = "payment_method",
                             "Season" = "season", 
                             "Subscription Status" = "subscription_status"),
                 selected = "category" 
               ),
               p("This app generates a stacked bar chart of consumer demographic variable based on the total purchase amount in a year."),
               p(em("*Figures in USD($)")),
               p(em("**Don't select the same variable for each input."))
             ),
             mainPanel(
               DTOutput("cross_table")
             )
           )
  ),
  
  # Tab for Information display

  tabPanel("Information",
           textOutput("project_info")
      )
    )
)
server <- function(input, output,session) { 

# Render a bar plot based on selected Y-axis and fill variables
output$bar_plot <- renderPlot({
  y_label <- switch(input$y_variable,
                    "age_bins" = "Age Bins",
                    "gender" = "Gender", 
                    "category" = "Category",
                    "season" = "Season",
                    "subscription_status" = "Subscription Status",
                    "payment_method" = "Payment Method",
                    "frequency_of_purchases" = "Frequency of Purchases")
  fill_label <- switch(input$fill_variable,
                       "age_bins" = "Age Bins",
                       "gender" = "Gender", 
                       "category" = "Category",
                       "season" = "Season",
                       "subscription_status" = "Subscription Status",
                       "payment_method" = "Payment Method",
                       "frequency_of_purchases" = "Frequency of Purchases")
  
  ggplot(shopping_behavior, aes(x = purchase_amount_usd, 
                                y = !!sym(input$y_variable), 
                                fill = !!sym(input$fill_variable))) +
    geom_bar(stat = "identity") +
    labs(
      x = "Purchase Amount (USD)",
      y = y_label,
      fill = fill_label
    ) +
    scale_fill_discrete() +
    theme_economist() +
    theme(
      panel.grid = element_blank(),
      plot.title = element_text(face = "bold", size = 25, colour = "blue"),
      panel.background = element_rect(fill = "white"),
      plot.background = element_rect(fill = "white")
    ) 
})

# Render a pie chart based on the selected fill variable
output$pie_chart <- renderPlot({
  fill_label2 <- switch(input$fill_variable2,
                        "age_bins" = "Age Bins",
                        "gender" = "Gender", 
                        "category" = "Category",
                        "season" = "Season",
                        "subscription_status" = "Subscription Status",
                        "payment_method" = "Payment Method",
                        "frequency_of_purchases" = "Frequency of Purchases")
  
  ggplot(shopping_behavior, aes(x = " ", y = purchase_amount_usd, 
                                fill = !!sym(input$fill_variable2))) +
    geom_bar(width = 1, stat = "identity") +
    coord_polar(theta = "y") +
    xlab(NULL) +
    ylab(NULL) +
    theme_minimal() +
    theme(panel.grid = element_blank(), 
          axis.text = element_blank()) +
    labs(
      title = "Demographic Pie Chart",
      fill = fill_label2
    ) +
    theme(
      panel.grid = element_blank(),
      plot.title = element_text(face = "bold", size = 25, colour = "blue")
    )    
})

# Render a DataTable displaying a cross table of demographic variables
output$cross_table <- renderDT({
  # Create the cross table with updated column names
  cross_table <- shopping_behavior %>%
    group_by(!!sym(input$y_variable2), !!sym(input$fill_variable3)) %>% 
    summarise(total = sum(purchase_amount_usd)) %>% 
    pivot_wider(names_from = !!sym(input$y_variable2), values_from = total) %>% 
    ungroup()
  
  # Convert the cross table data to a DataTable
  datatable(cross_table,
            options = list(dom = 't'),
            rownames = FALSE)
})


    observe({
      observeEvent(input$demographic, {
        updateSelectInput(session, "category_choice", 
                          choices = unique(shopping_behavior[[input$demographic]]),
                          selected = NULL)
      })
      
      # Render a map chart based on selected demographic and category choice
      output$map_chart <- renderPlot({
        req(input$category_choice)
        
        filter_data <- shopping_behavior %>%
          filter(.data[[input$demographic]] == input$category_choice)
        
        ggplot(filter_data, aes(fill = purchase_amount_usd)) +
          geom_sf(color="black", aes(geometry = geometry)) +
          theme_void() +
          labs(fill = "Purchase Amount Ratio") +
          scale_fill_gradient2(
            limits = c(0, 100),
            breaks = seq(0, 100, 25),
            midpoint = 50,
            high = "blue",
            mid = "purple",
            low = "red"
          )
      })
    })
      
      # Render project information as a formatted text output
      output$project_info <- renderText({
        
        text <- "This Shiny app serves as a tool for companies to dissect and visualize customer 
        demographic data, centering around their purchasing behaviors. Through an array of visualizations 
        encompassing bar charts, pie charts, maps, and cross-tabulations, users can delve into the relationships 
        between demographic variables—age bins, gender, categories, and more—and their impact on the total 
        annual purchase amounts. Uisng data sourced from Kaggle.com, this platform helps companies to discern 
        crucial insights about their customer base. The core funtion of the app lies in unraveling the correlations between 
        demographic attributes and purchasing habits, aiding companies in crafting targeted strategies, understanding 
        customer preferences, and tailoring their offerings to specific segments."
        "The strategic inclusion of various visualization types within this Shiny app elucidates nuanced insights 
        from the data. For instance, animated elements within the visualizations serve a pivotal role in showcasing 
        categorical shifts, enriching the understanding of how demographics influence purchasing 
        behavior. The choice of widgets within the app is purposeful, aiming to simplify the exploration 
        process for users. By enabling seamless selection and comparison of demographic variables, these widgets 
        assist in users deciphering intricate patterns and correlations and facilitating informed decision-making. 
        Ultimately, this tool aims to facilitate a comprehensive understanding of customer demographics, aiding 
        companies in strategizing, marketing, and optimizing their business approaches based on data-driven insights"

      })
    }
    
# Run the application 
shinyApp(ui = ui, server = server)    





