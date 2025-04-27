#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#
library(tidyverse)
library(dplyr)
library(here)
library(ggplot2)
library(leaflet)
library(pacman)
library(quanteda)
library(quanteda.textstats)
library(pdftools)
library(tibble)
library(sentimentr)
library(topicmodels)
library(tidytext)
library(countrycode)  
library(sf)
library(rnaturalearth)
library(WDI)
library(rsconnect)
library(shiny)

pacman::p_load(readtext)
pacman::p_load(quanteda.textplots, quanteda.textstats)
pacman::p_load("quanteda.dictionaries", "quanteda.sentiment")
set.seed(10)

# Setting WD

# Reading in data
#setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) 
app_dir <- if (interactive()) {
  dirname(rstudioapi::getActiveDocumentContext()$path)
} else {
  getwd()  # Fallback for deployment
}
setwd(app_dir)
un2 <- read.csv("un_final.csv") # UN data

# Maps
install.packages("rnaturalearthdata", repos = "https://cloud.r-project.org/")
world <- ne_countries(scale = "medium", returnclass = "sf") # creating map
world <- st_transform(world, crs = 4326) # setting crs
world <- st_make_valid(world)

map_un <- world %>%
  left_join(un2, by = c("adm0_iso" = "ccodealp"))

# Aliases for metric names
metric_aliases <- c(
  "GDP Per Capita (2015 USD)"= "GDP.per.capita..constant.2015.US..",
  "Gini" ="Gini.index",
  "Non-Western Nation" = "nonwest",
  "Major Power" = "major_power",
  "Regime Type" = "regime_status_name",
  "Women Empowerment Score" = "vdem_gender",
  "Democratic Country" = "dem_bi",
  "How close is speech to fostering International Order?" = "IdealPointAll"
  
)


# Define UI 
ui <- fluidPage(
  
  # Application title
  titlePanel("UN General Assembly Speeches: A Temporal Analysis of Topics and Sentiment by Country"),
  
  # Sidebar with a slider input for number of bins 
  tabsetPanel(  # Create a tabset panel
    tabPanel("Sentiment and Topics Over Time",  # First tab
             p("Choose a topic to see its frequency and sentiment trends"),
             (sidebarLayout(
               sidebarPanel(
                 selectInput("topic_choice", "Choose a Topic:", choices = sort(unique(un2$topic_title)))
               ),
               mainPanel(
                 plotOutput("topic_trend_plot")
               )
             )
             )
    ),
    tabPanel("Topic Instances",  # Second tab
             p("Choose a country to see the number of Topics covered by their UNGA Speeches"),
             (sidebarLayout(
               sidebarPanel(
                 selectInput("country_topic_choice", "Choose a Country:", choices = sort(unique(un2$country_name)))
               ),
               mainPanel(
                 plotOutput("topic_count_plot")
               )
             )
             )
    ),
    tabPanel("Map: Sentiment by Year",  # Third tab
             p("Choose a year to see a world map of countries by their sentiment score"),
             (sidebarLayout(
               sidebarPanel(
                 selectInput("year_choice", "Choose a Year:", choices = sort(unique(un2$year)))
               ),
               mainPanel(
                 plotOutput("sent_map")
               )
             )
             )
    ),
    tabPanel("Map: Topics by Year",  # Third tab
             p("Choose a year and hover to see a world map of countries by topic and metric"),
             (sidebarLayout(
               sidebarPanel(
                 selectInput("year_topic_choice", "Choose a Year:", choices = sort(unique(un2$year)))#,
                 #selectInput("metric_topic_choice", "Choose a Metric:", choices = metric_aliases)
               ),
               mainPanel(
                 leafletOutput("topic_map")
               )
             )
             )
    ),
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  ## Topic and sentiment trend
  output$topic_trend_plot <- renderPlot({
    un_long <- un2 %>% filter(topic_title==input$topic_choice) %>% 
      group_by(year) %>% summarize(n_topic = n(),
                                   mean_sent = mean(lsd_net_sent)) %>% 
      pivot_longer(cols = c(mean_sent, n_topic), names_to = "variable", values_to = "value")
    
    ggplot(un_long, aes(x = year, y = value, color = variable)) +
      geom_line(size=1) +
      scale_color_discrete(name = "Metric", labels = c("Mean Sentiment", "Number of Speeches")) + 
      labs(x = "Year", y = "Value")
  })
  
  ## Topic counts by country
  output$topic_count_plot <- renderPlot({
    ggplot(un2 %>% filter(country_name == input$country_topic_choice) %>% 
             group_by(topic_title) %>% summarize(n_topic = n()) %>% arrange(desc(n_topic)), aes(x = reorder(topic_title, -n_topic), y = n_topic)) +
      geom_col(fill='lightblue') +
      scale_x_discrete(labels = function(x) str_wrap(x, width = 5)) +
      labs(x = "Topic", y = "Count") +
      theme(axis.text.x = element_text(size = 10))
  })
  
  ## Map showing sentiment by year
  output$sent_map <- renderPlot({
    ggplot(map_un %>% filter(year == input$year_choice) %>% group_by(country_name) %>% 
             summarize(mean_sent = mean(lsd_net_sent), na.rm=TRUE)) +
      geom_sf(aes(fill = mean_sent), color = "darkgray", size=.1) +  # Color countries by 'value'
      scale_fill_gradient2(low = "red", mid = "white", high = "green", midpoint = 0) + 
      labs(title = "Sentiment by Country by Year",
           fill = "Sentiment (LSD) Score") +  # Legend title
      theme(axis.text = element_blank())})
  
  ## Map showing countries by topic by year
  
  output$topic_map <- renderLeaflet({
    # First verify the necessary columns exist
    required_columns <- c("topic_title", "year", #input$metric_topic_choice,
                          "GDP.per.capita..constant.2015.US..", "Gini.index", "regime_status_name")  # Update with your actual column names
    validate(
      need(all(required_columns %in% names(map_un)), 
           "Some required columns are missing from the data"
      ))
    
    filtered_data <- map_un %>% filter(year == input$year_topic_choice)
    
    # Get the correct country name column (update "NAME" to your actual column name)
    country_col <- "country_name"  # Change this to your actual country name column
    
    pal <- colorFactor("Set3", domain = filtered_data$topic_title)
    
    # OPTION 2: Manual color assignment (more control)
    # First get your unique topics
    unique_topics <- sort(unique(filtered_data$topic_title))
    
    # Create a named color vector
    topic_colors <- setNames(
      c("#2ca02c", "#8c564b", "#7f7f7f", "#98df8a", "#17becf",
        "#7f7f7f", "#e377c2", "#bcbd22", "#9edae5", "#d62728"
      ),
      unique_topics
    )
    
    pal <- colorFactor(palette = topic_colors, 
                       domain = filtered_data$topic_title)
    
    leaflet(filtered_data) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolygons(
        fillColor = ~pal(topic_title),
        weight = 1,
        opacity = 1,
        color = "white",
        fillOpacity = 0.7,
        highlight = highlightOptions(
          weight = 2,
          color = "#666",
          bringToFront = TRUE),
        label = ~sprintf(
          "<div style='font-size:14px;line-height:1.4'>
          <strong>%s</strong><br/>
          GDP Per Capita (2015 USD): %.2f<br/>
          Gini: %.2f<br/>
          Regime Type: %s
        </div>",
          get(country_col),  # Country name
          ifelse(is.na(GDP.per.capita..constant.2015.US..), NA, GDP.per.capita..constant.2015.US..),  # GDP per capita
          ifelse(is.na(Gini.index), NA, Gini.index),  # Gini coefficient
          regime_status_name  # Regime type
        ) %>% lapply(htmltools::HTML),
        labelOptions = labelOptions(
          style = list(
            "font-weight" = "normal", 
            padding = "6px 10px",
            "max-width" = "300px"
          ),
          textsize = "14px",
          direction = "auto"
        )
      ) %>%
      addLegend(
        pal = pal,
        values = ~topic_title,
        title = "Topics",
        position = "bottomright"
      )
  })
  
  
}


# Run the application 
shinyApp(ui = ui, server = server)


