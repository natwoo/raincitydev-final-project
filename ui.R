library(shiny)
library(ggplot2)
library(leaflet)


revenue.data <- read.csv("data/washington_school_district_level_revenue.csv", stringsAsFactors = FALSE)
districts <- revenue.data$district %>% 
  as.list() %>% 
  setNames(revenue.data$district)


ui <- fluidPage(
  #Title
  titlePanel("School Funding's Effect on Standardized Test Scores"),
  
  br(),
  # A two panel layout with sidebar and main panel
  sidebarLayout(
    # Sidebar
    sidebarPanel(
      radioButtons("grades.button", "Grade of Interest:",
                   c("6th Grade" = "6", 
                     "7th Grade" = "7", 
                     "8th Grade" = "8", 
                     "10th Grade" = "10")),
      
      br(),
      
      radioButtons("subject.check", "Test Subject:",
                   c("Reading" = "reading",
                     "Math" = "math",
                     "Writing" = "writing",
                     "Science" = "science")),
      
      br(),
      
      selectInput("school.district", "School Districts:",
                  districts),
      
      br(),
      
      sliderInput("years.slider", "Years:",
                  min = 2010,
                  max = 2013,
                  value = 2013,
                  step = 1,
                  sep = "")
    ),
    
    # Main panel
    mainPanel(
      # Create tabs
      leafletOutput("map"),
      br(),
      tabsetPanel(type = "tabs",


                  tabPanel("About", textOutput('about')),
                 
                  tabPanel("Per District Test Proficiency Distribution", plotOutput("plot2"), br(), textOutput("plot2.description")),
                
                  

                  tabPanel("Revenue vs. Test Score", textOutput("text"), plotOutput("plot1"), textOutput("text1")),
       
                  tabPanel("Data Table", textOutput("intro"), dataTableOutput('table'))

      )
    )
  )
)

shinyUI(ui)