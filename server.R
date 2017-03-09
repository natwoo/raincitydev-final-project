library(shiny)
library(ggplot2)
library(leaflet)
library(dplyr)
library(stringr)
library(rgdal)
library(htmltools)
library(htmlwidgets)
library(tidyr)

revenue.data <- read.csv("data/washington_school_district_level_revenue.csv", stringsAsFactors = FALSE)
msp.hspe.data <- read.csv("data/2010_to_2013_MSP_HSPE_Scores_by_District.csv", stringsAsFactors = FALSE)
final.data <- left_join(revenue.data, msp.hspe.data, by = c("district" = "District")) %>% mutate(year = paste(substr(final.data$SchoolYear, 1,2), substr(final.data$SchoolYear, 6,7), sep = ""))
View(final.data)
server <- function(input, output) {
  
  #this filtered variable contains data for the data table
  filtered <- reactive({
    data.msp <- final.data %>% 
      filter(GradeTested == input$grades.button) %>% filter(year == input$years.slider) %>% 
      select("District" = district, 
             "County" = County,
             "ESD" = ESD,
             "Year" = year, 
             "Total revenue" = total.revenue, 
             "Reading met standard (%)" = ReadingPercentMetStandardExcludingNoScore, 
             "Math met standard (%)" = MathPercentMetStandardExcludingNoScore, 
             "Writing met standard (%)" = WritingPercentMetStandardExcludingNoScore, 
             "Science met standard (%)" = SciencePercentMetStandardExcludingNoScore)                                           
    
    return(data.msp)
  })
  
  #renders the data table itself
  output$table <- renderDataTable({
    return(filtered())
  })
  
  #renders the text for the data table introduction
  output$intro <- renderText({
    print("This Table details the information we used to 
          plot and map the funding and test scores of different districts 
          in Washington. You can filter the data by changing the grade 
          and year on the widgets tab. The data we used is the percent of 
          people who met the standard in the different subject tests and 
          we excluded people with no scores. For more information on how
          grading is done, please visit this link: 
          http://www.k12.wa.us/assessment/StateTesting/ScaleScores.aspx")
  })
  
  #renders the text for the about tab
  output$about <- renderText({
    paste("Our project investigates whether more funding can
          lead to higher scores in the following subjects: Reading, Math, Writing,
          and Science. We limited our data to the school districts of Washington. 
          As you can see, we used two different data sets. One of them detials 
          the data for how well students did on their testing and the other on how
          much the school districts were funded. This topic is important because
          it will inform and educate policymakers which areas need more funding
          through the map we've made. Additional information such as plots, data
          tables, and aggregated plots can further help policymakers understand
          the data and make decisions on whether to fund certain districts more.
          for more information on how the scores are evaluated please refer to 
          the link:", "http://www.k12.wa.us/assessment/StateTesting/ScaleScores.aspx")
    
  }) 
}

shinyServer(server)