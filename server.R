library(shiny)
library(ggplot2)
<<<<<<< HEAD
library(leaflet)
library(dplyr)
library(stringr)
library(rgdal)
library(htmltools)
library(htmlwidgets)
=======
library(dplyr)
>>>>>>> origin/subject_vis
library(tidyr)

revenue.data <- read.csv("data/washington_school_district_level_revenue.csv", stringsAsFactors = FALSE)
msp.hspe.data <- read.csv("data/2010_to_2013_MSP_HSPE_Scores_by_District.csv", stringsAsFactors = FALSE)

server <- function(input, output) {
<<<<<<< HEAD
  
}

shinyServer(server)
=======
  # Plot 2: Test proficiency percentage distribution
  output$plot2 <- renderPlot({
    # Make sure a school district is selected
    validate(need(input$school.district, "Please select a school district to view."))
    # Gather data from the school district specified
    district.test.data <- filter(msp.hspe.data, District == input$school.district) %>% 
      # Remove data we don't want to use
      filter(GradeTested != 3 & GradeTested != 4 & GradeTested != 5) 
    district.test.data$SchoolYear <- paste0(substr(district.test.data$SchoolYear, 1, 2), substr(district.test.data$SchoolYear, 6, 7))
    # Gather data from the year specified
    district.test.data <- filter(district.test.data, SchoolYear == input$years.slider) %>% 
      select(District, GradeTested, 
             ReadingPercentMetStandardExcludingNoScore, 
             MathPercentMetStandardExcludingNoScore,
             WritingPercentMetStandardExcludingNoScore,
             SciencePercentMetStandardExcludingNoScore
      ) %>% 
      # Make into long data to graph
      gather(key = subject, value = percentage.reached.standard,
             ReadingPercentMetStandardExcludingNoScore,
             MathPercentMetStandardExcludingNoScore,
             WritingPercentMetStandardExcludingNoScore,
             SciencePercentMetStandardExcludingNoScore)
    
    # Editing column names and data names so they display nicely in the graph
    district.test.data$GradeTested <- paste0(district.test.data$GradeTested, "th Grade")
    district.test.data$subject <- gsub("PercentMetStandardExcludingNoScore", "", district.test.data$subject)
    
    # Editing the order that items in the graph will show up
    district.test.data$GradeTested <- factor(district.test.data$GradeTested, levels = c("6th Grade", "7th Grade", "8th Grade", "10th Grade"))
    district.test.data$subject <- factor(district.test.data$subject, levels = c("Reading",
                                                                                "Writing",
                                                                                "Math",
                                                                                "Science"))
    
    # Plot graph
    plot <- ggplot(data = district.test.data) +
      geom_bar(mapping = aes(x = subject, y = percentage.reached.standard, fill = subject), stat = "identity", na.rm = TRUE) +
      facet_wrap(~GradeTested) +
      labs(title = paste0("Distribution of Students Reaching Proficiency Standard on the ", input$years.slider , " MSP/HSPE\n (", input$school.district, ")"), x = "Test Subject", y = "Percentage Met Standard") +
      scale_fill_discrete("Subject", labels = c("Reading", "Writing", "Math", "Science")) +
      theme(plot.title = element_text(hjust = 0.5))
    plot
  })
  
  # Plot caption
  output$plot2.description <- renderText({
    validate(need(input$school.district, ""))
    "Fig. 2: This visualization shows the distribution of students who met the testing standard of the MSP/HSPE 
    Test in a particular school district and for a select year between 2010 and 2013. The graphs are organized 
    by grade and display grades 6-8 and 10. Each graph is also organized by test subject. Some school districts,  
    test subjects, and years may not have data, and will be reflected by the lack of such data shown in the visualization.
    6th grade only takes the reading and math sections; 7th grade takes reading, writing, and math; 8th grade takes
    reading, math, and science; 10th grade takes all four subjects, but we only have data for some subjects in select years. "
  })
}

shinyServer(server)
>>>>>>> origin/subject_vis
