library(shiny)
library(ggplot2)
library(leaflet)
library(dplyr)
library(rgdal)
library(maps)
library(leaflet)
library(tidyr)
library(htmltools)
library(htmlwidgets)


#Reading in the two data
revenue.data <- read.csv("data/washington_school_district_level_revenue.csv", stringsAsFactors = FALSE)
msp.hspe.data <- read.csv("data/2010_to_2013_MSP_HSPE_Scores_by_District.csv", stringsAsFactors = FALSE)
#Merged the two data together using Districts
final.data <- left_join(revenue.data, msp.hspe.data, by= c("district" = "District"))
#Changing so we can have years 2010, 2011, instead of 2010-2011, 2011-2012
final.data <- mutate(final.data,  year = paste(substr(final.data$SchoolYear, 1,2), substr(final.data$SchoolYear, 6,7), sep = ""))

# reads in shapefile of school district boundaries
school.district.shapefile <- readOGR("./data/shapefiles", "tl_2016_53_unsd", stringsAsFactors = FALSE) %>% 
  rmapshaper::ms_simplify()

View(final.data)
server <- function(input, output) {
  #Filtering data for the plot. This allows the data to be manipulated
  filtered1 <- reactive({
    data.final <- final.data %>%
      filter(year == input$years.slider) %>%
      filter(GradeTested == input$grades.button)
    return(data.final)
    
  })
  
  
  
  
  
  # Reactives
  map.data <- reactive({
    temp <- school.district.shapefile
    revenue.map.data <- mutate(revenue.data, NAME = district, year = paste0(substr(year, 1, 2), substr(year, 6, 7))) %>% 
      select(NAME, year, total.revenue.per.pupil) %>% 
      filter(year == input$years.slider) %>% 
      spread(year, total.revenue.per.pupil)
    colnames(revenue.map.data) <- c("NAME", "total.revenue.per.pupil")
    temp@data <- left_join(temp@data, revenue.map.data)
    temp
  })
  # Renders
  output$map <- renderLeaflet({
    
    
    pal <- colorNumeric(palette = "Blues", domain = school.district.shapefile@data$total.revenue.per.pupil)
    
    map <- leaflet(map.data()) %>% 
      addTiles() %>%
      addEasyButton(easyButton(
        icon="fa-globe", title="Zoom to Level 1",
        onClick=JS("function(btn, map){ map.setZoom(1); }"))) %>%
      addEasyButton(easyButton(
        icon="fa-crosshairs", title="Locate Me",
        onClick=JS("function(btn, map){ map.locate({setView: true}); }")))
    
    map <- map %>%
      addPolygons(weight = 1,
                  fillColor = ~pal(total.revenue.per.pupil),
                  fillOpacity = 0.8,
                  label = ~paste0(NAME," (Revenue per Pupil: $", total.revenue.per.pupil, ")")) %>% 
      addLegend("bottomright",
                pal = pal, values = ~total.revenue.per.pupil,
                title = "Total Revenue Per Pupil",
                labFormat = labelFormat(prefix = "$"),
                opacity = 1
      )
    
    
    
  })
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
  
  
  
  #Outputting the plot
  #By using the if statements, the clicked radiobutton subject will show up on the main panel.
  output$plot1 <- renderPlot({
    #Plot for reading test
    if (input$subject.check == "reading") {
      p <- ggplot(data = filtered1(), mapping = aes(x=total.revenue.per.pupil, y=ReadingPercentMetStandardExcludingNoScore)) +
        geom_point(aes(color = "district")) +
        xlim(5000,35000) +
        xlab("Total Revenue Per Student") +
        ylab("Reading Score Percent Standard Met (Excluding No Score)") +
        ggtitle("Revenue vs Reading Test Standard Met")
      return(p)
    } #Plot for Math Test
    else if (input$subject.check == "math") {
      p <- ggplot(data = filtered1(), mapping = aes(x=total.revenue.per.pupil, y=MathPercentMetStandardExcludingNoScore)) +
        geom_point(aes(color = "district")) +
        xlim(5000,35000) +
        xlab("Total Revenue Per Student") +
        ylab("Math Score Percent Standard Met (Excluding No Score)") +
        ggtitle("Revenue vs Math Test Standard Met")
      return(p)
    } #Plot for Writing Exam
    else if (input$subject.check == "writing") {
      p <- ggplot(data = filtered1(), mapping = aes(x=total.revenue.per.pupil, y=WritingPercentMetStandardExcludingNoScore)) +
        geom_point(aes(color = "district")) +
        xlim(5000,35000) +
        xlab("Total Revenue Per Student") +
        ylab("Writing Score Percent Standard Met (Excluding No Score)") +
        ggtitle("Revenue vs Writing Test Standard Met")
      return(p)   
    } #Plot for Science Test
    else if (input$subject.check == "science") {
      p <- ggplot(data = filtered1(), mapping = aes(x=total.revenue.per.pupil, y=SciencePercentMetStandardExcludingNoScore)) +
        geom_point(aes(color = "district")) +
        xlim(5000,35000) +
        xlab("Total Revenue Per Student") +
        ylab("Science Score Percent Standard Met (Excluding No Score)") +
        ggtitle("Revenue vs Science Test Standard Met")
      return(p)   
    }
    
  })
  output$text <- renderText({
    paste("In order to find the correlation between school funding 
          and school standardized test scores, we utilized a graph. 
          In this graph, the x-axis displays the total revenue per 
          student for the district and the y-axis displays the percent 
          of students who met the standard, excluding no scores in order 
          to eliminate outliers. As shown through the data, we concluded that
          there is little correlation between the education funding and state 
          standarized test scores. Writing tests which rely more on one's basic 
          grammar skills seem to have no correlation throughout the grades. 
          Most districts achieved the standard, with many placing around 60%-80% 
          range for most tests regardless of funding. However some correlation were shown 
          on the 7th and 8th grade tests. For example, there were a few districts that 
          achieved a higher percent of standard met than other districts which had lower
          funding than them for both reading and math tests. Although the correlation was
          shown only in 7th and 8th grade, we valued these importantly as reading and math 
          cannot be self-taught at a young age, therefore the students would rely the most 
          on knowledge learned in school. So in conclusion, our hypothesis, in which we predicted a
          positive relationship between school funding and test scores, was partially
          proven as the relationship shown in 7th and 8th grade data were vital, but not enough
          evidence to prove the entire hypothesis.")
  })
  output$text1 <- renderText({
    paste("Reading MSP tests have been given throughout 
          the grades 6-8 and grade 10 were given HSPE, High School Proficiency 
          Exam, so we have all the grade data for Reading Exams. Math MSP tests 
          were also taken by grades 6-8, but the HSPE focuses on reading and writing
          therefore we do not have a graph for math in 10th grade. For the writing tests,
          students in grades 7 and 10 took the exam. The 7th grade students took the special
          MSP for 7th graders while the 10th grade students took the writing HSPE. Science tests
          were given to 5th and 8th graders before they graduated elementary and middle school.
          We purposefully did not include elementary grades because we believed that middle schoolers
          would give us a more vital information and that the inclusion of 10th graders would reinforce 
          our data.") 
  })
  
}


shinyServer(server)

