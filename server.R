library(shiny)
library(ggplot2)
library(leaflet)
library(dplyr)
library(stringr)
library(rgdal)
library(htmltools)
library(htmlwidgets)
library(tidyr)

#Reading in the two data
revenue.data <- read.csv("data/washington_school_district_level_revenue.csv", stringsAsFactors = FALSE)
msp.hspe.data <- read.csv("data/2010_to_2013_MSP_HSPE_Scores_by_District.csv", stringsAsFactors = FALSE)
#Merged the two data together using Districts
final.data <- left_join(revenue.data, msp.hspe.data, by= c("district" = "District"))
#Changing so we can have years 2010, 2011, instead of 2010-2011, 2011-2012
final.data <- mutate(final.data,  year = paste(substr(final.data$SchoolYear, 1,2), substr(final.data$SchoolYear, 6,7), sep = ""))

server <- function(input, output) {
  #Filtering data for the plot. This allows the data to be manipulated
  filtered <- reactive({
    data.final <- final.data %>%
      filter(year == input$years.slider) %>%
      filter(GradeTested == input$grades.button)
    return(data.final)
    
  })
  
  
  #Outputting the plot
  #By using the if statements, the clicked radiobutton subject will show up on the main panel.
  output$plot1 <- renderPlot({
    #Plot for reading test
    if (input$subject.check == "reading") {
      p <- ggplot(data = filtered(), mapping = aes(x=total.revenue.per.pupil, y=ReadingPercentMetStandardExcludingNoScore)) +
        geom_point(aes(color = "district")) +
        xlim(5000,35000) +
        xlab("Total Revenue Per Student") +
        ylab("Reading Score Percent Standard Met (Excluding No Score)") +
        ggtitle("Revenue vs Reading Test Standard Met")
      return(p)
    } #Plot for Math Test
    else if (input$subject.check == "math") {
      p <- ggplot(data = filtered(), mapping = aes(x=total.revenue.per.pupil, y=MathPercentMetStandardExcludingNoScore)) +
        geom_point(aes(color = "district")) +
        xlim(5000,35000) +
        xlab("Total Revenue Per Student") +
        ylab("Math Score Percent Standard Met (Excluding No Score)") +
        ggtitle("Revenue vs Math Test Standard Met")
      return(p)
    } #Plot for Writing Exam
    else if (input$subject.check == "writing") {
      p <- ggplot(data = filtered(), mapping = aes(x=total.revenue.per.pupil, y=WritingPercentMetStandardExcludingNoScore)) +
        geom_point(aes(color = "district")) +
        xlim(5000,35000) +
        xlab("Total Revenue Per Student") +
        ylab("Writing Score Percent Standard Met (Excluding No Score)") +
        ggtitle("Revenue vs Writing Test Standard Met")
      return(p)   
    } #Plot for Science Test
    else if (input$subject.check == "science") {
      p <- ggplot(data = filtered(), mapping = aes(x=total.revenue.per.pupil, y=SciencePercentMetStandardExcludingNoScore)) +
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