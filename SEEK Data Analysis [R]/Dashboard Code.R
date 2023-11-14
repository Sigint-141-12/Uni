#Shiny dashboard code

library(shiny)
library(leaflet)
library(tidyverse)
library(viridis)
library(plotly)

load("examdata.RData")

#extra cleaning - remove jobs that don't have "data" in the title
examdata <- examdata %>%
  filter(grepl('data|Data', Title))

#state data for mapping
state_data <- data.frame(
  State = c("NSW", "VIC", "QLD", "SA", "WA", "TAS", "NT", "ACT"),
  Population = c(8238.8, 6704.3, 5378.3, 1834.3, 2825.2, 571.6, 250.1, 460.9),
  Latitude = c(-31.2532, -36.8437, -23.9667, -30.0002, -25.0000, -42.0000, -19.4914, -35.4735),
  Longitude = c(146.9211, 144.7535, 143.2104, 135.0000, 125.0000, 147.3272, 132.5509, 149.0124)
)




#### Define UI ----
ui <- fluidPage(
  titlePanel("Job Market for Data Scientists in Australia"),
  #3 tabs, one per 'question'
  tabsetPanel(
    # display role and salary data on an interactive map of Aus
    tabPanel("Map", fluid = TRUE,
             fluidRow(
               column(width = 12,
                      #choose the info displayed on the map
                      radioButtons("data_type", label = "Data Type",
                                   choices = c("Job Adverts", "Median Salary"),
                                   selected = "Job Adverts")
               )
             ),
             fluidRow(
               column(width = 6,
                      sliderInput("salary_range", label = "Salary Range",
                                  min = min(0), #0 included to count missing salary roles in the advert count
                                  max = max(examdata$Salary, na.rm = TRUE),
                                  value = c(0, max(examdata$Salary, na.rm = TRUE)),
                                  step = 1000)
               )
             ),
             fluidRow(
               column(width = 12,
                      leafletOutput("map")
               )
             ),
             
    ),
    #tab 2: job titles. Check box for industry in side panel then a title and salary plot to show abundance of each
    tabPanel("Job Titles", 
             sidebarLayout(
               sidebarPanel(
                      checkboxGroupInput("industry_filter", label = "Filter by Industry",
                                         choices = unique(examdata$Class),
                                         selected = unique(examdata$Class))
               
             ),
             mainPanel(
              plotlyOutput("title_plot"),
              plotlyOutput("salary_plot")
               )
             )),
    #tab 3: interactive time plots. Again can be filtered by industry
    tabPanel("Adverts Over Time", 
             sidebarLayout(
               sidebarPanel(
                 checkboxGroupInput("industry_filter_time", label = "Filter by Industry",
                                    choices = unique(examdata$Class),
                                    selected = unique(examdata$Class)
                 )
                 
               ),
               mainPanel(
                 plotlyOutput("time_plot"),
                 plotlyOutput("day_plot")
               )
             ))
    
    ))
    
  


##### Define server logic ----
server <- function(input, output) {

  
####Q1 map code####
ads_or_salary <- reactive({
    
  #salary data filter using inputs from slider
    salary_filtered_data <- examdata %>%
      filter(Salary >= input$salary_range[1] & Salary <= input$salary_range[2])
    
    #change data displayed on map using radio selector input
    if (input$data_type == "Job Adverts") {
      salary_filtered_data %>%
        group_by(State) %>%
        summarise(Value = n()) %>%
        replace_na(list(Value = 0)) %>%
        left_join(state_data, by = "State")
    } else {
      salary_filtered_data %>%
        group_by(State) %>%
        summarise(Value = median(Salary, na.rm = TRUE)) %>%
        replace_na(list(Value = 0)) %>%
        left_join(state_data, by = "State")
    }
  })
  
  
  
#code to create the dashboard map
  output$map <- renderLeaflet({
    leaflet() %>%
      setView(lng = 133.7751, lat = -25.2744, zoom = 3) %>%
      addProviderTiles("Esri.WorldGrayCanvas")
  })
  
 #add filtered data to the map
  observe({
    data <- ads_or_salary()
    
    leafletProxy("map") %>%
      clearMarkers() %>%
      addCircleMarkers(
        data = data,
        lng = ~Longitude,
        lat = ~Latitude,
        label = ~paste0(State, ": ", Value),
        labelOptions = labelOptions(noHide = TRUE, direction = "top", textOnly = TRUE),
        popup = ~paste0("<b>State:</b> ", State, "<br>",
                        ifelse(input$data_type == "Job Adverts", "<b>Job Adverts:</b> ", "<b>Median Salary: $</b> "),
                        Value)
      )
  })

#### Q2 title/industry code ####  
  
  #code to group by titles
  q2a_df_grouped <- examdata %>%
    mutate(JobGroup = case_when(
      str_detect(Title, "Engineer") ~ "Engineer",
      str_detect(Title, "Scientist") ~ "Scientist",
      str_detect(Title, "Analyst") ~ "Analyst",
      str_detect(Title, "Manager") ~ "Leadership",
      str_detect(Title, "Lead") ~ "Leadership",
      str_detect(Title, "Head") ~ "Leadership",
      str_detect(Title, "Director") ~ "Leadership",
      str_detect(Title, "Advisor") ~ "Advisor",
      str_detect(Title, "Consultant") ~ "Consulatant",
      str_detect(Title, "Biostatistician") ~ "Biostatistician",
      str_detect(Title, "Graduate") ~ "Graduate",
      str_detect(Title, "Developer") ~ "Developer",
      str_detect(Title, "Analytics") ~ "Analytics",
      str_detect(Title, "Trainer") ~ "Trainer",
      str_detect(Title, "Specialist") ~ "Specialist",
      str_detect(Title, "Modeller") ~ "Modeller",
      str_detect(Title, "Modeler") ~ "Modeller",
      str_detect(Title, "Architect ") ~ "Architect ",
      TRUE ~ "Other"))
    
  
  #filter data based on checkbox inputs
  output$title_plot <- renderPlotly({
    class_filtered_data <- q2a_df_grouped %>%
      filter(Class %in% input$industry_filter) %>%
      group_by(JobGroup) %>%
      summarise(Count = n()) %>%
      arrange(desc(Count))
    
    #oputput filtered data into lollipop plot
    ggplot(class_filtered_data, aes(x = reorder(JobGroup, Count), y = Count)) +
      geom_segment(aes(xend = reorder(JobGroup, Count), yend = 0), color = "gray") +
      geom_point(size = 5, aes(fill = reorder(JobGroup, Count)), color = "black", shape = 21, show.legend = FALSE) +
      scale_fill_viridis_d(option = "magma", begin = 0.2, end = 0.8) +
      labs(x = "Job Group", y = "Count", title = "Popularity of Job Titles") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      coord_flip()+
      #geom_text(aes(label = Count), vjust = -50, color = "black")+
      ylim(0, 200)
    
  })
  
  #filter data based on checkbox inputs
  output$salary_plot <- renderPlotly({
    class_filtered_data2 <- q2a_df_grouped %>%
      filter(Class %in% input$industry_filter) %>%
      group_by(Class) %>%
      summarise(
        MedianSalary = median(Salary, na.rm = TRUE),
        MeanSalary = mean(Salary, na.rm = TRUE)
      )
    #oputput filtered data into bar plot
    ggplot(class_filtered_data2, aes(x = Class, y = MedianSalary, fill = Class)) +
      geom_bar(stat = "identity", width = 0.7) +
      scale_fill_viridis_d(option = "magma", begin = 0.2, end = 0.8) +
      labs(x = "Industry", y = "Salary ($)", title = "Industry Median Salary") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      guides(fill = "none")+
      scale_y_continuous(labels = scales::comma)
    
  })


#### Q3 time code ####
  
  
#create day data from original df
q3a_df <- examdata %>%
  mutate(Date = as.Date(Date)) %>% #remove time
  mutate(Day = weekdays(Date))
    
  #filter by industry
    output$time_plot <- renderPlotly({
      time_filtered_data <- q3a_df %>%
        filter(Class %in% input$industry_filter_time) %>%
        group_by(Date) %>%
        summarise(
          Count = n())
        
  #plot filtered data
  ggplot(time_filtered_data, aes(x = Date, y = Count, fill = Date)) +
  geom_bar(stat = "identity") +
  scale_fill_viridis(option = "magma", begin = 0.2, end = 0.8, alpha = 0.8) +
  labs(x = "Date", y = "Count", title = "Count of Listings by Date") +
  theme_minimal()+
  guides(fill = "none")   
    
  
  })
    #filter by industry
    output$day_plot <- renderPlotly({
      day_data <- q3a_df %>%
        filter(Class %in% input$industry_filter_time) %>%
        group_by(Day) %>%
        summarise(Count = n()) %>%
        mutate(Day = factor(Day, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))) %>%
        group_by(Day) %>%
        summarise(DayCount = sum(Count))
      
      #plot filtered data
      ggplot(day_data, aes(x = factor(Day, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")), y = DayCount, fill = Day)) +
        geom_bar(stat = "identity") +
        scale_fill_viridis_d(option = "magma", begin = 0.2, end = 0.8, alpha = 0.8) +
        labs(x = "Date", y = "Count", title = "Count of Listings by Day of the Week") +
        theme_minimal()+
        guides(fill = "none")
      
     
    })
    
}
##### Run the app ----
shinyApp(ui = ui, server = server)