#data analysis code

#load cleaned data
load("examdata.RData")

library(tidyverse)
library(treemap)
library(viridis)
library(RColorBrewer)
library(wordcloud)


#extra cleaning - remove jobs that don't have "data" in the title
examdata <- examdata %>%
  filter(grepl('data|Data', Title))
  



#### Q1 ####

#Q1a Study the number of positions with respect to states
#Q1b distribution of salaries overall 
#Q1c distribution of salaries with respect to the State
#Q1d relationships between states, number of positions and salaries?

#q1a - number of jobs by state

#df for state counts
q1a_df <- examdata %>%
  group_by(State) %>%
  summarise(
    Count = n())

write.csv(q1a_df, file = "q1a_stats.csv")

#treemap showing distribution of vacancies
treemap(q1a_df,
            index = "State",
            vSize = "Count",
        palette = "Pastel1",
        title = "Number of Data Scientist Vacancies by State")



#population data from https://www.abs.gov.au/statistics/people/population/national-state-and-territory-population/latest-release
state_populations <- data.frame(
  State = c("NSW", "VIC", "QLD", "SA", "WA", "TAS", "NT", "ACT"),
  Population = c(8238.8, 6704.3, 5378.3, 1834.3, 2825.2, 571.6, 250.1, 460.9)
)

q1a_df_pop <- q1a_df %>%
  left_join(state_populations, by = "State") %>%
  mutate(NormalisedCount = (Count/Population)) %>%
  mutate(ReciprocalCount = 1/NormalisedCount)

write.csv(q1a_df_pop, file = "q1a_pop_stats.csv")


#treemap showing distribution of vacancies relative to pop size
treemap(q1a_df_pop,
        index = "State",
        vSize = "NormalisedCount",
        palette = "Pastel1",
        title = "Number of Data Scientist Vacancies by State Relative to Population Size")

#q1b - salary distribution

#q1b df to remove empty salary data

q1b_df <- examdata %>%
  filter(!is.na(Salary))

#summary stats for Q1b
q1b_sumstat <- q1b_df %>% 
  summarise(
    Mean = mean(Salary),
    Median = median(Salary),
    Q1 = quantile(Salary, 0.25),
    Q3 = quantile(Salary, 0.75),
    Minimum = min(Salary),
    Maximum = max(Salary),
    StandardDeviation = sd(Salary),
    Count = n()
  )

write.csv(q1b_sumstat, file = "q1b_sumstat.csv")

#q1b salary distribution

ggplot(q1b_df, aes(x=Salary)) +
  geom_density(color="darkblue", fill="lightblue")+
  ggtitle("Distribution of Salaries")+ 
  xlab("Salary ($)")+
  ylab("Count")+
  theme_minimal()+
  theme(legend.position = "none")+
  geom_vline(xintercept = 131703.4, color = "darkblue", linetype = "dashed", linewidth = 1.5)


#q1c distribution of salaries with respect to states

#q1c df to group q1b df by state
q1c_df <- q1b_df %>%
  group_by(State) %>%
  summarise(
    Mean = mean(Salary),
    Median = median(Salary),
    Q1 = quantile(Salary, 0.25),
    Q3 = quantile(Salary, 0.75),
    Minimum = min(Salary),
    Maximum = max(Salary),
    StandardDeviation = sd(Salary),
    Count = n()
  )
  
write.csv(q1c_df, file = "q1c_df.csv")

ggplot(examdata, aes(x = State, y = Salary, fill = State)) +
  geom_violin() +
  geom_boxplot(width = 0.1, colour = "black", outlier.shape = NA, size = 0.1, fill = "white") +
  scale_fill_viridis_d(option = "magma", begin = 0.2, end = 0.8, alpha = 0.8) +
  labs(x = "State", y = "Salary", title = "Violin Plot of Salary by State") +
  theme_minimal()

#Q1d relationships between states, number of positions and salaries?

#no need for a new df, use q1c

ggplot(q1c_df, aes(x = State, y = Median, size = Count, colour = State)) +
  geom_point() +
  geom_text(aes(label = Count), vjust = -2.5, show.legend = FALSE, size = 5) +
  labs(x = "State", y = "Median Salary ($)", title = "Scatter Plot of Median Salary by State") +
  scale_colour_viridis_d(option = "magma", begin = 0.2, end = 0.8, alpha = 0.8) +
  scale_size(range = c(2, 20)) +
  ylim(100000, 160000) +
  theme_minimal()


#### Q2 ####

# q2a: Study the popularity of different job titles
#q2b: Study the popularity of different industries
#q2c: Compare salaries in different industries


# Group the data by job titles and count occurrences
q2a_df <- examdata %>%
  group_by(Title) %>%
  summarise(Count = n()) %>%
  arrange(desc(Count))


wordcloud(words = q2a_df$Title, freq = q2a_df$Count, 
          colors = brewer.pal(8, "Accent"),
          random.order=TRUE,
          min.freq = 3,
          scale=c(3.5,0.5),
          rot.per = 0)


#group aggregate roles by "type"
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
    TRUE ~ "Other")) %>%
group_by(JobGroup) %>%
summarise(Count = n()) %>%
arrange(desc(Count))

write.csv(q2a_df_grouped, file = "q2a_df_grouped.csv")


ggplot(q2a_df_grouped, aes(x = reorder(JobGroup, Count), y = Count)) +
  geom_segment(aes(xend = reorder(JobGroup, Count), yend = 0), color = "gray") +
  geom_point(size = 5, aes(fill = reorder(JobGroup, Count)), color = "black", shape = 21, show.legend = FALSE) +
  scale_fill_viridis_d(option = "magma", begin = 0.2, end = 0.8) +
  labs(x = "Job Group", y = "Count", title = "Popularity of Job Titles") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  coord_flip()+
  geom_text(aes(label = Count), hjust = -1.5, color = "black")+
  ylim(0, 200)


#q2b: Study the popularity of different industries

#new to group by Clas (i.e. industry)
q2b_df <- examdata %>%
  group_by(Class) %>%
  summarise(Count = n()) %>%
  arrange(desc(Count))

#plot industry data using lollipop
ggplot(q2b_df, aes(x = reorder(Class, Count), y = Count)) +
  geom_segment(aes(xend = reorder(Class, Count), yend = 0), color = "gray") +
  geom_point(size = 5, aes(fill = reorder(Class, Count)), color = "black", shape = 21, show.legend = FALSE) +
  scale_fill_viridis_d(option = "magma", begin = 0.2, end = 0.8) +
  labs(x = "Industry", y = "Count", title = "Popularity of Data Science Roles in Different Industries") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  coord_flip()+
  geom_text(aes(label = Count), hjust = -1.5, color = "black")+
  ylim(0, 300) 


#q2c: Compare salaries in different industries

#df to remove missing salary data, group by industry and calculate summary stats
q2c_df <- examdata %>%
  filter(!is.na(Salary)) %>%
  group_by(Class) %>%
  summarise(
    MedianSalary = median(Salary, na.rm = TRUE),
    MeanSalary = mean(Salary, na.rm = TRUE),
    MinSalary = min(Salary, na.rm = TRUE),
    MaxSalary = max(Salary, na.rm = TRUE),
    Count = n()
  )

write.csv(q2c_df, file = "q2c_df.csv")

#note for this df: not all industries had salary data

#scatter plot to show distribution and number of data points in each group. Jitter to important to see all points
ggplot(examdata, aes(x = Class, y = Salary, color = Class)) +
  geom_point(size = 3, alpha = 0.7, position = position_jitter(width = 0.15)) +
  scale_color_viridis_d(option = "magma", begin = 0.2, end = 0.8) +
  labs(x = "Industry", y = "Salary ($)", title = "Industry Salary Distributions") +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  guides(color = "none")

#filter df to get industries with 3+ data points
q2c_df_filtered <- examdata %>%
  filter(Class %in% c("Consulting & Strategy", "Government & Defence", "Information & Communication Technology", "Marketing & Communications", "Accounting"))

#violin plot to view distributions of filtered industries
ggplot(q2c_df_filtered, aes(x = Class, y = Salary, fill = Class)) +
  geom_violin() +
  geom_boxplot(width = 0.1, colour = "black", outlier.shape = NA, size = 0.1, fill = "white") +
  scale_fill_viridis_d(option = "magma", begin = 0.2, end = 0.8, alpha = 0.8) +
  labs(x = "Industry", y = "Salary", title = "Violin Plot of Salary by Industry") +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 25, hjust = 1),legend.position = "none")+
  scale_y_continuous(labels = scales::comma) 





#### Q3 ####

#q3a number of positions advertised changes over time
#q3b analyse the relationship between day of the week and the number of positions

#df to count the submissions on each date
q3a_df <- examdata %>%
  mutate(Date = as.Date(Date)) %>% #remove time
  group_by(Date) %>%
  summarise(Count = n()) %>%
  arrange(Date)

#bar plot to show timecourse. Line plot wasn't suitable as it masked the days with 0
ggplot(q3a_df, aes(x = Date, y = Count, fill = Date)) +
  geom_bar(stat = "identity") +
  scale_fill_viridis(option = "magma", begin = 0.2, end = 0.8, alpha = 0.8) +
  labs(x = "Date", y = "Count", title = "Count of Listings by Date") +
  theme_minimal()+
  guides(fill = "none")

#df to determine day 
q3b_df <- q3a_df %>%
  mutate(Day = weekdays(Date)) %>%
  group_by(Day) %>%
  summarise(DayCount = sum(Count)) %>%
  arrange(factor(Day, c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))) %>%
  na.omit()

write.csv(q3b_df, file = "q3b.csv")

#plot of day data
ggplot(q3b_df, aes(x = factor(Day, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")), y = DayCount, fill = Day)) +
  geom_bar(stat = "identity") +
  scale_fill_viridis_d(option = "magma", begin = 0.2, end = 0.8, alpha = 0.8) +
  labs(x = "Date", y = "Count", title = "Count of Listings by Day of the Week") +
  theme_minimal()+
  guides(fill = "none")

