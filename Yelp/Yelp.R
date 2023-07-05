# COMP 5070 Statistical Programming # for Data Science
# Assignment 2 - YELP Review Analysis Code
# By Tom Midgley

#### Set Up ####

library(tidyverse)
library(viridis)
library(RColorBrewer)


#import data
yelp_data <- read.csv("yelp_reviews.csv")


#### Q2 ####
#Q2 - 2.	A statistical summary of stars, review_length, pos_words, neg_words and net_sentiment
# aim: output stats for each column, charts of distribution

#data frame for analysis of Q2 variables
q2_df <- select(yelp_data, stars, review_length, pos_words, neg_words, net_sentiment)

#number of observations to report
n_obs <- q2_df %>%
  summarise(count = n())

#get summary stats of Q2 variables. Rounded to 0 decimal places
q2_sumstat1 <- q2_df %>%  #get variables and rename them
  select(
    "Stars" = stars,
    "Review Length" = review_length,
    "Positive Words" = pos_words,
    "Negative Words" = neg_words,
    "Net Sentiment" = net_sentiment
  ) %>%
  summarise(across(everything(),list(Mean = mean, Min = min, Max = max, Median = median, SD = sd, Count = ~n(), Q1 = ~quantile(.,0.25), Q2 = ~quantile(.,0.75)))) %>% # calculate summary stats
  pivot_longer(everything(), names_to = "Variable", values_to = "Value")%>% #pivot table for tabulation
  separate("Variable", into = c("variable", "stat"), sep = "_") %>% #split variable into individual columns
  spread(stat, Value) %>% mutate(Mean = round(Mean,0)) %>% mutate(SD = round(SD,1)) #format data for presentation

write.csv(q2_sumstat1, file = "q2_sumstat1.csv")


#get summary stats of Q2 variables. Rounded to 1 decimal place for reporting average star value  
q2_sumstat2 <- q2_df %>%
  select(
    "Stars" = stars,
    "Review Length" = review_length,
    "Positive Words" = pos_words,
    "Negative Words" = neg_words,
    "Net Sentiment" = net_sentiment
  ) %>%
  summarise(across(everything(),list(Mean = mean, Min = min, Max = max, Median = median, SD = sd, Count = ~n(), Q1 = ~quantile(.,0.25), Q2 = ~quantile(.,0.75)))) %>%
  pivot_longer(everything(), names_to = "Variable", values_to = "Value")%>% #reshape data from wide to long
  separate("Variable", into = c("variable", "stat"), sep = "_") %>%
  spread(stat, Value) %>% mutate(Mean = round(Mean,1)) %>% mutate(SD = round(SD,1))

write.csv(q2_sumstat2, file = "q2_sumstat2.csv")


#separate df for pos, neg and sentiment for building into combined box plot for concise reporting
q2_words_sent <- q2_df %>%
  gather(key = "Variable", value = "Count", pos_words, neg_words, net_sentiment) %>%
  mutate(Variable = str_replace(Variable, "pos_words", "Positive Words")) %>%
  mutate(Variable = str_replace(Variable, "neg_words", "Negative Words")) %>%
  mutate(Variable = str_replace(Variable, "net_sentiment", "Net Sentiment"))

#triple box plot for pos, neg and sentiment distribution reporting
ggplot(q2_words_sent, aes(x = Variable, y = Count, fill = Variable)) +
  geom_boxplot(alpha = 0.3) +
  labs(x = "", y = "Count", title = "Distribution of Positive Words, Negative Words and Net Sentiment of Yelp Reviews")+
  scale_fill_brewer(palette = "Paired") +
  theme(legend.position="none")


# 'plot for star rating distribution
ggplot(q2_df, aes(x=stars, fill=..x..)) +
  geom_histogram(binwidth = 1, colour = "white")+
  scale_fill_viridis_c(option = "mako", begin = 0, end = 0.9) +
  ggtitle("Distribution of Stars")+ 
  xlab("Stars")+
  ylab("Count")+
  theme_minimal()+
  theme(legend.position = "none")

#plot of review length distribution
ggplot(q2_df, aes(x=review_length, fill=..x..)) +
  geom_histogram(binwidth = 10, colour = "white")+
  scale_fill_viridis_c(option = "mako") +
  ggtitle("Distribution of Yelp Review Length")+
  xlab("Number of Words per Review")+
  ylab("Number of Reviews")+
  theme_minimal()+
  theme(legend.position = "none")

#plot to count the number of each star rating for report table
q2_df_stars <- yelp_data %>%
  group_by(stars) %>%
  count()


####Q3####
#Q3a - Create tables of the counts of numbers of positive words per review and the counts of numbers of negative words per review
#Q3b - Produce a plot to display the first 20 entries

#get pos words data, add column for grouping
q3_pos_words <- yelp_data %>%
  group_by(pos_words) %>%
  count() %>%
  head(20) %>%
  mutate(group="Positive")


#get neg words data, add column for groupuing  
q3_neg_words <- yelp_data %>%
  group_by(neg_words) %>%
  count() %>%
  head(20) %>%
  mutate(group="Negative")

#merge pos and neg data tables together. Remove surplus columns
q3_all_words <- merge(q3_pos_words, q3_neg_words, by.x = "pos_words", by.y = "neg_words", all = TRUE) %>%
  rename(Positive = n.x, Negative= n.y, Count = pos_words) %>%
  mutate(group.x = NULL) %>%
  mutate(group.y= NULL)


#summary stats table generation
q3_sumstat1 <- q3_all_words %>%
  summarise(across(everything(),list(Mean = mean, 
                                     Min = min, 
                                     Max = max, 
                                     Median = median, 
                                     SD = sd, 
                                     Count = ~n(), 
                                     Q1 = ~quantile(.,0.25), 
                                     Q2 = ~quantile(.,0.75)))) %>%
  pivot_longer(everything(), names_to = "Variable", values_to = "Value")%>%
  separate("Variable", into = c("variable", "stat"), sep = "_") %>%
  spread(stat, Value) %>% 
  mutate((across(where(is.numeric), round, 0)))


write.csv(q3_sumstat1, file = "q3_sumstat1.csv")

#add group coulmn for pos and neg words for plotting
q3_grouped_words <- bind_rows(
  q3_pos_words %>% select(number = pos_words, words = n, group = group),
  q3_neg_words %>% select(number = neg_words, words = n, group = group)
)

#grouped bar plot of number of positive and negative reviews plot
ggplot(q3_grouped_words, aes(fill=group, x=number, y=words))+
  geom_bar(position = "dodge", stat = "identity")+
  scale_fill_viridis_d(option = "mako", begin = 0.3, end = 0.5)+
  labs(x = "Number of Positive or Negative Words", y = "Number of Reviews", fill = "") +
  ggtitle("Comparison of the Total Number of Positive or Negative Words in Yelp Reviews")+
  theme_minimal()



#### Q4 ####
#Q4 - Create tables net sentiment per review & produce a plot

#group data by net sentiment and count number of reviews for each score
q4_sentiment <- yelp_data %>%
  group_by(net_sentiment) %>%
  count() 

#density plot to show sentiment distibution; highlight 0 and shade positive 
ggplot(q4_sentiment, aes(x = net_sentiment, y = n)) +
  geom_line(aes()) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "red", size = 1.2) +
  geom_area(aes(fill = ifelse(net_sentiment >= 0, "Positive", "Negative")), alpha = 0.5) +
  scale_fill_viridis_d(option = "mako", begin = 0.3, end = 0.5) +
  labs(x = "Net Sentiment Score", y = "Number of Reviews", fill = "") +
  ggtitle("Net Sentiment Distribution") +
  theme_minimal()


#% of reviews with positive sentiment
q4_percentage_positive <- yelp_data %>%
  filter(net_sentiment > 0) %>%
  summarise(count=n()) / n_obs * 100

# calculate average number of pos/eng words at each sentiment level
q4_averages <- yelp_data %>%
  group_by(net_sentiment) %>%
  summarise(average_pos_words = mean(pos_words),
            average_neg_words = mean(neg_words))

#scatter plot highlight no. of pos and negative words at each sentiment level
ggplot(q4_averages, aes(x = net_sentiment)) +
  geom_point(aes(y = average_pos_words, colour = "Positive Words"), size = 2) +
  geom_point(aes(y = average_neg_words, colour = "Negative Words"), size = 2) +
  labs(x = "Net Sentiment", y = "Average Number of Positive or Negative Words", fill= "") +
  scale_colour_viridis_d(option = "mako", begin = 0.1, end = 0.5)+
  ggtitle("Average Number of Positive and Negative Words by Net Sentiment Score") +
  theme_minimal()


#### Q5 ####
#Q5a Present and discuss the average review length per star category
#Q5b Produce an appropriate visualisation 
#Q5c Explain your choice of average 


#data frame for analysis of Q5 variables
q5_df <- yelp_data %>%
  select(stars, review_length)

#distribution of review length by star rating with a violin plot
ggplot(q5_df,aes(x = as.factor(stars), y = review_length, fill = as.factor(stars))) +
  geom_violin()+
  geom_boxplot(width = 0.1, colour = "grey", outlier.shape = NA, size = 1.0, fill = "white") +
  labs(x = "Stars", y = "Review Length") +
  scale_fill_viridis_d(option = "mako")+
  ggtitle("Distribution of Review Length by Review Star Rating")+
  theme_minimal()+
  theme(legend.position = "none")

#summary stats for stars
q5_sumstat1 <- q5_df %>%
  group_by(stars) %>%
  summarise(Median = median(review_length),
            Q1 = quantile(review_length, 0.25),
            Q3 = quantile(review_length, 0.75),
            Min = min(review_length),
            Max = max(review_length),
            Count = n(),
            Mean = mean(review_length),
            StDev = sd(review_length))

write.csv(q5_sumstat1, file = "q5_sumstat1.csv")

####Q6####
# Q6a Analyse reviews voted as useful (variable votes_useful)
# Q6b are there any relationship between usefulness of reviews and star-rating provided 
# Q6c are there any relationship between usefulness of reviews and length of the review 

##Q6a - review length general stats, intro

#data frame with variables for Q6 only
q6_df <- yelp_data %>%
  select(votes_useful, stars, review_length)


#6a plot for review length of all reviews. No grouping (6a)
ggplot(q6_df, aes(x=votes_useful, fill=..x..)) +
  geom_histogram(binwidth = 1, colour = "white")+
  scale_fill_viridis(option = "mako", begin = 0.2, end = 1) +
  labs(title = "Distribution of Useful Votes", x = "Useful Votes", y = "Frequency") +
  theme_minimal()+
  theme(legend.position = "none") +
  xlim(-1,25)


#6a- sum stats for useful votes. No grouping yet. Use median. Mean output for purpose of interest
q6_useful_sumstat <- q6_df %>%
  summarise(Median = median(votes_useful),
            Q1 = quantile(votes_useful, 0.25),
            Q3 = quantile(votes_useful, 0.75),
            Min = min(votes_useful),
            Max = max(votes_useful),
            Count = n(),
            Mean = mean(votes_useful),
            StDeV = sd(votes_useful),
            Total = sum(votes_useful),
            Reviews_with_0_Votes = sum(votes_useful == 0),
            Reviews_with_1_plus_Votes = sum(votes_useful>0))

write_csv(q6_useful_sumstat, "q6_useful_sumstat.csv")

## - Q6b - useful reviews relative to star rating

#Q6b summary stats (star grouping)
q6_useful_sumstat_star <- q6_df %>%
  group_by(stars) %>%
  summarise(Median = median(votes_useful),
            Q1 = quantile(votes_useful, 0.25),
            Q3 = quantile(votes_useful, 0.75),
            Min = min(votes_useful),
            Max = max(votes_useful),
            Count = n(),
            Mean = mean(votes_useful),
            StDeV = sd(votes_useful),
            Total = sum(votes_useful)) %>%
  mutate(votes_per_review = Total/Count)

write_csv(q6_useful_sumstat_star, "q6_useful_sumstat_star.csv")



#boxplot for all data
ggplot(q6_df, aes(x = votes_useful, y = as.factor(stars), fill = as.factor(stars))) +
  geom_boxplot() +
  scale_fill_viridis_d(option = "mako")+
  labs(title = "Distribution of Useful Votes in Reviews with Different Star Ratings", x = "Useful Votes", y = "Star Rating") +
  theme_minimal()+
  theme(legend.position="none")

#boxplot with outliers removed to estimate statistical differences
ggplot(q6_df, aes(x = votes_useful, y = as.factor(stars), fill = as.factor(stars))) +
  geom_boxplot(outlier.shape = NA) +
  xlim(0,5)+
  scale_fill_viridis_d(option = "mako", begin = 0.3, end = 1)+
  labs(title = "Distribution of Useful Votes in Reviews with Different Star Ratings (Outliers Removed)", x = "Useful Votes", y = "Star Rating") +
  theme_minimal()+
  theme(legend.position="none")


#bar and line plot for volume: number of total votes cast vs number of votes per review
ggplot(q6_useful_sumstat_star, aes(x = stars)) +
  geom_bar(aes(y = Total, fill = as.factor(stars)), alpha = 0.9, stat = "identity") +
  geom_line(aes(y = votes_per_review*350000), colour = "blue", size =1.5) +
  scale_fill_viridis_d(option = "mako", end = 0.8) +
  labs(x = "Stars", y = "Number of Useful Votes Cast", fill = "") +
  ggtitle("Comparison of Total Number of Votes and Votes per Number of Reviews per Star Category") +
  theme_minimal() +
  scale_y_continuous(sec.axis = sec_axis(~./350000, 
                                         name = "Votes per Review"))+
  theme(legend.position = "none", axis.title.y.right = element_text(colour = "blue"), axis.text.y.right = element_text(colour = "blue"))
ggsave("Q6_ stars volume.png", dpi = "retina")

###Q6c - useful reviews relative to review length


#categorise reviews into length 'bins'
q6_length <- q6_df %>%
  mutate(review_length_category = cut(review_length, breaks = c(0, 100, 200, 300, 400, 500, 600, 700, 800, 900, Inf), labels = c("1-100", "101-200", "201-300", "301-400", "401-500", "601-700", "701-800", "801-900", "901-1000", "1000+")))

#summary stats with review length in bins
q6_useful_sumstat_length_bins <- q6_length %>%
  group_by(review_length_category) %>%
  summarise(Median = median(votes_useful),
            Q1 = quantile(votes_useful, 0.25),
            Q3 = quantile(votes_useful, 0.75),
            Min = min(votes_useful),
            Max = max(votes_useful),
            Count = n(),
            Mean = mean(votes_useful),
            StDeV = sd(votes_useful),
            Sum = sum(votes_useful),
  )

write_csv(q6_useful_sumstat_length_bins, "q6_useful_sumstat_length_bins.csv")




#boxplot for review length categories and votes useful
ggplot(q6_length, aes(x = as.factor(review_length_category), y = votes_useful, fill = as.factor(review_length_category))) +
  geom_boxplot() +
  scale_fill_viridis_d(option = "mako")+
  labs(x = "Review Length", y = "Useful Votes") +
  ggtitle("Scatter Plot of Review Length vs. Useful Votes", subtitle = "NA = Review of zero length")+
  theme_minimal()+
  theme(legend.position="none")

#boxplot for withoutliers removed and scale shortened
ggplot(q6_length, aes(x = as.factor(review_length_category), y = votes_useful, fill = as.factor(review_length_category))) +
  geom_boxplot(outlier.shape = NA) +
  ylim(0,5)+
  scale_fill_viridis_d(option = "mako", begin = 0.3)+
  labs(x = "Review Length", y = "Useful Votes") +
  ggtitle("Scatter Plot of Review Length vs. Useful Votes (Outliers Removed)", subtitle = "NA = Review of zero length")+
  theme_minimal()+
  theme(legend.position="none")


#bar and line plot for volume of reviews
ggplot(q6_useful_sumstat_length_bins, aes(x = as.factor(review_length_category))) +
  geom_bar(aes(y = Count, fill = as.factor(review_length_category)), alpha = 0.9, stat = "identity") +
  geom_line(aes(y = Sum, colour = Sum), size =1, group =1) +
  scale_fill_viridis_d(option = "mako", end = 0.8) +
  labs(x = "Review Length", y = "Number of Reviews", fill = "") +
  ggtitle("Comparison of Number of Review and Votes at Different Review Lengths") +
  theme_minimal() +
  scale_y_continuous(sec.axis = sec_axis(~./1, 
                                         name = "Total Number of Votes"))+
  theme(legend.position = "none", axis.title.y.right = element_text(colour = "blue"), axis.text.y.right = element_text(colour = "blue"))

ggsave("Q62.png", dpi = "retina")


#### Q7 ####

# 7.	Study the number of reviews per day and how does it change over time

#df for q7; grouped by dates and count the number of reviews per date
q7_df <- yelp_data %>%
  group_by(date) %>%
  summarise(count = n(), average_net_sentiment = mean(net_sentiment))

#convert date column to date format
q7_df$date <- as.Date(q7_df$date)

#for a bit of fun: reviews and sentiment on xmas day  
q7_xmas <- q7_df %>%
  filter(month(date) == 12, day(date) == 25)
q7_xmas_seniment <- mean(q7_xmas$average_net_sentiment)

#line plot with all data over time. Annotations added
ggplot(q7_df, aes(x=date, y=count)) + 
  geom_point(alpha = 0.2, colour = "skyblue") +
  geom_smooth() +
  annotate("text", x=as.Date("2005-10-12"), y =250, label = "First ever Yelp review: \n12th October 2004", colour = "black") +
  annotate("point", x=as.Date("2004-10-12"), y=1, size=10, shape=21, fill="transparent" )+
  annotate("text", x=as.Date("2013-10-12"), y =1872, label = "Highest ever number \nof reviews in one day: \n1972", colour = "black") +
  annotate("point", x=as.Date("2015-01-03"), y=1972, size=10, shape=21, fill="transparent" )+
  annotate("point", x=as.Date(q7_xmas$date), y = (q7_xmas$count), size=5, shape=21, fill="transparent", colour = "red")+
  annotate("text", x=as.Date("2012-01-01"), y =0, label = "Number of Yelp Revews on Christmas Day", colour = "red")+
  labs(x = "Date", y = "Number of Reviews") +
  ggtitle("Yelp Reviews Over Time")+
  theme_minimal()



#group into years for concise analysis of sum stats
q7_annual_reviews <- q7_df %>%
  group_by(year = format(date, "%Y")) %>%
  summarise(total_reviews = sum(count)) %>%
  arrange(year) %>%
  mutate(year_on_year_increase = total_reviews - lag(total_reviews),
         percentage_increase = (year_on_year_increase / lag(total_reviews)) * 100) %>%
  mutate(percentage_increase = round(percentage_increase, 1)) %>%
  mutate(YOY_growth = (year_on_year_increase - lag(year_on_year_increase)) / lag(year_on_year_increase)*100) %>%
  mutate(YOY_growth = round(YOY_growth, 1))

write_csv(q7_annual_reviews,"q7_annual_reviews.csv")

#### Q8 ####

# Q8a Best business
# Q8B Best user

#df for best business analysis
q8_business_df <- yelp_data %>%
  group_by(business_id) %>%
  summarise(count = n(),
            avg_stars = mean(stars),
            avg_review_length = mean(review_length),
            avg_net_sentiment = mean(net_sentiment),
            weighted_avg_stars = sum(stars * count) / count,
            weighted_avg_review_length = sum(review_length * count) / count,
            weighted_avg_net_sentiment = sum(net_sentiment*count)/count)



#scatter plot to highligt the high number of scores powered by low number of reviews
ggplot(q8_business_df, aes(x = avg_stars, y = count, colour = as.numeric(count))) +
  geom_point(alpha = 0.5) +
  scale_color_viridis(option="mako", direction =-1, end = 0.8)+
  labs(x = "Average Stars", y = "Number of Reviews", fill = "") +
  ggtitle("Scatter Plot of Average Stars and Number of Reviews") +
  theme_minimal() +
  theme(legend.position = "none") + 
  geom_text(data = q8_business_df %>%
              filter(count > 3000), 
            aes(label = business_id),             
            #nudge_y = -100,
            nudge_x = 0.4,
            size = 3
  )

#same but for net sentiment
ggplot(q8_business_df, aes(x = avg_net_sentiment, y = count, colour = as.numeric(count))) +
  geom_point(alpha = 0.5) +
  scale_color_viridis(option="mako", direction =-1, end = 0.8)+
  labs(x = "Average Sentiment", y = "Number of Reviews", fill = "") +
  ggtitle("Scatter Plot of Average Sentiment and Number of Reviews") +
  theme_minimal() +
  theme(legend.position = "none")+
  geom_text(data = q8_business_df %>%
              filter(count > 3000), 
            aes(label = business_id),             
            nudge_x = 5,
            size = 3
  )

q8_top_business_df <- q8_business_df %>%
  arrange(desc(weighted_avg_stars)) %>%
  slice_head(n = 5) %>%
  mutate(
    avg_stars = round(avg_stars, 1),
    avg_net_sentiment = round(avg_net_sentiment, 1),
    avg_review_length = round(avg_review_length, 0))

write_csv(q8_top_business_df, "q8_top_business_df.csv")


#8b - df for best user analysis
q8_user_df <- yelp_data %>%
  group_by(user_id) %>%
  summarise(count = n(),
            avg_stars = mean(stars),
            stars_sd = sd(stars),
            avg_review_length = mean(review_length),
            avg_net_sentiment = mean(net_sentiment),
            net_sentiment_sd = sd(net_sentiment),
            total_votes = sum(votes_total)) %>%
  mutate(words_output = avg_review_length * count)


#plot to look at highest "output" users - count and review length ("prolific reviewers")
ggplot(q8_user_df, aes(x = avg_review_length, y = count, colour = as.numeric(avg_review_length))) +
  geom_point(alpha = 0.5) +
  scale_color_viridis(option="mako", direction =-1, end = 0.8)+
  labs(x = "Average Review Length", y = "Number of Reviews", fill = "") +
  ggtitle("Scatter Plot of Average Number of Reviews by Yelp Users and Their Average Review Length") +
  theme_minimal() +
  theme(legend.position = "none") + 
  geom_text(data = q8_user_df %>%
              filter(count > 1000), 
            aes(label = user_id),             
            #nudge_y = -100,
            nudge_x = 100,
            size = 3) +
  geom_text(data = q8_user_df %>%
              filter(count > 100 & avg_review_length > 500), 
            aes(label = user_id),             
            #nudge_y = -100,
            nudge_x = 100,
            size = 3) +
  geom_text(data = q8_user_df %>%
              filter(count > 400 & avg_review_length > 375), 
            aes(label = user_id),             
            #nudge_y = -100,
            nudge_x = 100,
            size = 3) +
  geom_text(data = q8_user_df %>%
              filter(count > 500 & avg_review_length > 312.5), 
            aes(label = user_id),             
            #nudge_y = -100,
            nudge_x = 100,
            size = 3) 

#df for plotting most prolific users
q8_prolific_users_df <- q8_user_df %>%
  arrange(desc(words_output)) %>%
  slice_head(n = 10) %>%
  mutate(
    avg_stars = round(avg_stars, 1),
    avg_net_sentiment = round(avg_net_sentiment, 1),
    avg_review_length = round(avg_review_length, 0),
    user_id = reorder(user_id, words_output))

write_csv(q8_prolific_users_df, "q8_prolific_users_df.csv")

#barplot for prolific users
ggplot(q8_prolific_users_df, aes(y = words_output, x = user_id, fill = as.factor(user_id))) +
  geom_bar(alpha = 0.9, stat = "identity") +
  scale_fill_viridis_d(option="mako")+
  geom_text(aes(label = words_output, hjust = -0.1), size=3) +
  labs(x = "User", y = "Number of Words Written", fill = "") +
  coord_flip()+
  ggtitle("Most Prolific Yelp Reviewers") +
  theme_minimal() +
  theme(legend.position = "none") +
  ylim(0,400000)

ggsave("Q8_Most Prolific Yelp Reviewers.png", dpi = "retina")

#most popular users: those with the most votes
ggplot(q8_user_df, aes(x = total_votes, y = count, colour = as.numeric(total_votes))) +
  geom_point(alpha = 0.5) +
  scale_color_viridis(option="mako", direction =-1, end = 0.8)+
  labs(x = "Total Number of Votes", y = "Number of Reviews", fill = "") +
  ggtitle("Scatter Plot of Average Number of Reviews by Yelp Users and Their Number of Votes by the Yelp Community") +
  theme_minimal() +
  theme(legend.position = "none")+
  geom_text(data = q8_user_df %>%
              filter(total_votes > 10000 & count >500), 
            aes(label = user_id),             
            nudge_x = -2000,
            size = 3)

ggsave("Q8_Scatter Plot of Average Number of Reviews by Yelp Users and Their Number of Votes by the Yelp Community.png", dpi = "retina")

#df for most popular users
q8_popular_users_df <- q8_user_df %>%
  mutate(votes_per_review = total_votes / count) %>%
  filter(count >250) %>%
  arrange(desc(votes_per_review)) %>%
  slice_head(n = 10) %>%
  mutate(
    votes_per_review = round(votes_per_review, 0))

write_csv(q8_popular_users_df, "q8_popular_users_df.csv")

#barplot for most popular users
ggplot(q8_popular_users_df, aes(y = votes_per_review, x = user_id, fill = as.factor(user_id))) +
  geom_bar(alpha = 0.9, stat = "identity") +
  scale_fill_viridis_d(option="mako")+
  geom_text(aes(label = votes_per_review, hjust = -0.1), size=3) +
  labs(x = "User", y = "Votes per Review", fill = "") +
  coord_flip()+
  ggtitle("Most Popular Yelp Reviewers") +
  theme_minimal() +
  theme(legend.position = "none")

ggsave("Q8_Most Popular Yelp Reviewers.png", dpi = "retina")

#best user: most prolific and popular?
q8_best_user <- 
  q8_user_df %>%
  mutate(votes_per_review = total_votes / count) %>%
  mutate(words_output = avg_review_length * count) %>%
  filter(votes_per_review <52)


# combine user DFs to find best
q8_user_combined_df <- full_join(q8_popular_users_df, q8_prolific_users_df) %>%
  mutate(votes_per_review = total_votes / count) 


#plot for most prolifi and popular
ggplot(q8_user_combined_df, aes(x = words_output, y = votes_per_review, colour = as.numeric(votes_per_review))) +
  geom_point(size = 5) +
  scale_color_viridis(option="mako", direction =-1, end = 0.8)+
  labs(x = "Number of Words Written", y = "Votes per Review", fill = "") +
  ggtitle("Top Yelpers: Number of Words Written and Average Community Score") +
  theme_minimal() +
  theme(legend.position = "none") +
  geom_text(data = q8_user_combined_df %>%
              filter(user_id == "fczQCSmaWF78toLEmb0Zsw"), 
            aes(label = user_id),             
            nudge_x = 35000,
            size = 3)

ggsave("Q8_best_user.png", dpi = "retina")
