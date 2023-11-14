#data scraping code for exam

library(rvest)
library(dplyr)
library(stringr)
library(lubridate)

#job scraping function
scrape_jobs <- function() {
  page <- 1 #start at page 1
  
  # Create an empty data frame to store the scraped data
  df <- data.frame(
    Title = character(),
    Company = character(),
    Location = character(),
    Class = character(),
    Subclass = character(),
    Salary = character(),
    Date = character(),
    Description = character(),
    stringsAsFactors = FALSE # Keep columns as strings, adjust later if needed
  )
  
  while (TRUE) { #infinite loop, until break encountered
    # Read the HTML content of the web page with the given page number
    seek <- read_html(paste0("https://www.seek.com.au/data-scientist-jobs?page=", page))
    
    # Extract the job advertisements from the web page
    job_adverts <- seek %>% html_nodes("div._1wkzzau0.a1msqi7e")
    
    # Break the loop if no job advertisements are found
    if (length(job_adverts) == 0) {
      break
    }
    
    for (job_advert in job_adverts) {
      # Extract data from each job advertisement. If an element cannot be found, set it as NA to maintain consistent vector lengths.
      title <- job_advert %>% html_node("a[data-automation='jobTitle']") %>% html_text() %>% ifelse(length(.) == 0, NA, .)
      company <- job_advert %>% html_node("a[data-automation='jobCompany']") %>% html_text() %>% ifelse(length(.) == 0, NA, .)
      location <- job_advert %>% html_node(".a1msqi6e .a1msqi6q:nth-child(1) .lnocuo7") %>% html_text() %>% ifelse(length(.) == 0, NA, .)
      classification <- job_advert %>% html_node("a[data-automation='jobClassification']") %>% html_text() %>% ifelse(length(.) == 0, NA, .)
      subclassification <- job_advert %>% html_node("a[data-automation='jobSubClassification']") %>% html_text() %>% ifelse(length(.) == 0, NA, .)
      salary <- job_advert %>% html_node("div._1wkzzau0.v28kuf0.v28kuf4.v28kuf2") %>% html_text() %>% ifelse(length(.) == 0, NA, .)
      date <- job_advert %>% html_node("span[data-automation='jobListingDate']") %>% html_text() %>% ifelse(length(.) == 0, NA, .)
      desc <- job_advert %>% html_node("span[data-automation='jobShortDescription']") %>% html_text() %>% ifelse(length(.) == 0, NA, .)
      
      # Add the extracted data to the data frame
      df <- df %>% add_row(
        Title = title,
        Company = company,
        Location = location,
        Class = classification,
        Subclass = subclassification,
        Salary = salary,
        Date = date,
        Description = desc)
    }
    
    page <- page + 1 #increase page count and start again
  }
  
  return(df)
}


# Call the function to scrape all web page numbers
jobs_df <- scrape_jobs()


#data cleaning

jobs_df <- jobs_df %>%
  filter(!is.na(Title)) %>% #remove lines with no title
  mutate(State = case_when( #extract state from location
    grepl("NSW", Location, ignore.case = TRUE) ~ "NSW",
    grepl("VIC", Location, ignore.case = TRUE) ~ "VIC",
    grepl("QLD", Location, ignore.case = TRUE) ~ "QLD",
    grepl("SA", Location, ignore.case = TRUE) ~ "SA",
    grepl("WA", Location, ignore.case = TRUE) ~ "WA",
    grepl("TAS", Location, ignore.case = TRUE) ~ "TAS",
    grepl("NT", Location, ignore.case = TRUE) ~ "NT",
    grepl("ACT", Location, ignore.case = TRUE) ~ "ACT",
    TRUE ~ NA_character_ #if no states found, adds NA
  )) %>%
  mutate(Class = str_replace_all(Class, "\\(|\\)", "")) %>%  #removes brackets from Class
  mutate(Salary = str_replace_all(Salary, ",", "")) %>% #remove comma 1000 delimiter
  mutate(Salary = str_replace(Salary, "0k", "0000")) %>% #converts 'k' 1000 notation in 000
  mutate(Salary_Numbers = str_extract_all(Salary, "\\d+(\\.\\d+)?")) %>% #extract numerical values using regular expression (d+ = 1+ digits, (\\.\\d+) = decmimal+digits, ? = optional)
  mutate(Largest_Salary = sapply(Salary_Numbers, function(x) max(as.numeric(x)))) %>% #get largest value from salary numbers
  mutate(Largest_Salary = ifelse(as.numeric(Largest_Salary) < 50000, NA, Largest_Salary)) %>% #filter out low salaries. Possibly part time or paid per hour
  #grepl to check if "d or h", if present extract number. Multiply days by 24 to convert to hours
  mutate(Hours = case_when(
    grepl("\\d+h\\s*ago", Date) ~ as.numeric(str_extract(Date, "\\d+")),
    grepl("\\d+d\\s*ago", Date) ~ as.numeric(str_extract(Date, "\\d+")) * 24,
    TRUE ~ NA_integer_)) %>%
  mutate(extraction_time = Sys.time()) %>%
  mutate(age = extraction_time - hours(Hours) ) #estimate advert age by subtracting age website presents minus datetime when scraping

#new, clean df without excess columns
examdata <- jobs_df %>%
  select(Title, Company, State, Class, Subclass, Largest_Salary, age, Description) %>%
  rename(Salary = Largest_Salary) %>%
  rename(Date = age)



save(examdata, file = "examdata.RData")

