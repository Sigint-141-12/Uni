# 1~~ Introduction

#Hello and welcome to my code!
#This code is a model to simulate a COVID-19, or some other pathogen, pandemic.
#The model consists of a population with a number of variables that affect the spread of disease
#Individuals (indices in a vector) all have a health status represented by a number:
#0- Healthy - default condition. Can be infected
#1- Sick - infected for a period of 10 days. Can infect. Cannot be infected
#2 - Dead - Cannot infect. Cannot be infected
#3 - Immune - Has vaccination or disease acquired immunity which may modify susceptibility to disease

#Created using RStudio 2023.03.0+386 "Cherry Blossom" Release (3c53477afb13ab959aeb5b34df1f10c237b256c3, 2023-03-09) for Windows
#Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) RStudio/2023.03.0+386 Chrome/108.0.5359.179 Electron/22.0.3 Safari/537.36

##1.1~~ How to use
#Select input parameters in Section 2
##Note: I had some fun with it and created some additional parameters and worked them into my code. Hope you enjoy experimenting with those

#Running all the code in Section 3 should set up all the variables, vectors etc
#Naming the simulation will give the resulting data frame a unique ID allowing you to go back and view previous data
#Simulation name also embedded into the plot title for easy identification/comparison too

#Running all code in Sections 4 and 5 should execute your simulation! If you want to run again; adjust code in Section 2 and run all of Section 3 to reset

#Data is output:
#* A df for that simulation
#* Input parameters and key results (deaths & R0) for that simulation
#* The above also get input into a df called 'simulations' which can be used to track and compare all the simulations you've run
#* The simulations df and charts also get exported to your working directory as they get created



##1.2~~ Limitations
#Code is slow. Could be my hardware (old) but my code lacks finesse for sure :)
#Use of functions. One thing I found tricky about R compared it Python is you can only return a single variable in functions, which I found cumbersome to
#use with multiple variables. Or I missed something and it just didn't click
#Simulations run for a number of days only, I did not code it stop after an event e.g. everyone dead, healthy etc
#Code to track time until virus eradiacted or everyone died would have been interesting data
#Not sure if I've over commented! they're helpful while I'm learning, apologies if spam

##1.3~~ Results
#Even with a low mortality rate, number of deaths can be high if infections are high
#Best way to reduce R0/infections (and deaths too) was to reduce the number of max_contacts (e.g. lockdown) or reduce infectivity (e.g. mask wearing)
#A long lasting vaccination was of little use if it offered little protection; people were reinfected while immune
#A good (high immunity coefficient) but short-lived vaccine produced visually interesting data that showed strong disease resurgence as immunity began to wear off

# 2~~ input parameters
simulations <- data.frame() #df to track results from multiple simulations
library(tidyverse)


population <- 1000    #size of population
days <- 365           #no. of days to iterate
max_contacts <- 20     #random number of people met between 1-20
mortality <- 0.03    #mortality rate. chance of dying (0.002 = 0.2%)
infective <- 0.1      #infection rate. chance of infecting someone (0.3 = 30%)
immunity <- 0.1       #immunity rate. post infection modifier (0.1 = 10x)
immunity_wane <- 90    #time taken (days) for immunity to wane and become ineffective
vaccination_rate <- 0  #percentage vaccination or other pre-existing immunity in population 
sick_time <- 10       #duration(days) that people remain sick,infective and at risk of dying (default = 10)

# 3~~ set up

simulation_name <- "3% Mortality Rate + Masks"     #give the simulation a name; names the DF for chart for that simulation
pop_health <- numeric(population)   #creates vector to track health status (0,1,2,3)
pop_time <- numeric(population)     #creates vector to track time since infected (days)
no_days <- 1                        #day counter for the simulation
no_healthy <- population            #Counts the number of healthy people. Variable for the data frame 'pandemic' -
no_sick <- 0                        #Counts the number of sick people. Variable for the data frame 'pandemic'
no_dead <- 0                        #Counts the number of dead people. Variable for the data frame 'pandemic'
no_immune <- 0                      #Counts the number of immune people. Variable for the data frame 'pandemic'
pandemic <- data.frame(healthy = integer(), sick = integer(), dead = integer(), immune = integer()) #df to plot the data
immune_time <- numeric(population)  #creates a vector to track time since immunity acquired



# 4~~ Functions  

## 4.1~~ Vaccination - Vaccinates a percentage the population at t=0
vaccinate <- function(population, vaccination_rate, pop_health) {
  no_vaccinate <- round(population*vaccination_rate / 100) # how many people to vaccinate based on pop and vaccination rate
  who_vaccinate <- sample(length(pop_health), no_vaccinate) #randomly select indices to vaccinate (=3)
  pop_health[who_vaccinate] <- 3 #update health status in pop_health
  return(pop_health)
}


## 4.2~~ Infect patient zero - infects a random individual
infect_p0 <- function(population, pop_health){
  patient_zero <- sample(1:population, 1) #randomly identifies patient zero
  pop_health[patient_zero] <-1 #infects patient zero
  return(pop_health)
}

## 4.3~~ Death check - Identifies sick people and runs a death test based on mortality rate
death_check <- function(pop_health, mortality) {
  sick <- which(pop_health == 1) #index of all the sick people
  dead <- sick[runif(length(sick)) < mortality] #generates random number for each sick person, those that die get added to this list
  return(replace(pop_health, dead, 2)) #replace used update global pop_health with deaths
}

## 4.4~~ infection - Runs an infection test after identifying healthy/immune people
simulate_infection <- function(pop_health, max_contacts, infective) {
    #check: can anyone new be infected i.e. is anyone healthy/'0'/'3'? Abort function if nobody to infect
    healthy_count <- sum(pop_health ==0) + sum(pop_health ==3)
    if (healthy_count == 0) {
      return(pop_health)
    }
    # how many contacts on this day. Iterates for each person infected
    contacts_day <- sum(unlist(sapply(pop_health[pop_health == 1], function(x) sample(0:max_contacts, 1)))) #subsets pop health to find infected people (1), function calculates number of contacts between 0-max contacts, sapply applies it to all subsetted indexes. Returns all the values as sum
 
  # who can be potentially infected this day
  infectable_day <- which(pop_health %in% c(0, 3)) #creates a logical vector indicating which elements of pop_health are either 0 or 3, which returns these indices to the new variable
  
  # who becomes a disease contact
  disease_contacts <- sample(infectable_day, contacts_day, replace = TRUE) #selects a number (contacts_day) of potential contacts (infectable_day)

  healthy_contacts <- (pop_health[disease_contacts] == 0) #identify disease contacts who are healthy according to pop_health
  immune_contacts <- (pop_health[disease_contacts] == 3) #identify disease contacts who are immune according to pop_health
  
  pop_health[disease_contacts[healthy_contacts & runif(length(disease_contacts)) < infective]] <- 1 #gets subset of disease_contacts who are healthy_contacts > logical vector with a probability of infective > uses vector to change subset of healthy contacts to 1
  pop_health[disease_contacts[immune_contacts & runif(length(disease_contacts)) < infective * immunity]] <- 1 #as above but for iummune contacts so immunity factor is included

  return(pop_health)
}

## 4.5~~ increase infection counter
time_counter <- function(pop_health, pop_time){
  not_dead <- pop_health != 2 #local variable to prevent the below advancing timers of dead people
  pop_time[pop_health == 1] <- pop_time[pop_health == 1]+1 #increases the day counter for the infected
  pop_health[not_dead & pop_time == sick_time] <<- (pop_health[not_dead & pop_time == sick_time]<-3) #person recovers and is now immune ('3'). Updates Global pop_health (<<-)
  pop_time[pop_time == sick_time] <- (pop_time[pop_time == sick_time]<-0) #resets day counter after reset back to healthy
return(pop_time)
}

## 4.6~~ Immunity Counter - counts how long someone has been immune, resets back to 1 based on immunity waning time
imm_counter <- function(pop_health, immune_time, immunity_wane){
  not_dead <- pop_health != 2 #local variable to prevent the below advancing timers of dead people
  immune_time[pop_health == 3] <- immune_time[pop_health == 3] + 1 #increases the immune timer for people who are immune (3)
  pop_health_new <- pop_health #creates local cope of the pop_health vector
  pop_health_new[not_dead & immune_time == immunity_wane] <- 0 #resets health status to zero for people who are alive and have immune time = wane time
  immune_time[immune_time == immunity_wane] <- 0 #resets immune time to zero for people who are alive and have immune time = wane time
  return(list(pop_health = pop_health_new, immune_time = immune_time))#uses temp vector to update health_status. Outputs pop_health/immune_time vectors as a list to extract
}

# 5~~ Simulation

pop_health <- vaccinate(population, vaccination_rate, pop_health)
pop_health <- infect_p0(population, pop_health)


while (no_days < days) {
  pop_health <- death_check(pop_health, mortality)
  no_infected_before = sum(pop_health==1) # sum of people infected before round of infections. Used to calculate no. of new infections
  pop_health <- simulate_infection(pop_health, max_contacts, infective)
  no_infected_after = sum(pop_health==1) # sum of people infecte dafter the round of infections. Used to calculate no. of new infections
  pop_time <- time_counter(pop_health, pop_time)
  imm_data <- imm_counter(pop_health, immune_time, immunity_wane) #creates the list 'imm_data' which we extract the updates to pop_health and immune_time from
    pop_health <- imm_data$pop_health #extracts data from lists to update pop_health
    immune_time <- imm_data$immune_time
  no_days <- no_days +1
  no_healthy <- sum(pop_health == 0)
  no_sick <- sum(pop_health == 1)
  no_dead <- sum(pop_health == 2)
  no_immune <- sum(pop_health == 3)
  new_infections <- no_infected_after - no_infected_before #calculates new infections for this iteration
  R0 <- (1 + max_contacts/2) * infective * sick_time #contact rate * transmissability * infectious period
  
  # Add results of current iteration as a new row to the data frame
  
  new_row <- data.frame(no_days = no_days, 
                         no_healthy = no_healthy, 
                         no_sick = no_sick, 
                         no_dead = no_dead, 
                         no_immune = no_immune,
                        new_infections = new_infections,
                        R0 = R0) # creates new row to add to df
  pandemic <- rbind(pandemic, new_row) #adds it to df
  assign(simulation_name, pandemic) #gives df a unique name
}

simulation_params <- data.frame(simulation_name, max_contacts, mortality, infective, immunity, immunity_wane, vaccination_rate, no_dead, R0) #creates a df with the results of this simulation
simulations <- rbind(simulations, simulation_params) #uipdates the simulations df with results from this sim

#plot the data
ggplot(pandemic, aes(x = no_days)) +
  ggtitle(paste("Covid 19 Simulation -", simulation_name))+
  geom_line(aes(y = no_healthy, colour = "Healthy")) +
  geom_line(aes(y = no_sick, colour = "Sick")) +
  geom_line(aes(y = no_dead, colour = "Dead")) +
  geom_line(aes(y = no_immune, colour = "Immune")) +
  geom_line(aes(y = new_infections, colour = "New Infections")) +
  labs(colour = "Status", y = "Count") +
  scale_color_manual(values = c("Healthy" = "Green", "Sick" = "Red", "Dead" = "Black", "Immune" = "Blue", "New Infections" = "Purple"))+ #used this to get the legend to display useful info
  xlab("Time (Days)")+
  ylab("Number of People")+
  labs(subtitle = paste0("Population size: ", population, "\n",
                    "Number of simulated days: ", days, "\n",
                    "Maximum number of contacts: ", max_contacts, "\n",
                    "Mortality rate: ", mortality*100, "%\n",
                    "Infection rate: ", infective*100, "%\n",
                    "Immunity increase: ", 1/immunity, "x\n",
                    "Time for immunity to wane: ", immunity_wane, " days\n",
                    "Vaccination/pre-existing immunity rate: ", vaccination_rate, "%"))

write.csv(simulations)
plot_name <- paste(simulation_name,".png", sep="")
ggsave(plot_name)
