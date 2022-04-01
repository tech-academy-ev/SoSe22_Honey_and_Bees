##### Author: Inga Lasys
##### Date: March 2022
##### Solutions: Honey Bees Project, Summer Semester 2022

library(lubridate)
library(readr)
library(dplyr)
library(ggplot2)

###############################################################################
### Task 0: Import Data (ungraded, does not earn points b/c obviously)

bees <- read_csv("~/Honey_Bees/Data/bees.csv")
honey <- read_csv("~/Honey_Bees/Data/honey.csv")
weather <- read_csv("~/Honey_Bees/Data/weather.csv")

# Let start by looking at the bee & honey data. To start viewing the data you have just imported 
# use head(), str(), or class() to get an overview.Have a look at the frequency of the data (is there a difference?)
# Do you see any missing values or data entries that are different from the other entries?
# What are the data formats? Look at the unit of every variable, can you make sense of the units?
# write down in a couple of sentences which errors you first notice and what could be an issue later in the project 
# no worries, there is no right or wrong

### Proceed with honey data
### Task 1: Convert from pounds to kg and prices to $/kg
# As you can see in the column names, the honey data set is either in pounds as we are dealing with a 
# US Dataset. However, we naturally don't understand pounds, so we would like to replace a pound measure into the metric system.
# Tipp: we recommend multiplying the values that are in 1K first before transforming into kg), while you are at it, do you maybe want to give your new variable in kg an new, shorter name?

#Isi: maybe add here as Task 1:

### Proceed with honey data
### Task 1: Give some overall statements 
#Lets stick with the honey data: Since you inspected the data, it would be interesting to indicate some outstanding reportings. Which state had the most producing colonies ever and in which year? Is there something special to that state? (Not the over all United States ;) )
#Next, lets see what state had the lowest price for honey. How low was it?
#Finally, what is the total Honey production for 2016? #UNITES STATES EXCLUDIEREN?

colonies <- honey %>% 
  select(c(State, Year, `Honey producing colonies (thousand)`)) %>% 
  arrange(desc(`Honey producing colonies (thousand)`)) # it's North Dakota in 2018 , following states are all North Dakota

prices <- honey %>% 
  select(c(State, Year, `Average price per pound (dollars)`)) %>% 
  arrange(`Average price per pound (dollars)`) #it's Hawaii in 2019: 1.28 Dollar per pound

prod_colonies <- honey %>% 
  select(c(State, Year, `Honey producing colonies (thousand)`)) %>%
  filter (Year == 2016) %>%
  filter (State != "United States") %>% 
  summarise(sum(`Honey producing colonies (thousand)`)) #2775

### Task 2: Convert from pounds to kg and prices to $/kg
# As you can see in the column names, the honey data set is in pounds as we are dealing with a 
# US Dataset. However, we naturally don't understand pounds, so we would like to replace a pound measure into the metric system.
# while you are at it, do you maybe want to give your new variable in kg an new, shorter name? Finally, please only leave the necessary columns in the data set.
# Tipp: we recommend multiplying the values that are in 1K first before transforming into kg)

honey$producing_colonies <- honey$`Honey producing colonies (thousand)` * 1000    # Creates new variable
honey$yield_colony_kg <- honey$`Yield per colony (pounds)`*0.453592 # Creates new variable in kg
honey$production_kg <- honey$`Production (1,000 pounds)`*1000*0.453592 # in kgs (this was per 1000 pounds)
honey$stock_price_kg <- honey$`Stocks December 15  (1,000 pounds)`/1000*0.453592*100 #stock price per 100kg
honey$avg_price_kg <- honey$`Average price per pound (dollars)`*0.453592 # price per kg of honey (average price)
honey$prod_value <- honey$`Value of production (1,000 dollars)`*1000

honey <- honey[, -c(2:7)] #removes unnecessary columns (less clutter, less RAM used)

### Task 3: Have a look at your column values. Some columns now have irrelevant long decimal numbers. To simplify your data frame, round up to the decimal place you prefer (explain what decimal place you chose!). 
#Tipp: Best approach would be to write a loop or sapply

honey <- honey %>% mutate_if(is.numeric, round, digits=2)
# other option
for(i in 4:length(honey)){
  honey[, i] <- round(honey[, i], 2)
}


### Task 4: Bees data set: year into date format, 
# since we want to merge honey and the bees data set, we will now spend some time cleaning the bees data set.
# if it helps you,look at the bees data set using the appropriate commands (please do now show us in the markdown)
# Difference: year format in quarters!, therefore we need to work on the data before merging
# might have learned that in datacamp: check whether the var is in a date format with the command class():
# if you the var is not in date format yet, you can use the lubridate package to transform "years" into a date format. You will need this later!


class(bees$Date) #output is character 

bees$Date <- lubridate::yq(bees$Date)

### Task 5: So far you converted Years into an appropiate date format. But look at the other colums: They are numbers but are they also numeric? Check on all colums so you dont miss on one!
#For further calculations numbers need to be converted into a numeric factor. Can you do so?
##Tipp: also here a loop is the best approach. 

lapply(bees[,4:17],class)
# other option
sapply(3:14, function(i) {
  bees[, i] <<- as.numeric(unlist(bees[, i]))
})

### Task 6: Calculate the percentage of lost colonies & round, why id you decide to round the way you have to?
bees$lost_colonies_percent <- bees$`Lost Colonies` / bees$`Starting Colonies` * 100

bees$lost_colonies_percent <- round(bees$lost_colonies_percent, digits=0)

### Task 7: Look bees and missing values, decide on how you want to treat them and give us a detailed explanation why you have decided to treat the missing values


sum(is.na(bees$`Percent Renovated`)) #Let the participants freely describe what they see and how they decide, link to the DataCamp course dealing with missing values
#they should do that for the following task, keeping in mind that we want to summarize means!
#find patters for missing values and decide how you want to treat NAs

bees[] <- lapply(bees, function(x) { 
  x[is.na(x)] <- mean(x, na.rm = TRUE)
  x
}) #imputes missing values with mean if 


### Task 8: mean of all the quarters of one year t

bees_yearly <- bees %>%
  mutate(Year = year(Date)) %>% # obtain year from Date
  group_by(Latitude, Longitude, State, State_code, Year) %>% # group by State FIRST end then by Year
  select(-State, -Date, -State_code, -Latitude, -Longitude) %>% # remove variables where no average should be applied
  summarise_each(funs(round(mean(., na.rm = TRUE), 2))) 

#As you can see now, quarterly date has disappeared and we only have years left. Please mind how you are rounding your means, give us a short reasoning why
# you are rounding the way you are rounding and a short data table so we can check whether your data is correct


###Task 9: prepare to merge honey and bees into one data frame
# to merge give your "bees" data columns appropriate names (we recommend names where empty spaces are replaced with an underscore "_"), if you have not done so
# for the honey data set please do the same

bees_yearly <- bees_yearly %>% 
  rename("added_colonies" = "Added Colonies") %>%
  rename("Start_col" = 'Starting Colonies') %>%
  rename("Varroa_perc" = 'Varroa Mites (Percent)')#usw.

### Task 10: merge honey and bee dataset and explain us the process of how the way you have decided to merge your data works and what you needed to do
# in order to merge the data sets (hint: by what are you merging?)
# Wir implizieren hier, dass die Staaten sich reduzieren, den Teilnehmern am besten in die guide schreiben

HB <- merge(bees_yearly, honey) 


### Task 11: Bar Plot for all States (starting bee colonies = number of bee poplation, sorted)
# The participants can decide for themselves here whether they want to use the mean for all years or plot the graphs for just one year
## Plotted here for 2020, not the prettist version, but will make it prettier once approved

StartingColonies <- sum(mean(HB$Start_col))

HB %>% 
  filter(Year==2020) %>%
  ggplot(., aes(x = reorder(State, Start_col), y = Start_col, fill = State)) + 
  geom_col(show.legend = F, fill = "#E09A00") +
  coord_flip() +
  ggtitle("Starting Bee Colonies per State") + # for the main title
  xlab("State") +
  theme(plot.title = element_text(hjust = 0.5))

###Task 12: Split a subset for data between the X different Team members (we have 40 States) (ungraded)
HB$Group4 <- c(rep(1,60),rep(2,60), rep(3,60), rep(4, 60)) #split if the group has 4 Team Members
HB$Group3 <- c(rep(1,80), rep(2, 80), rep(3,80)) # split if the group has just 3 Team members

## This is not really a solution, we need to add the groups to the weather data too so that the 
## participants can analyse the data without any additional task, I am modifying the weather data set here
#weather <- weather %>% rename("State_code" = "name")
#merger4 <- HB %>% select(State_code, Group4)
#weather <- left_join(weather, merger4, by="State_code")
#merger3 <- HB %>% select(State_code, Group3)
#weather <- left_join(weather, merger3, by="State_code")

write_csv(weather, gzfile('weather.csv.gz'))


###Task 13: Boxplot aggregated for year: Honey Production, Parasite: Varroa Mite, Precipitation , Bee Colonies (6 boxen)

#Honey Production, example group 1
HB1production <- HB %>% 
  filter(Group4 == 1) %>% 
  select(Year, production_kg) %>% 
  mutate(Producion  = (production_kg/1000)) %>% 
  select(!production_kg)


ggplot(HB1production, aes(x=Year, y= Producion, group=Year)) +
  geom_boxplot(fill="#946500",  alpha=0.7) +
  ggtitle("Honey Production in 10 States of the US") +
  xlab("Year")+
  ylab("Production in 1000 kg")+
  geom_jitter(color="black", size=0.4, alpha=0.9) +
  labs(caption = "States considered: Alabama, Arizona, Arkansas, California, Colorado, 
       Florida, Georgia, Hawaii, Idaho, Illinois") +
  theme_minimal()

# Parasite: Varroa Mite, example group 2

HB %>%
  filter(Group4 == 2) %>%
  ggplot(aes(x=Year, y=Varroa_perc, group=Year)) +
  geom_boxplot(fill="#946500", alpha=.7) +
  geom_jitter(col="black", size=.4) +
  labs(title = "Varroa Mite Infestations in 10 US-States", 
       x= "Year", 
       y="Percent of Colonies",
       caption = "States considered: Indiana, Iowa, Kansas, Kentucky, Louisiana, Maine, Michigan, 
       Minnesota, Mississippi, Missouri") +
  theme_minimal()

# Precipitation, example group 3
test <- weather %>%
  filter(Group4 == 3) %>%
  select(datetime, State_code, precip) %>%
  group_by(State_code) %>% 
  mutate(Year = year(datetime)) %>%
  select(-datetime, -Year, -precip) %>%
  mutate(yearly_state_mean = mean(precip))

#heatMAP


Bar Plot mit 51 Staaten : Bienenpopulation (starting colonies) ; sorted by lowest to highest, + Strich representing mean. Welches Bundesstaat hat die höchste Population? (Farbschema: gelb, orange, honigfarben, schwarz)
Dataset innerhalb der Gruppe splitten für EDA (evtl. nach Region (West, East, North, South)), damit Kommunikation innerhalb der Gruppe stattfinden für EDA 
Honigproduktion in den verschiedenen Regionen/Bundesstaaten vergleichen, Bundesstaat in der Region mit der höchsten/niedrigsten Honigproduktion & dann Vergleich zwischen den Gruppen. Boxplot


Heat Map gefärbt je nach wie viel Honig produziert wird, wo die meisten Bienenkolonien sind, höchste Anzahl von Bienenkrankheiten (welche genau) , höchstes Bienensterben (4 Teammitglieder = 4 Verschiedene Mappen) 
Sinnvolle Schlussfolgerung aus ”Zusammenführung” der Heatmap outputs 
Dynamische Mappe (wie für Airbnb) für jeden Bundesstaat mit den wichtigsten Stats in einem eine “Notizpin” (? Wie heissen diese Dinger in den Mappen?)
