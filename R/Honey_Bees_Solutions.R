##### Author: Inga Lasys
##### Date: March 2022
##### Solutions: Honey Bees Project, Summer Semester 2022

library(lubridate)
library(readr)
library(dplyr)
library(ggplot2)

###############################################################################
### Task 0: Import Data (ungraded, does not earn points b/c obviuosly)

bees <- read_csv("~/Honey_Bees/Data/bees.csv")
honey <- read_csv("~/Honey_Bees/Data/honey.csv")

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

honey$producing_colonies <- honey$`Honey producing colonies (thousand)` * 1000    # Creates new variable
honey$yield_colony_kg <- honey$`Yield per colony (pounds)`*0.453592 # Creates new variable in kg
honey$production_kg <- honey$`Production (1,000 pounds)`*1000*0.453592 # in kgs (this was per 1000 pounds)
honey$stock_price_kg <- honey$`Stocks December 15  (1,000 pounds)`/1000*0.453592*100 #stock price per 100kg
honey$avg_price_kg <- honey$`Average price per pound (dollars)`*0.453592 # price per kg of honey (average price)
honey$prod_value <- honey$`Value of production (1,000 dollars)`*1000

honey <- honey[, -c(2:7)] #removes unnecessary columns (less clutter, less RAM used)

### Task 2: write loop to round up to the decimal place you prefer (explain what decimal place you chose!)

for(i in 4:length(honey)){
 honey[, i] <- round(honey[, i], 2)
}


### Task 3: Bees data set: year into date format, 
# since we want to merge honey and the bees data set, we will now spend some time cleaning the bees data set.
# if it helps you,look at the bees data set using the appropriate commands (please do now show us in the markdown)
# Difference: year format in quarters!, therefore we need to work on the data before merging
# might have learned that in datacamp: check whether the var is in a date format with the command class():
# if you the var is not in date format yet, you can use the lubridate package to transform "years" into a date format. You will need this later!


class(bees$Date) #output is character 

bees$Date <- lubridate::yq(bees$Date)

### Task 4: numbers into numeric format, numeric should be a loop


sapply(3:14, function(i) {
  bees[, i] <<- as.numeric(unlist(bees[, i]))
})



### Task 5: Calculate the percentage of lost colonies & round, why id you decide to round the way you have to?
bees$lost_colonies_percent <- bees$`Lost Colonies` / bees$`Starting Colonies` * 100

bees$lost_colonies_percent <- round(bees$lost_colonies_percent, digits=0)

### Task 6: Look bees and missing values, decide on how you want to treat them and give us a detailed explanation why you have decided to treat the missing values
 

sum(is.na(bees$`Percent Renovated`)) #Let the participants freely describe what they see and how they decide, link to the DataCamp course dealing with missing values
#they should do that for the following task, keeping in mind that we want to summarize means!

### Task 7: mean of all the quarters of one year t

bees <- bees %>%
  mutate(Year = year(Date)) %>% # obtain year from Date
  group_by(Latitude, Longitude, State, State_code, Year) %>% # group by State FIRST end then by Year
  select(-State, -Date, -State_code, -Latitude, -Longitude) %>% # remove variables where no average should be applied
  summarise_each(funs(round(mean(., na.rm = TRUE), 0))) %>%
  arrange(State)

#As you can see now, quarterly date has disappeared and we only have years left. Please mind how you are rounding your means, give us a short reasoning why
# you are rounding the way you are rounding and a short data table so we can check whether your data is correct


###Task 8: prepare to merge honey and bees into one data frame
# to merge give your "bees" data columns appropriate names (we recommend names where empty spaces are replaced with an underscore "_"), if you have not done so
# for the honey data set please do the same

### Task 9: merge honey and bee dataset and explain us the process of how the way you have decided to merge your data works and what you needed to do
# in order to merge the data sets (hint: by what are you merging?)

HB <- merge(bees, honey) #lol, as if I am going to do it the hard way. 


###Task 10: Split a subset for data between the X different Team members (we have 40 States) (ungraded)
states <- unique(HB$State_code)




### Task 11: Bar Plot for all States (starting bee colonies = number of bee poplation, sorted)
# The participants can decide for themselves here whether they want to use the mean for all years or plot the graphs for just one year
## Plotted here for 2020, not the prettist version, but will make it prettier once approved

HB %>% 
  filter(Year==2020) %>%
ggplot(., aes(x = reorder(State, Start_col), y = Start_col, fill = State)) + 
  geom_col(show.legend = F) +
  coord_flip()   



###################################################################
# Split for 4 Team Members, ask if you do not have 4 Team members
states1 <- states[1:10]
states2 <- states[11:20]
states3 <- states[21:30]
states4 <- states[31:40]

df %>% group_by(year) %>% 
  filter(length(unique(stage)) == 2)

HB1 <- HB %>%
  group_by(State_code) %>%
  select(State_code=states1)

Bar Plot mit 51 Staaten : Bienenpopulation (starting colonies) ; sorted by lowest to highest, + Strich representing mean. Welches Bundesstaat hat die höchste Population? (Farbschema: gelb, orange, honigfarben, schwarz)
Dataset innerhalb der Gruppe splitten für EDA (evtl. nach Region (West, East, North, South)), damit Kommunikation innerhalb der Gruppe stattfinden für EDA 
Honigproduktion in den verschiedenen Regionen/Bundesstaaten vergleichen, Bundesstaat in der Region mit der höchsten/niedrigsten Honigproduktion & dann Vergleich zwischen den Gruppen. Boxplot


Heat Map gefärbt je nach wie viel Honig produziert wird, wo die meisten Bienenkolonien sind, höchste Anzahl von Bienenkrankheiten (welche genau) , höchstes Bienensterben (4 Teammitglieder = 4 Verschiedene Mappen) 
Sinnvolle Schlussfolgerung aus "Zusammenführung" der Heatmap outputs 
Dynamische Mappe (wie für Airbnb) für jeden Bundesstaat mit den wichtigsten Stats in einem eine "Notizpin" (? Wie heissen diese Dinger in den Mappen?)
