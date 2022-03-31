##### Author: Inga Lasys
##### Date: March 2022
##### Solutions: Honey Bees Project, Summer Semester 2022
library(readr)
library(lubridate)
library(readr)
library(dplyr)
library(ggplot2)
install.packages("wesanderson")
library(wesanderson)

###############################################################################
### Task 0: Import Data (ungraded, does not earn points b/c obviuosly)

bees <- read_csv("C:\\Users\\is150\\OneDrive\\Desktop\\bees.csv", trim_ws = TRUE, col_names = TRUE)
honey <- read_csv("C:\\Users\\is150\\OneDrive\\Desktop\\honey.csv", trim_ws = TRUE, col_names = TRUE)

# Let start by looking at the bee & honey data. To start viewing the data you have just imported 
# use head(), str(), or class() to get an overview of each data frame.Have a look at the frequency of the data (is there a difference?)
# Do you see any missing values or data entries that are different from the other entries?
# What are the data formats? Look at the unit of every variable, can you make sense of the units?
# write down in a couple of sentences which errors you first notice and what could be an issue later in the project 
# no worries, there is no right or wrong

#Isi: maybe add here as Task 1:

### Proceed with honey data
### Task 1: Give some overall statements 
#Lets stick with the honey data: Since you inspected the data, it would be interesting to indicate some outstanding reportings. Which state had the most producing colonies ever and in which year? Is there something special to that state? (Not the over all United States ;) )
#Next, lets see what state had the lowest price for honey. How low was it?
#Finally, what is the total Honey production for 2016? #UNITES STATES EXCLUDIEREN?

honey %>% 
  select(c(State, Year, `Honey producing colonies (thousand)`)) %>% 
  arrange(desc(`Honey producing colonies (thousand)`)) # it's North Dakota in 2018 

honey %>% 
  select(c(State, Year, `Average price per pound (dollars)`)) %>% 
  arrange(`Average price per pound (dollars)`) #it's Hawaii in 2019: 1.28 Dollar per pound

honey %>% 
  select(c(State, Year, `Honey producing colonies (thousand)`)) %>%
  filter (Year == 2016) %>% 
  filter (State != "United States") %>% 
  summarise(sum(`Honey producing colonies (thousand)`)) #2775


### Task 2: Convert from pounds to kg and prices to $/kg
# As you can see in the column names, the honey data set is in pounds as we are dealing with a 
# US Dataset. However, we naturally don't understand pounds, so we would like to replace a pound measure into the metric system.
# while you are at it, do you maybe want to give your new variable in kg an new, shorter name? Finally, please only leave the necessary columns in the data set.
# Tipp: we recommend multiplying the values that are in 1K first before transforming into kg)

#Isi: maybe use mutate? 
honey <- honey %>% 
  mutate(`Producing colonies`= (`Honey producing colonies (thousand)`*1000)) %>% 
  mutate(`Yield per colony (kg)` = (`Yield per colony (pounds)`*0.453592)) %>% 
  mutate(`Production (kg)` = (`Production (1,000 pounds)`*1000*0.453592)) %>% 
  mutate(`Stock price (kg)`= (`Stocks December 15  (1,000 pounds)`/1000*0.453592*100)) %>% 
  mutate(`Average price (kg)` =(`Average price per pound (dollars)`*0.453592)) %>% 
  mutate(`Produciton Value`=(`Value of production (1,000 dollars)`*1000))

honey$producing_colonies <- honey$`Honey producing colonies (thousand)` * 1000    # Creates new variable
honey$yield_colony_kg <- honey$`Yield per colony (pounds)`*0.453592 # Creates new variable in kg
honey$production_kg <- honey$`Production (1,000 pounds)`*1000*0.453592 # in kgs (this was per 1000 pounds)
honey$stock_price_kg <- honey$`Stocks December 15  (1,000 pounds)`/1000*0.453592*100 #stock price per 100kg
honey$avg_price_kg <- honey$`Average price per pound (dollars)`*0.453592 # price per kg of honey (average price)
honey$prod_value <- honey$`Value of production (1,000 dollars)`*1000

honey <- honey[, -c(2:7)] #removes unnecessary columns (less clutter, less RAM used)

### Task 3: Have a look at your column values. Some columns now have irrelevant long decimal numbers. To simplify your data frame, round up to the decimal place you prefer (explain what decimal place you chose!). 
#Tipp: Best approache would be to write a loop

for(i in 4:length(honey)){
  honey[, i] <- round(honey[, i], 2)
}


### Task 4: Bees data set: year into date format, 
# Let's contiue with the bee data set. We want to combine ("merge") the honey and the bees data set next. We will now spend some time cleaning the bees data set.
# if it helps you,look at the bees data set using the appropriate commands (please do now show us in the markdown). You will see a difference compared to the honey data set:
# the year format is in quarters! Therefore we need to work on the data before merging.
# check whether the Year variables are in a proper "date format" with the command class().
# if you find out the variables are not in date format yet, you can use the lubridate package to transform "years" into a date format. You will need this later!


class(bees$Date) #output is character 

bees$Date <- lubridate::yq(bees$Date)

### Task 5: So far you converted Years into an appropiate date format. But look at the other colums: They are numbers but are they also numeric? Check on all colums so you dont miss on one!
#For further calculations numbers need to be converted into a numeric factor. Can you do so?
##Tipp: also here a loop is the best approach. 

lapply(bees[,4:17],class)

sapply(3:14, function(i) {
  bees[, i] <<- as.numeric(unlist(bees[, i]))
})


### Task 6: Calculate the percentage of lost colonies & round, why id you decide to round the way you have to?
bees$lost_colonies_percent <- bees$`Lost Colonies` / bees$`Starting Colonies` * 100

bees$lost_colonies_percent <- round(bees$lost_colonies_percent, digits=0)

### Task 7: Look bees and missing values, decide on how you want to treat them and give us a detailed explanation why you have decided to treat the missing values


sum(is.na(bees$`Percent Renovated`)) #Let the participants freely describe what they see and how they decide, link to the DataCamp course dealing with missing values
#they should do that for the following task, keeping in mind that we want to summarize means!

bees[] <- lapply(bees, function(x) { 
  x[is.na(x)] <- mean(x, na.rm = TRUE)
  x
}) #hab mal mean genommen

### Task 8: mean of all the quarters of one year t

#Version Isi
bees <- bees %>%
  mutate(Year = year(Date)) %>% 
  group_by(Latitude, Longitude, State, State_code, Year) %>% #obtain year from Date# group by State FIRST end then by Year
  select(-State, -Date, -State_code, -Latitude, -Longitude) %>%#remove variables where no average should be applied
  summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))) 


bees <- bees %>%
  mutate(Year = year(Date)) %>% # obtain year from Date
  group_by(Latitude, Longitude, State, State_code, Year) %>% # group by State FIRST end then by Year
  select(-State, -Date, -State_code, -Latitude, -Longitude) %>% # remove variables where no average should be applied
  summarise_each_(funs(round(mean(., na.rm = TRUE), 0))) 

#As you can see now, quarterly date has disappeared and we only have years left. Please mind how you are rounding your means, give us a short reasoning why
# you are rounding the way you are rounding and a short data table so we can check whether your data is correct


###Task 9: prepare to merge honey and bees into one data frame
# to merge give your "bees" data columns appropriate names (we recommend names where empty spaces are replaced with an underscore "_"), if you have not done so
# for the honey data set please do the same

bees <- bees %>% 
  rename("added_colonies" = "Added Colonies") #and so on, nto gonna do all of it #Isi

### Task 10: merge honey and bee dataset and explain us the process of how the way you have decided to merge your data works and what you needed to do
# in order to merge the data sets (hint: by what are you merging?)

HB <- merge(bees, honey) #lol, as if I am going to do it the hard way. 

###Task 11: Split a subset for data between the X different Team members (we have 40 States) (ungraded)
states <- unique(HB$State_code)


### Task 12: Bar Plot for all States (starting bee colonies = number of bee poplation, sorted)
# The participants can decide for themselves here whether they want to use the mean for all years or plot the graphs for just one year
## Plotted here for 2020, not the prettist version, but will make it prettier once approved

StartingColonies <- sum(mean(HB$`Starting Colonies`))

HB %>% 
  filter(Year==2020) %>% 
  ggplot(., aes(x = reorder(State, `Starting Colonies`), y = `Starting Colonies`, fill=State)) + 
  geom_col(show.legend = F) +
  coord_flip()+
  scale_fill_manual(values= rep(c("yellow", "black"),20))+
  ggtitle("Starting Bee Colonies per State")+ # for the main title
  xlab("State")+
  theme(plot.title = element_text(hjust = 0.5))+
  geom_hline(yintercept = StartingColonies, linetype="dashed", color = "black", size= 1)

#Isi: bekomme nicht hin, das es richitg abwecheselnd scharwz gelb ist 

###################################################################
# Split for 4 Team Members, ask if you do not have 4 Team members

states1 <- states[1:10]
states2 <- states[11:20]
states3 <- states[21:30]
states4 <- states[31:40]


df %>% group_by(year) %>% 
  filter(length(unique(stage)) == 2)

HB1 <- HB %>%
  group_by(State) %>%
filter(State == states1) 





Bar Plot mit 51 Staaten : Bienenpopulation (starting colonies) ; sorted by lowest to highest, + Strich representing mean. Welches Bundesstaat hat die höchste Population? (Farbschema: gelb, orange, honigfarben, schwarz)
Dataset innerhalb der Gruppe splitten für EDA (evtl. nach Region (West, East, North, South)), damit Kommunikation innerhalb der Gruppe stattfinden für EDA 
Honigproduktion in den verschiedenen Regionen/Bundesstaaten vergleichen, Bundesstaat in der Region mit der höchsten/niedrigsten Honigproduktion & dann Vergleich zwischen den Gruppen. Boxplot



Heat Map gefärbt je nach wie viel Honig produziert wird, wo die meisten Bienenkolonien sind, höchste Anzahl von Bienenkrankheiten (welche genau) , höchstes Bienensterben (4 Teammitglieder = 4 Verschiedene Mappen) 
Sinnvolle Schlussfolgerung aus "Zusammenführung" der Heatmap outputs 
Dynamische Mappe (wie für Airbnb) für jeden Bundesstaat mit den wichtigsten Stats in einem eine "Notizpin" (? Wie heissen diese Dinger in den Mappen?)