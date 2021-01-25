# Lesson 2 ####
# Title: DAL2 ####
# Author: Garrett Kent
# Date: 01/20/2021
# Load tidyverse package
library(tidyverse)
# Load data
data(mpg)
# Check the structure of mpg using str()
str(mpg)
# Answer below (using a comment) what type of data structure mpg is?
# mpg is a data frame
# Create analysis data frame: We do this so we preserve a raw version of the dataframe to which we can refer
mpg_df<-mpg
# Check the structure of mpg_df using str()
str(mpg_df)
# Scatter plot of displ x hwy
ggplot(mpg_df, aes(x=displ , y=hwy ))+
  geom_point()
# Scatter plot of displ x hwy by fuel type (fl)
ggplot(mpg_df, aes(x=displ , y=hwy, color=fl ) )+
  geom_point()
# On Your Own, Scatter plot of displ x hwy by class
ggplot(mpg_df, aes(x=displ , y=hwy, color=class ) )+
  geom_point()
# Lesson 3 ####
?ggplot()
# Call help for filter
?filter()
# Call help for geom_bar()
?geom_bar()
# Call help for facet_wrap()
?facet_wrap()
# ggplot - data layer, assign it to mpg_data
mpg_data<-ggplot(data= mpg)
# Call mpg_data
mpg_data
# Describe the output you see in the console.
Empty box, no plot
Now, let us add an aesthetic layer to the plot.
# Put engine size on the x-axis, highway mileage on y-axis, and color by class.
mpg_plot<-mpg_data +
geom_point(mapping = aes(x=displ , y=hwy, color=class))
# Call mpg_plot.
mpg_plot
# What do you see?
Looks like the graph used earlier; gray graph with a y axis of highway mileage, an x axis of engine size, colored by class
# Now, let's add an aesthetic layer to change the axis labels
mpg_axis <- mpg_plot +
  xlab("Engine Size in Liters")+
  ylab("Highway Mileage")+
  ggtitle("Engine Size and Mileage by Vehicle Class")
# Call mpg_axis
mpg_axis
# What do you see?
Same graph, but with a title and better x and y axis labels
# Create different graphs for each class
mpg_wrap<-mpg_axis +
  facet_wrap(~class, nrow=2)
# Call mpg_wrap
mpg_wrap
# What do you see?
Seven graphs, each graphing out one class of vehicle by highway mileage and engine size
# Re-assign mpg_wrap by adding facet wrapping, change nrow to 3.
mpg_wrap<-mpg_wrap +
  facet_wrap(~class, nrow=3)
mpg_wrap<-mpg_wrap +
  facet_wrap(~class, nrow=2)
mpg_wrap
# What do you see?
I cant quite tell the difference between these graphs and the previous ones. They appear to be oriented more horizontally, with the bases of the graphs now wider than the heights.

# Lesson 4 ####
install.packages("nycflights13")
# Libraries
# loading the installed packages:

library(tidyverse)
library(nycflights13)

# Call flights
flights

# Filter to keep only the flights that happened on 3-14.
filter(flights, month==3, day==14)

# Assign flights that happened on 3-14 to new variable: `Mar14`.
Mar14 <- "flights, month==3, day==14"

# Select flights that happened in either May or June
filter(flights, month == 5 | month == 6)

# Filter flights whose arrival was delayed over 60 minutes AND departed on time.
filter(flights, (arr_delay > 60 & dep_delay < 1))

# Filter flights whose arrival was delayed over 60 minutes AND departure was delayed less than 15 minutes.
filter(flights, (arr_delay > 60 & dep_delay < 15))

# Filter flights whose arrival was delayed over 60 minutes AND departure was delayed 15 minutes or more # Filter flights whose destination is Greenville-Spartanburg International Airport (GSP)
filter(flights, (arr_delay > 60 & dep_delay >= 15))

# Filter flights that were in the air over 3 hours (180 mins)
filter(flights, air_time > 180)

# Effects of NAs on Logical Operations
NA == 99

NA < 3

NA > 3

# Number of Wins for Clemson Football in the 2021 season
Clemson <- NA
# Number of Wins for Ohio State football in the 2021 season
OSU <- NA
# Do Clemson and OSU have the same number of wins
Clemson == OSU
## [1] NA
# Interpret this result using a comment your script file.
#It appears as though the code is saying that because we dont know the number of wins clemson has in a football season or the number of wins ohio state has, it has no way of knowing if the win numbers are equal

# use is.na(varName) to find whether it is NA. This command returns a logical vector of either TRUE or FALSE. 
is.na(Clemson)

is.na(OSU)

# Now let's see how it works on a data frame.
is.na(flights)
# Describe what you see.
#All of the values appeared to answer the question of whether they are NA or not with "false." I did not see any values that said "true"

# Let's see what happens when we combine filter() and is.na(). We will search for NAs in arrival delay.
filter(flights, is.na(arr_delay))

# We will search for observations with NO NAs in arrival delay.
filter(flights, !is.na(arr_delay))

# In the real world, we often want to know how many observations are in each subset data frame. We will 
# Number of observations in flights data set with missing data for arrival delay.
nrow(filter(flights, is.na(arr_delay)))

# Number of observations in flights data set with NO missing data for arrival delay.
nrow(filter(flights, !is.na(arr_delay)))

# On your own, number of observations in flights data set with missing data for departure delay.
nrow(filter(flights, is.na(dep_delay)))

# On your own, number of observations in flights data set with no missing data for departure delay.
nrow(filter(flights, !is.na(dep_delay)))

# In the real world, we often will filter out NAs across several variables and assign the resulting data 
flights_df<-filter(flights, (!is.na(arr_delay) & !is.na(dep_delay)))

# Call flights_df
flights_df

# On your own, filter out NAs across year, month, dep_delay, arr_delay, carrier, flight, air_time
flights_df<-filter(flights, (!is.na(arr_delay) & !is.na(dep_delay) & !is.na(year) & !is.na(carrier) & !is.na(flight) & !is.na(air_time)))

# Call flights_df

flights_df

# Create sample dataframe using sequence (use the help command to learn more about sequence)
sample<- sequence(10, from=10L, by=-1L )

sample_df <- as_tibble(sample)

# Call sample_df
sample_df

# Describe what the results that were returned.
#A list from one to ten. The actual values were counting down from ten, in the opposite direction of the numbers


# Arrange from 1 to 10.
arrange(sample_df, value)

# Describe what the results that were returned.
#Now the actual values are counting up from one to ten, matching their numbers

# Create sample dataframe with values from 1:100 using sequence.
sample<- sequence(100, from=1L, by=1L )

sample_df <- as_tibble(sample)

# Call sample_df
sample_df

# Describe what the results that were returned.
#A list from one to ten with '90 more rows' below

# Arrange sample_df$value in descending order using the syntax, arrange(df, desc(varible))
arrange(sample_df, desc(value))

# Describe what the results that were returned.
# The numbers were reordered counting down from 100 to 1

# What happens when we use arrange on a real life data frame (i.e., one that has missing values).
#It puts the missing values at the bottom of the arrangement

# Arrange flights by arrival delay and assign to arrange_df
arrange_df<-arrange(flights, arr_delay)

# Call the last six rows of (call help on tail() to find out more)
tail(arrange_df)

# Lets try arranging data on your own.

# Arrange flights to find the most delayed flights.
arrange_df<-arrange(flights, dep_delay)

arrange(arrange_df, desc(dep_delay))

# Arrange the flights that left earliest.
arrange_df<-arrange(flights, dep_time)
arrange_df

?select

# Example use of select
select(flights, year, month, day, carrier, flight)

# Select all columns between year and arrival delay
select(flights, year:arr_delay)

# Select all columns except tailnum
select(flights, -tailnum)

# Select all columns between dep_delay and time_hour.
select(flights, dep_delay:time_hour)

# Select the following columns: year, month, day, dep_delay, arr_delay, dest, distance. Assign to flights_# call flights_df
flights_df<-select(flights, year, month, day, dep_delay, arr_delay, dest, distance)

# call flights_df
flights_df

# Check the column names
names(sample_df)

# Rename sample_df$value as sample_df$count
rename(sample_df, count = value)

# Rename sample_df$value as sample_df$count. Assign to sample_df
sample_df <- rename(sample_df, count = value)

# To be completed on your own.

# Check column names in flights
names(flights)

# Rename arr_delay as `arrival_delay`. Assignt to flights.
flights <- rename(flights, arrival_delay = arr_delay)

# Rename dep_delay as `departure_delay`. Assign to flights.
flights <- rename(flights, departure_delay = dep_delay)

# Call flights
flights

# Create dataframe with fewer variables: flights_small
flights_small <- select(flights,
                        year:day,
                        ends_with("delay"),
                        distance,
                        air_time,
                        dest
                        )



# Create new column: avg_speed.
mutate(flights_small,
       speed = distance / air_time * 60)


# You can also use mutate to change existing variables.
# Change destination to factor
mutate(flights_small,
       dest = factor(dest))


# Create column `gain` using formula `dep_delay` - `arr_delay`.
mutate(flights_small, 
       gain = departure_delay - arrival_delay)

# Create new column for arrival delay in hours (i.e., divide by 60)
mutate(flights_small, 
       arrival_delay_hrs = arrival_delay / 60)


# Create new column for departure delay in hours (i.e., divide by 60)
mutate(flights_small, 
       departure_delay_hrs = departure_delay / 60)

# Change year to character
mutate(flights_small, year = as.character(year))

# Create new character variable using paste0 (call help to learn more and check the hint below) to combine

# Verse 1 as string

almaMater1<-"Where the Blue Ridge yawns its greatness; Where the Tigers play; Here the sons of dear Old Clemson, Reign supreme alway"

# Verse 2 as string
almaMater2<-"We will dream of great conquests For our past is grand, And her sons have fought and conquered Every foreign land."


# Verse 3 as string
almaMater3 <- "Where the mountains smile in grandeur O’er the hill and dale; Here the Tiger lair is nestling Swept by storm and gale."

# Verse 4 as string
almaMater4 <- "We are brothers strong in manhood For we work and strive; and our Alma Mater reigneth Forever in our lives."

# Chorus as string
almaMaterChorus <- "Dear Old Clemson, we will triumph And with all our might That the Tiger’s roar may echo O'er the mountain height"

# Combine vectors to create almaMater

almaMater<- paste0(almaMater1," ", almaMater2," ", almaMater3," ", almaMater4," ", almaMaterChorus)

# print almaMater to console.
almaMater

# Use summarise to calculate avg (mean) arrival delay.
summarise(flights, avgArrDelay= mean(arrival_delay, na.rm=TRUE))

#Use group_by and summarise to calculate avg arrival delay for all the carriers.
# Group flights by carrier: flights_carrier
flights_carrier <- group_by(flights, carrier)

#summarise average delay
summarise(flights_carrier, avgArrDelay=mean(arrival_delay, na.rm=TRUE))

# Summarise standard deviation of distance to destinations
summarise(flights, StDevofDisttoDest=sd(distance, na.rm=TRUE))

# Create new data frame: not_cancelled
not_cancelled <- filter(flights, !is.na(departure_delay) & !is.na(arrival_delay))

# Group by month, mean dep_delay
not_cancelled_year<- group_by(not_cancelled, month)

# Group by carrer, summarise mean dep_delay and mean arrival delay
flights_carrier_delay_means <- group_by(flights, carrier)
summarise(flights_carrier_delay_means, avgArrDelay=mean(arrival_delay, na.rm=TRUE), avgDepDelay=mean(departure_delay, na.rm=TRUE))

# Group by carrier, summarise sd of distance traveled
sd_of_dist<-group_by(flights, carrier)
summarise(sd_of_dist, StDevofDisttoDest=sd(distance, na.rm=TRUE))

# Filter for origin=="LGA". When do the first flights depart? when do they arrive?
filter(flights, origin=="LGA")

#The first few flights in this list depart at 533, 554, and 557, and arrive at 850, 812, and 709

# Lesson 5 ####

#Load nycflights13 packages
library(nycflights13)


# Load data
data("flights")

# Filter the data for November and December flights, count number of flights that arrived later than 59 

# Filter, assign new varName
flights_extract<-filter(flights, month==11 | month == 12 & arr_delay>60)

# Group by destination
flights_extract <- group_by(flights_extract,dest )

# Count
flights_extract <- count(flights_extract)

# Rename
flights_extract <- rename(flights_extract, number = n )

# Arrange
flights_extract <- arrange(flights_extract, desc(number))

# Keep only top 6 rows
flights_extract <- head(flights_extract, n=6)

# Make ggplot bar graph
ggplot(flights_extract, aes(x=reorder(dest,-number), y=number))+
  geom_bar(stat="identity")

# Now look at the same operations with pipes.
flights %>%
  filter(month==11 | month == 12 & arr_delay>60) %>%
  group_by(dest) %>%
  count() %>%
  rename(number=n) %>%
  arrange(desc(number)) %>%
  head(6) %>%
  ggplot(aes(x=reorder(dest,-number), y=number))+
  geom_bar(stat="identity")

# Filter the flights data to keep only May and June flights
flights %>%
  filter(month==5 | month==6)

# Filter the flights data to keep only April
flights %>%
  filter(month==4)

# Filter the flights data to keep only Delta flights
flights %>%
  filter(carrier=="DL")


# Which airport had the most flights from NYC's 3 airports to it? Filter flights data to keep only flights that arrived on-time or early, group by origin airport, arrange in descending order. 


flights %>%
  filter(arr_delay <= 0, origin=="LGA" | origin=="JFK" | origin=="EWR") %>%
  group_by(dest) %>%
  count() %>%
  rename(number=n) %>%
  arrange(desc(number)) %>%
  head(6) %>%
  ggplot(aes(x=reorder(dest,-number), y=number))+
  geom_bar(stat="identity") 

#ORD has the most flights from NYC's 3 airports

# Filter flights to keep only flights that departed JFK and arrived at ATL. 
#Keep only origin, destination, arrival delay,  departure delay,and carrier. 
#Create a ggplot scatterplot showing the relationship between departure delay and arrival delay. 
#Add carrier as color.

flights %>% 
  filter(origin=="JFK" & dest=="ATL") %>% 
  select(origin, dest, arr_delay, dep_delay, carrier) %>% 
  ggplot(aes(x=(dep_delay), y=arr_delay, color=carrier))+
  geom_point()

# To which airports did the most flights from NYC go? Make a ggplot visualization.

flights %>%
  filter(origin=="LGA" | origin=="JFK" | origin=="EWR") %>%
  group_by(dest) %>%
  count() %>%
  rename(number=n) %>%
  arrange(desc(number)) %>%
  head(6) %>%
  ggplot(aes(x=reorder(dest,-number), y=number))+
  geom_bar(stat="identity") 

#ORD, ATL, LAX, BOS


# Which carrier had the shortest mean arrival delay?
#Alaska airlines had the shortest mean arrival delay.

flights %>% 
  group_by(carrier) %>% 
  summarise(mean_delay = mean (arr_delay, na.rm=TRUE)) %>% 
  arrange(mean_delay)
  

# Which carrier experienced the most cancelled flights?

flights %>% 
  filter(is.na(dep_delay)) %>% 
  group_by(carrier) %>% 
  count() %>% 
  arrange(desc(n))
#Express Jet had the most cancelled flights

# On average, which carrier flew fastest.

names(flights)

flights %>% 
  mutate(speed = distance / air_time * 60) %>% 
  group_by(carrier) %>% 
  summarise(avg_carrier_speed = mean(speed, na.rm=TRUE)) %>% 
  arrange(desc(avg_carrier_speed))

#Hawaii airlines flew fastest.

