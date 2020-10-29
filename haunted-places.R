#loading the dataset
df <- read.csv("data/haunted_places.csv")

####Viewing the dataset
#what does the data look like?
head(df)
tail(df)
#what are the header titles?
colnames(df)
#check the data types of the file
str(df)
#look at summary statistics for the data
summary(df)

######Data aggregating 
library(dplyr)
#basic aggregation by state
haunted_states <- df %>%
  group_by(state_abbrev) %>%
  summarise(count = n()) 
  
most_haunted_states <- arrange(haunted_states, desc(count))
top_10_most_haunted_states <- most_haunted_states[1:10, ]

#let's make a barchart so we can compare the occurance rate in the 
#10 most haunted states
library(ggplot2)

ggplot(data=top_10_most_haunted_states, aes(x=state_abbrev, y=count)) +
  geom_bar(stat="identity",fill="yellowgreen")

ggplot(data=top_10_most_haunted_states, aes(x=state_abbrev, y=count)) +
  geom_bar(stat="identity", fill="yellowgreen") +
  ggtitle("Top 10 most haunted states \n by number of haunted locations") +
  xlab("State") + 
  ylab("Number of haunted locations") +
  geom_text(aes(label=count), vjust=1.6, color="white", size=3.5)+
  theme_minimal()


#aggregation by location
haunted_locations <- df %>%
  group_by(location) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

#what if we wanted to look at about sightings in walmarts when walmart was 
#listed as a part of the description and not the location??
haunted_walmarts <- df %>%
  filter(grepl('Wal-Mart', location) | grepl('Wal-Mart', description))

#which state has the most haunted walmarts?
haunted_walmarts  %>%
  group_by(state) %>%
  summarise(count = n())

#####Text analysis of the description field
library(tidytext)
#what are the top words that occur in the description fields of all hauntings?
#parse out each word for each sighting
#because we want to look at each sighting, we should create a unique index for each haunting
df$index <- 1:nrow(df)

#seperate each word in the description field 
df %>% unnest_tokens(word, description)
#this will yield an error, so do the next steps
class(df$description)
df$description <- as.character(df$description)
class(df$description)
#create a new dataset with all of the words
description_words <- df %>% 
  unnest_tokens(word, description) %>%
  select(index, state, word)
#remove english stop words
data("stop_words")
clean_description_words <- anti_join(description_words, stop_words)

#what are the top words that appear overall?
clean_description_words %>%
  group_by(word) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

#how can we start grouping word occurance on the state level?
state_words <- clean_description_words %>%
  group_by(state, word) %>%
  summarise(count = n()) 

#viewing data per a particular state
CT_words <- filter(state_words, state == "Connecticut")

#what is the top word for each state?
top_word_per_state <- state_words %>%
  group_by(state) %>%
  filter(count == max(count))

