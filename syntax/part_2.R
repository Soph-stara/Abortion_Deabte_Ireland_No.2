#### installing and loading relevant packages/libraries ####
install.packages(c("textstem", 
                   "spacyr", 
                   "stringr", 
                   "quanteda.textstats", 
                   "quanteda", 
                   "readr", 
                   "tokenizers", 
                   "tidytext", 
                   "tibble", 
                   "dplyr", 
                   "readxl", 
                   "ggplot2", 
                   "patchwork",
                   "visreg",
                   "moments",
                   "ggpubr"))

library(tidyverse)
library(ggplot2)
library(quanteda)
library(dplyr)
library(textstem)
library(spacyr)
library(stringr)
library(quanteda.textstats)
library(quanteda)
library(readr)
library(tokenizers)
library(tidytext)
library(tibble)
library(visreg)
library(dplyr)
library(patchwork)
library(moments)
library(ggpubr)

#loading dataset
data <- read.csv2("/Users/sophiehamann/Documents/MA_Universität_Wien/SoSe24/research_text_corpora/Bundschuh_Hamann_Portfolio/data/ireland_abortion_incl.gender_incl2018.csv", header = TRUE)
#View(data)

# correcting the column names
colnames(data) <- c("Rows", "title", "date", "member_gender", "member_name", "party_name", "speech", "speechID")
data$date <- as.Date(data$date, format = "%d.%m.%y")

data <- data[-1, ]
View(data)

#putting together parties that appear in the dataset with several different names
data <- data %>%
  mutate(party_name = ifelse(party_name == "Sinn Féin the Workers' Party", "The Workers' Party", party_name))
data <- data %>%
  mutate(party_name = ifelse(party_name == "Aontú", "Sinn Féin", party_name))
data <- data %>%
  mutate(party_name = ifelse(party_name == "The Labour Party", "Labour Party", party_name))

#filtering out most relevant parties
selected_parties <- c("Fianna Fáil", "Fine Gael", "Labour Party", "Sinn Féin", "The Workers' Party")

data <- data %>%
  filter(party_name %in% selected_parties)
#View(data)

#extracting the year
data$year <- format(data$date, "%Y")

#adding year to dataframe as new column
data$year <- as.integer(data$year)  # Convert the year to integer if needed

#checking results
head(data$year)


#### distribution of male and female speaker over the years ####
# Create a summary table
gender_distribution <- data %>%
  group_by(year, member_gender) %>%
  summarise(count = n()) %>%
  ungroup()

# View the result
print(gender_distribution)

# Plot the distribution of male and female speakers over the years
ggplot(gender_distribution, aes(x = year, y = count, fill = member_gender)) +
  geom_bar(stat = "identity", position = "dodge", width = 5.0) +
  labs(title = "Distribution of Male and Female Speakers Over the Years",
       x = "Year",
       y = "Number of Speakers",
       fill = "Gender") +
  theme_minimal()

#### Creating Corpus Object ####
# getting help from Claude AI -  check for duplicates and  create a new unique identifier - prompt: 
# "Error in corpus.character(x[[text_index]], docvars = docvars, docnames = docname,  : docnames must be unique" --> please fix this

# Checking for duplicates
sum(duplicated(data$speechID))

# Create a new unique identifier
data$unique_id <- paste(data$speechID, seq_len(nrow(data)), sep = "_")

# Create the corpus object with the new unique identifier
corpus_ireland <- corpus(data, 
                         text_field = "speech",
                         docid_field = "unique_id",
                         meta = c("title", "date", "member_gender", "member_name", "party_name", "year", "speechID"))

# Print some information about the corpus
summary(corpus_ireland)

#### Linguistic Complexity ####
# Create a corpus for each party
party_corpus_list <- lapply(selected_parties, function(party) {
  corpus_subset(corpus_ireland, party_name == party)
})
names(party_corpus_list) <- selected_parties

# Calculating Readability Metrics
readability_metrics <- lapply(names(party_corpus_list), function(party) {
  corpus <- party_corpus_list[[party]]
  
  fk <- textstat_readability(corpus, measure = "Flesch.Kincaid")
  f <- textstat_readability(corpus, measure = "Flesch")
  dc <- textstat_readability(corpus, measure = "Dale.Chall.old")
  
  data.frame(
    party = party,
    flesch_kincaid = mean(fk$Flesch.Kincaid, na.rm = TRUE),
    flesch = mean(f$Flesch, na.rm = TRUE),
    dale_chall = mean(dc$Dale.Chall.old, na.rm = TRUE)
  )
})

# Combining the results into a single data frame
readability_df <- do.call(rbind, readability_metrics)

# View the results
print(readability_df)

# Plot for Flesch-Kincaid
ggplot(readability_df, aes(x = party, y = flesch_kincaid, fill = party)) +
  geom_bar(stat = "identity", width = 0.7) +
  labs(title = "Flesch-Kincaid Readability by Party",
       x = "Party",
       y = "Flesch-Kincaid Score",
       fill = "Party") +
  theme_minimal()


# Plot for Flesch
ggplot(readability_df, aes(x = party, y = flesch, fill = party)) +
  geom_bar(stat = "identity", width = 0.7) +
  labs(title = "Flesch Readability by Party",
       x = "Party",
       y = "Flesch Score",
       fill = "Party") +
  theme_minimal()


# Plot for Dale-Chall
ggplot(readability_df, aes(x = party, y = dale_chall, fill = party)) +
  geom_bar(stat = "identity", width = 0.7) +
  labs(title = "Dale-Chall Readability by Party",
       x = "Party",
       y = "Dale-Chall Score",
       fill = "Party") +
  theme_minimal()

#### TTR and LTR with same-sized samples of the female and male coprora ####

# tokenizing the dataset
data$tokens <- sapply(data$speech, function(text) unlist(tokenize_words(text)))

# I asked Chat GPT for help in creating the same-sized samples
# my Prompt: I want to have same-sized samples for a male and female corpus, across parties and years. I want to do a type token ratio and lemma token  and a Lemma token ratio analysis focusing on years and gender distribution
# Calculate the minimum sample size for each party, year, and gender combination

# Calculate the minimum sample size for each gender within each party-year combination
min_size_per_group <- data %>%
  group_by(party_name, year, member_gender) %>%
  summarise(count = n()) %>%
  ungroup() %>%
  group_by(party_name, year) %>%
  summarise(min_count = min(count)) %>%
  ungroup()

# Sample data using the calculated minimum size for each group
balanced_corpus <- data %>%
  inner_join(min_size_per_group, by = c("party_name", "year")) %>%
  group_by(party_name, year, member_gender) %>%
  sample_n(min(min_count)) %>%
  ungroup()

# Create a new unique identifier for balanced corpus
balanced_corpus$unique_id <- paste(balanced_corpus$speechID, seq_len(nrow(balanced_corpus)), sep = "_")

# Convert the data frame to a quanteda corpus object
balanced_corpus <- corpus(balanced_corpus, text_field = "speech")
#View(balanced_corpus)

# extract docvars or perform other corpus-based operations
print(docvars(balanced_corpus))


#### Analyze lexical richness ####
#### Type Token Ratio ####
spacy_initialize(model = "en_core_web_sm")

# split into male and female corpus
male_corpus <- corpus_subset(balanced_corpus, member_gender == "M")
head(male_corpus)
docvars(male_corpus)

female_corpus <- corpus_subset(balanced_corpus, member_gender == "F")
head(female_corpus)
docvars(female_corpus)

# Lemmatize and PoS-tag male corpus

corpus_tagged_male <- spacy_parse(male_corpus,
                                  lemma = TRUE,
                                  pos = TRUE,
                                  tag = TRUE,
                                  entity = TRUE,
                                  dependency = TRUE)

# Define PoS tags to be removed
remove_pos <- c("PUNCT", "NUM", "SYM")
corpus_tagged_male_clean <- corpus_tagged_male[-which(corpus_tagged_male$pos %in% remove_pos),]

# Type Token ratio
male_corpus_ttr <- length(unique(corpus_tagged_male_clean$token))/length(corpus_tagged_male_clean$token)
print(male_corpus_ttr, 3)

# Lemma token ratio
male_corpus_ltr <- length(unique(corpus_tagged_male_clean$lemma))/length(corpus_tagged_male_clean$token)
print(male_corpus_ltr, 3)

#Hapax Percentage
male_corpus_table <- table(corpus_tagged_male_clean$lemma)
male_hapax <- male_corpus_table[which(male_corpus_table == 1)]
male_hpx_pc <- length(male_hapax)/length(male_corpus_table)*100
print(male_hpx_pc, 3)


# Lemmatize and PoS-tag female corpus

corpus_tagged_female <- spacy_parse(female_corpus,
                                  lemma = TRUE,
                                  pos = TRUE,
                                  tag = TRUE,
                                  entity = TRUE,
                                  dependency = TRUE)

# Define PoS tags to be removed
remove_pos <- c("PUNCT", "NUM", "SYM")
corpus_tagged_female_clean <- corpus_tagged_female[-which(corpus_tagged_female$pos %in% remove_pos),]

# Type Token ratio
female_corpus_ttr <- length(unique(corpus_tagged_female_clean$token))/length(corpus_tagged_female_clean$token)
print(female_corpus_ttr, 3)

# Lemma token ratio
female_corpus_ltr <- length(unique(corpus_tagged_female_clean$lemma))/length(corpus_tagged_female_clean$token)
print(female_corpus_ltr, 3)

#Hapax Percentage
female_corpus_table <- table(corpus_tagged_female_clean$lemma)
female_hapax <- female_corpus_table[which(female_corpus_table == 1)]
female_hpx_pc <- length(female_hapax)/length(female_corpus_table)*100
print(female_hpx_pc, 3)





#### largest speeches ####

library(dplyr)
library(stringr)

# Assuming 'data' is your original dataframe

# Verify the date format
print(head(data$date))

# Filter for the year 2018 and the specified parties
speeches_2018 <- data %>%
  filter(substr(date, 1, 4) == "2018", 
         party_name %in% c("Sinn Féin", "Fianna Fáil", "Fine Gael"))

# Calculate the number of words in each speech
speeches_2018 <- speeches_2018 %>%
  mutate(word_count = str_count(speech, '\\w+'))

# Find the largest speech for each party, now including speechID
largest_speeches <- speeches_2018 %>%
  group_by(party_name) %>%
  slice_max(order_by = word_count, n = 1) %>%
  select(party_name, member_name, date, speechID, word_count, speech)

# Print the results
print(largest_speeches)

# If you want to see the full speeches, you can use:
cat(largest_speeches$speech[1])  # For Sinn Féin
cat(largest_speeches$speech[2])  # For Fianna Fáil
cat(largest_speeches$speech[3])  # For Fine Gael

# If you want to see just the speechIDs:
print(largest_speeches %>% select(party_name, speechID, word_count))
