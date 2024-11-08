#installing and loading relevant packages/libraries
install.packages(c("textstem", "spacyr", "stringr", "quanteda.textstats", "quanteda", "readr", "tokenizers", "tidytext", "tibble", "dplyr", "readxl", "ggplot2", "patchwork","visreg","moments","ggpubr"))

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
data <- read.csv2("/Users/Clara/Desktop/MA Digital Humanities/ue text corpora/ireland_abortion_incl.gender_incl2018.csv", header = TRUE)
View(data)

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
View(data)

#extracting the year
data$year <- format(data$date, "%Y")

#adding year to dataframe as new column
data$year <- as.integer(data$year)  # Convert the year to integer if needed

#checking results
head(data$year)

#preprocessing: lowercasing and removing punctuation
data$speech <- tolower(data$speech)  # 
data$speech <- gsub("[[:punct:]]", " ", data$speech) 

#tokenizing
data$tokens <- sapply(data$speech, function(text) unlist(tokenize_words(text)))
View(data)

data$tokens <- lapply(data$tokens, as.character)

#removing stopwords
data$tokens <- lapply(data$tokens, function(tokens) tokens[!tokens %in% stopwords("en")])

View(data)

#loading sentiment scores, header=FALSE for NRC lexicon
sentiment_scores <- read.delim("/Users/Clara/Desktop/MA Digital Humanities/ue text corpora/Session 4 UK Parliament amp sentiment-20240515/UK_sent.txt", header=TRUE, sep="\t", stringsAsFactors=FALSE)
View(sentiment_scores)

#correcting column names (only for nrc lexicon, comment out if else)
print(colnames(sentiment_scores))

#colnames(sentiment_scores)[1] <- "Word"
#colnames(sentiment_scores)[2] <- "Valence" 
#colnames(sentiment_scores)[3] <- "Arousal" 
#colnames(sentiment_scores)[4] <- "Dominance"

View(sentiment_scores)

#creating sentiment dict
sentiment_dict <- setNames(lapply(1:nrow(sentiment_scores), function(i) {
  c(Valence = sentiment_scores$Valence[i], Arousal = sentiment_scores$Arousal[i], Dominance = sentiment_scores$Dominance[i])
}), sentiment_scores$Word)

#chatgpt:
# Initialize an empty data frame to store token-level sentiment scores and metadata
token_sentiment_df <- data.frame(Token = character(), Valence = numeric(), Arousal = numeric(), Dominance = numeric(),
                                 Member = character(), Title = character(), Date = character(),
                                 member_gender = character(), party_name = character(), year = character(),
                                 stringsAsFactors = FALSE)

# Function to calculate sentiment scores for each token in a speech
calculate_sentiment_scores <- function(tokens, member, title, date, member_gender, party_name, year) {
  token_scores <- data.frame(Token = character(), Valence = numeric(), Arousal = numeric(), Dominance = numeric(),
                             Member = character(), Title = character(), Date = character(),
                             member_gender = character(), party_name = character(), year = character(),
                             stringsAsFactors = FALSE)
  
  for (token in tokens) {
    if (!is.null(sentiment_dict[[token]])) {
      valence <- as.numeric(sentiment_dict[[token]]["Valence"])
      arousal <- as.numeric(sentiment_dict[[token]]["Arousal"])
      dominance <- as.numeric(sentiment_dict[[token]]["Dominance"])
      token_scores <- rbind(token_scores, data.frame(Token = token, Valence = valence, Arousal = arousal, Dominance = dominance,
                                                     Member = member, Title = title, Date = date,
                                                     member_gender = member_gender, party_name = party_name, year = year,
                                                     stringsAsFactors = FALSE))
    } else {
      token_scores <- rbind(token_scores, data.frame(Token = token, Valence = NA, Arousal = NA, Dominance = NA,
                                                     Member = member, Title = title, Date = date,
                                                     member_gender = member_gender, party_name = party_name, year = year,
                                                     stringsAsFactors = FALSE))
    }
  }
  
  return(token_scores)
}

# Apply the function to each set of tokens
for (i in seq_len(nrow(data))) {
  member <- data$member_name[i]
  title <- data$title[i]
  date <- data$date[i]
  tokens <- data$tokens[[i]]
  member_gender <- data$member_gender[i]
  party_name <- data$party_name[i]
  year <- data$year[i]
  
  # Calculate sentiment scores for tokens in the current speech
  token_sentiment_scores <- calculate_sentiment_scores(tokens, member, title, date, member_gender, party_name, year)
  
  # Append token sentiment scores to token_sentiment_df
  token_sentiment_df <- rbind(token_sentiment_df, token_sentiment_scores)
}

# View the updated data frame
View(token_sentiment_df)

#count instances with n/a values
na_count <- sum(is.na(token_sentiment_df$Dominance))
print(na_count)

#remove n/a values
token_sentiment_df <- token_sentiment_df[!is.na(token_sentiment_df$Arousal), ]

#count tokens per party
speech_counts <- token_sentiment_df %>%
  group_by(party_name) %>%
  summarize(num_speeches = n())

print(speech_counts)

#gender differences
average_dominance_by_gender <- token_sentiment_df %>%
  group_by(member_gender) %>%
  summarize(Dominance = mean(Dominance, na.rm = TRUE))
print(average_dominance_by_gender)

average_valence_by_gender <- token_sentiment_df %>%
  group_by(member_gender) %>%
  summarize(Valence = mean(Valence, na.rm = TRUE))
print(average_valence_by_gender)

average_arousal_by_gender <- token_sentiment_df %>%
  group_by(member_gender) %>%
  summarize(Arousal = mean(Arousal, na.rm = TRUE))
print(average_arousal_by_gender)

female_measures <- token_sentiment_df[which(token_sentiment_df$member_gender == "F"),]
male_measures <- token_sentiment_df[which(token_sentiment_df$member_gender == "M"),]
t.test(female_measures$Valence, male_measures$Valence)
t.test(female_measures$Arousal, male_measures$Arousal)
t.test(female_measures$Dominance, male_measures$Dominance)

ggplot(token_sentiment_df %>%
         pivot_longer(cols = c(Dominance, Valence, Arousal), names_to = "Measure", values_to = "Value"), 
       aes(x = member_gender, y = Value, fill = member_gender)) +
  geom_boxplot() +
  facet_wrap(~ Measure, scales = "free_y") +
  scale_fill_manual(values = c("F" = "red", "M" = "blue")) +
  ggtitle("Comparison of Sentiment by Gender (NRC lexicon)") + #or lexicon by warriner et al.
  xlab("Gender") +
  ylab("Value") +
  theme_minimal()

#party differences

#averages
average_dominance_by_party <- token_sentiment_df %>%
  group_by(party_name) %>%
  summarize(Dominance = mean(Dominance, na.rm = TRUE))
print(average_dominance_by_party)

average_valence_by_party <- token_sentiment_df %>%
  group_by(party_name) %>%
  summarize(Valence = mean(Valence, na.rm = TRUE))
print(average_valence_by_party)

average_arousal_by_party <- token_sentiment_df %>%
  group_by(party_name) %>%
  summarize(Arousal = mean(Arousal, na.rm = TRUE))
print(average_arousal_by_party)

#subsetting the data
fianna_fail <- token_sentiment_df[which(token_sentiment_df$party_name == "Fianna Fáil"),]
fine_gael <- token_sentiment_df[which(token_sentiment_df$party_name == "Fine Gael"),]
labour_party <- token_sentiment_df[which(token_sentiment_df$party_name == "Labour Party"),]
sinn_fein <- token_sentiment_df[which(token_sentiment_df$party_name == "Sinn Féin"),]
workers_party <- token_sentiment_df[which(token_sentiment_df$party_name == "The Workers' Party"),]

#testing statistical significance
t.test(labour_party$Valence, fianna_fail$Valence)
t.test(labour_party$Dominance, fianna_fail$Dominance)
t.test(labour_party$Arousal, fianna_fail$Arousal)

t.test(sinn_fein$Valence, fianna_fail$Valence)
t.test(sinn_fein$Dominance, fianna_fail$Dominance)
t.test(sinn_fein$Arousal, fianna_fail$Arousal)

t.test(fine_gael$Valence, fianna_fail$Valence)
t.test(fine_gael$Dominance, fianna_fail$Dominance)
t.test(fine_gael$Arousal, fianna_fail$Arousal)

t.test(workers_party$Valence, fianna_fail$Valence)
t.test(workers_party$Dominance, fianna_fail$Dominance)
t.test(workers_party$Arousal, fianna_fail$Arousal)

#anova analysis (code taken from analysis in class)

## Arousal
# Plot histogram

hist(token_sentiment_df$Arousal)

# Compare to normal distribution

hist(rnorm(1000000, mean = mean(token_sentiment_df$Arousal, sd = sd(token_sentiment_df$Arousal))))

# Determine skewness 

ggdensity(token_sentiment_df, x = "Arousal", fill = "lightgray", title = "Arousal") +
  scale_x_continuous(limits = c(0, 10)) +
  stat_overlay_normal_density(color = "red", linetype = "dashed")

skewness(token_sentiment_df$Arousal, na.rm = TRUE) #moderate positive skew (0.64/0.57)(Warriner/NRC)

# Moderate positive skew => transform with square root

token_sentiment_df$Arousal_trans <- sqrt(token_sentiment_df$Arousal)

# Inspect distribution of square-root transformed Arousal

hist(token_sentiment_df$Arousal_trans)

ggdensity(token_sentiment_df, x = "Arousal_trans", fill = "lightgray", title = "Arousal_trans") +
  scale_x_continuous(limits = c(min(token_sentiment_df$Arousal_trans), max(token_sentiment_df$Arousal_trans))) +
  stat_overlay_normal_density(color = "red", linetype = "dashed")

skewness(token_sentiment_df$Arousal_trans, na.rm = TRUE) 

# Plot histogram

hist(token_sentiment_df$Dominance)

# Determine skewness

ggdensity(token_sentiment_df, x = "Dominance", fill = "lightgray", title = "Dominance") +
  scale_x_continuous(limits = c(0, 10)) +
  stat_overlay_normal_density(color = "red", linetype = "dashed")

skewness(token_sentiment_df$Dominance, na.rm = TRUE) 

# Moderate negative skew => transform by squaring

token_sentiment_df$Dominance_trans <- (token_sentiment_df$Dominance)^2

# Inspect distribution of square-transformed Dominance

hist(token_sentiment_df$Dominance_trans)

ggdensity(token_sentiment_df, x = "Dominance_trans", fill = "lightgray", title = "Dominance_trans") +
  scale_x_continuous(limits = c(min(token_sentiment_df$Dominance_trans), max(token_sentiment_df$Dominance_trans))) +
  stat_overlay_normal_density(color = "red", linetype = "dashed")

skewness(token_sentiment_df$Dominance_trans, na.rm = TRUE) 
# Plot histogram

hist(token_sentiment_df$Valence)

# Determine skewness

ggdensity(token_sentiment_df, x = "Valence", fill = "lightgray", title = "Valence") +
  scale_x_continuous(limits = c(0, 10)) +
  stat_overlay_normal_density(color = "red", linetype = "dashed")

skewness(token_sentiment_df$Valence, na.rm = TRUE) #moderate negative skew

token_sentiment_df$Valence_trans <- (token_sentiment_df$Valence)^2

# Inspect distribution of square-transformed Valence

hist(token_sentiment_df$Valence_trans)

ggdensity(token_sentiment_df, x = "Valence_trans", fill = "lightgray", title = "Valence_trans") +
  scale_x_continuous(limits = c(min(token_sentiment_df$Valence_trans), max(token_sentiment_df$Valence_trans))) +
  stat_overlay_normal_density(color = "red", linetype = "dashed")

skewness(token_sentiment_df$Valence_trans, na.rm = TRUE) 

#center and normalize data
token_sentiment_df$Arousal_trans <- scale(token_sentiment_df$Arousal_trans)
token_sentiment_df$Dominance_trans <- scale(token_sentiment_df$Dominance_trans)
token_sentiment_df$Valence_trans <- scale(token_sentiment_df$Valence_trans)

hist(token_sentiment_df$Arousal_trans)
hist(token_sentiment_df$Dominance_trans)
hist(token_sentiment_df$Valence_trans)

## Model Valence with both Party and Gender as predictors

model_val <- lm(Valence_trans ~ member_gender + party_name, data = token_sentiment_df)

# Interpret the model

summary(model_val)

# Plot

visreg(model_val, "party_name", rug = FALSE, partial = FALSE)
visreg(model_val, "member_gender", rug = FALSE, partial = FALSE)


## Model Valence with both Party and Gender as predictors + interactions

model_val <- lm(Valence_trans ~ member_gender + party_name + member_gender:party_name, data = token_sentiment_df)

# Interpret the model

summary(model_val)

# Plot

visreg(model_val, "member_gender", by = "party_name", rug = FALSE, partial = FALSE)


## Model Arousal with both Party and Gender as predictors
model_ar <- lm(Arousal_trans ~ member_gender + party_name + member_gender:party_name, data = token_sentiment_df)

# Interpret the model

summary(model_ar)

# Plot

visreg(model_ar, "member_gender", by = "party_name", rug = FALSE, partial = FALSE)

## Model Dominance with both Party and Gender as predictors
model_dom <- lm(Dominance_trans ~ member_gender + party_name + member_gender:party_name, data = token_sentiment_df)

# Interpret the model

summary(model_dom)

# Plot

visreg(model_dom, "member_gender", by = "party_name", rug = FALSE, partial = FALSE)


#year variable
average_dominance_by_year <- token_sentiment_df %>%
  group_by(year) %>%
  summarize(Dominance = mean(Dominance, na.rm = TRUE))
print(average_dominance_by_year)

average_valence_by_year <- token_sentiment_df %>%
  group_by(year) %>%
  summarize(Valence = mean(Valence, na.rm = TRUE))
print(average_valence_by_year)

average_arousal_by_year <- token_sentiment_df %>%
  group_by(year) %>%
  summarize(Arousal = mean(Arousal, na.rm = TRUE))
print(average_arousal_by_year)

sentiment_data <- merge(average_dominance_by_year, average_valence_by_year, by = "year", suffixes = c("_dominance", "_valence"))
sentiment_data <- merge(sentiment_data, average_arousal_by_year, by = "year")

#reshaping data (chatgpt)
library(tidyr)

sentiment_data_long <- sentiment_data %>%
  pivot_longer(cols = c(Dominance, Valence, Arousal),
               names_to = "Sentiment",
               values_to = "Average")

#creating plot
library(ggplot2)

ggplot(sentiment_data_long, aes(x = year, y = Average, color = Sentiment)) +
  geom_line() +
  labs(title = "Average Sentiment Measures by Year",
       x = "Year",
       y = "Average Value") +
  scale_color_manual(values = c("Dominance" = "#659dfb", "Valence" = "#04d43e", "Arousal" = "#fb756d")) +
  theme_minimal()

#find most frequent tokens
most_frequent_tokens_fianna <- token_sentiment_df %>%
  filter(party_name == "Fianna Fáil") %>%
  count(Token, sort = TRUE)
head(most_frequent_tokens_fianna, 20)

most_frequent_tokens_fine <- token_sentiment_df %>%
  filter(party_name == "Fine Gael") %>%
  count(Token, sort = TRUE)
head(most_frequent_tokens_fine, 20)

most_frequent_tokens_sinn <- token_sentiment_df %>%
  filter(party_name == "Sinn Féin") %>%
  count(Token, sort = TRUE)
head(most_frequent_tokens_sinn, 20)

most_frequent_tokens_labour <- token_sentiment_df %>%
  filter(party_name == "Labour Party") %>%
  count(Token, sort = TRUE)
head(most_frequent_tokens_labour, 20)

most_frequent_tokens_workers <- token_sentiment_df %>%
  filter(party_name == "The Workers' Party") %>%
  count(Token, sort = TRUE)
head(most_frequent_tokens_workers, 20)
