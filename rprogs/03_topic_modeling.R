
##########
# Set Up #
##########

# Prepare script
rm(list = ls())
graphics.off()
setwd("C:\\Users\\roepk_000\\OneDrive\\Documents\\Data\\data\\text_mining\\text_messages")

# Load libraries
library(tidyverse)
library(tidytext)
library(topicmodels)

# Get info
sessionInfo()
Sys.info()
Sys.time()

#########################
# Load and Prepare Data #
#########################

# Load data
text_data <- read_rds(path = "input/text_data.rds")

# Prepare Data
texting_dtm <- text_data %>%
  # remove stop words
  dplyr::filter(!(word %in% c("i", "you", "to", "the", "and", "that", "a", "it", "i'm", "just", "for", "is",
                       "okay", "be", "are", "so", "have", "it's", "but", "me", "at", "that's", "was",
                       "of", "in", "do", "we", "yeah", "oh", "no", "my", "not", "what", "don't", "like",
                       "with", "if", "go", "up", "about", "know", "can", "good", "want", "well", "on",
                       "your", "sorry", "she", "i'll", "there", "yes", "or", "when", "get", "this", "how",
                       "now", "would", "ok", "will", "they", "all", "then", "did", "really", "he", "sure",
                       "could", "think", "going", "gonna", "really", "got", "you're", "too", "here",
                       "been", "didn't", "haha", "am", "one", "time", "work"))) %>%
  dplyr::select(word, date) %>%
  dplyr::mutate(date = as.character(date)) %>%
  # count number of words per days
  dplyr::count(date, word) %>%
  # cast into document term matrix
  tidytext::cast_dtm(date, word, n)

###########################
# Topic Modeling Analysis #
###########################

# Split into two topics
texting_lda_2 <- LDA(texting_dtm, k = 2, control = list(seed = 1234))

# Tidy LDA object into a matrix
texting_topics <- tidy(texting_lda_2, matrix = "beta")

# Find top terms in each topic
texting_top_terms <- texting_topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

# Calculate beta spread to find relative freuencies of topics
beta_spread <- texting_topics %>%
  mutate(topic = paste0("topic_", topic)) %>%
  spread(topic, beta) %>%
  filter(topic_1 > 0.0001 | topic_2 > 0.0001) %>%
  mutate(log_ratio = log2(topic_2/topic_1))

##################
# Visualizations #
##################

# Show most common words for each topic
texting_top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()

# Find most relatively common words in topics
mutate(select(top_n(beta_spread, 10, log_ratio), log_ratio = log_ratio, term)) %>%
  bind_rows(select(top_n(beta_spread, -10, log_ratio), log_ratio = log_ratio, term)) %>%
  mutate(term = reorder(term, log_ratio)) %>%
  ggplot(aes(term, log_ratio)) +
  geom_col(show.legend = FALSE) +
  coord_flip()