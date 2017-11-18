



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

# Get info
sessionInfo()
Sys.info()
Sys.time()

#############
# Load Data #
#############

text_data <- read_rds(path = "input/text_data.rds")

##############################
# Perform sentiment analysis #
##############################

# Find number of words texted on each day
words_per_day <- text_data %>%
  dplyr::group_by(date) %>%
  dplyr::summarize(number_of_words = n())

# Find sentiment of texting on each day
texting_sentiment <- text_data %>%
  # join to sentiment data
  dplyr::inner_join(get_sentiments("bing")) %>%
  # count positive and negative sentiment on each day
  dplyr::count(type, index = date, sentiment) %>%
  # create a positive and negative column for each date
  spread(sentiment, n, fill = 0) %>%
  # join to number of words texted on each date
  inner_join(words_per_day, by = c("index" = "date")) %>%
  # find sentiment/word for each day by person
  mutate(sentiment = (positive - negative)/number_of_words,
         sender = ifelse(type == "1", "Sent by Sophie", "Sent by Mark"))

############################
# Visualization of results #
############################

# Create visual
ggplot(texting_sentiment, aes(index, sentiment, fill = sender)) +
  geom_col(show.legend = FALSE) +
  scale_y_continuous(lim = c(-.05, .05)) +
  scale_x_date(lim = as.Date(c("2016-12-23", "2017-5-26"))) +
  scale_fill_manual(values = c("darkblue", "darkgreen")) +
  facet_wrap(~sender, ncol = 2) +
  labs(title = "Sentiment Analysis: Text Messages Between Sophie & Mark",
       subtitle = "Communication is overwhelmingly positive, but Mark has more negatively dominated days of texting",
       y = "Sentiment",
       x = "Date",
       caption = "Sentiment = (Positivity Score - Negativity Score) / Number of Words") +
  theme_bw()

# Save visual
ggsave(filename = "output/sophie_and_mark.png",
       dpi = 900,
       width = 10,
       height = 5)

###
Sys.time()
