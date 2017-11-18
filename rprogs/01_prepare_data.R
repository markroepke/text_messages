
##########
# Set Up #
##########

# Prepare script
rm(list = ls())
graphics.off()
setwd("C:\\Users\\roepk_000\\OneDrive\\Documents\\Data\\data\\text_mining\\text_messages")

# Load libraries
library(XML)
library(tidyverse)
library(tidytext)
library(lubridate)

# Get info
sessionInfo()
Sys.info()
Sys.time()

#############
# Load Data #
#############

# Load data from app
xml_data <- xmlParse("input/text_messages.xml") %>%
  xmlToList()

##############
# Parse Data #
##############

# Convert data from nested list to data frame

for (i in 1:length(xml_data)){
  
  new_row <- xml_data[[i]] %>% 
    as.data.frame() %>% 
    t() %>%
    as.data.frame()
  
  # if first iteration, make loop data with the single row
  if (i == 1){
    loop_data <- new_row
  # if not first iteration, append the row
  # will be slow with large data
  } else {
    loop_data <- loop_data %>% bind_rows(new_row)
  }
  
  # print status message
  if (i %% 1000 == 0){
    print(paste0("Finished parsing ", round((i/length(xml_data))*100, 2), "% of text messages."))
  }
  
}

# remove unnesesary data
rm(new_row); rm(xml_data)

#############
# Tidy data #
#############

text_data <- loop_data %>%
  # filter to only includes texts with Sophie
  filter(contact_name == "Sophie Stewart") %>%
  select(address, date, type, body, read, date_sent, readable_date, contact_name) %>%
  # parse date and time out of date-time
  mutate(date = mdy(str_sub(readable_date, 1, 12)),
         time = str_sub(readable_date, 14, 24)) %>%
  select(date, time, type, body, read) %>%
  # unnest tokens to made tidytext data
  unnest_tokens(word, body)
  
#############
# Save Data #
#############

# Writing to RDS file to use with later scripts
write_rds(text_data, path = "input/text_data.rds")


###
Sys.time()