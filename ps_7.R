library(tidyverse)
library(stringr)
library(fs)
library(dplyr)
library(rebus)
library(tools)
library(readxl)
library(janitor)
library(knitr)
library(scales)
library(purrr)
library(lubridate)
library(kableExtra)
library(haven)
library(foreign)
library(shiny)
library(scales)

upshot_zip <- download.file(url = "https://goo.gl/ZRCBda",
                            destfile = "upshot.zip",
                            quiet = TRUE,
                            mode = "wb") #Download Upshot data zipfile

unzip("upshot.zip") #Unzip file

file_names <- dir_ls("2018-live-poll-results-master/data") #Set file_names to vector with all file names (polls) in directory (Upshot data)

big_table <- map_dfr(file_names, read_csv, .id = "source") #Build one dataframe that includes all polls and new column denoting source of data.

big_named <- big_table %>%
  separate(source, into = c("path", "race"), 
           sep = "s-poll-", fill = "right",
           remove = TRUE) %>% #Separate source path column into a column with the uninformatice beginning portion of the path and the informative end of the path (race).
  mutate(state = str_to_upper(str_sub(race, 0, 2)), #Column with capitalized state abbreviation
         wave = str_sub(race, -5, -5), #Column with polling wave number
         house_seat = str_sub(race, 3, 4), #Column with house seat district number (will be "se" or "go" if Senator or Governor race respectively)
         position = ifelse(house_seat == "se", "Senate", "House"), #Column with office associated with election polled   #Set all Senate seats to "Senate" and all others to "House"
         position = ifelse(house_seat == "go", "Governorship", position), #If there are governor's races, set position variable to "Governorship", if not stay same as stated above
         house_seat = ifelse(house_seat == "se", NA, house_seat), #For Senate seats, set house seat number to NA
         house_seat = ifelse(house_seat == "go", NA, house_seat), #For Governorships, set house seat number to NA
         house_seat = as.numeric(house_seat), #Parse house seat district number as a numeric
         state_dist = str_sub(race, 0, 4), #Column with poll state and district as in source (lowercase state abbreviation and two digit district number: aa00)
         state_dist = ifelse(position == "House", state_dist, NA)) %>% #If not a House poll, set state_dist to NA
  select(- path) #Take the ininformative part of the path out of the table

file_delete(c("upshot.zip", "2018-live-poll-results-master")) #Delete superfluous files

#Thank you to Mr. Shroeder for compiling this data table of actual results!
actual_res <- read_csv("actual_results.csv") %>%
  mutate(win_name = ifelse(win_name == "Mia Love", "Ben McAdams", win_name)) %>%
  mutate(win_party = ifelse(win_name == "Ben McAdams", "D", win_party)) #Fix mistakes found in data

advantage_pred <- big_named %>%
  group_by(response, race) %>%
  tally(wt = final_weight) %>% #Count the number of party responses for each race, weighted 
  spread(key = response, value = n) %>% #Spread responses to widen data table
  rename("three" = `3`, "four" = `4`, "five" = `5`, "six" = `6`) %>% #Rename minor party labels
  mutate(three = ifelse(is.na(three), 0, three),
         four = ifelse(is.na(four), 0, four),
         five = ifelse(is.na(five), 0, five),
         six = ifelse(is.na(six), 0, six)) %>% #Set all NA values in minor parties to 0 to mean no people responded this party
  mutate(state_dist = str_sub(race, 0, 4),
         total = Dem + Rep + Und + three + four + five + six,
         rep_adv = (Rep - Dem) / total, #Find the Republican Advantage found in each poll
         state = str_to_upper(str_sub(state_dist, 0, 2)),
         district = str_sub(state_dist, 3, 4),
         position = ifelse(district == "se", "Senate", "House"),
         position = ifelse(district == "go", "Governorship", position),
         state_dist = paste(state, "-", district, sep = "")) %>% #Add punctiation, change case, round, and parse for leftjoining in table
  arrange(desc(race)) %>% #Arrange the races by descending alphabetical order, meaning that, for each race, the latest wave will be displayed first
  distinct(state_dist, .keep_all = TRUE) %>% #Keep only only distinct race for each set of waves, this will be the latest one because of the prior arrange
  arrange(race) %>% #Rearrange to alphabetical order
  select(state_dist, state, district, position, rep_adv, race) #Select necessary columns

actual_processed <- actual_res %>%
  mutate(state_dist = paste(state, "-", district, sep = ""), #Add punctuation for left join
         rep_adv_actual = (rep_votes - dem_votes) / (dem_votes + rep_votes + other_votes)) #Calculate the actual Republican Advantage

advantage_pred_std <- advantage_pred %>%
  mutate(state_dist = ifelse(state_dist == "AZ-se", "AZ-sen", state_dist), 
         state_dist = ifelse(state_dist == "FL-go", "FL-gov", state_dist),
         state_dist = ifelse(state_dist == "FL-se", "FL-sen", state_dist),
         state_dist = ifelse(state_dist == "NV-se", "NV-sen", state_dist),
         state_dist = ifelse(state_dist == "TN-se", "TN-sen", state_dist),
         state_dist = ifelse(state_dist == "TX-se", "TX-sen", state_dist),
         house_state_dist = paste(state, "-", district, sep = ""),
         state_dist = ifelse(state_dist %in% c("AZ-sen", "FL-gov", "FL-sen",
                                               "NV-sen", "TN-sen", "TX-sen"), 
                             state_dist, house_state_dist)) %>% #Restructure all state district names for left joining
  select(race, state_dist, state, district, position, rep_adv) #Include only necessary columns

#Left join the predicted and actual republican advantages by state district
predict_actual <- left_join(advantage_pred_std, actual_processed, by = "state_dist") %>%
  filter(position == "House") %>% #Only use House predictions for more accurate comparisons
  select(race, state_dist, position, rep_adv, rep_adv_actual, win_party)

mistakes <- predict_actual %>%
  mutate(win_predicted = ifelse(rep_adv > 0, "R", "D"),
         accuracy = ifelse(win_predicted == win_party, "Correct", "Incorrect"),
         rep_adv_difference = rep_adv_actual - rep_adv) %>% #Democrats overperforming yields a NEGATIVE difference 
  rename(win_actual = win_party)

count_phone <- big_named %>% 
  filter(position == "House") %>%
  group_by(race) %>%
  count(phone_type) %>%
  spread(phone_type, n) %>%
  mutate(`percent_cell` = `Cell` / (`Cell` + `Landline`)) %>%
  select(race, percent_cell)

count_raceth <- big_named %>% 
  filter(position == "House") %>%
  group_by(race) %>%
  count(race_eth) %>%
  spread(race_eth, n) %>%
  mutate(total = `[DO NOT READ] Don't know/Refused` + `Asian` + `Black` + 
                 `Hispanic` + `Other` + `White`,
         percent_white = `White` / total,
         percent_black = `Black` / total,
         percent_hispanic = `Hispanic` / total,
         percent_asian = `Asian` / total) %>%
  select(race, percent_white, percent_black, percent_hispanic, percent_asian)

count_gender <- big_named %>% 
  filter(position == "House") %>%
  group_by(race) %>%
  count(gender) %>%
  spread(gender, n) %>%
  select(- `[DO NOT READ] Don't know/Refused`) %>%
  mutate(total = `Female` + `Male`,
         percent_male = `Male` / total) %>%
  select(race, percent_male)

count_partyid <- big_named %>% 
  filter(position == "House") %>%
  group_by(race) %>%
  count(partyid) %>%
  spread(partyid, n) %>%
  select(- `[DO NOT READ] Don't know/Refused`, - `[DO NOT READ] Refused`, - `or as a member of another political party`) %>%
  mutate(total = Democrat + `Independent (No party)` + Republican,
         percent_dem = Democrat / total,
         percent_rep = Republican / total,
         percent_ind = `Independent (No party)` / total) %>%
  select(race, percent_dem, percent_rep, percent_ind)

mistakes_info <- mistakes %>%
  left_join(count_phone, by = "race") %>%
  left_join(count_raceth, by = "race") %>%
  left_join(count_gender, by = "race") %>%
  left_join(count_partyid, by = "race")

