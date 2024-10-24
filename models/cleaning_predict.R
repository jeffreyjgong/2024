# read from data/SGH_elections_clean.csv
df <- read.csv("data/SGH_elections_clean.csv")

table(df$office)

library(rvest)
library(tidyr)
library(dplyr)
library(lubridate)


#### Call Times ####
# Predict call times (in minutes) for Arizona, Georgia, Michigan, Nevada, 
# North Carolina, Pennsylvania, and Wisconsin.

predict_states <- c("Arizona", "Georgia", "Michigan", "Nevada", "North Carolina", 
                    "Pennsylvania", "Wisconsin")

calltimes <- read_html("https://www.270towin.com/2020-election-results-live/")

calltimes_state <- calltimes %>% 
  html_nodes(xpath = '//*[(@id = "live_results_presidential_stats_wrapper")]//*[contains(concat( " ", @class, " " ), concat( " ", "border-left", " " ))]') %>% 
  html_text()

calltimes_df <- data.frame(calltimes_state, rep("", 58))
calltimes_df <- calltimes_df[-c(1,58), ]


calltimes_df <- calltimes_df %>% 
  separate(calltimes_state, into = c("a", "State", "Time"), sep = "\n") %>% 
  select(State, Time)
calltimes_df$Date <- ifelse(grepl("/", calltimes_df$Time), trimws(substr(calltimes_df$Time, 1, 5)), "11/3")
calltimes_df$Time <- ifelse(grepl("/", calltimes_df$Time), trimws(substr(calltimes_df$Time, 6, nchar(calltimes_df$Time))), calltimes_df$Time)

poll_closing_times <- data.frame(
  State = c("Georgia", "Indiana", "Kentucky", "South Carolina", "Vermont", "Virginia", 
            "North Carolina", "Ohio", "West Virginia", "Alabama", "Connecticut", 
            "Delaware", "Washington, DC", "Florida", "Illinois", "Maine", "Maryland", "Massachusetts", 
            "Mississippi", "Missouri", "New Hampshire", "New Jersey", "Oklahoma", 
            "Pennsylvania", "Rhode Island", "Tennessee", "Arkansas", "Arizona", 
            "Colorado", "Kansas", "Louisiana", "Michigan", "Minnesota", "Nebraska", 
            "New Mexico", "New York", "North Dakota", "South Dakota", "Texas", 
            "Wisconsin", "Wyoming", "Iowa", "Montana", "Nevada", "Utah", 
            "California", "Idaho", "Oregon", "Washington", "Hawaii", "Alaska"),
  Closing_Time = c("7:00 PM", "7:00 PM", "7:00 PM", "7:00 PM", "7:00 PM", "7:00 PM", 
                   "7:30 PM", "7:30 PM", "7:30 PM", "8:00 PM", "8:00 PM", "8:00 PM", 
                   "8:00 PM", "8:00 PM", "8:00 PM", "8:00 PM", "8:00 PM", "8:00 PM", 
                   "8:00 PM", "8:00 PM", "8:00 PM", "8:00 PM", "8:00 PM", "8:00 PM", 
                   "8:00 PM", "8:00 PM", "8:30 PM", "9:00 PM", "9:00 PM", "9:00 PM", "9:00 PM", 
                   "9:00 PM", "9:00 PM", "9:00 PM", "9:00 PM", "9:00 PM", "9:00 PM", 
                   "9:00 PM", "9:00 PM", "9:00 PM", "9:00 PM", "10:00 PM", "10:00 PM", 
                   "10:00 PM", "10:00 PM", "11:00 PM", "11:00 PM", "11:00 PM", 
                   "11:00 PM", "12:00 AM", "1:00 AM"))

# Clean up the state names in the original dataframe to match the poll_closing_times dataframe
calltimes_df <- calltimes_df %>%
  mutate(Clean_State = gsub(" \\(.*\\)| CD.*", "", State),
         Clean_State = trimws(Clean_State)) # Remove everything in parentheses

# Join the closing time data with the main dataframe
calltimes_df <- left_join(calltimes_df, poll_closing_times, by = c("Clean_State" = "State"))

calltimes_df$call_time <- as.POSIXct(paste0("2020/", calltimes_df$Date, " ", calltimes_df$Time), format = "%Y/%m/%d %I:%M %p")
calltimes_df$poll_closing_time <- as.POSIXct(paste0("2020/11/03 ", calltimes_df$Closing_Time), format = "%Y/%m/%d %I:%M %p")

calltimes_df$call_time <- as.POSIXct(calltimes_df$call_time, format = "%Y/%m/%d %I:%M %p")
calltimes_df$poll_closing_time <- as.POSIXct(calltimes_df$poll_closing_time, format = "%Y/%m/%d %I:%M %p")
# Calculate the difference between the call time and the poll closing time
calltimes_df$time_diff_2020 <- calltimes_df$call_time - calltimes_df$poll_closing_time

calltimes_df <- calltimes_df %>% 
  select(State, Clean_State, call_time, poll_closing_time, time_diff_2020)

calltimes_df$Clean_State[calltimes_df$Clean_State == "Washington, DC"] <- "District of Columbia"

calltimes_df$Clean_State <- toupper(calltimes_df$Clean_State)

# View the result
head(calltimes_df)

# write.csv(calltimes_df, file = "data/calltimes_df.csv")

pres_df <- read.csv("data/1976-2020-president.csv")
head(pres_df)

pres_df_2020 <- pres_df %>% 
  filter(year == 2020) %>% 
  select(state, state_po, candidate, candidatevotes, totalvotes) %>% 
  mutate(vote_percent = candidatevotes / totalvotes)

# output difference between top two vote_percent by state
# keep top two vote_percent by state
pres_df_2020 <- pres_df_2020 %>% 
  group_by(state) %>% 
  arrange(desc(vote_percent)) %>% 
  mutate(vote_diff = first(vote_percent) - nth(vote_percent, 2)) %>% 
  filter(row_number() <= 1)

calltimes_df <- calltimes_df %>% 
  right_join(pres_df_2020, by = c("Clean_State" = "state"))


