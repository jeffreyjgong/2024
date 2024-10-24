# read from data/SGH_elections_clean.csv
df <- read.csv("data/SGH_elections_clean.csv")

table(df$office)

library(rvest)
library(tidyr)
library(dplyr)
library(lubridate)

calltimes <- read_html("https://www.270towin.com/2020-election-results-live/")



calltimes_state <- calltimes %>% 
  html_nodes(xpath = '//*[(@id = "live_results_presidential_stats_wrapper")]//*[contains(concat( " ", @class, " " ), concat( " ", "border-left", " " ))]') %>% 
  html_text()

# join calltimes_state and blank column
calltimes_df <- data.frame(calltimes_state, rep("", 58))
# remove first and last row
calltimes_df <- calltimes_df[-c(1,58), ]

# separate character string in calltimes_df$calltimes_state
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
calltimes_df$diff_2020 <- calltimes_df$call_time - calltimes_df$poll_closing_time

calltimes_df <- calltimes_df %>% 
  select(State, Clean_State, call_time, poll_closing_time, diff_2020)

# View the result
head(calltimes_df)

write.csv(calltimes_df, file = "data/calltimes_df.csv")

