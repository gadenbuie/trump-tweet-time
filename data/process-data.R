library(dplyr)
library(tidyr)
library(purrr)
library(readr)
library(lubridate)

in_hours <- function(x) {
  hour(x) + minute(x)/60 + second(x)/60^2
}

event_type_labels <- c(
  "Executive Time",
  "Weekend",
  "Travel",
  "Lunch",
  "Meeting",
  "Event",
  "Unknown"
)

# Exec Time Downloaded from https://docs.google.com/spreadsheets/d/1oITCuVsYdhNXtY7GElLelsrbjRRIPJ1ce-_v-8J1X_A/edit#gid=0
exec_time <-
  read_csv(
    here::here(
      "data",
      "axios_trump_schedule_2018-11-07--2019-02-02.csv"
    ),
    col_types = cols(.default = col_character())
  ) %>%
  mutate(
    event_id   = row_number(),
    time_start = paste(date, time_start),
    time_end   = paste(date, time_end),
    time_start = ymd_hm(time_start, tz = "America/New_York"),
    time_end   = ymd_hm(time_end, tz = "America/New_York"),
    time_end   = if_else(time_start > time_end, time_end + hours(24), time_end),
    # truncate day at 8am and 5pm
    time_start = if_else(
      in_hours(time_start) < 8 & in_hours(time_end) > 8,
      floor_date(time_start, "day") + hours(8),
      time_start
    ),
    time_end = if_else(
      in_hours(time_start) < 17 & in_hours(time_end) > 17,
      floor_date(time_end, "day") + hours(17),
      time_end
    ),
    time_inc   = map2(time_start, time_end, seq, by = "5 mins")
  ) %>%
  select(event_id, time_start, time_end, time_inc, listed_title, listed_location, top_category, notes)

djt <- readRDS(here::here("data", "djt-tweets-2018-09--2019-02.rds")) %>% 
  distinct(status_id, .keep_all = TRUE)

djt_simple <- 
  djt %>% 
  filter(!is_retweet) %>% 
  mutate(
    created_at = with_tz(created_at, tzone = "America/New_York"),
    time_inc = floor_date(created_at, "5 min")
  ) %>% 
  select(created_at, time_inc, text, status_id)

djt_joined <-
  exec_time %>%
  unnest() %>%
  filter(time_end != time_inc) %>%
  full_join(djt_simple, by = "time_inc") %>%
  # select(event_id, time_inc, created_at, listed_title, top_category, text) %>%
  filter(!is.na(text)) %>%
  filter(time_inc <= max(exec_time$time_end)) %>%
  mutate(
    wday = wday(created_at, abbr = FALSE, week_start = 1), 
    top_category = case_when(
      !is.na(top_category) ~ paste(top_category),
      wday > 5 ~ "Weekend", 
      # between(wday, 1, 5) & hour(created_at) < 6 ~ "Early Morning (before 6am)",
      # between(wday, 1, 5) & hour(created_at) < 8 ~ "Morning (6-8 am)",
      # between(wday, 1, 5) & hour(created_at) > 17 ~ "Evening (after 5pm)",
      is.na(top_category) ~ "Unknown",
      TRUE ~ paste(top_category))
  ) %>%
  mutate(
    top_category = recode(
      top_category,
      "executive_time" = "Executive Time",
      "event"   = "Event",
      "lunch"   = "Lunch",
      "meeting" = "Meeting",
      "no_data" = "Unknown",
      "travel"  = "Travel")
  )

saveRDS(djt_joined, here::here("data", "djt_joined.rds"))
