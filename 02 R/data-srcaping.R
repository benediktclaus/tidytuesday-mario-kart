library(tidyverse)
library(rvest)
library(lubridate)
library(hms)


# Helper function to download WR tables
download_wr_table <- function(url) {
  read_html(url) %>%
    html_element(xpath = "//*[@id=\"main\"]/table[3]") %>%
    html_table() %>%
    janitor::clean_names() %>%
    select(-nation)
}




# Track Data --------------------------------------------------------------
# All tracks
tracks <- tibble(
  track = c("Luigi Raceway", "Moo Moo Farm", "Koopa Troopa Beach", "Kalimari Desert", "Toad's Turnpike",
            "Frappe Snowland", "Choco Mountain", "Mario Raceway", "Wario Stadium", "Sherbet Land", "Royal Raceway",
            "Bowser's Castle", "D.K.'s Jungle Parkway", "Yoshi Valley", "Banshee Boardwalk", "Rainbow Road"),
  shortcut = c(TRUE, FALSE, FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, TRUE, TRUE, FALSE, TRUE)
)


# Create URLs for all track-laptype-shortcut combination
urls <- tracks %>%
  mutate(
    url_track        = str_replace_all(track, " ", "+"),
    url_track        = str_replace(url_track, "\'", "%27"),
    threelap_nosc    = str_glue("https://mkwrs.com/mk64/display.php?track={ url_track }"),
    singlelap_nosc   = str_glue("{ threelap_nosc }&f=1"),
    threelap_sc      = if_else(shortcut, str_glue("{ threelap_nosc }&m=1"), NA_character_),
    singlelap_sc     = if_else(shortcut, str_glue("{ threelap_nosc }&f=1"), NA_character_)
  ) %>%
  select(-(shortcut:url_track)) %>%
  pivot_longer(
    cols = -track,
    names_to = c("type", "shortcut"),
    values_to = "url",
    names_pattern = "^(.*)_(.*)"
  ) %>%
  na.omit()


# Download all tables from mkwrs.com
downloads <- urls %>%
  mutate(
    wr_tables = map(url, download_wr_table)
  )


# Preliminary cleaning of the raw data, i.e., unnest records tables, rename some
# variables for clarity and recode factors
raw_data <- downloaded %>% 
  select(-url) %>% 
  mutate(
    wr_tables = map(wr_tables, ~ mutate(., duration = as.character(duration)))
  ) %>% 
  unnest(wr_tables) %>% 
  select(-ntsc_time) %>% 
  rename(unparsed_date = date, system_played = system, time = pal_time, record_duration = duration) %>% 
  mutate(
    type            = fct_recode(type, "Three Lap" = "threelap", "Single Lap" = "singlelap"),
    shortcut        = fct_recode(shortcut, "No" = "nosc", "Yes" = "sc"),
    record_duration = if_else(record_duration == "<1", "0", record_duration),
    record_duration = as.numeric(record_duration)
  )


# Impute record date based on the duration of the previous record. First, the
# date is parsed with lubdridate, which should throw 175 parsing failures.
with_dates <- raw_data %>% 
  mutate(
    date = as_date(unparsed_date)
  ) %>% 
  group_by(track, type, shortcut)


while (any(is.na(with_dates$date))) {
  with_dates <- with_dates %>% 
    mutate(
      previous_record = date - days(lag(record_duration)),
      date = if_else(is.na(date), lead(previous_record), date)
    ) %>% 
    select(-previous_record)
}


# Extract time in seconds and save file
with_dates %>% 
  ungroup() %>% 
  select(-unparsed_date) %>% 
  separate(time, c("minutes", "seconds", "millisecs"), sep = "['\\\"]") %>% 
  mutate(
    millisecs = if_else(millisecs == "xx", str_glue("990"), str_glue("{ millisecs }0")),
    across(minutes:millisecs, as.numeric),
    time_period = minutes(minutes) + seconds(seconds) + milliseconds(millisecs),
    time        = seconds(time_period),
    time        = as.numeric(time)
  ) %>% 
  select(-(minutes:millisecs)) %>% 
  relocate(record_duration, .after = time) %>% 
  write_rds("01 Data/world-records.rds")




# Driver Data -------------------------------------------------------------
# Get driver records
driver_table <- read_html("https://mkwrs.com/mk64/rankings.php") %>% 
  html_element(css = "#main > table") %>% 
  html_table() %>% 
  select(-Nation)


# Get raw table for extraction of player's nation
raw_table <- read_html("https://mkwrs.com/mk64/rankings.php") %>% 
  html_element(xpath = "/html/body/div[1]/div[3]/div/table") 


# Get individual players
players <- raw_table %>% 
  html_elements("a") %>% 
  html_text()


# Get nations
nations <- raw_table %>% 
  html_elements("img") %>% 
  html_attr("title")


# Stitch both together
nation_table <- tibble(
  player = players,
  nation = nations
) %>% 
  mutate(
    nation = na_if(nation, "Unknown")
  )


# Join to data frame, fix years, and save
driver_table %>% 
  janitor::clean_names() %>% 
  rename(position = 1) %>% 
  pivot_longer(
    cols = -(position:total),
    names_to = "year",
    values_to = "records"
  ) %>% 
  mutate(
    year = if_else(str_detect(year, "x9\\d"), str_replace(year, "x(\\d{2})", "19\\1"), str_replace(year, "x(\\d{2})", "20\\1")),
    year = as.numeric(year)
  ) %>% 
  left_join(nation_table, by = "player") %>%
  write_rds("01 Data/drivers.rds")
