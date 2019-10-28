library(tidyverse)
library(blscrapeR)

data <- read_csv("retired_hurricanes.csv")

text_to_number <- function (x) {
  result <- as.numeric(x)
  if(is.na(result)){
    text <- str_remove(x, "\\$")
    text <- gsub("million", "*1e6", text, ignore.case = T)
    text <- gsub("billion", "*1e9", text, ignore.case = T)
    result <- eval(parse(text = text))
  } 
  return(result)
}

data_clean <- 
  data %>%
  janitor::clean_names() %>%
  mutate(peak_classification = ifelse(peak_classification == "Tropical storm", 0, peak_classification),
         peak_classification = parse_number(peak_classification)) %>%
  separate(wind_speeds, into = c("wind_speed_mph", "wind_speed_kmh"), sep = "\\(") %>%
  separate(pressure, into = c("pressure_hPa", "pressure_inHg"), sep = "\\(") %>%
  mutate(wind_speed_mph = parse_number(wind_speed_mph), 
         wind_speed_kmh = parse_number(wind_speed_kmh),
         pressure_hPa = parse_number(pressure_hPa),
         pressure_inHg = parse_number(pressure_inHg)) %>%
  group_by(name) %>%
  mutate(damage = text_to_number(damage)) %>%
  ungroup() %>%
  separate(dates_active, into = c("time", "year"), sep = ",") %>%
  separate(time, into = c("start_date", "end_date"), sep = "–") %>%
  separate(start_date, into = c("start_month", "start_day"), sep = " ") %>%
  mutate(end_date = trimws(end_date)) %>%
  separate(end_date, into = c("end_month", "end_day"), sep = " ") %>%
  mutate(end_day_copy = end_day, 
         end_day = ifelse(is.na(end_day), end_month, end_day),
         end_month = ifelse(is.na(end_day_copy), start_month, end_month)) %>%
  select(-end_day_copy) %>%
  mutate(start_date = paste(paste(start_month, start_day, sep = " "), year, sep = ","),
         end_date = paste(paste(end_month, end_day, sep = " "), year, sep = ","),
         start_date = lubridate::mdy(start_date),
         end_date = lubridate::mdy(end_date)) %>%
  mutate(year = as.numeric(year)) %>%
  group_by(name) %>%
  mutate(adj_factor = (inflation_adjust(year)$adj_value[73])) %>%
  ungroup() %>%
  mutate(damage_inflation_adjusted = damage * adj_factor,
         name_date = paste(name, start_date, sep = "_"))

write_excel_csv(data_clean, "retired_hurricanes_clean.csv")
