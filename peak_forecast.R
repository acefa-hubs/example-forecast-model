library(tidyverse)

make_peak_forecasts <- function(
  case_counts, date_information
) {
  
  round_id <- first(date_information$round_id)
  
  # Filter for just this season
  case_counts_yearly <- case_counts %>% 
    filter(pathogen != "SARSCOV2")%>%
    mutate(year = floor_date(notification_date, "year")) %>%
    filter(notification_date < ymd("2025-01-01"))
  
  previous_peaks <- case_counts_yearly %>%
    group_by(location, pathogen, year) %>%
    summarise(peak_date = first(notification_date[cases == max(cases, na.rm = TRUE)]),
              peak_cases = first(cases[cases == max(cases, na.rm = TRUE)]))
  
  peak_summary <- previous_peaks %>%
    mutate(peak_log_cases = log(peak_cases),
           peak_day = as.numeric(peak_date - year)) %>% 
    group_by(location, pathogen) %>%
    summarise(mean_day = mean(peak_day),
              mean_log_cases = mean(peak_log_cases),
              sd_day = sd(peak_day),
              sd_log_cases = sd(peak_log_cases))
  
  peak_samples <- tibble(
    sample = 1:4000
  ) %>%
    expand_grid(peak_summary) %>% 
    mutate(peak_day = rnorm(n(), mean_day, sd_day),
           peak_log_cases = rnorm(n(), mean_log_cases, sd_log_cases)) %>%
    
    mutate(peak_day = pmin(365, peak_day),
           peak_day = as.integer(pmax(0, peak_day)),
           peak_cases = round(exp(peak_log_cases)))
  
  
  intermediate_data <- peak_samples %>%
    select(location, pathogen, peak_day, peak_cases, sample) %>%
    
    left_join(date_information, by = join_by(location, pathogen)) %>%
    
    mutate(
      output_type = "sample",
      output_type_id = sample
    )
  
  forecast_data <- bind_rows(
    intermediate_data %>%
      mutate(target = "peak_day_of_year") %>% 
      select(
        round_id, origin_date,
        target, location, pathogen,
        
        output_type, output_type_id,
        
        value = peak_day
      ),
    intermediate_data %>%
      mutate(target = "peak_case_incidence") %>% 
      select(
        round_id, origin_date,
        target, location, pathogen,
        
        output_type, output_type_id,
        
        value = peak_cases
      )
  ) %>%
    mutate(value = as.integer(value),
           horizon = as.integer(0))
  
  return(forecast_data)
}


