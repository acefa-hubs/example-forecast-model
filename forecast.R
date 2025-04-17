

library(tidyverse)
library(mgcv)
library(gratia)

# Read in the data we are going to forecast
case_counts <- read_csv("Combined-count-2025-04-17.csv")

# Read and extract the relevant date information
date_information <- read_csv("date-information-2025-04-17.csv") %>%
  rename(round_id = forecast_date) %>%
  
  filter(pathogen != "SARSCOV2", location != "NZ")

# Filter for just this season
case_counts_recent <- case_counts %>%
  filter(notification_date >= ymd("2025-01-01")) %>%
  
  left_join(date_information) %>% # TODO remove when data fixed
  filter(notification_date <= origin_date) %>%
  select(-c(origin_date, round_id, date_received))

# Extract the round id for this week (should be the same as in the filename)
round_id <- first(date_information$round_id)

# Plot out data
ggplot() +
  geom_line(aes(x = notification_date, y = cases),
            case_counts_recent) +
  
  geom_vline(aes(xintercept = origin_date),
             date_information,
             colour = ggokabeito::palette_okabe_ito(5)) +
  
  facet_wrap(~location * pathogen, scales = "free_y") +
  
  theme_bw()

# Modify our data to make it readable by mgcv
model_data <- case_counts_recent %>%
  filter(notification_date >= max(notification_date) - days(14)) %>% 
  mutate(t = as.numeric(notification_date - min(notification_date)),
         day_of_week = wday(notification_date),
         log_count = log(cases + 1))


model_data_split <- model_data %>%
  group_split(location, pathogen)

gam_fits <- map(
  model_data_split,
  function(x) {
    mgcv::gam(log_count ~ t + s(day_of_week, k = 3), data = x)
  }
)

gratia::draw(gam_fits[[2]])

forecasting_predictor_data_split <- date_information %>%
  select(location, pathogen, origin_date) %>%
  rowwise() %>%
  mutate(notification_date = list(origin_date + 1:28)) %>%
  unnest(notification_date) %>% 
  mutate(t = as.numeric(notification_date - min(notification_date)),
         day_of_week = wday(notification_date)) %>%
  group_split(location, pathogen)

# Produce our predictions across the posterior of each gam
forecasting_predictions <- pmap(
  list(gam_fits, forecasting_predictor_data_split),
  function(gam_fit, pred_data) {
    gratia::posterior_samples(
      gam_fit,
      n = 2000,
      data = pred_data
    ) %>%
      left_join(pred_data %>% mutate(.row = row_number()), by = join_by(.row)) %>%
      
      mutate(pred_count = round(pmin(1e6, pmax(0, exp(.response) - 1))))
  }
) %>%
  bind_rows()
  
  
  

  


# Plot our predictions
ggplot() +
  
  geom_line(aes(x = notification_date, y = cases),
            case_counts_recent) +
  
  geom_line(aes(x = notification_date, y = pred_count, group = .draw),
            alpha = 0.5, colour = ggokabeito::palette_okabe_ito(5),
            forecasting_predictions %>% filter(.draw < 5)) +
  
  geom_vline(aes(xintercept = origin_date),
             date_information,
             colour = ggokabeito::palette_okabe_ito(5)) +
  
  facet_wrap(~location * pathogen, scales = "free_y") +
  
  theme_bw()


ggplot() +
  
  ggdist::stat_ribbon(
    aes(x = notification_date, y = pred_count),
    forecasting_predictions
  ) +
  geom_line(aes(x = notification_date, y = cases),
            case_counts_recent) +
  
  geom_vline(aes(xintercept = origin_date),
             date_information,
             colour = ggokabeito::palette_okabe_ito(5)) +
  
  facet_wrap(~location * pathogen, scales = "free_y") +
  
  scale_fill_brewer() +
  
  scale_y_continuous(limits = c(0, 900)) +
  
  theme_bw()


# Reformat out predictions to match the necessary table
# format required by the hub
forecast_data <- forecasting_predictions %>%
  
  mutate(
    horizon = as.integer(notification_date - origin_date),
    target = "case_incidence", 
    origin_date = origin_date,
    round_id = round_id,
    output_type = "sample",
    output_type_id = .draw,
    value = as.integer(pred_count)
  ) %>%
  
  select(
    round_id, origin_date,
    target, horizon, location, pathogen,
    
    output_type, output_type_id,
    
    value
  )

forecast_data

# Make sure our file name matches round_id and our model name
forecast_file_name <- str_c(round_id, "-uom-testing.parquet")

# Write out our forecast_data as a parquet file
arrow::write_parquet(forecast_data, forecast_file_name)


