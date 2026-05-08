install.packages(c("httr","jsonlite","dplyr","lubridate","tidyr","purrr","readr","stringr","ggplot2","broom","reshape2","patchwork","tidyverse","scales","ggridges"))

## httr: fetch weather data from the Open-Meteo API.
## readr: read AQHI CSV files from the Government of Canada.
## jsonlite: parse JSON responses from the API.
## dplyr: clean, filter, group, and summarize data.
## lubridate: handle date-time parsing and timezone adjustments.
## tidyr: reshape data (pivoting, dropping NA, creating buckets).
## purrr: loop API calls for all cities.
## stringr: light string manipulation when needed.
## ggplot2: create all visualizations for the analysis.
## broom: tidy outputs from t-tests and linear models.
## reshape2: reshape data frames for certain plots.
## patchwork: combine multiple plots into one layout.
## tidyverse: collection of core data-wrangling tools.
## scales: format axis labels, breaks, and color scaling.
## ggridges: create ridgeline density plots for AQHI distributions.
library(httr)
library(readr)
library(jsonlite)
library(dplyr)
library(lubridate)
library(tidyr)
library(purrr)
library(stringr)
library(ggplot2)
library(broom)
library(reshape2)
library(patchwork)
library(tidyverse)
library(scales)
library(ggridges)


## This section retrieves hourly weather data for five Canadian cities using the 
## Open-Meteo API. A table of city names and coordinates is created, and a custom 
## function (get_city_weather) sends an API request for temperature, humidity, 
## precipitation, and wind speed within a defined UTC date range. The results for 
## all cities are combined into one dataset (weather_all), converted into a tidy 
## tibble, and inspected with a preview of the first few rows per city. 
cities <- data.frame(
  city = c("Calgary", "Vancouver", "Toronto", "Victoria", "Ottawa"),
  lat  = c(51.05, 49.28, 43.65, 48.43, 45.42),
  lon  = c(-114.07, -123.12, -79.38, -123.37, -75.70)
)

start_utc <- ymd_hms("2025-10-20 01:00:00", tz = "UTC")
end_utc   <- ymd_hms("2025-10-27 00:00:00", tz = "UTC")

get_city_weather <- function(city, lat, lon){
  w <- GET("https://api.open-meteo.com/v1/forecast",
           query = list(
             latitude  = lat,
             longitude = lon,
             hourly    = "temperature_2m,relative_humidity_2m,precipitation,wind_speed_10m",
             start_date = "2025-10-20",
             end_date   = "2025-10-27",
             timezone   = "UTC"
           ))
  fromJSON(content(w, as="text"))$hourly |>
    as_tibble() |>
    mutate(datetime = ymd_hm(time), city = city) |>
    select(datetime, city, temperature_2m, relative_humidity_2m, precipitation, wind_speed_10m)
}

weather_all <- pmap_dfr(cities, get_city_weather) |>
  filter(datetime >= start_utc, datetime <= end_utc)

glimpse(weather_all)

weather_preview <- weather_all |>
  group_by(city) |>
  slice_head(n = 3)


## This section imports and prepares AQHI data from the Government of Canada CSV
## files. A station-to-city mapping is defined so each AQHI station is assigned to 
## its correct city. The read_aqhi_file() function loads each CSV, identifies the 
## date and hour columns automatically, reshapes the data into long format, builds 
## a proper UTC datetime column, converts AQHI values to numeric, and keeps only 
## the selected monitoring stations. Three city-level AQHI datasets are then read, 
## previewed, combined, and averaged by city and timestamp. Finally, the AQHI data 
## are merged with the weather dataset using an inner join to ensure complete rows, 
## and all remaining missing values across key variables are removed.
station_map <- c(
  "IAKID" = "Calgary",
  "JAZBU" = "Vancouver",
  "FDQBU" = "Toronto",
  "FEVNT" = "Ottawa",
  "JCLMX" = "Victoria"
)

##
getwd()
setwd("C:/Users/dchie/Downloads")

station_map <- c(
  "IAKID" = "Calgary",
  "JAZBU" = "Vancouver",
  "FDQBU" = "Toronto",
  "FEVNT" = "Ottawa",
  "JCLMX" = "Victoria"
)

read_aqhi_file <- function(file_path, stations_keep = names(station_map)) {
  df <- read_csv(file_path, show_col_types = FALSE)
  date_col <- names(df)[str_detect(names(df), regex("^date$", ignore_case = TRUE))][1]
  hour_col <- names(df)[str_detect(names(df), regex("hour", ignore_case = TRUE))][1]
  
  df |>
    pivot_longer(-c(all_of(date_col), all_of(hour_col)),
                 names_to = "station", values_to = "AQHI") |>
    mutate(
      datetime = ymd_hms(paste(.data[[date_col]],
                               sprintf("%02d:00:00", as.integer(.data[[hour_col]]))),
                         tz = "UTC"),
      AQHI = suppressWarnings(as.numeric(AQHI))
    ) |>
    filter(station %in% stations_keep) |>
    mutate(city = unname(station_map[station])) |>
    select(datetime, city, station, AQHI) |>
    arrange(city, datetime)
}

files <- list(
  Calgary   = "C:/Users/dchie/Downloads/2025102700_AQHI_Calgary_SiteObs.csv",
  Vancouver = "C:/Users/dchie/Downloads/2025102700_AQHI_Vancouver+victoria_SiteObs.csv",
  Ontario   = "C:/Users/dchie/Downloads/2025102700_AQHI_ottawa+toronto_SiteObs.csv"
)

aqhi_calgary   <- read_aqhi_file(files$Calgary,   "IAKID")
aqhi_vancouver <- read_aqhi_file(files$Vancouver, c("JAZBU", "JCLMX"))
aqhi_ontario   <- read_aqhi_file(files$Ontario,   c("FDQBU", "FEVNT"))

###
aqhi_preview <- aqhi_vancouver |>
  group_by(city) |>
  slice_head(n = 3)
###

aqhi_all <- bind_rows(aqhi_calgary, aqhi_vancouver, aqhi_ontario) |>
  group_by(city, datetime) |>
  summarise(AQHI = mean(AQHI, na.rm = TRUE), .groups = "drop")

merged_inner <- weather_all |> inner_join(aqhi_all, by = c("city","datetime"))

merged_inner <- merged_inner %>% drop_na(
  AQHI,
  temperature_2m,
  relative_humidity_2m,
  wind_speed_10m,
  precipitation
)

###
setwd("C:/Users/dchie/Downloads")
write_csv(merged_inner, "merged_inner.csv")
###
merged_preview <- merged_inner |>
  arrange(city, datetime) |>
  group_by(city) |>
  slice_head(n = 3) |>
  ungroup()
###

## This section builds and analyzes the regression models for Question 1. 
## First, a full linear model is estimated to examine how temperature, humidity, 
## wind speed, and precipitation relate to AQHI. A cleaned analysis dataset is 
## then constructed, followed by a correlation matrix to explore initial 
## relationships between variables. Two regression models are fitted: a baseline 
## model and one including city fixed effects. To compare effect sizes fairly, 
## all predictors and AQHI are standardized, and a standardized regression model 
## (m_std) is run. The final 'imp' table extracts and labels the standardized 
## beta coefficients, which are used to identify the strongest weather influence 
## on AQHI.
#### Q1
##
model_q1 <- lm(
  AQHI ~ temperature_2m + relative_humidity_2m + wind_speed_10m + precipitation,
  data = merged_inner
)
summary(model_q1)

##

# Build analysis dataset df from merged_inner
df <- merged_inner %>%
  select(datetime, city, AQHI,
         temperature_2m, relative_humidity_2m,
         wind_speed_10m, precipitation) %>%
  drop_na(AQHI)

# ---- Correlation matrix data ----
num_vars <- c("AQHI","temperature_2m","relative_humidity_2m",
              "wind_speed_10m","precipitation")

corr_mat <- cor(df[, num_vars], use = "pairwise.complete.obs")

# ---- Regression models for standardized betas ----
# baseline model without city fixed effects
m1 <- lm(AQHI ~ temperature_2m +
           relative_humidity_2m +
           wind_speed_10m +
           precipitation,
         data = df)

# model with city fixed effects
m2 <- lm(AQHI ~ temperature_2m +
           relative_humidity_2m +
           wind_speed_10m +
           precipitation +
           city,
         data = df)

# standardize predictors + AQHI so we can compare effect sizes
df_z <- df %>%
  mutate(across(c(AQHI,
                  temperature_2m,
                  relative_humidity_2m,
                  wind_speed_10m,
                  precipitation),
                ~ as.numeric(scale(.)),
                .names = "z_{col}"))

m_std <- lm(
  z_AQHI ~ z_temperature_2m +
    z_relative_humidity_2m +
    z_wind_speed_10m +
    z_precipitation +
    city,
  data = df_z
)

imp <- broom::tidy(m_std) %>%
  filter(grepl("^z_", term)) %>%
  mutate(
    var = sub("^z_", "", term),
    estimate = estimate
  )

# ---- Plots ----
## These four plots visualize the relationship between AQHI and each weather variable.
## The first chart (AQHI vs Temperature) uses scatter points and a linear trend line 
## to show how AQHI changes with temperature across different cities. The same plotting 
## structure is reused for wind speed, humidity, and precipitation—only the x-variable 
## and axis labels change. Each plot is saved as a high-resolution PNG for use in the report.
# AQHI vs Temperature
p1 <- ggplot(df, aes(x = temperature_2m, y = AQHI, color = city)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "AQHI vs Temperature by City",
    x = "Temperature (°C)",
    y = "AQHI"
  ) +
  theme_minimal(base_size = 12)

print(p1)
###
ggsave(
  filename = "aqhi_temperature_by_city.png",
  plot     = p1,
  width    = 10,   
  height   = 6,
  dpi      = 300   
)
###

# AQHI vs Wind Speed
p2 <- ggplot(df, aes(x = wind_speed_10m, y = AQHI, color = city)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "AQHI vs Wind Speed by City",
    x = "Wind Speed (m/s)",
    y = "AQHI"
  ) +
  theme_minimal(base_size = 12)

print(p2)

###
ggsave(
filename = "aqhi_windspeed_by_city.png",
plot     = p2,
width    = 10,   
height   = 6,
dpi      = 300   
)
##

# AQHI vs Relative Humidity
p3 <- ggplot(df, aes(x = relative_humidity_2m, y = AQHI, color = city)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "AQHI vs Relative Humidity by City",
    x = "Relative Humidity (%)",
    y = "AQHI"
  ) +
  theme_minimal(base_size = 12)

print(p3)

##
ggsave(
  filename = "aqhi_humidity_by_city.png",
  plot     = p3,
  width    = 10,   
  height   = 6,
  dpi      = 300   
)
##

# AQHI vs Precipitation
p4 <- ggplot(df, aes(x = precipitation, y = AQHI, color = city)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "AQHI vs Precipitation by City",
    x = "Precipitation (mm)",
    y = "AQHI"
  ) +
  theme_minimal(base_size = 12)

print(p4)
##
ggsave(
  filename = "aqhi_Precipitation_by_city.png",
  plot     = p4,
  width    = 10,   
  height   = 6,
  dpi      = 300   
)
##
## Q1 Follow-up: This section identifies which weather variable has the strongest
## influence on AQHI. First, I compute a correlation matrix to show pairwise 
## relationships between AQHI and all weather factors. Then I run a standardized 
## regression (beta coefficients) so the effect sizes are comparable on the same scale. 
## The correlation heatmap (p_corr) and standardized beta plot (p_beta) are combined 
## to visually compare the strength of each predictor. 
##
## Finally, I include an additional faceted plot showing Temperature vs AQHI by city 
## to verify whether the strong temperature effect appears consistently across 
## different locations.
###Q1 follow up

df <- merged_inner %>%
  select(datetime, city, AQHI,
         temperature_2m, relative_humidity_2m,
         wind_speed_10m, precipitation) %>%
  drop_na(AQHI)

# --- (1) Correlation matrix ---
num_vars <- c("AQHI","temperature_2m","relative_humidity_2m",
              "wind_speed_10m","precipitation")

corr_mat <- cor(df[, num_vars], use = "pairwise.complete.obs")

p_corr <- melt(corr_mat) %>%
  ggplot(aes(Var1, Var2, fill = value)) +
  geom_tile() +
  geom_text(aes(label = round(value, 2)),
            color = "black", size = 3.5, fontface = "bold") +
  scale_fill_gradientn(
    colours = c("#2c7bb6", "#ffffbf", "#d7191c"),
    limits = c(-1, 1),
    breaks = c(-0.5, 0, 0.5, 1),
    labels = c("-0.5", "0", "0.5", "1.0")
  ) +
  labs(
    title = "Correlation between AQHI and Weather Variables",
    x = NULL, y = NULL, fill = "Correlation"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 30, hjust = 1),
    panel.grid = element_blank()
  )

# --- ---
p_beta <- ggplot(
  imp,
  aes(x = reorder(var, estimate), y = estimate)
) +
  geom_col(fill = "#0072B2") +
  geom_hline(yintercept = 0, linewidth = 0.4, color = "gray30") +
  coord_flip() +
  labs(
    title = "Standardized Effects of Weather Variables on AQHI",
    x = NULL,
    y = "Standardized Beta (Effect Size)"
  ) +
  theme_minimal(base_size = 12) +
  theme(panel.grid.major.y = element_blank())

# --- ---
final_followup <- p_corr / p_beta +
  plot_annotation(
    title = "Follow-up Question 1 – Which Weather Factor Influences AQHI Most?"
  )

print(final_followup)
###
ggsave(
  filename = "followup_q1_aqhi_weather.png",  
  plot     = final_followup,                   
  width    = 10,                              
  height   = 6,                               
  dpi      = 300                              
)
###
Temperature_AQHI_City <- ggplot(df, aes(x = temperature_2m, y = AQHI)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  facet_wrap(~city) +
  labs(title = "Temperature vs AQHI by City",
       x = "Temperature (°C)", y = "AQHI") +
  theme_minimal(base_size = 13)

ggsave(
  filename = "Temperature_AQHI_City.png",  
  plot     = Temperature_AQHI_City,                   
  width    = 10,                              
  height   = 6,                               
  dpi      = 300                              
)
## Question 2: This section compares AQHI between coastal and inland cities.
## First, I map each city to a region type ("Coastal" or "Inland") and add it to the dataset.
## I then compute summary statistics (mean, median, sd, sample size) for each region type
## to see the basic AQHI differences. 
##
## After that, I run an independent t-test (Welch) to statistically test whether
## the mean AQHI differs between coastal and inland regions. The tidy output makes it
## easier to extract key values (estimate, p-value, confidence interval) for interpretation.
####Question 2
city_type_map <- c(
  "Vancouver" = "Coastal",
  "Victoria"  = "Coastal",
  "Calgary"   = "Inland",
  "Toronto"   = "Inland",
  "Ottawa"    = "Inland"
)

df2 <- df %>%
  mutate(
    region_type = city_type_map[city],
    region_type = factor(region_type, levels = c("Coastal", "Inland"))
  ) %>%
  drop_na(AQHI)

aqhi_summary <- df2 %>%
  group_by(region_type) %>%
  summarise(
    mean_AQHI   = mean(AQHI, na.rm = TRUE),
    median_AQHI = median(AQHI, na.rm = TRUE),
    sd_AQHI     = sd(AQHI, na.rm = TRUE),
    n           = n(),
    .groups = "drop"
  )

print(aqhi_summary)

t_test_result <- t.test(
  AQHI ~ region_type,
  data = df2,
  var.equal = FALSE
)

print(t_test_result)
tidy_t <- broom::tidy(t_test_result)
print(tidy_t)

## Q2 Plots: These visualizations compare AQHI distributions between coastal and inland cities.
## 1) A density plot overlays the two regions to show differences in overall AQHI shape.
## 2) A ridgeline plot shows the distribution for each individual city, letting us see
##    how coastal vs inland patterns vary across locations.
## 3) A histogram provides another view of the frequency distribution between the two groups.
## Together, these plots help verify whether inland cities consistently show higher AQHI levels
## and provide visual support for the t-test results.
# ---- Plots ----
p <- ggplot(df2, aes(x = AQHI, fill = region_type)) +
  geom_density(alpha = 0.5) +
  scale_fill_manual(values = c("Coastal" = "#4C9BD3", "Inland" = "#E3B44C")) +
  labs(
    title = "AQHI Density: Coastal vs Inland Cities",
    x = "AQHI",
    y = "Density",
    fill = "Region Type"
  ) +
  theme_minimal(base_size = 13)
p
###
ggsave(
  filename = "aqhi_density_coastal_vs_inland.png",
  plot = p,
  width = 10,       
  height = 6,       
  dpi = 300         
)

###
a <- ggplot(merged_inner, aes(x = AQHI, y = city, fill = city)) +
  geom_density_ridges(alpha = 0.7, scale = 1.2) +
  labs(title = "Distribution of AQHI Across Cities (Coastal vs Inland)",
       x = "AQHI", y = "City") +
  theme_ridges(font_size = 12, grid = TRUE) +
  theme(legend.position = "none")
a
ggsave(
  filename = "aqhi_distribution of .png",
  plot = a,
  width = 10,       
  height = 6,       
  dpi = 300         
)
###
p_hist <- ggplot(df2, aes(x = AQHI, fill = region_type)) +
  geom_histogram(alpha = 0.5, position = "identity", bins = 30) +
  scale_fill_manual(values = c("Coastal" = "#4C9BD3", "Inland" = "#E3B44C")) +
  labs(
    title = "Histogram of AQHI: Coastal vs Inland Cities",
    x = "AQHI",
    y = "Count",
    fill = "Region Type"
  ) +
  theme_minimal(base_size = 13)

p_hist
ggsave(
  filename = "p_hist .png",
  plot = p_hist,
  width = 10,       
  height = 6,       
  dpi = 300         
)
###
# Question 2 - Regression Model 2A 
m2a <- lm(AQHI ~ region_type, data = df2)

summary(m2a)
broom::tidy(m2a)


## Q2 Follow-up: This section compares AQHI across individual cities.
## We compute mean AQHI, standard deviation, standard error, and 95% confidence intervals
## for each city, then rank cities from highest to lowest AQHI.
## The bar chart (with CI bars) visualizes which cities have higher average AQHI and
## whether differences are statistically meaningful based on interval overlap.
###Q2 follow up
###
sum_city <- merged_inner %>%
  group_by(city) %>%
  summarise(
    n = n(),
    mean_AQHI = mean(AQHI, na.rm = TRUE),
    sd  = sd(AQHI, na.rm = TRUE),
    se  = sd/sqrt(n),
    ci_low  = mean_AQHI - 1.96*se,
    ci_high = mean_AQHI + 1.96*se,
    .groups = "drop"
  ) %>%
  arrange(desc(mean_AQHI)) %>%
  mutate(city = fct_reorder(city, mean_AQHI))

b <- ggplot(sum_city, aes(x = city, y = mean_AQHI, fill = city)) +
  geom_col(width = 0.6, show.legend = FALSE) +
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high), width = 0.15) +
  coord_flip() +
  labs(title = "Average AQHI by City (with 95% CI)",
       x = NULL, y = "Mean AQHI") +
  theme_minimal(base_size = 13)

b

ggsave(
  filename = "aqhi_95 CI .png",
  plot = b,
  width = 10,       
  height = 6,       
  dpi = 300         
)
#### regression model Q2 follow up
m_city <- lm(AQHI ~ city, data = merged_inner)

summary(m_city)

broom::tidy(m_city)
###





## Q3: Compare AQHI between weekdays and weekends.
## This section creates a dataset with a weekday/weekend indicator,
## computes mean AQHI and 95% confidence intervals for each city,
## and performs two types of t-tests:
## (1) an overall test comparing all weekdays vs weekends across all cities,
## (2) separate t-tests within each city to detect location-specific differences.
#### Question 3

df3 <- merged_inner %>%
  select(datetime, city, AQHI) %>%
  tidyr::drop_na(AQHI) %>%
  mutate(
    day_type = if_else(wday(datetime, week_start = 1) %in% 6:7, "Weekend", "Weekday"),
    day_type = factor(day_type, levels = c("Weekday", "Weekend"))
  )

# ---- Descriptive statistics + 95% Confidence Intervals ----
sum3 <- df3 %>%
  group_by(city, day_type) %>%
  summarise(
    n = n(),
    mean_AQHI = mean(AQHI),
    sd = sd(AQHI),
    se = sd / sqrt(n),
    ci_low = mean_AQHI - 1.96 * se,
    ci_high = mean_AQHI + 1.96 * se,
    .groups = "drop"
  )

print(sum3)


tt_overall <- t.test(AQHI ~ day_type, data = df3)
print(tt_overall %>% tidy())


tt_city <- df3 %>%
  group_by(city) %>%
  do(tidy(t.test(AQHI ~ day_type, data = .))) %>%
  ungroup()

print(tt_city)
## Q3 plots:
## - Boxplot: displays the distribution of AQHI for Weekday vs Weekend inside each city.
## - 95% CI Bar Chart: visualizes city-level mean AQHI and uncertainty (CI), making
##   weekday–weekend differences easier to compare.
# ---- Boxplot by city ----
p_box <- ggplot(df3, aes(day_type, AQHI, fill = day_type)) +
  geom_boxplot(alpha = 0.75, width = 0.65, outlier.alpha = 0.25) +
  facet_wrap(~ city, ncol = 3) +
  scale_fill_manual(values = c("#0072B2", "#E69F00")) +
  labs(title = "AQHI: Weekday vs Weekend (by City)", x = NULL, y = "AQHI") +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none")

print(p_box)
ggsave(
  filename = "aqhi_weekvsday .png",
  plot = p_box,
  width = 10,       
  height = 6,       
  dpi = 300         
)

# ----  CI 95% ----
p_bar <- ggplot(sum3, aes(city, mean_AQHI, fill = day_type)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.65) +
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high),
                position = position_dodge(width = 0.7), width = 0.15) +
  scale_fill_manual(values = c("#0072B2", "#E69F00"), name = "Day Type") +
  labs(title = "Average AQHI with 95% CI: Weekday vs Weekend",
       x = "City", y = "Mean AQHI") +
  theme_minimal(base_size = 13)

print(p_bar)
ggsave(
  filename = "aqhi_weekvsdaybar .png",
  plot = p_bar,
  width = 10,       
  height = 6,       
  dpi = 300         
)
### regression model Q3 
m3_overall <- lm(AQHI ~ day_type, data = df3)
summary(m3_overall)

tidy_m3_overall <- broom::tidy(m3_overall)
print(tidy_m3_overall)
###
## Q3 follow-up: explore AQHI by time of day and day type
## - Build df_q3fu with a time_of_day factor (Morning, Afternoon, Evening, Night)
##   and a day_type factor (Weekday / Weekend) from the UTC datetime.
## - Compute mean AQHI and 95% confidence intervals for each City × DayType × TimeOfDay.
## - Plot a line chart with error bars to compare weekday vs weekend patterns,
##   and a difference bar chart showing Weekend − Weekday AQHI by time of day.
## - Create a ridgeline density plot for commute-related periods, plus violin and
##   jitter plots to show the full weekday–weekend distribution of AQHI.
## - Use a circular “clock” chart to summarize how average AQHI changes across
##   the four time-of-day buckets for weekdays and weekends.
#### Q3 follow up 

#Build dataset with day_type + time_of_day from UTC 'datetime'
df_q3fu <- merged_inner %>%
  mutate(
    hour_utc = hour(datetime),
    day_type = if_else(wday(datetime, week_start = 1) %in% 6:7, "Weekend", "Weekday"),
    day_type = factor(day_type, levels = c("Weekday", "Weekend")),
    time_of_day = case_when(
      hour_utc >= 5  & hour_utc < 12 ~ "Morning",
      hour_utc >= 12 & hour_utc < 17 ~ "Afternoon",
      hour_utc >= 17 & hour_utc < 21 ~ "Evening",
      TRUE                           ~ "Night"
    )
  ) %>%
  select(city, AQHI, day_type, time_of_day) %>%
  tidyr::drop_na(AQHI)

# Summary stats by City x DayType x TimeOfDay (mean, SE, 95% CI)
sum_q3fu <- df_q3fu %>%
  group_by(city, day_type, time_of_day) %>%
  summarise(
    n = n(),
    mean_AQHI = mean(AQHI),
    sd = sd(AQHI),
    se = sd / sqrt(n),
    ci_low = mean_AQHI - 1.96 * se,
    ci_high = mean_AQHI + 1.96 * se,
    .groups = "drop"
  )

print(sum_q3fu)

# Line + error bars (Weekday vs Weekend) by time_of_day, faceted by city
p_line <- ggplot(
  sum_q3fu,
  aes(x = time_of_day, y = mean_AQHI, group = day_type, color = day_type)
) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high), width = 0.15) +
  facet_wrap(~ city, ncol = 3) +
  scale_color_manual(values = c("Weekday" = "#0072B2", "Weekend" = "#E69F00")) +
  labs(
    title = "AQHI by Time of Day: Weekday vs Weekend (UTC buckets)",
    x = "Time of Day", y = "Mean AQHI", color = "Day Type"
  ) +
  theme_minimal(base_size = 13)
print(p_line)
ggsave(
  filename = "aqhi_weekvsdayline .png",
  plot = p_line,
  width = 10,       
  height = 6,       
  dpi = 300         
)


#Difference (Weekend - Weekday) by time_of_day for each city
diff_q3fu <- sum_q3fu %>%
  select(city, day_type, time_of_day, mean_AQHI) %>%
  pivot_wider(names_from = day_type, values_from = mean_AQHI) %>%
  mutate(diff_Weekend_minus_Weekday = Weekend - Weekday)

p_diff <- ggplot(
  diff_q3fu,
  aes(x = time_of_day, y = diff_Weekend_minus_Weekday, fill = diff_Weekend_minus_Weekday > 0)
) +
  geom_col(alpha = 0.9) +
  geom_hline(yintercept = 0, linewidth = 0.4, color = "gray35") +
  facet_wrap(~ city, ncol = 3) +
  scale_fill_manual(values = c("#0072B2", "#E69F00"),
                    labels = c("Weekend lower", "Weekend higher")) +
  labs(
    title = "Weekend − Weekday AQHI by Time of Day (UTC buckets)",
    x = "Time of Day", y = "Difference (Weekend − Weekday)", fill = NULL
  ) +
  theme_minimal(base_size = 13)
print(p_diff)
ggsave(
  filename = "aqhi_weekvsdaydiff .png",
  plot = p_diff,
  width = 10,       
  height = 6,       
  dpi = 300         
)


# Commute-focused ridge plot (UTC-based buckets)
df_ridge <- merged_inner %>%
  mutate(
    hour_utc = hour(datetime),
    DayType = ifelse(wday(datetime, week_start = 1) %in% c(6,7), "Weekend", "Weekday"),
    period = case_when(
      hour_utc >= 6  & hour_utc < 9    ~ "Morning Commute (6–9 UTC)",
      hour_utc >= 17 & hour_utc < 19   ~ "Evening Commute (17–19 UTC)",
      hour_utc %in% c(23, 0, 1, 2)     ~ "Late Night (23–02 UTC)",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(period))

p_timeplot <- ggplot(df_ridge, aes(x = AQHI, y = period, fill = DayType)) +
  ggridges::geom_density_ridges(alpha = 0.6, scale = 1.2, rel_min_height = 0.01, color = "white") +
  scale_fill_manual(values = c("Weekday" = "#0072B2", "Weekend" = "#E69F00")) +
  labs(
    title = "AQHI Distribution by Time Period and Day Type (UTC)",
    subtitle = "Comparing patterns between Weekdays and Weekends",
    x = "AQHI", y = "Time Period", fill = "Day Type"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold"),
    axis.text.y = element_text(face = "bold")
  )
p_timeplot
ggsave(
  filename = "aqhi_timeplot .png",
  plot = p_timeplot,
  width = 10,       
  height = 6,       
  dpi = 300         
)

###
p_violin <- ggplot(df3, aes(day_type, AQHI, fill = day_type)) +
  geom_violin(trim = FALSE, alpha = 0.6) +
  geom_boxplot(width = 0.15, alpha = 0.9, outlier.alpha = 0.2) +
  scale_fill_manual(values = c("#0072B2", "#E69F00")) +
  labs(title = "Violin Plot: AQHI on Weekday vs Weekend",
       x = "", y = "AQHI") +
  theme_minimal(base_size = 13)
p_violin 
ggsave(
  filename = "aqhi_weekvsdayviolin .png",
  plot = p_violin,
  width = 10,       
  height = 6,       
  dpi = 300         
)
###

p_scalelot <- ggplot(df3, aes(day_type, AQHI, color = day_type)) +
  geom_jitter(width = 0.12, alpha = 0.4) +
  scale_color_manual(values = c("#0072B2", "#E69F00")) +
  labs(title = "AQHI Individual Points: Weekday vs Weekend") +
  theme_minimal(base_size = 13)
p_scalelot
ggsave(
  filename = "aqhi_weekvsdayscalelot .png",
  plot = p_scalelot,
  width = 10,       
  height = 6,       
  dpi = 300         
)
##
p_clock <-ggplot(sum_q3fu, aes(x = time_of_day, y = mean_AQHI, fill = day_type)) +
  geom_col(position = "dodge") +
  coord_polar() +
  scale_fill_manual(values = c("#0072B2", "#E69F00")) +
  labs(title = "Circular Chart: AQHI by Time of Day") +
  theme_minimal(base_size = 13)
p_clock
ggsave(
  filename = "aqhi_weekvsdayclock .png",
  plot = p_clock,
  width = 10,       
  height = 6,       
  dpi = 300         
)

##



