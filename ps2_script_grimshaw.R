library(tidyquant)
library(tidyverse)

# unemp_raw <- tq_get(
#  c("UNRATE", "ORUR", "CAUR", "TXUR", "NYUR"),
#  get = "economic.data",
#  from = "1976-01-01"
#)

# head(unemp_raw)

# dir.create("data", showWarnings = FALSE)

# write_rds(unemp_raw, "data/unemp_raw.rds")

unemp_raw = read_rds("data/unemp_raw.rds")

unique(unemp_raw$symbol)

unemp <- unemp_raw |>
  rename(unemployment_rate = price) |>
  mutate(
    region = case_when(
      symbol == "UNRATE" ~ "United States",
      symbol == "ORUR" ~ "Oregon",
      symbol == "CAUR" ~ "California",
      symbol == "TXUR" ~ "Texas",
      symbol == "NYUR" ~ "New York"
    )
  )
head(unemp)

unemp <- unemp |>
  arrange(region, date) |>
  group_by(region) |>
  mutate(
    unemployment_rate = replace(
      unemployment_rate,
      date == as.Date("2025-10-01"),
      unemployment_rate[date == as.Date("2025-09-01")]
    )
  ) |>
  ungroup()

unemp |>
  filter(date %in%  as.Date(c("2025-09-01", "2025-10-01"))) |>
  select(region, date, unemployment_rate)

unemp <- unemp |>
  mutate(
    unemployment_rate = replace(
      unemployment_rate,
      date == as.Date("2025-10-01"),
      unemployment_rate[date == as.Date("2025-09-01")]
    )
  )

unemp <- unemp |>
  arrange(region, date) |>
  group_by(region) |>
  mutate(
    unemployment_rate_change = unemployment_rate - lag(unemployment_rate)
  ) |>
  ungroup()

unemp_recent <- unemp |>
  filter(date >= as.Date("2015-01-01"))
head(unemp_recent)

unemp_summary <- unemp |>
  group_by(region) |>
  summarize(
    mean_rate = mean(unemployment_rate, na.rm = TRUE),
    min_rate = min(unemployment_rate, na.rm = TRUE),
    max_rate = max(unemployment_rate, na.rm = TRUE),
  )
unemp_summary

unemp_recent_summary <- unemp_recent |>
  group_by(region) |>
  summarize(
    mean_rate = mean(unemployment_rate, na.rm = TRUE),
    min_rate = min(unemployment_rate, na.rm = TRUE),
    max_rate = max(unemployment_rate, na.rm = TRUE)
    )
unemp_recent_summary

unemp_top_increases <- unemp |>
  slice_max(
    order_by = unemployment_rate_change,
    n = 5
  )
unemp_top_decreases <- unemp |>
  slice_min(
    order_by = unemployment_rate_change,
    n = 5
  )
unemp_top_increases
unemp_top_decreases

unemp_top_increases_no_covid <- unemp |>
  filter(date < as.Date("2020-01-01") |
           date >= as.Date("2021-01-01")) |>
  slice_max(
    order_by = unemployment_rate_change,
    n = 5
    )
unemp_top_increases_no_covid

unemp_top_decreases_no_covid <- unemp |>
  filter(date < as.Date("2020-01-01") |
           date >= as.Date("2021-01-01")) |>
  slice_min(
    order_by = unemployment_rate_change,
    n = 5
  )

unemp_top_decreases_no_covid

ggplot(unemp, aes(x = date, y = unemployment_rate, color = region)) +
  geom_line() +
  labs(
    title = "Unemployment Rates Over Time by Region",
    x = "Year",
    y = "Unemployment Rate (%)",
    color = "Region"
  ) +
  theme_minimal()

ggplot(unemp, aes(x = date, y = unemployment_rate)) +
  geom_line(color = "steelblue") +
  facet_wrap(~ region) +
  labs(
    title = "Unemployment Rate Trends by Region",
    x = "Year",
    y = "Unemployment Rate (%)"
  ) +
  theme_minimal()

ggplot(unemp_summary,
       aes(x = fct_reorder(region, mean_rate),
           y = mean_rate)) +
  geom_col(fill = "steelblue") +
  labs(
    title = "Average Unemployment Rate by Region",
    x = "Region",
    y = "Average Unemployment Rate (%)"
  ) +
  theme_minimal()

ggplot(unemp_recent,
       aes(x = date, y = unemployment_rate, color = region)) +
  geom_line(linewidth = 0.8) +
  labs(
    title = "Unemployment Rates Since 2015",
    subtitle = "Comparing the United States and Four States",
    x = "Year",
    y = "Unemployment Rate (%)",
    color = "Region"
  ) +
  theme_minimal()
# I chose to examine unemployment rates since 2015 to focus on
# more recent labor market trends across the five regions.
# This graph highlights the sharp increase in unemployment during
# the COVID- 19 pandemic and the subsequent recovery.

