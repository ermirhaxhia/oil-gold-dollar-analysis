library(tidyverse)
library(lubridate)

# ── WTI ───────────────────────────────────────────────
wti <- read_csv("data/raw/wti.csv") |>
  rename(date = observation_date, 
         price_wti = DCOILWTICO) |>
  mutate(date = mdy(date))

# ── BRENT ─────────────────────────────────────────────
brent <- read_csv("data/raw/brent.csv") |>
  mutate(date = ymd(date))

# ── DXY ───────────────────────────────────────────────
dxy <- read_csv("data/raw/dxy.csv") |>
  rename(date = observation_date, 
         dxy = DTWEXBGS) |>
  mutate(date = mdy(date))

# ── REAL RATE ─────────────────────────────────────────
real_rate <- read_csv("data/raw/real_rate.csv") |>
  rename(date = observation_date, 
         real_rate = DFII10) |>
  mutate(date = mdy(date))

# ── GOLD (ka \t brenda) ───────────────────────────────
gold <- read_lines("data/raw/gold.csv") |>
  tibble(raw_line = _) |>
  mutate(
    date = mdy(str_extract(raw_line, "^[^\\t]+")),
    price_gold = as.numeric(str_extract(raw_line, "(?<=\\t)[^\\t]+"))
  ) |>
  select(-raw_line) |>
  filter(!is.na(date))

# ── BASHKO TË GJITHA ──────────────────────────────────
master <- wti |>
  left_join(brent, by = "date") |>
  left_join(dxy, by = "date") |>
  left_join(gold, by = "date") |>
  left_join(real_rate, by = "date") |>
  filter(date >= ymd("2006-01-01"),
         date <= ymd("2022-02-01")) |>
  drop_na()

# ── RUAJ ──────────────────────────────────────────────
write_csv(master, "data/clean/master_2006_2022.csv")

# ── KONTROLLO ─────────────────────────────────────────
cat("Rreshta:", nrow(master), "\n")
cat("Kolonat:", paste(names(master), collapse = ", "), "\n")
head(master, 3)

# ── PERIUDHA 2022-2024 për krahasim ───────────────────
gold_2024 <- read_lines("data/raw/gold.csv") |>
  tibble(raw_line = _) |>
  mutate(
    date = mdy(str_extract(raw_line, "^[^\\t]+")),
    price_gold = as.numeric(str_extract(raw_line, "(?<=\\t)[^\\t]+"))
  ) |>
  select(-raw_line) |>
  filter(!is.na(date))

master_test <- read_csv("data/raw/wti.csv") |>
  rename(date = observation_date, price_wti = DCOILWTICO) |>
  mutate(date = mdy(date)) |>
  left_join(
    read_csv("data/raw/brent.csv") |> mutate(date = ymd(date)),
    by = "date"
  ) |>
  left_join(
    read_csv("data/raw/dxy.csv") |>
      rename(date = observation_date, dxy = DTWEXBGS) |>
      mutate(date = mdy(date)),
    by = "date"
  ) |>
  left_join(gold_2024, by = "date") |>
  left_join(
    read_csv("data/raw/real_rate.csv") |>
      rename(date = observation_date, real_rate = DFII10) |>
      mutate(date = mdy(date)),
    by = "date"
  ) |>
  filter(date >= ymd("2022-02-01"),
         date <= ymd("2024-12-31")) |>
  drop_na()

write_csv(master_test, "data/clean/master_2022_2024.csv")

cat("✅ master_2022_2024.csv u krijua:", 
    nrow(master_test), "rreshta\n")