library(dplyr)
library(lubridate)

# ── WTI ───────────────────────────────────────────────
wti <- read.csv("data/raw/wti.csv") |>
  rename(date = observation_date, 
         price_wti = DCOILWTICO) |>
  mutate(date = as.Date(date, format = "%m/%d/%Y"))

# ── BRENT (kolonat gati) ──────────────────────────────
# ── BRENT (format i ndryshëm date) ───────────────────
brent <- read.csv("data/raw/brent.csv") |>
  mutate(date = as.Date(date, format = "%Y-%m-%d"))

# ── DXY ───────────────────────────────────────────────
dxy <- read.csv("data/raw/dxy.csv") |>
  rename(date = observation_date, 
         dxy = DTWEXBGS) |>
  mutate(date = as.Date(date, format = "%m/%d/%Y"))

# ── REAL RATE ─────────────────────────────────────────
real_rate <- read.csv("data/raw/real_rate.csv") |>
  rename(date = observation_date, 
         real_rate = DFII10) |>
  mutate(date = as.Date(date, format = "%m/%d/%Y"))

# ── GOLD (ka \t brenda) ───────────────────────────────
gold_raw <- readLines("data/raw/gold.csv")
gold <- data.frame(
  date       = as.Date(
    gsub("\\t.*", "", gold_raw),
    format = "%m/%d/%Y"),
  price_gold = as.numeric(
    gsub(".*\\t(.*)\\t", "\\1", gold_raw))
) |> filter(!is.na(date))

# ── BASHKO TË GJITHA ──────────────────────────────────
master <- wti |>
  left_join(brent,     by = "date") |>
  left_join(dxy,       by = "date") |>
  left_join(gold,      by = "date") |>
  left_join(real_rate, by = "date") |>
  filter(date >= as.Date("2006-01-01"),
         date <= as.Date("2022-2-1")) |>
  filter(complete.cases(pick(everything())))

# ── RUAJ ──────────────────────────────────────────────
write.csv(master, "data/clean/master_2006_2022.csv", row.names = FALSE)

# ── KONTROLLO ─────────────────────────────────────────
cat("Rreshta:", nrow(master), "\n")
cat("Kolonat:", names(master), "\n")
head(master, 3)







# ── PERIUDHA 2022-2024 për krahasim ───────────────────
master_test <- read.csv("data/raw/wti.csv") |>
  rename(date = observation_date, price_wti = DCOILWTICO) |>
  mutate(date = as.Date(date, format = "%m/%d/%Y")) |>
  left_join(
    read.csv("data/raw/brent.csv") |>
      mutate(date = as.Date(date, format = "%Y-%m-%d")),
    by = "date"
  ) |>
  left_join(
    read.csv("data/raw/dxy.csv") |>
      rename(date = observation_date, dxy = DTWEXBGS) |>
      mutate(date = as.Date(date, format = "%m/%d/%Y")),
    by = "date"
  ) |>
  left_join(
    read.csv("data/raw/gold.csv") |>
      (\(x) {
        raw <- readLines("data/raw/gold.csv")
        data.frame(
          date       = as.Date(gsub("\\t.*", "", raw), format = "%m/%d/%Y"),
          price_gold = as.numeric(gsub(".*\\t(.*)\\t", "\\1", raw))
        )
      })(),
    by = "date"
  ) |>
  left_join(
    read.csv("data/raw/real_rate.csv") |>
      rename(date = observation_date, real_rate = DFII10) |>
      mutate(date = as.Date(date, format = "%m/%d/%Y")),
    by = "date"
  ) |>
  filter(date >= as.Date("2022-02-01"),
         date <= as.Date("2024-12-31")) |>
  filter(complete.cases(pick(everything())))

write.csv(master_test, "data/clean/master_2022_2024.csv", 
          row.names = FALSE)
cat("✅ master_2022_2024.csv u krijua:", 
    nrow(master_test), "rreshta\n")