library(tidyverse)
library(lubridate)
library(MASS)
library(purrr)

# ── BLLOKU 1: NGARKO PARAMETRAT ───────────────────────
# Lexo të dhënat e trajnimit 2006-2022
master <- read_csv("data/clean/master_2006_2022.csv") |>
  mutate(date = ymd(date)) |>
  arrange(date) |>
  mutate(
    log_ret_oil  = log(price_wti  / lag(price_wti)),
    log_ret_gold = log(price_gold / lag(price_gold)),
    log_ret_dxy  = log(dxy        / lag(dxy))
  ) |>
  filter(!is.na(log_ret_oil))

# Parametrat GBM
params_gbm <- tibble(
  asset = c("oil", "gold", "dxy"),
  mu    = c(mean(master$log_ret_oil), mean(master$log_ret_gold), mean(master$log_ret_dxy)),
  sigma = c(sd(master$log_ret_oil), sd(master$log_ret_gold), sd(master$log_ret_dxy)),
  S0    = c(tail(master$price_wti, 1), tail(master$price_gold, 1), tail(master$dxy, 1))
)

# Matrica e korrelacionit
cor_matrix <- master |>
  select(log_ret_oil, log_ret_gold, log_ret_dxy) |>
  cor()

# Lambda nga Poisson
lambda <- 1.588

cat("✅ Parametrat u ngarkuan:\n")
params_gbm |>
  mutate(across(c(mu, sigma), ~ round(., 4))) |>
  print()

cat("   λ =", lambda, "\n")

# ── BLLOKU 2: FUNKSIONI I SIMULIMIT ───────────────────
# Çfarë bën: simulon T muaj çmimesh me GBM + korrelacion
# n_sim = numri i skenarëve, T = numri i muajve

simulate_paths <- function(S0_oil, S0_gold, S0_dxy,
                           mu_oil, mu_gold, mu_dxy,
                           sigma_oil, sigma_gold, sigma_dxy,
                           cor_matrix, T, n_sim,
                           crisis_shock = FALSE) {
  
  # Matrica e kovariancës për gjenero lëvizje të lidhura
  sigma_vec <- c(sigma_oil, sigma_gold, sigma_dxy)
  cov_matrix <- diag(sigma_vec) %*% cor_matrix %*% diag(sigma_vec)
  
  # Rezultatet — çmimi final i çdo skenari
  results <- matrix(0, nrow = n_sim, ncol = 3)
  colnames(results) <- c("oil", "gold", "dxy")
  
  for (i in 1:n_sim) {
    
    # Gjenero T lëvizje të lidhura (multivariate normal)
    shocks <- mvrnorm(n = T, mu = c(0, 0, 0), Sigma = cov_matrix)
    
    # GBM — llogarit çmimin për çdo muaj
    paths <- tibble(
      oil  = S0_oil,
      gold = S0_gold,
      dxy  = S0_dxy
    )
    
    for (t in 1:T) {
      
      # Shok krize Poisson — me probabilitet λ/12 çdo muaj
      crisis <- rbinom(1, 1, prob = lambda / 12)
      
      # Nëse kemi krizë dhe crisis_shock = TRUE
      extra_shock <- if (crisis_shock && crisis == 1) {
        c(+0.20, -0.05, +0.08)  # nafta ↑20%, ari ↓5%, dollar ↑8%
      } else {
        c(0, 0, 0)
      }
      
      # GBM: S_t = S_{t-1} * exp(μ - σ²/2 + σε)
      paths <- paths |>
        mutate(
          oil  = oil  * exp((mu_oil  - 0.5*sigma_oil^2)  + shocks[t,1] + extra_shock[1]),
          gold = gold * exp((mu_gold - 0.5*sigma_gold^2) + shocks[t,2] + extra_shock[2]),
          dxy  = dxy  * exp((mu_dxy  - 0.5*sigma_dxy^2)  + shocks[t,3] + extra_shock[3])
        )
    }
    
    results[i,] <- as.numeric(paths)
  }
  
  return(as_tibble(results))
}

cat("✅ Funksioni i simulimit është gati\n")
cat("   Parametrat: T muaj, n_sim skenarë\n")
cat("   Shok krize: P(krizë/muaj) =", round(lambda/12, 3), "\n")

# ── BLLOKU 3: SIMULIMI 2022-2024 ─────────────────────
# Çfarë bën: simulon 24 muaj (2022-2024) dhe krahason
# me çmimet reale — teston sa mirë parashikon modeli

set.seed(42)  # për riprodhueshmëri
n_sim <- 10000
T <- 24   # 24 muaj = 2 vjet

cat("\nDuke simuluar", n_sim, "skenarë për", T, "muaj...\n")

sim_results <- simulate_paths(
  params_gbm$S0[1], params_gbm$S0[2], params_gbm$S0[3],
  params_gbm$mu[1], params_gbm$mu[2], params_gbm$mu[3],
  params_gbm$sigma[1], params_gbm$sigma[2], params_gbm$sigma[3],
  cor_matrix,
  T = T,
  n_sim = n_sim,
  crisis_shock = FALSE
)

# ── STATISTIKAT E SIMULIMIT ───────────────────────────
cat("\n📊 Rezultatet e Simulimit 2022-2024:\n\n")

sim_summary <- sim_results |>
  summarise(
    across(everything(), list(
      mean = ~ round(mean(.), 2),
      ci_lower = ~ round(quantile(., 0.025), 2),
      ci_upper = ~ round(quantile(., 0.975), 2)
    ))
  ) |>
  pivot_longer(everything(), names_to = c("asset", ".value"), names_sep = "_")

sim_summary |>
  mutate(
    label = case_when(
      asset == "oil" ~ "🛢️  Naftë (WTI)",
      asset == "gold" ~ "🥇 Ar (Gold)",
      asset == "dxy" ~ "💵 Dollar (DXY)"
    )
  ) |>
  select(label, mean, ci_lower, ci_upper) |>
  rowwise() |>
  mutate(output = paste0(label, ":\n   Mesatarja e simuluar: ", mean,
                         "\n   95% CI: [", ci_lower, ", ", ci_upper, "]\n")) |>
  pull(output) |>
  cat(sep = "\n")

# ── KRAHASO ME REALITETIN ─────────────────────────────
# Lexo të dhënat reale 2022-2024
master_real <- read_csv("data/clean/master_2022_2024.csv") |>
  mutate(date = ymd(date))

real_values <- master_real |>
  summarise(
    oil  = last(price_wti),
    gold = last(price_gold),
    dxy  = last(dxy)
  )

sim_means <- sim_results |>
  summarise(across(everything(), mean))

cat("\n📈 Krahasimi Simulim vs Realitet (Dhjetor 2024):\n\n")

comparison <- tibble(
  asset = c("Naftë", "Ar", "Dollar"),
  simulated = c(sim_means$oil, sim_means$gold, sim_means$dxy),
  real = c(real_values$oil, real_values$gold, real_values$dxy)
) |>
  mutate(error_pct = round(abs(simulated - real) / real * 100, 1))

cat("        | Simuluar (mesatare) | Real   | Gabimi\n")
cat("--------|---------------------|--------|--------\n")
comparison |>
  rowwise() |>
  mutate(line = sprintf("%-7s | %-19.2f | %-6.2f | %.1f%%\n", 
                        asset, simulated, real, error_pct)) |>
  pull(line) |>
  cat()

# ── BLLOKU 4: SIMULIMI HORMUZ 2026 ───────────────────
# Çfarë bën: injekton shok të jashtëm (krizë Hormuz)
# dhe simulon 12 muaj nga Janar 2026

S0_2026 <- real_values

set.seed(123)

# Simulim BEZ shok Hormuz — baseline
sim_baseline <- simulate_paths(
  S0_2026$oil, S0_2026$gold, S0_2026$dxy,
  params_gbm$mu[1], params_gbm$mu[2], params_gbm$mu[3],
  params_gbm$sigma[1], params_gbm$sigma[2], params_gbm$sigma[3],
  cor_matrix,
  T = 12, n_sim = 10000,
  crisis_shock = FALSE
)

# Simulim ME shok Hormuz
sim_hormuz <- simulate_paths(
  S0_2026$oil, S0_2026$gold, S0_2026$dxy,
  params_gbm$mu[1], params_gbm$mu[2], params_gbm$mu[3],
  params_gbm$sigma[1], params_gbm$sigma[2], params_gbm$sigma[3],
  cor_matrix,
  T = 12, n_sim = 10000,
  crisis_shock = TRUE
)

cat("\n🌊 Parashikimi Hormuz 2026 (12 muaj):\n\n")

hormuz_summary <- tibble(
  asset = c("oil", "gold", "dxy"),
  label = c("Naftë  ", "Ar     ", "Dollar "),
  baseline = c(mean(sim_baseline$oil), mean(sim_baseline$gold), mean(sim_baseline$dxy)),
  hormuz = c(mean(sim_hormuz$oil), mean(sim_hormuz$gold), mean(sim_hormuz$dxy))
) |>
  mutate(diff_pct = round((hormuz - baseline) / baseline * 100, 1))

cat("              | Baseline        | Hormuz Crisis   | Ndryshimi\n")
cat("--------------|-----------------|-----------------|----------\n")
hormuz_summary |>
  rowwise() |>
  mutate(line = sprintf("%-13s | %-15.2f | %-15.2f | %.1f%%\n",
                        label, baseline, hormuz, diff_pct)) |>
  pull(line) |>
  cat()

# ── BLLOKU 5: VIZUALIZIMI FINAL ───────────────────────

# Data frame për vizualizim — Hormuz comparison
hormuz_df <- hormuz_summary |>
  select(asset, label, baseline, hormuz) |>
  pivot_longer(cols = c(baseline, hormuz),
               names_to = "scenario",
               values_to = "price") |>
  mutate(
    scenario = recode(scenario,
                      "baseline" = "Baseline (Pa Krizë)",
                      "hormuz" = "Krizë Hormuz 2026"),
    asset = recode(asset,
                   "oil" = "Naftë",
                   "gold" = "Ar",
                   "dxy" = "Dollar")
  )

# Grafiku 1 — Bar plot krahasues
p1 <- ggplot(hormuz_df, aes(x = scenario, y = price, fill = scenario)) +
  geom_col(width = 0.5, alpha = 0.85) +
  geom_text(aes(label = round(price, 0)),
            vjust = -0.5, size = 4, fontface = "bold") +
  scale_fill_manual(values = c(
    "Baseline (Pa Krizë)" = "#2980B9",
    "Krizë Hormuz 2026" = "#E74C3C"
  )) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  facet_wrap(~asset, scales = "free_y") +
  labs(
    title = "Parashikimi Monte Carlo — Krizë Hormuz 2026",
    subtitle = "10,000 skenarë | 12 muaj | Baseline vs Krizë",
    x = "", y = "Çmimi i Parashikuar", fill = ""
  ) +
  theme_minimal() +
  theme(
    legend.position = "top",
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  )

ggsave("output/07_hormuz_forecast.png", plot = p1, width = 10, height = 5, dpi = 300)

# Grafiku 2 — Shpërndarja e Skenarëve
sim_compare <- tibble(
  oil_base = sim_baseline$oil,
  oil_hormuz = sim_hormuz$oil
) |>
  pivot_longer(everything(), names_to = "scenario", values_to = "price") |>
  mutate(scenario = recode(scenario,
                           "oil_base" = "Baseline",
                           "oil_hormuz" = "Hormuz"))

p2 <- ggplot(sim_compare, aes(x = price, fill = scenario)) +
  geom_density(alpha = 0.5) +
  geom_vline(xintercept = real_values$oil,
             linetype = "dashed", color = "black") +
  scale_fill_manual(values = c(
    "Baseline" = "#2980B9",
    "Hormuz" = "#E74C3C"
  )) +
  labs(
    title = "Shpërndarja e Skenarëve — Naftë 2026",
    subtitle = paste0("Vija e zezë = çmimi real Dhjetor 2024 ($", round(real_values$oil, 2), ")"),
    x = "Çmimi WTI ($)", y = "Densiteti",
    fill = ""
  ) +
  theme_minimal() +
  theme(legend.position = "top")

ggsave("output/08_density_oil.png", plot = p2, width = 8, height = 5, dpi = 300)
cat("✅ Grafikët u ruajtën në output/\n")