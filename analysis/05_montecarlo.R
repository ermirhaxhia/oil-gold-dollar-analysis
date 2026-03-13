library(dplyr)
library(ggplot2)
library(MASS)

# ── BLLOKU 1: NGARKO PARAMETRAT ───────────────────────
# Lexo të dhënat e trajnimit 2006-2022
master <- read.csv("data/clean/master_2006_2022.csv") |>
  mutate(date = as.Date(date))

# Llogarit log-kthimet
master <- master |>
  arrange(date) |>
  mutate(
    log_ret_oil  = log(price_wti  / lag(price_wti)),
    log_ret_gold = log(price_gold / lag(price_gold)),
    log_ret_dxy  = log(dxy        / lag(dxy))
  ) |>
  filter(!is.na(log_ret_oil))

# Parametrat GBM
mu_oil  <- mean(master$log_ret_oil)
mu_gold <- mean(master$log_ret_gold)
mu_dxy  <- mean(master$log_ret_dxy)

sigma_oil  <- sd(master$log_ret_oil)
sigma_gold <- sd(master$log_ret_gold)
sigma_dxy  <- sd(master$log_ret_dxy)

# Matrica e korrelacionit
cor_matrix <- cor(master[, c("log_ret_oil",
                             "log_ret_gold",
                             "log_ret_dxy")])

# Lambda nga Poisson
lambda <- 1.588

# Çmimet e fundit të 2022 — pikënisja e simulimit
S0_oil  <- tail(master$price_wti,  1)
S0_gold <- tail(master$price_gold, 1)
S0_dxy  <- tail(master$dxy,        1)

cat("✅ Parametrat u ngarkuan:\n")
cat("   μ oil:", round(mu_oil, 4), "| σ oil:", round(sigma_oil, 4), "\n")
cat("   μ gold:", round(mu_gold, 4), "| σ gold:", round(sigma_gold, 4), "\n")
cat("   μ dxy:", round(mu_dxy, 4), "| σ dxy:", round(sigma_dxy, 4), "\n")
cat("   λ =", lambda, "\n")
cat









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
  cov_matrix <- diag(sigma_vec) %*% 
    cor_matrix %*% 
    diag(sigma_vec)
  
  # Rezultatet — çmimi final i çdo skenari
  results <- matrix(0, nrow = n_sim, ncol = 3)
  colnames(results) <- c("oil", "gold", "dxy")
  
  for (i in 1:n_sim) {
    
    # Gjenero T lëvizje të lidhura (multivariate normal)
    shocks <- mvrnorm(n    = T,
                      mu   = c(0, 0, 0),
                      Sigma = cov_matrix)
    
    # GBM — llogarit çmimin për çdo muaj
    oil_path  <- S0_oil
    gold_path <- S0_gold
    dxy_path  <- S0_dxy
    
    for (t in 1:T) {
      
      # Shok krize Poisson — me probabilitet λ/12 çdo muaj
      crisis <- rbinom(1, 1, prob = lambda / 12)
      
      # Nëse kemi krizë dhe crisis_shock = TRUE
      # injektojmë shok shtesë (simulimi Hormuz)
      extra_shock <- if (crisis_shock && crisis == 1) {
        c(+0.20, -0.05, +0.08)  # nafta ↑20%, ari ↓5%, dollar ↑8%
      } else {
        c(0, 0, 0)
      }
      
      # GBM: S_t = S_{t-1} * exp(μ - σ²/2 + σε)
      oil_path  <- oil_path  * exp((mu_oil  - 0.5*sigma_oil^2)  +
                                     shocks[t,1] + extra_shock[1])
      gold_path <- gold_path * exp((mu_gold - 0.5*sigma_gold^2) +
                                     shocks[t,2] + extra_shock[2])
      dxy_path  <- dxy_path  * exp((mu_dxy  - 0.5*sigma_dxy^2)  +
                                     shocks[t,3] + extra_shock[3])
    }
    
    results[i,] <- c(oil_path, gold_path, dxy_path)
  }
  
  return(as.data.frame(results))
}

cat("✅ Funksioni i simulimit është gati\n")
cat("   Parametrat: T muaj, n_sim skenarë\n")
cat("   Shok krize: P(krizë/muaj) =", round(lambda/12, 3), "\n")






# ── BLLOKU 3: SIMULIMI 2022-2024 ─────────────────────
# Çfarë bën: simulon 24 muaj (2022-2024) dhe krahason
# me çmimet reale — teston sa mirë parashikon modeli

set.seed(42)  # për riprodhueshmëri
n_sim <- 10000
T     <- 24   # 24 muaj = 2 vjet

cat("Duke simuluar", n_sim, "skenarë për", T, "muaj...\n")

sim_results <- simulate_paths(
  S0_oil, S0_gold, S0_dxy,
  mu_oil, mu_gold, mu_dxy,
  sigma_oil, sigma_gold, sigma_dxy,
  cor_matrix,
  T     = T,
  n_sim = n_sim,
  crisis_shock = FALSE
)

# ── STATISTIKAT E SIMULIMIT ───────────────────────────
cat("\n📊 Rezultatet e Simulimit 2022-2024:\n\n")

# Nafta
cat("🛢️  Naftë (WTI):\n")
cat("   Mesatarja e simuluar:", round(mean(sim_results$oil), 2), "\n")
cat("   95% CI: [", round(quantile(sim_results$oil, 0.025), 2),
    ",", round(quantile(sim_results$oil, 0.975), 2), "]\n")

# Ari
cat("🥇 Ar (Gold):\n")
cat("   Mesatarja e simuluar:", round(mean(sim_results$gold), 2), "\n")
cat("   95% CI: [", round(quantile(sim_results$gold, 0.025), 2),
    ",", round(quantile(sim_results$gold, 0.975), 2), "]\n")

# Dollari
cat("💵 Dollar (DXY):\n")
cat("   Mesatarja e simuluar:", round(mean(sim_results$dxy), 2), "\n")
cat("   95% CI: [", round(quantile(sim_results$dxy, 0.025), 2),
    ",", round(quantile(sim_results$dxy, 0.975), 2), "]\n")

# ── KRAHASO ME REALITETIN ─────────────────────────────
# Lexo të dhënat reale 2022-2024
master_real <- read.csv("data/clean/master_2022_2024.csv") |>
  mutate(date = as.Date(date))

real_oil_end  <- tail(master_real$price_wti,  1)
real_gold_end <- tail(master_real$price_gold, 1)
real_dxy_end  <- tail(master_real$dxy,        1)

cat("\n📈 Krahasimi Simulim vs Realitet (Dhjetor 2024):\n\n")
cat("        | Simuluar (mesatare) | Real   | Gabimi\n")
cat("--------|---------------------|--------|--------\n")
cat("Naftë   |", round(mean(sim_results$oil),  2),
    "             |", real_oil_end,
    "|", round(abs(mean(sim_results$oil)  - real_oil_end)  / real_oil_end  * 100, 1), "%\n")
cat("Ar      |", round(mean(sim_results$gold), 2),
    "            |", real_gold_end,
    "|", round(abs(mean(sim_results$gold) - real_gold_end) / real_gold_end * 100, 1), "%\n")
cat("Dollar  |", round(mean(sim_results$dxy),  2),
    "              |", real_dxy_end,
    "|", round(abs(mean(sim_results$dxy)  - real_dxy_end)  / real_dxy_end  * 100, 1), "%\n")







# ── BLLOKU 4: SIMULIMI HORMUZ 2026 ───────────────────
# Çfarë bën: injekton shok të jashtëm (krizë Hormuz)
# dhe simulon 12 muaj nga Janar 2026

# Çmimet fillestare — Dhjetor 2024 (real)
S0_oil_2026  <- real_oil_end
S0_gold_2026 <- real_gold_end
S0_dxy_2026  <- real_dxy_end

set.seed(123)

# Simulim BEZ shok Hormuz — baseline
sim_baseline <- simulate_paths(
  S0_oil_2026, S0_gold_2026, S0_dxy_2026,
  mu_oil, mu_gold, mu_dxy,
  sigma_oil, sigma_gold, sigma_dxy,
  cor_matrix,
  T = 12, n_sim = 10000,
  crisis_shock = FALSE
)

# Simulim ME shok Hormuz
sim_hormuz <- simulate_paths(
  S0_oil_2026, S0_gold_2026, S0_dxy_2026,
  mu_oil, mu_gold, mu_dxy,
  sigma_oil, sigma_gold, sigma_dxy,
  cor_matrix,
  T = 12, n_sim = 10000,
  crisis_shock = TRUE
)

cat("🌊 Parashikimi Hormuz 2026 (12 muaj):\n\n")
cat("              | Baseline        | Hormuz Crisis   | Ndryshimi\n")
cat("--------------|-----------------|-----------------|----------\n")

for (asset in c("oil", "gold", "dxy")) {
  label <- switch(asset,
                  "oil"  = "Naftë  ",
                  "gold" = "Ar     ",
                  "dxy"  = "Dollar "
  )
  b_mean <- round(mean(sim_baseline[[asset]]), 2)
  h_mean <- round(mean(sim_hormuz[[asset]]),   2)
  diff   <- round((h_mean - b_mean) / b_mean * 100, 1)
  
  cat(label, "       |", b_mean,
      "          |", h_mean,
      "          |", diff, "%\n")
}







# ── BLLOKU 5: VIZUALIZIMI FINAL ───────────────────────
library(tidyr)

# Krijo data frame për vizualizim
hormuz_df <- data.frame(
  asset     = c("Naftë", "Ar", "Dollar"),
  baseline  = c(mean(sim_baseline$oil),
                mean(sim_baseline$gold),
                mean(sim_baseline$dxy)),
  hormuz    = c(mean(sim_hormuz$oil),
                mean(sim_hormuz$gold),
                mean(sim_hormuz$dxy))
) |>
  pivot_longer(cols = c(baseline, hormuz),
               names_to  = "scenario",
               values_to = "price") |>
  mutate(scenario = recode(scenario,
                           "baseline" = "Baseline (Pa Krizë)",
                           "hormuz"   = "Krizë Hormuz 2026"
  ))

ggplot(hormuz_df, aes(x = scenario, y = price, fill = scenario)) +
  geom_bar(stat = "identity", width = 0.5, alpha = 0.85) +
  geom_text(aes(label = round(price, 0)),
            vjust = -0.5, size = 4, fontface = "bold") +
  scale_fill_manual(values = c(
    "Baseline (Pa Krizë)" = "#2980B9",
    "Krizë Hormuz 2026"   = "#E74C3C"
  )) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  facet_wrap(~asset, scales = "free_y") +
  labs(
    title    = "Parashikimi Monte Carlo — Krizë Hormuz 2026",
    subtitle = "10,000 skenarë | 12 muaj | Baseline vs Krizë",
    x = "", y = "Çmimi i Parashikuar", fill = ""
  ) +
  theme_minimal() +
  theme(
    legend.position  = "top",
    axis.text.x      = element_blank(),
    axis.ticks.x     = element_blank()
  )

ggsave("output/07_hormuz_forecast.png", width = 10, height = 5)
cat("✅ Grafiku u ruajt\n")

ggsave("output/07_hormuz_forecast.png", width = 10, height = 5)

# Grafiku 2 — Shpërndarja e Skenarëve
sim_compare <- data.frame(
  oil_base   = sim_baseline$oil,
  oil_hormuz = sim_hormuz$oil,
  gold_base  = sim_baseline$gold,
  gold_hormuz= sim_hormuz$gold
)

ggplot(sim_compare) +
  geom_density(aes(x = oil_base,   fill = "Baseline"),
               alpha = 0.5) +
  geom_density(aes(x = oil_hormuz, fill = "Hormuz"),
               alpha = 0.5) +
  geom_vline(xintercept = real_oil_end,
             linetype = "dashed", color = "black") +
  scale_fill_manual(values = c(
    "Baseline" = "#2980B9",
    "Hormuz"   = "#E74C3C"
  )) +
  labs(
    title    = "Shpërndarja e Skenarëve — Naftë 2026",
    subtitle = "Vija e zezë = çmimi real Dhjetor 2024 ($70.12)",
    x = "Çmimi WTI ($)", y = "Densiteti",
    fill = ""
  ) +
  theme_minimal() +
  theme(legend.position = "top")

ggsave("output/08_density_oil.png", width = 8, height = 5)
cat("✅ Grafikët u ruajtën në output/\n")