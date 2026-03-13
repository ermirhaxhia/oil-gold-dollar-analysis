library(dplyr)
library(ggplot2)

# ── BLLOKU 1: LEXO DHE LLOGARIT LOG-KTHIMET ──────────
# Çfarë bën: log-kthimet janë input i GBM
# ln(S_t / S_t-1) — ndryshimi logaritmik mes dy muajve

master <- read.csv("data/clean/master_2006_2022.csv") |>
  mutate(date = as.Date(date))

master <- master |>
  arrange(date) |>
  mutate(
    log_ret_oil  = log(price_wti  / lag(price_wti)),
    log_ret_gold = log(price_gold / lag(price_gold)),
    log_ret_dxy  = log(dxy        / lag(dxy))
  ) |>
  filter(!is.na(log_ret_oil))

# ── KONTROLLO ─────────────────────────────────────────
cat("Numri i observimeve:", nrow(master), "\n")
cat("\nLog-kthimet e para:\n")
head(master[, c("date", "log_ret_oil", 
                "log_ret_gold", "log_ret_dxy")], 5)




# ── BLLOKU 2: ESTIMO PARAMETRAT GBM ──────────────────
# Çfarë bën: gjen μ (drift) dhe σ (volatilitet) për çdo asset
# μ = mesatarja e log-kthimeve mujore
# σ = devijimi standard i log-kthimeve mujore

params <- data.frame(
  asset = c("Naftë (WTI)", "Ar (Gold)", "Dollar (DXY)"),
  
  # Drift — trend mujor
  mu    = c(mean(master$log_ret_oil),
            mean(master$log_ret_gold),
            mean(master$log_ret_dxy)),
  
  # Volatilitet mujor
  sigma = c(sd(master$log_ret_oil),
            sd(master$log_ret_gold),
            sd(master$log_ret_dxy))
) |>
  mutate(
    # Konverto në vjetor (× 12 për μ, × √12 për σ)
    mu_annual    = round(mu    * 12,        4),
    sigma_annual = round(sigma * sqrt(12),  4),
    mu    = round(mu,    4),
    sigma = round(sigma, 4)
  )

cat("Parametrat GBM:\n")
print(params)



# ── BLLOKU 3: MATRICA E KORRELACIONIT ────────────────
# Çfarë bën: mat lidhjen midis lëvizjeve të 3 asseteve
# +1 = lëvizin bashkë, -1 = lëvizin kundër, 0 = të pavarur

cor_matrix <- cor(master[, c("log_ret_oil",
                             "log_ret_gold",
                             "log_ret_dxy")])

rownames(cor_matrix) <- c("Naftë", "Ar", "Dollar")
colnames(cor_matrix) <- c("Naftë", "Ar", "Dollar")

cat("Matrica e Korrelacionit:\n")
print(round(cor_matrix, 3))

# ── VIZUALIZIM — HEATMAP ──────────────────────────────
library(reshape2)

cor_melt <- melt(cor_matrix)

ggplot(cor_melt, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "white", linewidth = 1) +
  geom_text(aes(label = round(value, 3)),
            size = 5, fontface = "bold") +
  scale_fill_gradient2(
    low      = "#E74C3C",
    mid      = "white",
    high     = "#2980B9",
    midpoint = 0,
    limits   = c(-1, 1)
  ) +
  labs(title    = "Korrelacioni mes Naftës, Arit dhe Dollarit",
       subtitle = "2006-2022  |  Log-kthime mujore",
       x = "", y = "", fill = "Korrelacioni") +
  theme_minimal() +
  theme(axis.text = element_text(size = 12, face = "bold"))

ggsave("output/05_correlation.png", width = 6, height = 5)
cat("✅ Heatmap u ruajt në output/05_correlation.png\n")




# ── BLLOKU 4: TESTO NORMALITETIN ─────────────────────
# Çfarë bën: GBM supozon që log-kthimet janë normale
# Shapiro-Wilk teston këtë supozim

shapiro_oil  <- shapiro.test(master$log_ret_oil)
shapiro_gold <- shapiro.test(master$log_ret_gold)
shapiro_dxy  <- shapiro.test(master$log_ret_dxy)

cat("Testi Shapiro-Wilk (normaliteti):\n\n")
cat("Naftë  — p-value:", round(shapiro_oil$p.value,  4), 
    ifelse(shapiro_oil$p.value  > 0.05, "✅ Normale", "⚠️  Jo normale"), "\n")
cat("Ar     — p-value:", round(shapiro_gold$p.value, 4),
    ifelse(shapiro_gold$p.value > 0.05, "✅ Normale", "⚠️  Jo normale"), "\n")
cat("Dollar — p-value:", round(shapiro_dxy$p.value,  4),
    ifelse(shapiro_dxy$p.value  > 0.05, "✅ Normale", "⚠️  Jo normale"), "\n")




# ── BLLOKU 5: VIZUALIZIM LOG-KTHIME ──────────────────
# Histogram + kurba normale teorike për çdo asset

library(tidyr)

# Ristrukturoj për ggplot
returns_long <- master |>
  select(date, log_ret_oil, log_ret_gold, log_ret_dxy) |>
  pivot_longer(
    cols      = c(log_ret_oil, log_ret_gold, log_ret_dxy),
    names_to  = "asset",
    values_to = "return"
  ) |>
  mutate(asset = recode(asset,
                        "log_ret_oil"  = "Naftë (WTI)",
                        "log_ret_gold" = "Ar (Gold)",
                        "log_ret_dxy"  = "Dollar (DXY)"
  ))

ggplot(returns_long, aes(x = return)) +
  geom_histogram(aes(y = after_stat(density)),
                 bins  = 30,
                 fill  = "#2980B9",
                 alpha = 0.6,
                 color = "white") +
  stat_function(
    fun  = dnorm,
    args = list(mean = 0, sd = sd(returns_long$return)),
    color = "#E74C3C",
    linewidth = 1
  ) +
  facet_wrap(~asset, scales = "free") +
  labs(
    title    = "Shpërndarja e Log-Kthimeve",
    subtitle = "Histogrami real vs Kurba Normale Teorike (e kuqe)",
    x = "Log-kthimi", y = "Densiteti"
  ) +
  theme_minimal()

ggsave("output/06_distributions.png", width = 10, height = 4)
cat("✅ Grafiku u ruajt në output/06_distributions.png\n")