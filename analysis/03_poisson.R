library(dplyr)
library(ggplot2)

# ── BLLOKU 1: LEXO TË DHËNAT ──────────────────────────
master <- read.csv("data/clean/master_2006_2022.csv") |>
  mutate(date = as.Date(date))

# ── BLLOKU 2: IDENTIFIKO KRIZAT ───────────────────────
# Çfarë bën: shënon çdo muaj krizë si ngjarje Poisson (0 ose 1)
kriza <- master |>
  mutate(
    year      = format(date, "%Y"),
    is_crisis = ifelse(regime == "Krize", 1, 0)
  ) |>
  group_by(year) |>
  summarise(
    n_crisis = sum(is_crisis),  # sa muaj krizë ka pasur çdo vit
    .groups  = "drop"
  )

cat("Krizat për vit:\n")
print(kriza)

# ── BLLOKU 3: ESTIMO λ ────────────────────────────────
# λ = mesatarja e ngjarjeve (krizave) për vit
# Ky është parametri kryesor i Procesit Poisson

lambda <- mean(kriza$n_crisis)
cat("Lambda (λ) =", round(lambda, 3), "kriza/vit\n")
cat("Interpretim: mesatarisht", round(lambda, 2),
    "muaj krizë për çdo vit\n\n")

# ── BLLOKU 4: TESTO HIPOTEZËN POISSON ────────────────
# Çfarë bën: krahason shpërndarjen reale me Poisson teorike
# Nëse janë të ngjashme → krizat ndjekin Poisson

# Shpërndarja reale — sa shpesh ndodh 0, 1, 2, 3... kriza/vit
real_dist <- table(kriza$n_crisis)
cat("Shpërndarja reale e krizave:\n")
print(real_dist)

# Shpërndarja teorike Poisson me λ
max_k <- max(kriza$n_crisis)
poisson_dist <- dpois(0:max_k, lambda) * nrow(kriza)

cat("\nShpërndarja teorike Poisson:\n")
theoretical <- data.frame(
  n_crisis    = 0:max_k,
  real        = as.numeric(table(factor(kriza$n_crisis,
                                        levels = 0:max_k))),
  theoretical = round(poisson_dist, 2)
)
print(theoretical)

# ── BLLOKU 5: TESTI FORMAL — Chi-Square ───────────────
# Teston nëse diferenca real vs teorike është statistikisht
# e rëndësishme apo rastësore
# Bashkojmë 3+ kriza sepse Chi-Square kërkon min 5 obs/kategori

kriza_grouped <- kriza |>
  mutate(n_grouped = ifelse(n_crisis >= 3, "3+",
                            as.character(n_crisis)))

real_grouped <- table(kriza_grouped$n_grouped)

# Probabilitetet Poisson të grupuara
p0 <- dpois(0, lambda)
p1 <- dpois(1, lambda)
p2 <- dpois(2, lambda)
p3 <- 1 - p0 - p1 - p2  # P(X >= 3)

probs_grouped <- c(p0, p1, p2, p3)

chisq_test <- chisq.test(
  x = as.numeric(real_grouped),
  p = probs_grouped,
  rescale.p = TRUE
)

cat("Testi Chi-Square (i korrigjuar):\n")
cat("p-value =", round(chisq_test$p.value, 4), "\n")
cat("Interpretim:",
    ifelse(chisq_test$p.value > 0.05,
           "✅ Krizat ndjekin shpërndarje Poisson (p > 0.05)",
           "⚠️  Krizat NUK ndjekin plotësisht Poisson (p < 0.05)"), "\n")

# ── BLLOKU 6: VIZUALIZIM ──────────────────────────────
ggplot(theoretical, aes(x = factor(n_crisis))) +
  geom_bar(aes(y = real, fill = "Reale"),
           stat = "identity", alpha = 0.7, width = 0.4,
           position = position_nudge(x = -0.2)) +
  geom_bar(aes(y = theoretical, fill = "Poisson Teorike"),
           stat = "identity", alpha = 0.7, width = 0.4,
           position = position_nudge(x = 0.2)) +
  scale_fill_manual(values = c("Reale"            = "#E74C3C",
                               "Poisson Teorike"  = "#2980B9")) +
  labs(title    = "Krizat Ekonomike — Reale vs Poisson Teorike",
       subtitle = paste0("λ = ", round(lambda, 3), " kriza/vit  |  p-value = ",
                         round(chisq_test$p.value, 4)),
       x        = "Numri i Krizave për Vit",
       y        = "Frekuenca (vite)",
       fill     = "") +
  theme_minimal() +
  theme(legend.position = "top")

ggsave("output/04_poisson.png", width = 7, height = 5)
cat("✅ Grafiku u ruajt në output/04_poisson.png\n")