library(dplyr)

# ── LEXO TË DHËNAT ────────────────────────────────────
master <- read.csv("data/clean/master_2006_2022.csv") |>
  mutate(date = as.Date(date))

# ── LLOGARIT KTHIMET MUJORE ───────────────────────────
master <- master |>
  arrange(date) |>
  mutate(
    return_wti  = (price_wti  / lag(price_wti)  - 1) * 100,
    return_gold = (price_gold / lag(price_gold) - 1) * 100,
    return_dxy  = (dxy        / lag(dxy)        - 1) * 100
  ) |>
  filter(!is.na(return_wti))

# ── KONTROLLO ─────────────────────────────────────────
head(master, 3)



# ── BLLOKU 3: KLASIFIKO REGJIMED ─────────────────────
# Çdo muaj merr një etiketë bazuar në drejtimin e 3 variablave
# + = rritje atë muaj, - = ulje atë muaj

master <- master |>
  mutate(
    regime = case_when(
      # Krizë: nafta ↑, dollari ↑, ari ↓
      return_wti > 0 & return_dxy > 0 & return_gold < 0 ~ "Krize",
      
      # Stagflacion: nafta ↑, dollari ↓, ari ↑
      return_wti > 0 & return_dxy < 0 & return_gold > 0 ~ "Stagflacion",
      
      # Recesion: nafta ↓, dollari ↓, ari ↑  
      return_wti < 0 & return_dxy < 0 & return_gold > 0 ~ "Recesion",
      
      # Normal: gjithçka tjetër
      TRUE ~ "Normal"
    )
  )



# ── BLLOKU 4: MATRICA E TRANZICIONIT P ───────────────
# Çfarë bën: numëron sa herë sistemi kaloi nga regjimi X → regjimi Y
# pastaj e ndan me totalin e rreshtit për të marrë probabilitetet

# Krijo çiftet: regjimi aktual dhe regjimi i muajit tjetër
transitions <- data.frame(
  from = master$regime[-nrow(master)],    # muaji aktual
  to   = master$regime[-1]               # muaji tjetër
)

# Numëro kalimet
count_matrix <- table(transitions$from, transitions$to)
cat("Numri i kalimeve:\n")

# Normalizimi — pjesëto çdo rresht me totalin e tij
P <- count_matrix / rowSums(count_matrix)
cat("\nMatrica e Tranzicionit P:\n")


# ── BLLOKU 5: SHPËRNDARJA STACIONARE π ───────────────
# Çfarë bën: gjen probabilitetin afatgjatë të çdo regjimi
# Zgjidhim: π = π × P  me kushtin  sum(π) = 1

# Metoda: eigenvalues e matricës P
P_matrix <- t(as.matrix(P))

# Gjej eigenvektorin për eigenvalue = 1
eig <- eigen(P_matrix)
pi_vector <- Re(eig$vectors[, 1])
pi_vector <- pi_vector / sum(pi_vector)

# Emërto
names(pi_vector) <- rownames(P)

cat("Shpërndarja Stacionare π:\n")
print(round(pi_vector, 3))
cat("\nInterpretim: sistemi kalon këtë % të kohës në çdo regjim afatgjatë\n")


# ── RUAJ MASTER ME REGIME ─────────────────────────────
write.csv(master, "data/clean/master_2006_2022.csv", row.names = FALSE)
cat("✅ master_2006_2022.csv u përditësua me kolonën regime\n")



# ── BLLOKU 6: VIZUALIZIM ──────────────────────────────
library(ggplot2)

# Grafiku 1 — Regjimed në kohë
ggplot(master, aes(x = date, y = regime, color = regime)) +
  geom_point(size = 2) +
  labs(title = "Regjimed e Tregut 2006-2022",
       x = "Data", y = "Regjimi") +
  theme_minimal() +
  theme(legend.position = "none")

ggsave("output/01_regjimet.png", width = 10, height = 4)

# Grafiku 2 — Shpërndarja Stacionare
pi_df <- data.frame(
  regime = names(pi_vector),
  value  = as.numeric(pi_vector)
)

ggplot(pi_df, aes(x = regime, y = value, fill = regime)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(round(value*100, 1), "%")), 
            vjust = -0.5) +
  labs(title = "Shpërndarja Stacionare π",
       x = "Regjimi", y = "Probabiliteti") +
  theme_minimal() +
  theme(legend.position = "none")

ggsave("output/02_stacionare.png", width = 6, height = 4)

cat("✅ Grafika u ruajtën në output/\n")





library(DiagrammeR)
grViz("
digraph markov {

  graph [layout = neato,
         overlap = false,
         fontname = 'Helvetica',
         splines = curved]

  node [shape = circle,
        style = filled,
        fixedsize = true,
        width = 1.4,
        fontsize = 13,
        fontcolor = white,
        fontname = 'Helvetica Bold']

  Normal      [fillcolor = '#27AE60', pos = '0,2!']
  Krize       [fillcolor = '#E74C3C', pos = '3,2!']
  Stagflacion [fillcolor = '#2980B9', pos = '0,0!']
  Recesion    [fillcolor = '#E67E22', pos = '3,0!']

  edge [fontsize = 10, fontname = 'Helvetica', color = '#555555']

  // Vetë-unazat — jashtë nyjeve
  Normal      -> Normal      [label = '0.58', tailport = n, headport = n]
  Krize       -> Krize       [label = '0.15', tailport = e, headport = e]
  Stagflacion -> Stagflacion [label = '0.33', tailport = s, headport = s]

  // Normal → të tjerat
  Normal      -> Krize       [label = '0.16']
  Normal      -> Stagflacion [label = '0.23']
  Normal      -> Recesion    [label = '0.04']

  // Krizë → të tjerat
  Krize       -> Normal      [label = '0.63']
  Krize       -> Stagflacion [label = '0.15']
  Krize       -> Recesion    [label = '0.07']

  // Stagflacion → të tjerat
  Stagflacion -> Normal      [label = '0.47']
  Stagflacion -> Krize       [label = '0.12']
  Stagflacion -> Recesion    [label = '0.08']

  // Recesion → të tjerat
  Recesion    -> Normal      [label = '0.40']
  Recesion    -> Stagflacion [label = '0.60']
}
")