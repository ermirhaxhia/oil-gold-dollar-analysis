library(shiny)
library(bslib)
library(plotly)
library(tidyverse)
library(lubridate)
library(visNetwork)
library(DT)
library(echarts4r)
library(shinyWidgets)
library(shinycssloaders)
library(rlang)
library(bsicons)

# ══════════════════════════════════════════════════════
#  PALETA  (TradingView / Bloomberg–inspired navy dark)
# ══════════════════════════════════════════════════════
BG      <- "#111827"   # fundo kryesor
CARD    <- "#1f2937"   # kartë
BORDER  <- "#374151"   # kufi kartë
TEXT    <- "#e5e7eb"   # tekst primar
MUTED   <- "#9ca3af"   # tekst dytësor
C_OIL   <- "#f87171"   # naftë – e kuqe
C_GOLD  <- "#fbbf24"   # ar – antar
C_DXY   <- "#60a5fa"   # dollar – blu
C_GREEN <- "#34d399"   # pozitive / normale
C_ACC   <- "#6366f1"   # accent indigo

CHART_LAYOUT <- list(
  paper_bgcolor = "rgba(0,0,0,0)",
  plot_bgcolor  = "rgba(0,0,0,0)",
  font          = list(color = TEXT, family = "Inter, sans-serif", size = 12),
  xaxis         = list(gridcolor = BORDER, zerolinecolor = BORDER,
                       linecolor = BORDER, tickfont = list(color = MUTED)),
  yaxis         = list(gridcolor = BORDER, zerolinecolor = BORDER,
                       linecolor = BORDER, tickfont = list(color = MUTED)),
  legend        = list(bgcolor = "rgba(0,0,0,0)", font = list(size = 11)),
  margin        = list(t = 10, b = 40, l = 55, r = 20),
  hovermode     = "x unified"
)

# ══════════════════════════════════════════════════════
#  TEMA  bslib
# ══════════════════════════════════════════════════════
my_theme <- bs_theme(
  version = 5,
  bg           = BG,
  fg           = TEXT,
  primary      = C_ACC,
  secondary    = MUTED,
  success      = C_GREEN,
  warning      = C_GOLD,
  danger       = C_OIL,
  info         = C_DXY,
  "navbar-bg"          = CARD,
  "card-bg"            = CARD,
  "card-border-color"  = BORDER,
  "input-bg"           = "#1a2332",
  "input-border-color" = BORDER,
  base_font    = font_google("Inter"),
  heading_font = font_google("JetBrains Mono"),
  code_font    = font_google("Fira Code")
)

# ══════════════════════════════════════════════════════
#  NGARKIMI I TË DHËNAVE
# ══════════════════════════════════════════════════════
master <- read_csv("data/master_2006_2022.csv") |>
  mutate(date = ymd(date))

# ══════════════════════════════════════════════════════
#  FUNKSIONE NDIHMËSE
# ══════════════════════════════════════════════════════

# Karta grafiku: titulli lart, shpjegimi poshtë grafikut
chart_card <- function(..., title, subtitle = NULL, full_screen = TRUE) {
  card(
    full_screen = full_screen,
    card_header(
      class = "border-0",
      style = paste0("background:", CARD, "; padding:0.85rem 1rem 0.5rem;"),
      tags$p(title, class = "mb-0 fw-semibold",
             style = "font-size:0.97rem; letter-spacing:0.01em;")
    ),
    card_body(class = "pt-2 pb-0", ...),
    if (!is.null(subtitle))
      div(
        class = "px-4 py-3",
        style = paste0("background:", CARD, "; border-top:1px solid ", BORDER, ";"),
        tags$p(subtitle, class = "mb-0",
               style = paste0("font-size:0.85rem; color:#94a3b8;",
                              " line-height:1.65; font-style:italic;"))
      )
  )
}

# Karta KPI (Bloomberg-style: bord majtas + numër i madh + label)
kpi_card <- function(id, label, color, description) {
  div(
    class = "card h-100",
    style = paste0("background:", CARD, "; border:1px solid ", BORDER,
                   "; border-left:4px solid ", color, "; border-radius:8px;"),
    div(
      class = "card-body py-3 px-4",
      tags$p(label, class = "mb-1 small",
             style = paste0("color:", MUTED, "; font-size:0.75rem; text-transform:uppercase; letter-spacing:0.06em;")),
      div(
        textOutput(id, inline = TRUE),
        class = "fw-bold",
        style = paste0("font-family:'JetBrains Mono',monospace; font-size:1.7rem; color:", TEXT, ";")
      ),
      tags$p(description, class = "mb-0 mt-1",
             style = paste0("font-size:0.74rem; color:", MUTED, ";"))
    )
  )
}

# ── FOOTER ────────────────────────────────────────────
AUTHOR_EMAIL    <- "ermir.haxhia10@gmail.com"
AUTHOR_GITHUB   <- "https://github.com/ermirhaxhia"
AUTHOR_LINKEDIN <- "https://www.linkedin.com/in/ermir-haxhia-b988212b5"

make_footer <- function() {
  div(
    class = "mt-5",
    style = paste0(
      "border-top: 1px solid ", BORDER, ";",
      " padding: 28px 0 16px;",
      " margin-top: 2rem;"
    ),
    div(
      class = "d-flex flex-wrap justify-content-between align-items-start gap-3",
      div(
        tags$p("Ermir Haxhia",
               style = paste0("font-family:'JetBrains Mono',monospace;",
                              " font-weight:700; font-size:0.97rem;",
                              " color:", TEXT, "; margin:0 0 4px;")),
        tags$p("Student MSc · Inxhinieri Matematik dhe Informatik",
               style = paste0("font-size:0.8rem; color:", MUTED, "; margin:0 0 2px;")),
        tags$p("Departamenti i Matematikës së Aplikuar · FSHN · Universiteti i Tiranës",
               style = paste0("font-size:0.78rem; color:", MUTED, "; margin:0;"))
      ),
      div(
        class = "d-flex gap-3 align-items-center flex-wrap",
        tags$a(
          href    = paste0("mailto:", AUTHOR_EMAIL),
          title   = "Email",
          onclick = paste0("window.location.href='mailto:", AUTHOR_EMAIL, "'; return false;"),
          style   = paste0(
            "display:inline-flex; align-items:center; gap:6px;",
            " padding:6px 14px; border-radius:6px;",
            " border:1px solid ", BORDER, ";",
            " background:#1a2436; color:", TEXT, ";",
            " font-size:0.8rem; text-decoration:none;",
            " cursor:pointer;"
          ),
          tags$span(style = paste0("color:", C_ACC, "; font-weight:600;"), "Email"),
          tags$span(AUTHOR_EMAIL, style = paste0("color:", MUTED, ";"))
        ),
        tags$a(
          href    = AUTHOR_GITHUB,
          target  = "_blank",
          rel     = "noopener noreferrer",
          onclick = paste0("window.open('", AUTHOR_GITHUB, "','_blank'); return false;"),
          style   = paste0(
            "display:inline-flex; align-items:center; gap:6px;",
            " padding:6px 14px; border-radius:6px;",
            " border:1px solid ", BORDER, ";",
            " background:#1a2436; color:", TEXT, ";",
            " font-size:0.8rem; text-decoration:none;",
            " cursor:pointer;"
          ),
          tags$span(style = paste0("color:", C_GREEN, "; font-weight:600;"), "GitHub"),
          tags$span("ermirhaxhia", style = paste0("color:", MUTED, ";"))
        ),
        tags$a(
          href    = AUTHOR_LINKEDIN,
          target  = "_blank",
          rel     = "noopener noreferrer",
          onclick = paste0("window.open('", AUTHOR_LINKEDIN, "','_blank'); return false;"),
          style   = paste0(
            "display:inline-flex; align-items:center; gap:6px;",
            " padding:6px 14px; border-radius:6px;",
            " border:1px solid ", BORDER, ";",
            " background:#1a2436; color:", TEXT, ";",
            " font-size:0.8rem; text-decoration:none;",
            " cursor:pointer;"
          ),
          tags$span(style = paste0("color:", C_DXY, "; font-weight:600;"), "LinkedIn"),
          tags$span("Ermir Haxhia", style = paste0("color:", MUTED, ";"))
        )
      )
    ),
    tags$p(
      paste0("© ", format(Sys.Date(), "%Y"),
             "  ·  Lëvizja e Çmimit të Arit, Naftës dhe Indeksit të Dollarit në Bursë  ·  të dhëna: FRED, Macrotrends"),
      style = paste0("font-size:0.72rem; color:#4b5563; margin-top:16px; margin-bottom:0;")
    )
  )
}

# ══════════════════════════════════════════════════════
#  UI
# ══════════════════════════════════════════════════════
ui <- page_navbar(
  title = tags$div(
    style = "display:flex; align-items:center; gap:12px;",
    tags$div(
      style = paste0("width:4px; height:32px; background:linear-gradient(180deg,",
                     C_ACC, ",", C_GREEN, "); border-radius:2px;")
    ),
    tags$div(
      tags$div("Ar · Naftë · Dollar",
               class = "navbar-title-main",
               style = paste0("font-weight:700; font-size:0.95rem;",
                              " font-family:'JetBrains Mono',monospace; color:", TEXT, ";")),
      tags$div("Lëvizja e Çmimit në Bursë  |  2006 – 2024",
               class = "navbar-subtitle",
               style = paste0("font-size:0.70rem; color:", MUTED, "; margin-top:1px;"))
    )
  ),
  theme    = my_theme,
  bg       = CARD,
  inverse  = TRUE,
  fillable = FALSE,
  
  # ── TAB 0 · ABSTRAKT ─────────────────────────────────
  nav_panel("Abstrakt",
            div(class = "container py-4",
                div(class = "text-center mb-5",
                    tags$h1("Lëvizja e Çmimit të Arit, Naftës dhe Indeksit të Dollarit në Bursë në Kohë Krize dhe në Kohë Normale",
                            style = paste0("font-family:'JetBrains Mono',monospace;",
                                           " font-size:1.4rem; font-weight:700; color:", TEXT, ";")),
                    tags$p("Ar (XAU/USD)  ·  Naftë WTI  ·  Dollar Index (DXY)  |  Periudha 2006 – 2024",
                           style = paste0("font-size:1rem; color:", MUTED, "; margin-top:6px;")),
                    div(style = paste0("width:60px; height:3px; background:linear-gradient(90deg,",
                                       C_ACC, ",", C_GREEN, "); margin:16px auto 0; border-radius:2px;"))
                ),
                
                card(
                  style = paste0("background:", CARD, "; border:1px solid ", BORDER,
                                 "; border-left:4px solid ", C_ACC, "; border-radius:8px;"),
                  card_body(
                    tags$h5("Abstrakt", class = "fw-semibold mb-3",
                            style = paste0("color:", TEXT, "; font-size:0.95rem;",
                                           " text-transform:uppercase; letter-spacing:0.08em;")),
                    tags$p(
                      "Ky projekt analizon dinamikën e tregut global të lëndëve të para nëpërmjet tre treguesve",
                      " kryesorë: çmimi i naftës bruto WTI, çmimi i arit (XAU/USD) dhe Indeksi i Dollarit Amerikan (DXY).",
                      " Duke përdorur të dhëna mujore për periudhën 2006–2022, ndërtojmë një model Zinxhir Markovi me",
                      " katër regjime ekonomike (Normal, Krizë, Recesion, Stagflacion) të drejtuar nga goditje stokastike",
                      " të modeluara si Proces Poisson. Volatiliteti modelohet me Lëvizje Browniane Gjeometrike (GBM) dhe",
                      " parashikimet afatshkurtra prodhohen nëpërmjet simulimeve Monte Carlo (10 000 rrugë). Analiza",
                      " plotësohet me regresion OLS dhe studim korrelacioni ndër-aktiv.",
                      class = "mb-0",
                      style = paste0("font-size:0.9rem; color:#cbd5e1; line-height:1.8;")
                    )
                  )
                ),
                
                div(class = "mt-4"),
                
                layout_columns(
                  col_widths = c(5, 7),
                  card(
                    style = paste0("background:", CARD, "; border:1px solid ", BORDER, "; border-radius:8px;"),
                    card_header(
                      style = paste0("background:", CARD, "; border-bottom:1px solid ", BORDER, ";"),
                      tags$h6("Projekti", class = "mb-0 fw-semibold",
                              style = paste0("color:", TEXT, "; font-size:0.88rem;",
                                             " text-transform:uppercase; letter-spacing:0.07em;"))
                    ),
                    card_body(
                      tags$table(class = "w-100",
                                 style = paste0("font-size:0.87rem; color:#cbd5e1;"),
                                 tags$tr(
                                   tags$td("Lënda:", style = paste0("color:", MUTED, "; padding:5px 12px 5px 0; width:40%;"),
                                           class = "fw-semibold"),
                                   tags$td("Proceset Stokastike")
                                 ),
                                 tags$tr(
                                   tags$td("Institucioni:", style = paste0("color:", MUTED, "; padding:5px 12px 5px 0;"),
                                           class = "fw-semibold"),
                                   tags$td("Universiteti i Tiranës")
                                 ),
                                 tags$tr(
                                   tags$td("Periudha:", style = paste0("color:", MUTED, "; padding:5px 12px 5px 0;"),
                                           class = "fw-semibold"),
                                   tags$td("2006 – 2024")
                                 ),
                                 tags$tr(
                                   tags$td("Metodologji:", style = paste0("color:", MUTED, "; padding:5px 12px 5px 0;"),
                                           class = "fw-semibold"),
                                   tags$td("Markov, Poisson, GBM, Monte Carlo, OLS")
                                 ),
                                 tags$tr(
                                   tags$td("Mjetet:", style = paste0("color:", MUTED, "; padding:5px 12px 5px 0;"),
                                           class = "fw-semibold"),
                                   tags$td("R / RStudio / Shiny")
                                 )
                      )
                    )
                  ),
                  card(
                    style = paste0("background:", CARD, "; border:1px solid ", BORDER, "; border-radius:8px;"),
                    card_header(
                      style = paste0("background:", CARD, "; border-bottom:1px solid ", BORDER, ";"),
                      tags$h6("Burimet e të Dhënave", class = "mb-0 fw-semibold",
                              style = paste0("color:", TEXT, "; font-size:0.88rem;",
                                             " text-transform:uppercase; letter-spacing:0.07em;"))
                    ),
                    card_body(
                      tags$table(class = "w-100",
                                 style = paste0("font-size:0.85rem; color:#cbd5e1;"),
                                 tags$tr(
                                   tags$td(style = "padding:6px 0; vertical-align:top; width:35%;",
                                           tags$span("Nafta WTI", style = paste0("color:", C_OIL, "; font-weight:600;"))
                                   ),
                                   tags$td(style = "padding:6px 0;",
                                           tags$a("FRED · DCOILWTICO", href="https://fred.stlouisfed.org/series/DCOILWTICO",
                                                  target="_blank",
                                                  style = paste0("color:", C_ACC, "; text-decoration:none;",
                                                                 " border-bottom:1px dotted ", C_ACC, ";"))
                                   )
                                 ),
                                 tags$tr(
                                   tags$td(style = "padding:6px 0; vertical-align:top;",
                                           tags$span("Nafta Brent", style = paste0("color:", C_OIL, "; font-weight:600;"))
                                   ),
                                   tags$td(style = "padding:6px 0;",
                                           tags$a("FRED · DCOILBRENTEU",
                                                  href="https://fred.stlouisfed.org/series/DCOILBRENTEU",
                                                  target="_blank",
                                                  style = paste0("color:", C_ACC, "; text-decoration:none;",
                                                                 " border-bottom:1px dotted ", C_ACC, ";"))
                                   )
                                 ),
                                 tags$tr(
                                   tags$td(style = "padding:6px 0; vertical-align:top;",
                                           tags$span("Dollar Index", style = paste0("color:", C_DXY, "; font-weight:600;"))
                                   ),
                                   tags$td(style = "padding:6px 0;",
                                           tags$a("FRED · DTWEXBGS",
                                                  href="https://fred.stlouisfed.org/series/DTWEXBGS",
                                                  target="_blank",
                                                  style = paste0("color:", C_ACC, "; text-decoration:none;",
                                                                 " border-bottom:1px dotted ", C_ACC, ";"))
                                   )
                                 ),
                                 tags$tr(
                                   tags$td(style = "padding:6px 0; vertical-align:top;",
                                           tags$span("Çmimi i Arit", style = paste0("color:", C_GOLD, "; font-weight:600;"))
                                   ),
                                   tags$td(style = "padding:6px 0;",
                                           tags$a("Macrotrends · Gold Prices",
                                                  href="https://www.macrotrends.net/1333/historical-gold-prices-100-year-chart",
                                                  target="_blank",
                                                  style = paste0("color:", C_ACC, "; text-decoration:none;",
                                                                 " border-bottom:1px dotted ", C_ACC, ";"))
                                   )
                                 ),
                                 tags$tr(
                                   tags$td(style = "padding:6px 0; vertical-align:top;",
                                           tags$span("Norma Reale", style = paste0("color:", C_GREEN, "; font-weight:600;"))
                                   ),
                                   tags$td(style = "padding:6px 0;",
                                           tags$a("FRED · DFII10",
                                                  href="https://fred.stlouisfed.org/series/DFII10",
                                                  target="_blank",
                                                  style = paste0("color:", C_ACC, "; text-decoration:none;",
                                                                 " border-bottom:1px dotted ", C_ACC, ";"))
                                   )
                                 )
                      )
                    )
                  )
                ),
                
                div(class = "mt-4"),
                
                card(
                  style = paste0("background:", CARD, "; border:1px solid ", BORDER, "; border-radius:8px;"),
                  card_header(
                    style = paste0("background:", CARD, "; border-bottom:1px solid ", BORDER, ";"),
                    tags$h6("Pasqyrë e Dataset-it", class = "mb-0 fw-semibold",
                            style = paste0("color:", TEXT, "; font-size:0.88rem;",
                                           " text-transform:uppercase; letter-spacing:0.07em;"))
                  ),
                  card_body(
                    DTOutput("abstract_table")
                  )
                ),
                
                make_footer()
            )
  ),
  
  # ── TAB 1 · TREGU ────────────────────────────────────
  nav_panel("Tregu",
            div(class = "container-fluid py-3",
                layout_columns(
                  col_widths = c(4, 4, 4),
                  kpi_card("kpi_oil",  "Nafta Bruto WTI",       C_OIL,
                           "Dollare amerikanë për fuçi (bbl) — vlera e fundit e serisë"),
                  kpi_card("kpi_gold", "Ar (XAU/USD)",           C_GOLD,
                           "Dollare amerikanë për unce — treguesi kryesor i 'sigurt'"),
                  kpi_card("kpi_dxy",  "Dollar Index (DXY)",     C_DXY,
                           "Indeksi i fuqisë së dollarit ndaj 6 monedhave kryesore")
                ),
                div(class = "mt-3"),
                chart_card(
                  title    = "Evoluimi Historik i Çmimeve · 2006 – 2022",
                  subtitle = paste(
                    "Çmimet mujore të tre treguesve kryesorë. Dy boshtet djathtas (Ar, DXY) janë",
                    "ndarë nga nafta për legjibilitet. Zonat e kuqe tregojnë periudhat e regjimuara si 'Krizë'",
                    "sipas modelit Markov. Vini re si kriza financiare e 2008-09 dhe COVID-19 (2020)",
                    "prodhuan rënie dramatike të naftës, ndërsa ari lëvizi si aktiv mbrojtës (safe-haven)."
                  ),
                  sliderInput("date_range", NULL,
                              min = ymd("2006-01-01"), max = ymd("2022-01-01"),
                              value = c(ymd("2006-01-01"), ymd("2022-01-01")),
                              timeFormat = "%Y-%m", width = "100%"),
                  plotlyOutput("main_chart", height = "400px") |> withSpinner(color = C_ACC)
                )
            )
  ),
  
  # ── TAB 2 · REGJIMET ─────────────────────────────────
  nav_panel("Regjimet",
            div(class = "container-fluid py-3",
                layout_columns(
                  col_widths = c(6, 6),
                  chart_card(
                    title    = "Zinxhiri i Markovit",
                    subtitle = paste(
                      "Çdo nyje = regjim ekonomik. Trashësia e shigjetës është proporcionale me",
                      "probabilitetin e kalimit. Regjimet me kalime të forta mes veti (p.sh. Recesion → Stagflacion)",
                      "tregojnë se ekonomia nuk del lehtë nga gjendje negative."
                    ),
                    visNetworkOutput("network_plot", height = "380px") |>
                      withSpinner(color = C_GREEN)
                  ),
                  chart_card(
                    title    = "Matrica e Kalimit P",
                    subtitle = paste(
                      "Çdo rresht tregon nga cili regjim jemi sot; çdo kolonë tregon ku mund të shkojmë muajin tjetër.",
                      "Numrat tregojnë gjasat e kalimit — vlera më e lartë në diagonale do të thotë se ai regjim tenton të vazhdojë.",
                      "Regjimi Normal është më i qëndrueshmi: ka 57.5% gjasa të mbetet normal edhe muajin pasardhës."
                    ),
                    DTOutput("table_P"),
                    tags$hr(class = "my-3"),
                    tags$div(
                      style = paste0("background:#1a2436; border-left:3px solid ", C_ACC,
                                     "; padding:10px 14px; border-radius:0 6px 6px 0; font-size:0.8rem; color:", MUTED, ";"),
                      tags$strong(style = paste0("color:", TEXT, ";"), "Shpërndarje afatgjatë (stacionare):"),
                      tags$br(),
                      tags$span("Normal 54.6%", style = paste0("color:", C_GREEN, ";")), " · ",
                      tags$span("Stagflacion 26.1%", style = paste0("color:", C_DXY, ";")), " · ",
                      tags$span("Krizë 14.0%", style = paste0("color:", C_OIL, ";")), " · ",
                      tags$span("Recesion 5.2%", style = paste0("color:", C_GOLD, ";")),
                      tags$br(),
                      tags$span(style = paste0("color:", MUTED, ";"),
                                "Sipas modelit, ekonomia pritet të kalojë rreth gjysmës së kohës në gjendje normale.")
                    )
                  )
                ),
                div(class = "mt-3"),
                chart_card(
                  title    = "Çmimi i Naftës sipas Regjimit Ekonomik",
                  subtitle = paste(
                    "Çdo pikë është një muaj — ngjyra tregon regjimin ekonomik të atij muaji.",
                    "Periudhat e kuqe (Krizë) korrespondojnë me rëniet e mëdha të çmimit.",
                    "Vini re se pas çdo krizë, tregu ka kaluar nëpër Stagflacion ose Recesion para se të kthehet në Normal."
                  ),
                  plotlyOutput("regime_timeline", height = "280px")
                )
            )
  ),
  
  # ── TAB 3 · KRIZAT ───────────────────────────────────
  nav_panel("Krizat",
            div(class = "container-fluid py-3",
                layout_columns(
                  col_widths = c(4, 4, 4),
                  value_box(
                    title    = "Intensiteti λ (Lambda)",
                    value    = textOutput("vbox_lambda"),
                    showcase = bs_icon("lightning-charge-fill"),
                    theme    = value_box_theme(bg = "#2d1a1a", fg = C_OIL),
                    p("Kriza mesatarisht për vit kalendarik (2006–2022)", class = "small")
                  ),
                  value_box(
                    title    = "p-value Chi-Square",
                    value    = "0.1813",
                    showcase = bs_icon("check-circle-fill"),
                    theme    = value_box_theme(bg = "#112d24", fg = C_GREEN),
                    p("p > 0.05 — shpërndarja Poisson vërtetohet statistikisht", class = "small")
                  ),
                  value_box(
                    title    = "Muaj të Regjistruar si Krizë",
                    value    = textOutput("vbox_ncrisis"),
                    showcase = bs_icon("calendar-event-fill"),
                    theme    = value_box_theme(bg = "#2d2510", fg = C_GOLD),
                    p("Nga 192 muaj total në periudhën e studimit", class = "small")
                  )
                ),
                div(class = "mt-3"),
                chart_card(
                  title    = "Çmimi i Naftës WTI me Periudhat e Krizave",
                  subtitle = paste(
                    "Vija gri tregon lëvizjen e çmimit të naftës gjatë viteve 2006-2022.",
                    "Zonat e hijëzuara me të kuqe janë muajt e identifikuar si krizë nga modeli ynë.",
                    "Vini re se çdo herë që çmimi ka rënë ndjeshëm, ajo periudhë është shënuar si krizë."
                  ),
                  plotlyOutput("plot_crisis_timeline", height = "300px") |> withSpinner(color = C_OIL)
                ),
                div(class = "mt-3"),
                layout_columns(
                  col_widths = c(6, 6),
                  chart_card(
                    title    = "Vërtetimi i Modelit të Krizave",
                    subtitle = paste(
                      "Shtylla portokalli tregon sa vite reale kishin 0, 1, 2... muaj krizë.",
                      "Vija blu tregon çfarë parashikon modeli teorik.",
                      "Përputhja e mirë midis tyre konfirmon se modeli i krizave është i saktë."
                    ),
                    echarts4rOutput("plot_poisson_dist", height = "300px")
                  ),
                  chart_card(
                    title    = "Muaj Krizë sipas Vitit",
                    subtitle = paste(
                      "Vitet 2013 dhe 2021 shënojnë numrin më të lartë të muajve të krizës — nga 4 muaj secilin.",
                      "Vitet 2014 dhe 2020 nuk regjistruan asnjë muaj krizë sipas modelit tonë."
                    ),
                    DTOutput("table_crisis")
                  )
                )
            )
  ),
  
  # ── TAB 4 · VOLATILITETI ─────────────────────────────
  nav_panel("Volatiliteti",
            div(class = "container-fluid py-3",
                layout_columns(
                  col_widths = c(4, 4, 4),
                  kpi_card("vbox_mu_oil",  "Kthim Mesatar Vjetor — Naftë",  C_OIL,
                           "Sa ka ndryshuar mesatarisht çmimi i naftës çdo vit"),
                  kpi_card("vbox_mu_gold", "Kthim Mesatar Vjetor — Ar",     C_GOLD,
                           "Sa ka ndryshuar mesatarisht çmimi i arit çdo vit"),
                  kpi_card("vbox_mu_dxy",  "Kthim Mesatar Vjetor — Dollar", C_DXY,
                           "Sa ka ndryshuar mesatarisht vlera e dollarit çdo vit")
                ),
                div(class = "mt-2"),
                layout_columns(
                  col_widths = c(4, 4, 4),
                  kpi_card("vbox_sig_oil",  "Luhatja Vjetore — Naftë",  C_OIL,
                           "Nafta është aktivi më i paqëndrueshëm — luhatja më e madhe"),
                  kpi_card("vbox_sig_gold", "Luhatja Vjetore — Ar",     C_GOLD,
                           "Ari është shumë më i qëndrueshëm se nafta"),
                  kpi_card("vbox_sig_dxy",  "Luhatja Vjetore — Dollar", C_DXY,
                           "Dollari ka luhatjen më të ulët nga të tre aktivet")
                ),
                div(class = "mt-3"),
                layout_sidebar(
                  sidebar = sidebar(
                    title = NULL,
                    width = 230,
                    style = paste0("background:", CARD, "; border-color:", BORDER, ";"),
                    tags$p("Zgjidh Asset", class = "small fw-semibold mb-2",
                           style = paste0("color:", MUTED, ";")),
                    pickerInput("asset_select", NULL,
                                choices  = c("Naftë WTI" = "oil", "Ar (Gold)" = "gold",
                                             "Dollar (DXY)" = "dxy"),
                                selected = "oil",
                                width    = "100%",
                                options  = list(style = "btn-outline-secondary btn-sm")),
                    hr(style = paste0("border-color:", BORDER, ";")),
                    tags$p("Statistikat Mujore (%)", class = "small fw-semibold mb-2",
                           style = paste0("color:", MUTED, ";")),
                    tableOutput("asset_stats")
                  ),
                  chart_card(
                    title    = "Ndryshimet Mujore të Çmimeve",
                    subtitle = paste(
                      "Çdo pikë tregon sa për qind ka ndryshuar çmimi nga muaji paraardhës.",
                      "Pikat larg zeros tregojnë muajt e goditjeve të mëdha — si kriza financiare 2008 apo COVID-19.",
                      "Vini re si periudhat e trazuara tentojnë të ndjekin njëra-tjetrën."
                    ),
                    plotlyOutput("plot_logret", height = "380px")
                  )
                ),
                div(class = "mt-3"),
                layout_columns(
                  col_widths = c(5, 7),
                  chart_card(
                    title    = "Matrica e Korrelacionit",
                    subtitle = paste(
                      "Blu tregon se dy aktive lëvizin në të njëjtin drejtim.",
                      "E kuqe tregon lëvizje të kundërta — kur njëri bie, tjetri ngrihet.",
                      "Dollari i dobët tenton të rrisë çmimin e naftës dhe të arit."
                    ),
                    plotlyOutput("plot_cor_heatmap", height = "350px")
                  ),
                  chart_card(
                    title    = "Shpërndarja e Ndryshimeve vs Kurba Normale",
                    subtitle = paste(
                      "Shtylla portokalli tregon shpërndarjen reale të ndryshimeve të çmimeve.",
                      "Vija e bardhë tregon si do dukej shpërndarja nëse tregu do ishte plotësisht i rregullt.",
                      "Dallimet mes tyre tregojnë se ngjarjet ekstreme ndodhin më shpesh se sa pritet."
                    ),
                    echarts4rOutput("plot_hist_normal", height = "350px")
                  )
                )
            )
  ),
  
  # ── TAB 5 · SIMULIMI ─────────────────────────────────
  nav_panel("Simulimi",
            div(class = "container-fluid py-3",
                layout_columns(
                  col_widths = c(3, 9),
                  card(
                    fill = FALSE,
                    style = paste0("background:", CARD, "; border-color:", BORDER, ";"),
                    card_header("Parametrat", style = paste0("font-size:0.85rem; color:", MUTED,
                                                             "; background:", CARD, "; border-color:", BORDER, ";")),
                    card_body(
                      class = "pt-2",
                      tags$div(style = "padding: 4px 0;",
                               sliderInput("monte_nsim", "Numri i Simulimeve", 
                                           min = 50, max = 500, value = 200, step = 50,
                                           width = "100%")
                      ),
                      hr(style = paste0("border-color:", BORDER, "; margin:10px 0;")),
                      pickerInput("monte_asset", "Asset", width = "100%",
                                  choices  = c("Naftë WTI" = "oil", "Ar (Gold)" = "gold",
                                               "Dollar Index" = "dxy"),
                                  selected = "oil",
                                  options  = list(style = "btn-outline-secondary btn-sm")),
                      pickerInput("monte_scenario", "Skenari", width = "100%",
                                  choices  = c("Bazë (historik)" = "baseline",
                                               "Krizë Hormuz 2026" = "hormuz"),
                                  selected = "baseline",
                                  options  = list(style = "btn-outline-secondary btn-sm")),
                      hr(style = paste0("border-color:", BORDER, "; margin:10px 0;")),
                      tags$div(class = "small",
                               tags$div(class = "d-flex justify-content-between py-1",
                                        style = paste0("border-bottom:1px solid ", BORDER, ";"),
                                        tags$span("Mesatarja", style = paste0("color:", MUTED, ";")),
                                        tags$strong(textOutput("vbox_sim_mean", inline=TRUE), style = paste0("color:", TEXT, ";"))
                               ),
                               tags$div(class = "d-flex justify-content-between py-1",
                                        style = paste0("border-bottom:1px solid ", BORDER, ";"),
                                        tags$span("Kufi i poshtëm 2.5%", style = paste0("color:", MUTED, ";")),
                                        tags$strong(textOutput("vbox_sim_ci_low", inline=TRUE), style = paste0("color:", C_GREEN, ";"))
                               ),
                               tags$div(class = "d-flex justify-content-between py-1",
                                        tags$span("Kufi i sipërm 97.5%", style = paste0("color:", MUTED, ";")),
                                        tags$strong(textOutput("vbox_sim_ci_hi", inline=TRUE), style = paste0("color:", C_OIL, ";"))
                               )
                      ),
                      hr(style = paste0("border-color:", BORDER, "; margin:10px 0;")),
                      downloadButton("download_sim", "Shkarko CSV",
                                     class = "btn-sm btn-outline-success w-100")
                    )
                  ),
                  chart_card(
                    title    = "Parashikim Monte Carlo — 12 Muaj Pasardhëse",
                    subtitle = paste(
                      "Çdo vijë e hollë përfaqëson një skenar të mundshëm të çmimit gjatë 12 muajve të ardhshëm.",
                      "Zona e errët mbulon 95% të skenarëve — pra tregu pritet të mbetet brenda asaj zone me gjasë të lartë.",
                      "Vija e bardhë është mesatarja e të gjithë skenarëve."
                    ),
                    full_screen = TRUE,
                    plotlyOutput("plot_fan", height = "450px") |> withSpinner(color = C_ACC)
                  )
                )
            )
  ),
  
  # ── TAB 6 · REGRESIONI ───────────────────────────────
  nav_panel("Regresioni",
            div(class = "container-fluid py-3",
                layout_columns(
                  col_widths = c(4, 4, 4),
                  kpi_card("vbox_r2",    "R² — Fuqia Shpjeguese",    C_DXY,
                           "Nafta + Dollari shpjegojnë kaq % të variacionit të çmimit të Arit"),
                  kpi_card("vbox_fstat", "F-Statistika",              C_GREEN,
                           "Teston nëse bashkësia e variablave ka fuqi shpjeguese të rëndësishme"),
                  kpi_card("vbox_pval",  "p-value (F-test)",          C_OIL,
                           "Probabiliteti i hipotezës nule: p < 0.001 konfirmon modelin")
                ),
                div(class = "mt-3"),
                layout_columns(
                  col_widths = c(5, 7),
                  chart_card(
                    title    = "Ndikimi i Naftës dhe Dollarit mbi Çmimin e Arit",
                    subtitle = paste(
                      "Çdo shirit tregon sa ndikon secili faktor mbi çmimin e arit dhe sa i sigurt është ai ndikim.",
                      "Nëse shiriti nuk e prek vijën e kuqe të zeros, faktori është i rëndësishëm.",
                      "Nafta ndikon pozitivisht; dollari i fortë ul çmimin e arit."
                    ),
                    plotlyOutput("plot_coef", height = "300px")
                  ),
                  chart_card(
                    title    = "Tabela e Regresionit  ·  Ar ~ Naftë + Dollar",
                    subtitle = paste(
                      "Tabela tregon fuqinë shpjeguese të modelit dhe rëndësinë statistikore të secilit faktor.",
                      "Jeshile tregon faktorë shumë të rëndësishëm; i verdhë — mjaft të rëndësishëm; i kuq — jo të rëndësishëm."
                    ),
                    DTOutput("table_ols")
                  )
                ),
                div(class = "mt-3"),
                chart_card(
                  title    = "Si Ndryshon Ndikimi i Naftës dhe Dollarit me Kalimin e Kohës",
                  subtitle = paste(
                    "Ky grafik tregon nëse lidhja midis arit, naftës dhe dollarit ka qenë e qëndrueshme gjatë viteve.",
                    "Lëvizjet e mëdha përputhen me periudhat e krizave — si 2008 dhe rënia e naftës 2014-16."
                  ),
                  plotlyOutput("plot_rolling", height = "280px")
                ),
                div(class = "mt-3"),
                chart_card(
                  title    = "Gabimet e Modelit  ·  Ar ~ Naftë + Dollar",
                  subtitle = paste(
                    "Pikat tregojnë diferencën midis çmimit real të arit dhe çmimit që parashikoi modeli.",
                    "Shpërndarja e rastësishme rreth zeros tregon se modeli funksionon mirë.",
                    "Grupime ose tendenca tregojnë se ka faktorë të tjerë që modeli nuk i ka kapur."
                  ),
                  plotlyOutput("plot_resid", height = "220px"),
                  full_screen = FALSE
                )
            )
  ),
  
  # ── TAB 7 · KORRELACIONI ─────────────────────────────
  nav_panel("Korrelacioni",
            div(class = "container-fluid py-3",
                layout_columns(
                  col_widths = c(4, 4, 4),
                  kpi_card("vbox_cor_oil_gold", "ρ  Naftë ↔ Ar",     C_GOLD,
                           "Lidhja midis dy lëndëve të para — ngjyrosur nga doll. amerikan"),
                  kpi_card("vbox_cor_oil_dxy",  "ρ  Naftë ↔ Dollar", C_DXY,
                           "Dollari i fortë shtrenjton importet: naftë zakonisht bie"),
                  kpi_card("vbox_cor_gold_dxy", "ρ  Ar ↔ Dollar",    C_OIL,
                           "Lidhja klasike inverse: dollari i fortë → ar i lirë")
                ),
                div(class = "mt-2"),
                sliderInput("corr_range", NULL,
                            min = ymd("2006-01-01"), max = ymd("2022-01-01"),
                            value = c(ymd("2006-01-01"), ymd("2022-01-01")),
                            timeFormat = "%Y-%m", width = "100%"),
                layout_columns(
                  col_widths = c(5, 7),
                  chart_card(
                    title    = "Heatmap i Korrelacionit",
                    subtitle = paste(
                      "E kuqe tregon se dy aktive lëvizin në drejtime të kundërta — kur njëri bie, tjetri ngrihet.",
                      "Blu tregon lëvizje në të njëjtin drejtim.",
                      "Dollari ka lidhje të kundërt me të dy aktivet — kur dollari forcohet, nafta dhe ari zakonisht bien."
                    ),
                    plotlyOutput("plot_corr_heatmap2", height = "340px")
                  ),
                  chart_card(
                    title    = "Krahasimi Mujor: Naftë kundër Arit",
                    subtitle = paste(
                      "Çdo pikë përfaqëson një muaj — pozicioni i saj tregon si ndryshuan të dyja çmimet atë muaj.",
                      "Shpërndarja e gjerë dhe pa drejtim të qartë tregon se nafta dhe ari shpesh lëvizin në mënyrë të pavarur.",
                      "Vija e kaltër është tendenca e përgjithshme."
                    ),
                    plotlyOutput("plot_scatter_matrix", height = "340px")
                  )
                ),
                div(class = "mt-3"),
                chart_card(
                  title    = "Si Ndryshon Lidhja Midis Aktiveve me Kalimin e Kohës",
                  subtitle = paste(
                    "Lidhja midis aktiveve nuk është e njëjtë gjithnjë — ndryshon sipas situatës ekonomike.",
                    "Momentin kur vija kalon nga e kuqe në blu (ose anasjelltas) është shenjë e rëndësishme:",
                    "lidhja midis tyre ka ndryshuar dhe mund të ndikojë strategjinë e investimeve."
                  ),
                  pickerInput("corr_pair", NULL,
                              choices  = c("Naftë ↔ Ar" = "oil_gold",
                                           "Naftë ↔ Dollar" = "oil_dxy",
                                           "Ar ↔ Dollar" = "gold_dxy"),
                              options  = list(style = "btn-outline-secondary btn-sm")),
                  plotlyOutput("plot_rolling_corr", height = "260px")
                )
            )
  ),
  
  # ── TAB 8 · KONKLUZIONET ─────────────────────────────
  nav_panel("Konkluzionet",
            div(class = "container py-4",
                div(class = "text-center mb-5",
                    tags$h2("Konkluzionet e Studimit",
                            style = paste0("font-family:'JetBrains Mono',monospace;",
                                           " font-size:1.3rem; font-weight:700; color:", TEXT, ";")),
                    tags$p("Lëvizja e Çmimit të Arit, Naftës dhe Indeksit të Dollarit në Bursë",
                           style = paste0("font-size:0.85rem; color:", MUTED, "; margin-top:4px;")),
                    div(style = paste0("width:50px; height:3px; background:linear-gradient(90deg,",
                                       C_ACC, ",", C_GREEN, "); margin:12px auto 0; border-radius:2px;"))
                ),
                
                layout_columns(
                  col_widths = c(6, 6),
                  
                  div(class = "card h-100",
                      style = paste0("background:", CARD, "; border:1px solid ", BORDER,
                                     "; border-left:4px solid ", C_OIL, "; border-radius:8px;"),
                      div(class = "card-body",
                          tags$h6("1 · Regjimet e Tregut",
                                  style = paste0("color:", C_OIL, "; font-size:0.9rem;",
                                                 " text-transform:uppercase; letter-spacing:0.06em; font-weight:700;")),
                          tags$p("Ekonomia globale kalon nëpër katër faza: Normale, Krizë, Recesion dhe Stagflacion.",
                                 " Gjendia Normale dominon me rreth 55% të kohës, por periudhat e Krizës dhe Stagflacionit",
                                 " zënë bashkërisht deri 40% të ciklit ekonomik.",
                                 " Kjo tregon se paqëndrueshmëria nuk është e rrallë — por pjesë e rregullt e tregut.",
                                 style = paste0("font-size:0.87rem; color:#cbd5e1; line-height:1.7; margin-bottom:0;"))
                      )
                  ),
                  
                  div(class = "card h-100",
                      style = paste0("background:", CARD, "; border:1px solid ", BORDER,
                                     "; border-left:4px solid ", C_GOLD, "; border-radius:8px;"),
                      div(class = "card-body",
                          tags$h6("2 · Krizat dhe Frekuenca e Tyre",
                                  style = paste0("color:", C_GOLD, "; font-size:0.9rem;",
                                                 " text-transform:uppercase; letter-spacing:0.06em; font-weight:700;")),
                          tags$p("Modeli ynë tregon se mesatarisht ndodhin rreth 1.6 muaj krizë në vit.",
                                 " Testi statistikor konfirmon me 95% besueshmëri se krizat ndodhin rastësisht",
                                 " dhe jo sipas një modeli të caktuar — çdo muaj krizë është i pavarur nga ai paraardhës.",
                                 style = paste0("font-size:0.87rem; color:#cbd5e1; line-height:1.7; margin-bottom:0;"))
                      )
                  ),
                  
                  div(class = "card h-100",
                      style = paste0("background:", CARD, "; border:1px solid ", BORDER,
                                     "; border-left:4px solid ", C_DXY, "; border-radius:8px;"),
                      div(class = "card-body",
                          tags$h6("3 · Luhatja e Çmimeve",
                                  style = paste0("color:", C_DXY, "; font-size:0.9rem;",
                                                 " text-transform:uppercase; letter-spacing:0.06em; font-weight:700;")),
                          tags$p("Nafta WTI është aktivi më i paqëndrueshëm — çmimi i saj luhat rreth 40% në vit.",
                                 " Ari luhatet rreth 17% në vit, ndërsa dollari vetëm 5%.",
                                 " Ngjarjet ekstreme si COVID-19 apo lufta në Ukrainë tregojnë se goditjet e mëdha",
                                 " ndodhin më shpesh sesa parashikon teoria klasike.",
                                 style = paste0("font-size:0.87rem; color:#cbd5e1; line-height:1.7; margin-bottom:0;"))
                      )
                  ),
                  
                  div(class = "card h-100",
                      style = paste0("background:", CARD, "; border:1px solid ", BORDER,
                                     "; border-left:4px solid ", C_GREEN, "; border-radius:8px;"),
                      div(class = "card-body",
                          tags$h6("4 · Çmimi i Arit — Nafta dhe Dollari",
                                  style = paste0("color:", C_GREEN, "; font-size:0.9rem;",
                                                 " text-transform:uppercase; letter-spacing:0.06em; font-weight:700;")),
                          tags$p("Modeli ynë shpjegon mbi 70% të lëvizjeve të çmimit të arit nëpërmjet naftës dhe dollarit.",
                                 " Rezultati kryesor: kur dollari forcohet, ari zakonisht bie.",
                                 " Kur nafta ngrihet, ari tenton të ngrihet lehtë.",
                                 " Ky model është verifikuar statistikisht dhe ka gabim mesatar nën 5% krahasuar me çmimet reale.",
                                 style = paste0("font-size:0.87rem; color:#cbd5e1; line-height:1.7; margin-bottom:0;"))
                      )
                  ),
                  
                  div(class = "card h-100",
                      style = paste0("background:", CARD, "; border:1px solid ", BORDER,
                                     "; border-left:4px solid ", C_ACC, "; border-radius:8px;"),
                      div(class = "card-body",
                          tags$h6("5 · Lidhja Midis Aktiveve",
                                  style = paste0("color:", C_ACC, "; font-size:0.9rem;",
                                                 " text-transform:uppercase; letter-spacing:0.06em; font-weight:700;")),
                          tags$p("Nafta dhe dollari lëvizin kryesisht në drejtime të kundërta: kur dollari bie me 10%, nafta",
                                 " tenton të rritet. Ari ka gjithashtu lidhje të kundërt me dollarin.",
                                 " Nafta dhe ari lëvizin pothuajse të pavarur nga njëri-tjetri —",
                                 " kombinimi i tyre në portofol redukton riskun e përgjithshëm.",
                                 style = paste0("font-size:0.87rem; color:#cbd5e1; line-height:1.7; margin-bottom:0;"))
                      )
                  ),
                  
                  div(class = "card h-100",
                      style = paste0("background:", CARD, "; border:1px solid ", BORDER,
                                     "; border-left:4px solid ", MUTED, "; border-radius:8px;"),
                      div(class = "card-body",
                          tags$h6("6 · Simulimi i Çmimeve për 2026",
                                  style = paste0("color:", TEXT, "; font-size:0.9rem;",
                                                 " text-transform:uppercase; letter-spacing:0.06em; font-weight:700;")),
                          tags$p("Simulimi Monte Carlo bazuar në të dhënat historike 2006-2022 parashikon me 95% besueshmëri",
                                 " se çmimi i naftës do të mbetet brenda intervalit të parashikuar.",
                                 " Skenari i krizës së ngushticës së Hormuzit tregon rritje të mundshme deri +41% për naftën,",
                                 " +14% për dollarin, dhe rënie deri −8% për arin — rezultate të ngjashme me episodet historike.",
                                 style = paste0("font-size:0.87rem; color:#cbd5e1; line-height:1.7; margin-bottom:0;"))
                      )
                  )
                ),
                
                div(class = "mt-4"),
                div(
                  style = paste0("background:#1a2436; border:1px solid ", BORDER,
                                 "; border-radius:8px; padding:20px 24px;"),
                  tags$h6("Kufizimet dhe Hapat e Ardhshëm",
                          style = paste0("color:", TEXT, "; font-size:0.88rem;",
                                         " text-transform:uppercase; letter-spacing:0.07em; font-weight:700;")),
                  tags$p(
                    "Çdo model ka kufizimet e tij. Modeli ynë supozon se mënyra e sjelljes së tregut nuk ndryshon me kalimin e kohës —",
                    " supozim që nuk mban gjithnjë, sidomos gjatë krizave të mëdha strukturore si pandemia apo luftërat.",
                    " Parashikimet e çmimeve afatgjata janë gjithnjë të pasigurta dhe nuk duhen interpretuar si siguri absolute.",
                    " Hapat e ardhshëm: shtimi i faktorëve si inflacioni dhe rritja ekonomike,",
                    " dhe testimi i modelit me të dhëna nga periudha pas 2022-shit për të matur saktësinë reale.",
                    class = "mb-0",
                    style = paste0("font-size:0.87rem; color:#94a3b8; line-height:1.7;")
                  )
                ),
                
                make_footer()
            )
  ),
  
  # ══ CSS ═══════════════════════════════════════════════
  tags$head(tags$style(HTML(paste0("
    body, .bslib-page-fill { background-color:", BG, " !important; }
    .navbar { border-bottom: 1px solid ", BORDER, " !important; }
    .card   { border-radius: 8px; }
    .card-header { padding: 0.75rem 1rem; }
    .sidebar { background:", CARD, " !important; border-color:", BORDER, " !important; }
    .bslib-value-box { border-radius: 8px; border: 1px solid ", BORDER, " !important; }
    .irs--shiny .irs-bar, .irs--shiny .irs-from, .irs--shiny .irs-to,
    .irs--shiny .irs-single { background:", C_ACC, "; border-color:", C_ACC, "; }
    .irs--shiny .irs-handle  { background:", C_ACC, "; border-color:", C_ACC, "; width:12px; height:12px; top:23px; }
    .irs--shiny .irs-handle:hover { background: white; }
    .dataTables_wrapper { color:", TEXT, "; }
    table.dataTable { color:", TEXT, "; background:", CARD, "; }
    table.dataTable thead th { border-bottom: 1px solid ", BORDER, " !important;
      color:", MUTED, "; font-size:0.8rem; background:", CARD, "; }
    table.dataTable.stripe tbody tr.odd { background:", BG, "; }
    table.dataTable tbody tr { background:", CARD, "; }
    table.dataTable tbody tr:hover { background: #1e3a5f !important; }
    table.dataTable td { color:", TEXT, "; font-size:0.84rem; }
    .dataTables_info, .dataTables_length, .dataTables_filter { color:", MUTED, "; font-size:0.8rem; }
    ::-webkit-scrollbar { width:6px; height:6px; }
    ::-webkit-scrollbar-track { background:", BG, "; }
    ::-webkit-scrollbar-thumb { background:", BORDER, "; border-radius:3px; }
    ::-webkit-scrollbar-thumb:hover { background:#4b5563; }
    .withSpinner { min-height: 120px; }
    a { color:", C_ACC, "; }
    a:hover { color:white; }

    /* ─── Mobile responsive ─── */
    @media (max-width: 768px) {
      .container, .container-fluid { padding-left:10px; padding-right:10px; }
      .card-body { padding:0.6rem !important; }
      .layout-columns { gap:0.6rem !important; }
      .kpi-val { font-size:1.25rem !important; }
      h1, h2 { font-size:1.1rem !important; }
      .card-header { padding:0.6rem 0.75rem !important; }
      .navbar-subtitle { display:none !important; }
      .navbar-title-main { font-size:0.82rem !important; }
    }
    @media (max-width: 576px) {
      .layout-columns > * { flex: 0 0 100% !important; max-width:100% !important; }
    }
  "))))
)

# ══════════════════════════════════════════════════════
#  SERVER
# ══════════════════════════════════════════════════════
server <- function(input, output, session) {
  
  # ── TAB 0: ABSTRAKT ───────────────────────────────
  output$abstract_table <- renderDT({
    d <- master |>
      select(date, price_wti, price_gold, dxy) |>
      rename(
        "Data"                = date,
        "Nafta WTI ($/bbl)"  = price_wti,
        "Ari ($/oz)"         = price_gold,
        "Dollar Index (DXY)" = dxy
      )
    if ("regime" %in% colnames(master)) {
      d$Regjimi <- master$regime
    }
    datatable(d,
              options = list(
                pageLength = 12,
                scrollX    = TRUE,
                dom        = "lfrtip",
                language   = list(
                  search     = "Kërko:",
                  lengthMenu = "Shfaq _MENU_ rreshta",
                  info       = "Duke treguar _START_ deri _END_ nga _TOTAL_ rekorde",
                  paginate   = list(previous = "Prapa", `next` = "Tjetër")
                )
              ),
              class    = "compact stripe hover",
              rownames = FALSE) |>
      formatRound(columns = c("Nafta WTI ($/bbl)", "Ari ($/oz)", "Dollar Index (DXY)"), digits = 2) |>
      formatStyle("Nafta WTI ($/bbl)",  color = C_OIL)  |>
      formatStyle("Ari ($/oz)",         color = C_GOLD) |>
      formatStyle("Dollar Index (DXY)", color = C_DXY)
  })
  
  # ── REACTIVE DATA ─────────────────────────────────
  filtered_master <- reactive({
    master |> filter(date >= input$date_range[1], date <= input$date_range[2])
  })
  
  log_ret <- reactive({
    master |> arrange(date) |>
      mutate(
        log_ret_oil  = log(price_wti  / lag(price_wti)),
        log_ret_gold = log(price_gold / lag(price_gold)),
        log_ret_dxy  = log(dxy        / lag(dxy))
      ) |>
      filter(!is.na(log_ret_oil))
  })
  
  crisis_periods_df <- reactive({
    master |>
      arrange(date) |>
      mutate(grp = cumsum(regime != lag(regime, default = first(regime)))) |>
      group_by(grp) |>
      summarise(start = min(date), end = max(date), regime = first(regime), .groups = "drop") |>
      filter(regime == "Krize")
  })
  
  # ── TAB 1: KPI ────────────────────────────────────
  output$kpi_oil  <- renderText(paste0("$", format(round(last(master$price_wti),  2), big.mark=",")))
  output$kpi_gold <- renderText(paste0("$", format(round(last(master$price_gold), 2), big.mark=",")))
  output$kpi_dxy  <- renderText(format(round(last(master$dxy), 2), nsmall=2))
  
  output$main_chart <- renderPlotly({
    d <- filtered_master()
    p <- plot_ly() |>
      add_trace(data=d, x=~date, y=~price_wti,  type="scatter", mode="lines",
                name="Nafta WTI",
                line=list(color=C_OIL, width=2),
                hovertemplate="Naftë: <b>$%{y:.2f}</b><extra></extra>") |>
      add_trace(data=d, x=~date, y=~price_gold, type="scatter", mode="lines",
                name="Ar", yaxis="y2",
                line=list(color=C_GOLD, width=2),
                hovertemplate="Ar: <b>$%{y:.2f}</b><extra></extra>") |>
      add_trace(data=d, x=~date, y=~dxy,        type="scatter", mode="lines",
                name="DXY", yaxis="y3",
                line=list(color=C_DXY, width=2),
                hovertemplate="DXY: <b>%{y:.2f}</b><extra></extra>") |>
      layout(
        CHART_LAYOUT,
        yaxis  = list(title="Naftë WTI ($)", gridcolor=BORDER, titlefont=list(color=C_OIL, size=11)),
        yaxis2 = list(overlaying="y", side="right", showgrid=FALSE,
                      title="Ar ($)", titlefont=list(color=C_GOLD, size=11)),
        yaxis3 = list(overlaying="y", side="right", anchor="free", position=0.97,
                      showgrid=FALSE, title="DXY", titlefont=list(color=C_DXY, size=11)),
        xaxis  = list(title = "", gridcolor = BORDER,
                      range = c(format(input$date_range[1], "%Y-%m-%d"),
                                format(input$date_range[2], "%Y-%m-%d"))),
        legend = list(orientation="h", y=-0.18, font=list(size=11)),
        margin = list(t=10, b=50, l=60, r=80)
      )
    if (nrow(crisis_periods_df()) > 0) {
      cp2 <- crisis_periods_df()
      shp <- lapply(seq_len(nrow(cp2)), function(i) {
        list(type      = "rect",
             xref      = "x", yref = "paper",
             x0        = format(cp2$start[i], "%Y-%m-%d"),
             x1        = format(cp2$end[i],   "%Y-%m-%d"),
             y0        = 0, y1 = 1,
             fillcolor = "rgba(248,113,113,0.12)",
             line      = list(width = 0),
             layer     = "below")
      })
      p <- plotly::layout(p, shapes = shp)
    }
    p
  })
  
  # ── TAB 2: MARKOV ─────────────────────────────────
  P_mat <- matrix(c(0.148,0.630,0.074,0.148,
                    0.160,0.575,0.038,0.226,
                    0.000,0.400,0.000,0.600,
                    0.122,0.469,0.082,0.327),
                  nrow=4, byrow=TRUE)
  reg_labels <- c("Krizë","Normal","Recesion","Stagflacion")
  reg_colors <- c(C_OIL, C_GREEN, C_GOLD, C_DXY)
  
  output$network_plot <- renderVisNetwork({
    nodes <- tibble(
      id             = 1:4,
      label          = reg_labels,
      color.background = reg_colors,
      color.border   = "rgba(255,255,255,0.25)",
      color.highlight.background = "white",
      shape          = "dot",
      size           = 36,
      font.color     = "white",
      font.size      = 14,
      font.face      = "bold",
      shadow         = TRUE
    )
    
    edges_df <- crossing(from = 1:4, to = 1:4) |>
      mutate(prob = map2_dbl(from, to, ~ P_mat[.x, .y])) |>
      filter(prob >= 0.05) |>
      mutate(
        value = prob * 10,
        label = paste0(round(prob * 100), "%"),
        arrows = "to",
        color.color = "rgba(229,231,235,0.4)",
        color.highlight = "white",
        font.color = TEXT,
        font.size = 10,
        smooth = TRUE
      )
    
    visNetwork(nodes, edges_df, background = BG) |>
      visEdges(width=2, scaling=list(min=1, max=9)) |>
      visPhysics(solver="forceAtlas2Based",
                 forceAtlas2Based=list(gravitationalConstant=-90, springLength=160)) |>
      visOptions(highlightNearest=list(enabled=TRUE, degree=1, hover=TRUE)) |>
      visInteraction(navigationButtons=FALSE, tooltipDelay=100)
  })
  
  output$table_P <- renderDT({
    df <- as_tibble(round(P_mat, 3), .name_repair = ~ reg_labels) |>
      mutate(!!sym("") := reg_labels, .before = 1)
    
    datatable(df,
              options = list(dom="t", pageLength=4, ordering=FALSE),
              class   = "compact",
              rownames = FALSE) |>
      formatRound(columns=2:5, digits=3)
  })
  
  output$regime_timeline <- renderPlotly({
    d <- master |> arrange(date) |>
      mutate(regime_label = recode(regime,
                                   "Krize"       = "Krizë",
                                   "Normal"      = "Normal",
                                   "Recesion"    = "Recesion",
                                   "Stagflacion" = "Stagflacion"
      ))
    
    reg_col <- c(
      "Krizë"       = C_OIL,
      "Normal"      = C_GREEN,
      "Recesion"    = C_GOLD,
      "Stagflacion" = C_DXY
    )
    
    p <- plot_ly()
    
    p <- add_trace(p,
                   data = d, x = ~date, y = ~price_wti,
                   type = "scatter", mode = "lines",
                   name = "Nafta WTI",
                   line = list(color = "rgba(150,150,150,0.3)", width = 1.5),
                   hoverinfo = "none", showlegend = FALSE
    )
    
    for (reg in names(reg_col)) {
      dd <- d |> filter(regime_label == reg)
      if (nrow(dd) == 0) next
      p <- add_trace(p,
                     data = dd, x = ~date, y = ~price_wti,
                     type = "scatter", mode = "markers",
                     name = reg,
                     marker = list(color = reg_col[reg], size = 6, opacity = 0.85,
                                   line = list(width = 0)),
                     hovertemplate = paste0(
                       "<b>", reg, "</b><br>",
                       "%{x|%b %Y}<br>",
                       "WTI: $%{y:.1f}<extra></extra>"
                     )
      )
    }
    
    p |> layout(
      paper_bgcolor = "rgba(0,0,0,0)",
      plot_bgcolor  = "rgba(0,0,0,0)",
      font   = list(color = TEXT, size = 11),
      xaxis  = list(title = "", gridcolor = BORDER, tickfont = list(color = MUTED),
                    range = c("2006-01-01", "2022-06-01")),
      yaxis  = list(title = "Naftë WTI ($/bbl)", gridcolor = BORDER,
                    tickfont = list(color = MUTED)),
      legend = list(orientation = "h", y = -0.22, font = list(size = 11),
                    bgcolor = "rgba(0,0,0,0)"),
      margin = list(t = 5, b = 55, l = 55, r = 15),
      hovermode = "closest"
    )
  })
  
  # ── TAB 3: POISSON ────────────────────────────────
  crisis_data   <- reactive({ 
    master |> mutate(year = year(date), is_crisis = as.integer(regime == "Krize")) 
  })
  kriza_per_vit <- reactive({ 
    crisis_data() |> group_by(year) |> summarise(n_crisis = sum(is_crisis), .groups = "drop") 
  })
  lambda_val    <- reactive({ mean(kriza_per_vit()$n_crisis) })
  
  output$vbox_lambda  <- renderText(round(lambda_val(), 3))
  output$vbox_ncrisis <- renderText(sum(crisis_data()$is_crisis))
  
  output$plot_crisis_timeline <- renderPlotly({
    cp <- crisis_periods_df()
    shp_c <- lapply(seq_len(nrow(cp)), function(i) {
      list(type      = "rect",
           xref      = "x", yref = "paper",
           x0        = format(cp$start[i], "%Y-%m-%d"),
           x1        = format(cp$end[i],   "%Y-%m-%d"),
           y0        = 0, y1 = 1,
           fillcolor = "rgba(248,113,113,0.18)",
           line      = list(width = 0),
           layer     = "below")
    })
    plot_ly(master, x = ~date, y = ~price_wti,
            type = "scatter", mode = "lines", name = "Nafta WTI",
            line = list(color = MUTED, width = 1.5),
            hovertemplate = "WTI: <b>$%{y:.2f}</b><extra></extra>") |>
      layout(
        CHART_LAYOUT,
        xaxis  = list(title = "", gridcolor = BORDER,
                      range  = c("2006-01-01", "2022-06-01")),
        yaxis  = list(title = "WTI ($/bbl)", gridcolor = BORDER),
        shapes = shp_c
      )
  })
  
  output$plot_poisson_dist <- renderEcharts4r({
    lam   <- lambda_val()
    max_k <- max(kriza_per_vit()$n_crisis)
    tibble(
      n       = 0:max_k,
      real    = as.numeric(table(factor(kriza_per_vit()$n_crisis, levels = 0:max_k))),
      teorike = round(dpois(0:max_k, lam) * nrow(kriza_per_vit()), 2)
    ) |>
      e_charts(n) |>
      e_bar(real,    name="Vëzhguar",       itemStyle=list(color=C_GOLD, opacity=0.8)) |>
      e_line(teorike, name="Poisson teorike", smooth=TRUE,
             lineStyle=list(width=3, color=C_ACC), symbolSize=8,
             itemStyle=list(color=C_ACC)) |>
      e_tooltip(trigger="axis") |>
      e_legend(bottom=0, textStyle=list(color=TEXT)) |>
      e_grid(top=20, bottom=55, left=45, right=20) |>
      e_x_axis(axisLabel=list(color=MUTED)) |>
      e_y_axis(axisLabel=list(color=MUTED))
  })
  
  output$table_crisis <- renderDT({
    kriza_per_vit() |>
      rename(Viti = year, "Muaj Krizë" = n_crisis) |>
      datatable(options=list(dom="t", pageLength=17, ordering=FALSE),
                class="compact", rownames=FALSE) |>
      formatStyle("Muaj Krizë",
                  color              = "white",
                  backgroundColor    = styleInterval(c(0, 3),
                                                     c(CARD, "#2a2510", "#2d1a1a")))
  })
  
  # ── TAB 4: BROWNIAN ───────────────────────────────
  output$vbox_mu_oil   <- renderText(paste0(round(mean(log_ret()$log_ret_oil)  * 12*100, 2), "%"))
  output$vbox_mu_gold  <- renderText(paste0(round(mean(log_ret()$log_ret_gold) * 12*100, 2), "%"))
  output$vbox_mu_dxy   <- renderText(paste0(round(mean(log_ret()$log_ret_dxy)  * 12*100, 2), "%"))
  output$vbox_sig_oil  <- renderText(paste0(round(sd(log_ret()$log_ret_oil)    * sqrt(12)*100, 2), "%"))
  output$vbox_sig_gold <- renderText(paste0(round(sd(log_ret()$log_ret_gold)   * sqrt(12)*100, 2), "%"))
  output$vbox_sig_dxy  <- renderText(paste0(round(sd(log_ret()$log_ret_dxy)    * sqrt(12)*100, 2), "%"))
  
  output$asset_stats <- renderTable({
    col  <- switch(input$asset_select, "oil"="log_ret_oil", "gold"="log_ret_gold", "dxy"="log_ret_dxy")
    vals <- log_ret()[[col]] * 100
    tibble(
      Statistika = c("Mesatare","Std. Dev","Min","Max","Skewness","Kurtosis"),
      `Vlerë %`  = c(round(mean(vals),3), round(sd(vals),3),
                     round(min(vals),3),  round(max(vals),3),
                     round(mean((vals-mean(vals))^3)/sd(vals)^3,3),
                     round(mean((vals-mean(vals))^4)/sd(vals)^4,3))
    )
  }, striped=TRUE, hover=TRUE, spacing="xs")
  
  output$plot_logret <- renderPlotly({
    col   <- switch(input$asset_select, "oil"="log_ret_oil", "gold"="log_ret_gold", "dxy"="log_ret_dxy")
    color <- switch(input$asset_select, "oil"=C_OIL, "gold"=C_GOLD, "dxy"=C_DXY)
    d <- log_ret()
    d$y_col <- d[[col]]
    plot_ly(d, x=~date, y=~y_col, type="scatter", mode="lines",
            line=list(color=color, width=1.2), showlegend=FALSE,
            hovertemplate="%{x}<br>%{y:.4f}<extra></extra>") |>
      add_segments(x=min(d$date), xend=max(d$date), y=0, yend=0,
                   line=list(color=MUTED, dash="dot", width=1),
                   showlegend=FALSE) |>
      layout(CHART_LAYOUT, yaxis=list(title="Log-kthimi", gridcolor=BORDER))
  })
  
  output$plot_cor_heatmap <- renderPlotly({
    cm <- log_ret() |> select(log_ret_oil, log_ret_gold, log_ret_dxy) |> cor()
    labs <- c("Naftë","Ar","Dollar")
    plot_ly(x=labs, y=labs, z=round(cm,3), type="heatmap",
            colorscale=list(c(0,C_OIL), c(0.5,"#374151"), c(1,C_DXY)),
            zmid=0, zmin=-1, zmax=1,
            hovertemplate="ρ(%{x}, %{y}) = %{z:.3f}<extra></extra>") |>
      add_annotations(x=rep(labs,each=3), y=rep(labs,3),
                      text=as.vector(round(cm,3)), showarrow=FALSE,
                      font=list(color="white", size=18)) |>
      layout(paper_bgcolor="rgba(0,0,0,0)", plot_bgcolor="rgba(0,0,0,0)",
             font=list(color=TEXT), margin=list(t=5, b=5, l=5, r=5))
  })
  
  output$plot_hist_normal <- renderEcharts4r({
    col   <- switch(input$asset_select, "oil"="log_ret_oil", "gold"="log_ret_gold", "dxy"="log_ret_dxy")
    vals  <- log_ret()[[col]]
    color <- switch(input$asset_select, "oil"=C_OIL, "gold"=C_GOLD, "dxy"=C_DXY)
    h     <- hist(vals, breaks=30, plot=FALSE)
    tibble(
      x      = h$mids,
      y_real = h$density,
      y_norm = dnorm(h$mids, mean=mean(vals), sd=sd(vals))
    ) |>
      e_charts(x) |>
      e_bar(y_real, name="Reale",
            itemStyle=list(color=color, opacity=0.75)) |>
      e_line(y_norm, name="Normale teorike", smooth=TRUE,
             lineStyle=list(width=2.5, color=TEXT), showSymbol=FALSE) |>
      e_tooltip(trigger="axis") |>
      e_legend(bottom=0, textStyle=list(color=TEXT)) |>
      e_grid(top=20, bottom=55, left=55, right=20) |>
      e_x_axis(axisLabel=list(color=MUTED)) |>
      e_y_axis(axisLabel=list(color=MUTED))
  })
  
  # ── TAB 5: MONTE CARLO ────────────────────────────
  gbm_params <- reactive({
    lr <- log_ret()
    list(
      mu    = c(oil=mean(lr$log_ret_oil), gold=mean(lr$log_ret_gold), dxy=mean(lr$log_ret_dxy)),
      sigma = c(oil=sd(lr$log_ret_oil),   gold=sd(lr$log_ret_gold),   dxy=sd(lr$log_ret_dxy))
    )
  })
  
  S0_reactive <- reactive({
    mt <- read_csv("data/master_2022_2024.csv") |> mutate(date = ymd(date))
    list(oil = last(mt$price_wti), gold = last(mt$price_gold), dxy = last(mt$dxy))
  })
  
  simulate_paths <- function(s0, mu, sigma, T, n_sim, apply_shock, shock_val, lambda=1.588) {
    mat <- matrix(0, nrow=T+1, ncol=n_sim)
    mat[1,] <- s0
    for (s in 1:n_sim) {
      p <- s0
      for (t in 1:T) {
        eps    <- rnorm(1)
        crisis <- rbinom(1, 1, prob=lambda/12)
        extra  <- if (apply_shock && crisis==1) shock_val else 0
        p <- p * exp((mu - 0.5*sigma^2) + sigma*eps + extra)
        mat[t+1, s] <- p
      }
    }
    mat
  }
  
  sim_paths <- reactive({
    set.seed(42)
    asset <- input$monte_asset
    shock <- switch(asset, "oil"=0.20, "gold"=-0.05, "dxy"=0.08)
    simulate_paths(S0_reactive()[[asset]],
                   gbm_params()$mu[asset], gbm_params()$sigma[asset],
                   12, input$monte_nsim,
                   apply_shock = input$monte_scenario == "hormuz",
                   shock_val   = shock)
  })
  
  output$vbox_sim_mean   <- renderText(paste0("$", round(mean(sim_paths()[13,]), 2)))
  output$vbox_sim_ci_low <- renderText(paste0("$", round(quantile(sim_paths()[13,], 0.025), 2)))
  output$vbox_sim_ci_hi  <- renderText(paste0("$", round(quantile(sim_paths()[13,], 0.975), 2)))
  
  output$plot_fan <- renderPlotly({
    paths <- sim_paths()
    months <- 0:12
    col <- switch(input$monte_asset, "oil"=C_OIL, "gold"=C_GOLD, "dxy"=C_DXY)
    ci_025  <- apply(paths, 1, quantile, 0.025)
    ci_975  <- apply(paths, 1, quantile, 0.975)
    ci_10   <- apply(paths, 1, quantile, 0.10)
    ci_90   <- apply(paths, 1, quantile, 0.90)
    mu_path <- rowMeans(paths)
    
    plot_ly() |>
      add_trace(x=c(months,rev(months)), y=c(ci_975,rev(ci_025)),
                type="scatter", mode="none", fill="toself",
                fillcolor=paste0(col,"28"), name="95% CI", hoverinfo="skip",
                showlegend=TRUE) |>
      add_trace(x=c(months,rev(months)), y=c(ci_90,rev(ci_10)),
                type="scatter", mode="none", fill="toself",
                fillcolor=paste0(col,"55"), name="80% CI", hoverinfo="skip",
                showlegend=TRUE) |>
      add_trace(x=months, y=mu_path, type="scatter", mode="lines+markers",
                name="Rruga mesatare",
                line=list(color="white", width=2.5),
                marker=list(color="white", size=5),
                hovertemplate="Muaji %{x}: <b>%{y:.2f}</b><extra>Mesatarja</extra>") |>
      layout(
        paper_bgcolor="rgba(0,0,0,0)", plot_bgcolor="rgba(0,0,0,0)",
        font=list(color=TEXT, family="Inter, sans-serif"),
        xaxis=list(title="Muajt", gridcolor=BORDER, tickvals=0:12),
        yaxis=list(title="Çmimi i Parashikuar", gridcolor=BORDER),
        legend=list(orientation="h", y=-0.18, font=list(size=11)),
        hovermode="x unified",
        hoverlabel=list(bgcolor=CARD, font=list(color=TEXT, family="Inter, sans-serif")),
        margin=list(t=10, b=55, l=60, r=20)
      )
  })
  
  output$download_sim <- downloadHandler(
    filename = function() paste0("monte_carlo_", input$monte_asset, "_", Sys.Date(), ".csv"),
    content  = function(file) {
      df <- as_tibble(sim_paths())
      colnames(df) <- paste0("sim_", seq_len(ncol(df)))
      df$muaji <- 0:12
      write_csv(df, file)
    }
  )
  
  # ── TAB 6: OLS ────────────────────────────────────
  ols_model <- reactive({ lm(price_gold ~ price_wti + dxy, data=master) })
  
  output$vbox_r2    <- renderText(round(summary(ols_model())$r.squared, 4))
  output$vbox_fstat <- renderText(round(summary(ols_model())$fstatistic[1], 2))
  output$vbox_pval  <- renderText({
    pv <- pf(summary(ols_model())$fstatistic[1],
             summary(ols_model())$fstatistic[2],
             summary(ols_model())$fstatistic[3], lower.tail=FALSE)
    format(pv, scientific=TRUE, digits=3)
  })
  
  output$plot_coef <- renderPlotly({
    ci <- confint(ols_model())
    cf <- coef(ols_model())
    df <- tibble(
      term = c("Intercept","Oil (WTI)","Dollar (DXY)"),
      est  = as.numeric(cf),
      lo   = as.numeric(ci[,1]),
      hi   = as.numeric(ci[,2])
    ) |> filter(term != "Intercept")
    
    plot_ly(df, x=~est, y=~term, type="scatter", mode="markers",
            marker=list(size=14, color=C_DXY),
            error_x=list(type="data", symmetric=FALSE,
                         array=~hi-est, arrayminus=~est-lo,
                         color=C_DXY, thickness=2, width=6),
            hovertemplate="%{y}: β = %{x:.4f}<extra></extra>") |>
      add_segments(x=0, xend=0, y=0.5, yend=2.5,
                   line=list(color=C_OIL, dash="dash", width=1.5),
                   showlegend=FALSE) |>
      layout(paper_bgcolor="rgba(0,0,0,0)", plot_bgcolor="rgba(0,0,0,0)",
             font=list(color=TEXT),
             xaxis=list(title="Koeficienti β", gridcolor=BORDER),
             yaxis=list(title=""),
             margin=list(l=110, t=10, b=40, r=20))
  })
  
  output$table_ols <- renderDT({
    s  <- summary(ols_model())
    df <- as_tibble(s$coefficients, rownames = "Variable")
    df <- df |> 
      select(Variable, Estimate, `Std. Error`, `t value`, `Pr(>|t|)`) |>
      rename(β = Estimate, `Std. Error` = `Std. Error`, `t-val` = `t value`, `p-value` = `Pr(>|t|)`)
    
    datatable(df, options=list(dom="t", ordering=FALSE), rownames=FALSE,
              class="compact") |>
      formatRound(columns=c("β","Std. Error","t-val"), digits=4) |>
      formatSignif(columns="p-value", digits=3) |>
      formatStyle("p-value",
                  color = "white",
                  backgroundColor = styleInterval(c(0.01, 0.05),
                                                  c("#112d24", "#2a2510", "#2d1a1a")))
  })
  
  output$plot_rolling <- renderPlotly({
    win <- 24; n <- nrow(master)
    dates    <- master$date[(win+1):n]
    b_oil    <- numeric(n-win)
    b_dxy    <- numeric(n-win)
    for (i in 1:(n-win)) {
      fit    <- lm(price_gold ~ price_wti + dxy, data=master[i:(i+win-1),])
      b_oil[i] <- coef(fit)["price_wti"]
      b_dxy[i] <- coef(fit)["dxy"]
    }
    df <- tibble(date=dates, b_oil=b_oil, b_dxy=b_dxy)
    plot_ly(df, x=~date) |>
      add_trace(y=~b_oil, type="scatter", mode="lines", name="β Oil",
                line=list(color=C_OIL, width=2)) |>
      add_trace(y=~b_dxy, type="scatter", mode="lines", name="β DXY",
                line=list(color=C_DXY, width=2)) |>
      add_segments(x=min(dates), xend=max(dates), y=0, yend=0,
                   line=list(color=MUTED, dash="dot"), showlegend=FALSE) |>
      layout(CHART_LAYOUT,
             yaxis=list(title="Vlera β", gridcolor=BORDER),
             legend=list(orientation="h", y=-0.22))
  })
  
  output$plot_resid <- renderPlotly({
    df <- tibble(date=master$date, r=residuals(ols_model()))
    plot_ly(df, x=~date, y=~r, type="scatter", mode="markers",
            marker=list(color=C_OIL, size=4, opacity=0.45),
            hovertemplate="%{x}<br>Residual: %{y:.2f}<extra></extra>") |>
      add_segments(x=min(master$date), xend=max(master$date), y=0, yend=0,
                   line=list(color="white", dash="dot", width=1),
                   showlegend=FALSE) |>
      layout(CHART_LAYOUT, yaxis=list(title="Residual ($)", gridcolor=BORDER))
  })
  
  # ── TAB 7: KORRELACIONI ───────────────────────────
  corr_data <- reactive({
    log_ret() |> filter(date >= input$corr_range[1], date <= input$corr_range[2])
  })
  corr_mat <- reactive({
    corr_data() |> select(log_ret_oil, log_ret_gold, log_ret_dxy) |> cor()
  })
  
  output$vbox_cor_oil_gold <- renderText(round(corr_mat()["log_ret_oil","log_ret_gold"], 3))
  output$vbox_cor_oil_dxy  <- renderText(round(corr_mat()["log_ret_oil","log_ret_dxy"],  3))
  output$vbox_cor_gold_dxy <- renderText(round(corr_mat()["log_ret_gold","log_ret_dxy"], 3))
  
  output$plot_corr_heatmap2 <- renderPlotly({
    cm   <- corr_mat()
    labs <- c("Naftë","Ar","Dollar")
    plot_ly(x=labs, y=labs, z=round(cm,3), type="heatmap",
            colorscale=list(c(0,C_OIL), c(0.5,"#374151"), c(1,C_DXY)),
            zmid=0, zmin=-1, zmax=1,
            hovertemplate="ρ(%{x}, %{y}) = %{z:.3f}<extra></extra>") |>
      add_annotations(x=rep(labs,each=3), y=rep(labs,3),
                      text=as.vector(round(cm,3)), showarrow=FALSE,
                      font=list(color="white", size=18)) |>
      layout(paper_bgcolor="rgba(0,0,0,0)", plot_bgcolor="rgba(0,0,0,0)",
             font=list(color=TEXT), margin=list(t=5, b=5, l=5, r=5))
  })
  
  output$plot_scatter_matrix <- renderPlotly({
    d       <- corr_data()
    lm_fit  <- lm(log_ret_gold ~ log_ret_oil, data=d)
    x_range <- seq(min(d$log_ret_oil), max(d$log_ret_oil), length.out=60)
    y_pred  <- predict(lm_fit, newdata=tibble(log_ret_oil=x_range))
    
    plot_ly() |>
      add_trace(data=d, x=~log_ret_oil, y=~log_ret_gold,
                type="scatter", mode="markers", name="Muajt",
                marker=list(color=C_OIL, size=5, opacity=0.4),
                hovertemplate="Naftë: %{x:.4f}<br>Ar: %{y:.4f}<extra></extra>") |>
      add_trace(x=x_range, y=y_pred, type="scatter", mode="lines",
                name="Trend", line=list(color="white", width=1.5, dash="dot")) |>
      layout(CHART_LAYOUT,
             xaxis=list(title="Log-kthimi Naftë", gridcolor=BORDER, zerolinecolor=BORDER),
             yaxis=list(title="Log-kthimi Ar",    gridcolor=BORDER, zerolinecolor=BORDER))
  })
  
  output$plot_rolling_corr <- renderPlotly({
    win  <- 24; lr <- log_ret(); n <- nrow(lr)
    dates <- lr$date[(win+1):n]
    pair  <- input$corr_pair
    cols  <- switch(pair,
                    "oil_gold" = c("log_ret_oil","log_ret_gold"),
                    "oil_dxy"  = c("log_ret_oil","log_ret_dxy"),
                    "gold_dxy" = c("log_ret_gold","log_ret_dxy"))
    col   <- switch(pair, "oil_gold"=C_OIL, "oil_dxy"=C_DXY, "gold_dxy"=C_GOLD)
    rc    <- numeric(n-win)
    for (i in 1:(n-win)) {
      sub  <- lr[i:(i+win-1), cols]
      rc[i] <- cor(sub[,1], sub[,2])
    }
    df <- tibble(date=dates, rho=rc)
    plot_ly(df, x=~date, y=~rho, type="scatter", mode="lines",
            line=list(color=col, width=2),
            fill="tozeroy", fillcolor=paste0(col,"22"),
            hovertemplate="%{x}<br>ρ = %{y:.3f}<extra></extra>") |>
      add_segments(x=min(dates), xend=max(dates), y=0, yend=0,
                   line=list(color=MUTED, dash="dot", width=1),
                   showlegend=FALSE) |>
      layout(CHART_LAYOUT,
             yaxis=list(title="Korrelacioni ρ", range=c(-1,1), gridcolor=BORDER))
  })
  
} # ── FUND SERVER ──────────────────────────────────────

shinyApp(ui, server)