# Lëvizja e Çmimit të Arit, Naftës dhe Indeksit të Dollarit në Bursë në Kohë Krize dhe në Kohë Normale


<p align="center">
  <img src="https://img.shields.io/badge/R-4.3%2B-276DC3?style=for-the-badge&logo=r&logoColor=white"/>
  &nbsp;
  <img src="https://img.shields.io/badge/Shiny-Dashboard-4b9cd3?style=for-the-badge&logo=rstudio&logoColor=white"/>
  &nbsp;
  <img src="https://img.shields.io/badge/Periudha-2006--2024-374151?style=for-the-badge"/>
  &nbsp;
  <img src="https://img.shields.io/badge/Status-Live%20%E2%97%8F-22c55e?style=for-the-badge"/>
</p>
<br/>
<p align="center">
  <a href="https://ermir-haxhia.shinyapps.io/oil-gold-dollar/">
    <img src="https://img.shields.io/badge/🚀%20%20Shiko%20Dashboard%20Live-%20%20shinyapps.io-6366f1?style=for-the-badge&logoColor=white" alt="View Live Dashboard"/>
  </a>
</p>
---

## Përmbajtja

- [Përshkrimi i Projektit](#përshkrimi-i-projektit)
- [Struktura e Projektit](#struktura-e-projektit)
- [Të Dhënat](#të-dhënat)
- [Metodologjia](#metodologjia)
- [Rezultatet Kryesore](#rezultatet-kryesore)
- [Struktura e Dashboard-it](#struktura-e-dashboard-it)
- [Instalimi dhe Ekzekutimi](#instalimi-dhe-ekzekutimi)
- [Teknologjitë e Përdorura](#teknologjitë-e-përdorura)
- [Autori](#autori)

---

## Përshkrimi i Projektit

Ky projekt analizon sjelljen e tre treguesve kryesorë financiarë globalë gjatë periudhës **2006–2024**:

| Aktivi | Simboli | Burimi |
|--------|---------|--------|
| Nafta bruto WTI | $/bbl | FRED – DCOILWTICO |
| Ari | XAU/USD ($/oz) | Macrotrends |
| Indeksi i Dollarit Amerikan | DXY | FRED – DTWEXBGS |

Pyetja qendrore e studimit është: **Si reagojnë çmimet e naftës, arit dhe dollarit ndaj njëri-tjetrit gjatë krizave ekonomike, dhe si ndryshon ky raport në periudha normale?**

Projekti kombinon metoda të proceseve stokastike me vizualizim interaktiv në web për t'u bërë i aksesueshëm jo vetëm për analistë financiarë, por edhe për çdo person të interesuar për tregjet globale.

---

## Struktura e Projektit

```
oil-gold-dollar/
│
├── README.md                        ← ky skedar
│
├── data/
│   ├── raw/                         ← të dhënat origjinale të shkarkuara
│   └── clean/
│       ├── master_2006_2022.csv     ← dataset kryesor i pastruar
│       └── master_2022_2024.csv     ← zgjatim periudhe deri 2024
│
├── analysis/
│   ├── 01_import_clean.R            ← import dhe pastrim i të dhënave
│   ├── 02_markov.R                  ← ndërtimi i Zinxhirit të Markovit
│   ├── 03_poisson.R                 ← analiza e Procesit Poisson
│   ├── 04_brownian.R                ← Lëvizja Browniane Gjeometrike (GBM)
│   └── 05_montecarlo.R              ← Simulimi Monte Carlo
│
├── output/                          ← grafike PNG të eksportuara
│
└── app/
    ├── app.R                        ← aplikacioni kryesor Shiny (UI + Server)
    └── data/
        ├── master_2006_2022.csv
        └── master_2022_2024.csv
```

---

## Të Dhënat

Të dhënat janë mujore dhe mbulojnë periudhën **Janar 2006 – Dhjetor 2024** (rreth 216 vëzhgime).

### Variablat kryesorë

| Variabli | Përshkrimi |
|----------|------------|
| `date` | Data mujore (formati YYYY-MM-DD) |
| `price_wti` | Çmimi mesatar mujor i naftës WTI në $/bbl |
| `price_gold` | Çmimi mesatar mujor i arit në $/oz |
| `dxy` | Indeksi i Dollarit Amerikan (bazë = 100) |
| `regime` | Regjimi ekonomik i klasifikuar nga modeli Markov: `Normal`, `Krize`, `Recesion`, `Stagflacion` |

### Burimet

- **Nafta WTI:** [FRED – DCOILWTICO](https://fred.stlouisfed.org/series/DCOILWTICO)
- **Nafta Brent:** [FRED – DCOILBRENTEU](https://fred.stlouisfed.org/series/DCOILBRENTEU)
- **Dollar Index:** [FRED – DTWEXBGS](https://fred.stlouisfed.org/series/DTWEXBGS)
- **Çmimi i Arit:** [Macrotrends – Gold Prices](https://www.macrotrends.net/1333/historical-gold-prices-100-year-chart)
- **Norma Reale:** [FRED – DFII10](https://fred.stlouisfed.org/series/DFII10)

---

## Metodologjia

Projekti zbaton pesë metoda matematikore dhe statistikore, të organizuara në pesë skripte analize:

### 1. Zinxhiri i Markovit — `02_markov.R`

Ekonomia klasifikohet në **katër regjime** bazuar në ndryshimet e çmimit të naftës dhe treguesve makroekonomikë:

- **Normal** — rritje e qëndrueshme, volatilitet i ulët
- **Krizë** — rënie e mprehtë çmimi, goditje të mëdha
- **Recesion** — tkurrje ekonomike e zgjatur
- **Stagflacion** — inflacion i lartë me rritje të ngadaltë

Ndërtohet **matrica e kalimit P** (4×4), e cila tregon probabilitetin që ekonomia të kalojë nga një regjim në tjetrin brenda një muaji. Shpërndarja afatgjatë (stacionare):

| Regjimi | Shpërndarja Afatgjatë |
|---------|----------------------|
| Normal | 54.6% |
| Stagflacion | 26.1% |
| Krizë | 14.0% |
| Recesion | 5.2% |

### 2. Procesi Poisson — `03_poisson.R`

Krizat ekonomike modelohen si ngjarje të rastit me Procesin Poisson. Intensiteti i vlerësuar: **λ = 1.588 kriza/vit**. Testi Chi-Square konfirmon përshtatshmërinë e modelit (p-value = 0.1813 > 0.05).

### 3. Lëvizja Browniane Gjeometrike — `04_brownian.R`

Parametrat e vlerësuar nga të dhënat historike 2006–2022:

| Aktivi | Kthimi Vjetor (μ) | Volatiliteti Vjetor (σ) |
|--------|-------------------|------------------------|
| Naftë WTI | +2.48% | 39.56% |
| Ar (Gold) | +7.68% | 17.37% |
| Dollar (DXY) | +0.86% | 4.68% |

Korrelacionet midis aktiveve:

| Çifti | Korrelacioni |
|-------|-------------|
| Naftë ↔ Dollar | −0.531 |
| Ar ↔ Dollar | −0.314 |
| Naftë ↔ Ar | +0.024 |

### 4. Simulimi Monte Carlo — `05_montecarlo.R`

Ndërtohen **10,000 rrugë simulimi** për 12 muajt pasardhës me dy skenare:

- **Bazë (historik):** parametrat e vlerësuara nga 2006–2022
- **Krizë Hormuz 2026:** goditje stokastike pozitive (+41.3% naftë, +14.1% dollar) dhe negative (−7.6% ar)

### 5. Regresioni OLS dhe Korrelacioni

Modeli **Ar ~ Naftë + Dollar** arrin R² > 0.70 me p < 0.001. Llogaritet edhe regresioni rrotullues me dritare 24-mujore për të matur qëndrueshmërinë e koeficientëve në kohë.

---

## Rezultatet Kryesore

1. Ekonomia kalon nëpër katër faza të dallueshme — regjimi Normal dominon, por paqëndrueshmëria zë deri **40% të kohës**
2. Krizat ndodhin rastësisht me mesatarisht **1.6 muaj krizë në vit** — konfirmuar statistikisht
3. Nafta është aktivi më i paqëndrueshëm **(σ ≈ 40%)**, ndërsa dollari është më i qëndrueshmi **(σ ≈ 5%)**
4. Nafta dhe ari janë pothuajse të pavarur nga njëri-tjetri — kombinim ideal për **diversifikim portofoli**
5. Dollari i dobët sjell naftë dhe ar më të shtrenjta — lidhje e qëndrueshme gjatë 18 viteve
6. Skenari Hormuz parashikon rritje të mprehtë të naftës (+41%) dhe rënie të arit (−8%) — në linjë me episodet historike

---

## Struktura e Dashboard-it

Dashboard-i Shiny është organizuar në **9 tab-e**:

| Tab | Titulli | Përmbajtja |
|-----|---------|------------|
| 0 | **Abstrakt** | Përshkrimi i projektit, burimet, preview i dataset-it |
| 1 | **Tregu** | Grafiku kryesor 3 boshte Y (Naftë, Ar, DXY) + periudhat e krizave |
| 2 | **Regjimet** | Rrjeti Markov + matrica e kalimit + çmimi i naftës sipas regjimit |
| 3 | **Krizat** | KPI Poisson + timeline krizash + vërtetimi i modelit + tabela vjetore |
| 4 | **Volatiliteti** | Kthimet dhe luhatjet vjetore + ndryshimet mujore + histogram |
| 5 | **Simulimi** | Fan chart Monte Carlo + kontroll parametrash + skenare + shkarkim CSV |
| 6 | **Regresioni** | OLS Ar~Naftë+Dollar + koeficientët + regresioni rrotullues + residualët |
| 7 | **Korrelacioni** | Heatmap + scatter naftë vs ar + korrelacioni rrotullues 24-mujor |
| 8 | **Konkluzionet** | 6 blloqe konkluzionesh + kufizimet e modelit |

Dizajni ndjek estetikën **TradingView / Bloomberg** — temë e errët navy me aksente ngjyrash për secilin aktiv (🔴 Naftë · 🟡 Ar · 🔵 Dollar · 🟢 Normal).

---

## Instalimi dhe Ekzekutimi

### Kërkesat

- R 4.3+
- RStudio (rekomandohet)

### Instalimi i paketave

```r
install.packages(c(
  "shiny", "bslib", "plotly", "dplyr", "visNetwork",
  "DT", "echarts4r", "shinyWidgets", "shinycssloaders",
  "lubridate", "rlang", "bsicons", "rsconnect"
))
```

### Ekzekutimi lokal

```r
setwd("C:/path/to/oil-gold-dollar")
shiny::runApp("app/")
```

### Deploy në shinyapps.io

```r
file.copy("app.R", "app/app.R", overwrite = TRUE)
rsconnect::deployApp("app/", appName = "oil-gold-dollar")
```

---

## Teknologjitë e Përdorura

| Teknologjia | Roli |
|-------------|------|
| **R** | Gjuha kryesore e programimit |
| **Shiny** | Framework për web aplikacionin interaktiv |
| **bslib** | Tema dhe layout i UI (Bootstrap 5) |
| **plotly** | Grafike interaktive me hover dhe zoom |
| **echarts4r** | Grafike shtesë (histogram, Poisson) |
| **visNetwork** | Vizualizimi i rrjetit Markov |
| **DT** | Tabela interaktive me kërkim dhe paginim |
| **dplyr / lubridate** | Manipulim dhe transformim i të dhënave |
| **shinyWidgets** | Komponentë UI të avancuar |
| **shinyapps.io** | Hosting i aplikacionit në cloud |

---

## Autori

**Ermir Haxhia**  
Student MSc · Inxhinieri Matematik dhe Informatik  
Departamenti i Matematikës së Aplikuar · FSHN · Universiteti i Tiranës

| | |
|---|---|
| 📧 Email | ermir.haxhia10@gmail.com |
| 💼 LinkedIn | [linkedin.com/in/ermir-haxhia-b988212b5](https://www.linkedin.com/in/ermir-haxhia-b988212b5) |
| 🐙 GitHub | [github.com/ermirhaxhia](https://github.com/ermirhaxhia) |
| 🌐 Portfolio | [ermir-haxhia.vercel.app]([https://ermir-haxhia.vercel.app](https://infrequent-network-348.notion.site/Ermir-Haxhia-Data-Analysis-Portfolio-32c61957cde1800ebff6d7c1190170ae)) |

---

*Lënda: Proceset Stokastike · Universiteti i Tiranës · FSHN · –2026*
