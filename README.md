# 🎲 Proiect Probabilități și Statistică

**Simularea Variabilelor Aleatoare și Aplicații Monte Carlo**

Proiect academic realizat în cadrul cursului de Probabilități și Statistică, ianuarie 2026.

---

## 📋 Descriere

Acest proiect explorează metode fundamentale de simulare statistică prin trei exerciții complementare:

1. **Simularea pe discul unitar** - Comparație între metoda accept-reject și coordonate polare
2. **Aplicație Shiny interactivă** - Vizualizare ECDF pentru 5 distribuții
3. **Problema Acului lui Buffon** - Estimarea constantei π prin probabilitate geometrică

---

## 🛠️ Tehnologii Utilizate

- **R** 4.0+
- **RStudio** (IDE recomandat)
- **Biblioteci**: `ggplot2`, `shiny`, `gridExtra`
- **LaTeX** pentru documentație profesională

---

## 📦 Instalare

```r
# Instalează pachetele necesare
install.packages(c("ggplot2", "shiny", "gridExtra"))

# Verifică versiunile (opțional)
packageVersion("ggplot2")   # >= 3.4.0
packageVersion("shiny")      # >= 1.7.5
```

---

## 🚀 Utilizare

### Exercițiul 1: Simulare pe disc
```r
source("exercitiu1.R")
# Generează automat 6 grafice de analiză
```

### Exercițiul 2: Aplicație Shiny
```r
# Rulează aplicația interactivă
shiny::runApp("exercitiu2_shiny.R")

# SAU în RStudio: deschide fișierul și click "Run App"
```

### Exercițiul 3: Acul lui Buffon
```r
source("exercitiu3.R")
# Simulează toate cele 5 variante ale problemei
```

---

## 📊 Rezultate Principale

### Exercițiul 1
- ✅ **Rata de acceptare**: 77.64% (vs. π/4 ≈ 78.54% teoretic)
- ✅ **Media distanței**: 0.6757 (vs. 2/3 ≈ 0.6667 teoretic)
- ✅ **Eroare relativă**: < 1.5%

### Exercițiul 2
- 🎨 **5 distribuții**: Normal, Exponențială, Poisson, Binomială
- 📈 **4-5 grafice ECDF** per distribuție
- ✅ Toate testele K-S: p-values > 0.05

### Exercițiul 3
- 🎯 **Estimare π**: 3.14-3.15 (pentru N=10,000)
- 📉 **Reducere varianță**: 57% (Cruce vs. Ac simplu)
- ✅ **5 variante** validate: clasic, cruce, general, linie aleatoare, grilă

---

## 🎯 Caracteristici Cheie

### Exercițiul 1
- ✨ **Două metode** de simulare implementate și comparate
- 📐 **Demonstrații matematice** complete (densități, independență)
- 📊 **6 grafice** de validare (scatter, histograme, Q-Q plot, ECDF)
- 🧪 **Teste statistice** (Chi-pătrat, Kolmogorov-Smirnov)

### Exercițiul 2
- 🖱️ **Interfață interactivă** cu controale validate
- 🎨 **Paneluri condiționate** pentru parametri specifici
- 📈 **Vizualizare ECDF** pentru transformări: X, 3+2X, X², ΣXᵢ, ΣXᵢ²
- ⚡ **Sistem de buton** pentru control manual al recalculării

### Exercițiul 3
- 🎲 **5 variante** ale problemei Buffon
- 📉 **Analiză comparativă**: varianță, eficiență, precizie
- 🔬 **Demonstrații teoretice** complete pentru fiecare caz
- 🎯 **Clasificare algoritmică**: Monte Carlo vs. Las Vegas

---

## 📖 Documentație

Documentația completă (50-60 pagini) include:

- 📐 **Demonstrații matematice** riguroase
- 💻 **Cod comentat** extensiv
- 📊 **Rezultate și analiză** detaliată
- 🧪 **Teste statistice** cu interpretări
- 🔍 **Identificarea dificultăților** și soluții
- 🚀 **Probleme deschise** și direcții viitoare

---

## 👥 Autori

- **Drăgunoi Miruna**
- **Panaet Maria-Alexandra**

---

## 📚 Referințe Principale

- Robert, C. P., & Casella, G. (2004). *Monte Carlo Statistical Methods*. Springer.
- Wickham, H. (2016). *ggplot2: Elegant Graphics for Data Analysis*. Springer.
- Wickham, H. (2021). *Mastering Shiny*. O'Reilly Media.
- Ross, S. M. (2014). *Introduction to Probability Models*. Academic Press.

---

## 📄 Licență

Proiect academic realizat în scop educațional pentru cursul de Probabilități și Statistică.

---

<div align="center">

*Ianuarie 2026 | Probabilități și Statistică*

</div>
