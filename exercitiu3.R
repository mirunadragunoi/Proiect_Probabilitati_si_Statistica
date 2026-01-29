# Exercitiul 3

# Subpunctul a

simulare_buffon_clasic <- function(N = 10000) {
  
  # 1. Generare variabile (Pozitie si Unghi)
  # x este distanta pana la linie (la 0.5)
  x <- runif(N, min = 0, max = 0.5)
  
  # theta este unghiul (0 la pi)
  theta <- runif(N, min = 0, max = pi)
  
  # 2. Verificare conditie intersectie
  # Proiectia verticala a jumatatii de ac (L/2 = 0.5)
  limita <- 0.5 * sin(theta)
  intersectie <- x <= limita
  
  # 3. Calcul probabilitate
  prob_estimata <- mean(intersectie)
  return (prob_estimata)
}

# Rulare pentru N mare
set.seed(42)
rezultat <- simulare_buffon_clasic(N = 10000)
# Rezultat asteptat: aprox 0.6366

# Subpunctul b

simulare_cruce <- function(N = 10000) {
  # 1. Gnenerare (centru comun x, unghi theta1)
  x <- runif(N, min = 0, max = 0.5)
  theta1 <- runif(N, min = 0, max = pi)
  
  # Acul 2 este perpendicular (adaugam pi/2)
  theta2 <- theta1 + pi/2
  
  # 2. Verificare intersectii
  # Acul 1
  i1 <- x <= 0.5 * sin(theta1)
  # Acul 2 (folosim abs() deoarece sin poate fi negativ > pi)
  i2 <- x <= 0.5 * abs(sin(theta2))
  
  # 3. Variabila Z (total intersectii: 0, 1 sau 2)
  Z <- as.numeric(i1) + as.numeric(i2)
  
  # Variabila de interes este Z/2
  valori <- Z / 2
  
  # 4. Returnam statisticile
  return (list(
    media = mean(valori),
    varianta = var(valori)
  ))
}

# Rulare si afisare
set.seed(123)
rezultate <- simulare_cruce(10000)
# Media empirica ar trebui sa fie aprox 0.6366
# Varianta empirica ar trebui sa fie aprox 0.099

# Subpunctul c

simulare_general <- function(N, L, d) {
  if (L >= d) stop("L trebuie sa fie mai mic decat d")
  
  # 1. Generare (x pana la d/2)
  x <- runif(N, min = 0, max = d/2)
  theta <- runif(N, min = 0, max = pi)
  
  # 2. Conditia (L/2)
  intersectie <- x <= (L/2) * sin(theta)
  
  # 3. Rezultat
  prob_est <- mean(intersectie)
  prob_teo <- (2 * L) / (pi * d)
  
  return (list(
    estimat = prob_est,
    teoretic = prob_teo
  ))
}

# Exemplu: L=0.8, d=2
# Rezultat asteptat: 0.8 / pi = aprox 0.2546
simulare_general(10000, 0.8, 2)

# Subpunctul d

simulare_linie_aleatoare <- function(N, L, d) {
  # 1. Generam parametrii LINIEI
  # r = distanta de la centrul acului la linie
  r <- runif(N, min = 0, max = d/2)
  
  # alpha = unghiul normalei la linie
  alpha <- runif(N, min = 0, max = 2*pi)
  
  # 2. Conditia de intersectie
  # Acul e fix pe axa Ox. Proiectia sa pe normala este (L/2)*|cos(alpha)|
  # Linia intersecteaza daca distanta r este mai mica decat aceasta proiectie
  limita <- (L/2) * abs(cos(alpha))
  intersectie <- r <= limita
  
  # 3. Calcul
  prob_est <- mean(intersectie)
  prob_teo <- (2 * L) / (pi * d)
  
  return (list(
    estimat = prob_est,
    teoretic = prob_teo
  ))
}

# Testam pentru L=0.8, d=2
# Asteptat: 0.2546
simulare_linie_aleatoare(10000, 0.8, 2)

# Subpunctul e

simulare_grid <- function(N, L, d1, d2) {
  # 1. Generare variabile
  # x raportat la d1, y raportat la d2
  x <- runif(N, min = 0, max = d1/2)
  y <- runif(N, min = 0, max = d2/2)
  
  theta <- runif(N, min = 0, max = pi/2)
  
  # 2. Verificare intersectii individuale
  # Intersectie verticala (depinde de cosinus in aces sistem de axe)
  int_vert <- x <= (L/2) * cos(theta)
  # Intersectie orizontala (depinde de sinus)
  int_oriz <- y <= (L/2) * sin(theta)
  
  # 3. Reuniune (SAU logic)
  # Acul intersecteaza planul daca atinge ORICARE linie
  intersectie_totala <- int_vert | int_oriz
  
  # 4. Calcul
  prob_est <- mean(intersectie_totala)
  prob_teo <- (L * (2*d1 + 2*d2 - L)) / (pi * d1 * d2)
  
  return (list(
    estimat = prob_est,
    teoretic = prob_teo
  ))
}

# Exemplu: L=1, d1=2, d2=3
# Asteptat: aprox 0.477
simulare_grid(10000, 1, 2, 3)

# Subpunctul f

algoritm_las_vegas <- function(tinta, interval_max = 100) {
  incercari <- 0
  gasit <- FALSE
  
  # Ruleaza pana gaseste solutia corecta
  while (!gasit) {
    incercari <- incercari + 1
    # Generam o propunere aleatoare
    propunere <- sample(1:interval_max, 1)
    
    # Verificam daca este solutia corecta
    if (propunere == tinta) {
      gasit <- TRUE
    }
  }
  
  # Returneaza numarul de pasi (care este variabil)
  # Totusi, rezultatul "Am gasit tinta" este garantat corect
  return (incercari)
}

# Rulare: Cautam numarul 42 in intervalul [1, 100]
set.seed(123)
pasi_necesari <- algoritm_las_vegas(42, 100)
cat("Algoritmul a gasit solutia corecta dupa ", pasi_necesari, " incercari.\n")



# Grafice
library(ggplot2)
library(gridExtra)

# 1. Vizualizare pentru Subpunctul A (Acul Simplu)
plot_buffon_simple <- function() {
  # Generam 150 de ace pentru vizualizare
  N_plot <- 150
  # Generam coordonate absolute
  y_center <- runif(N_plot, 0, 4) 
  angle <- runif(N_plot, 0, pi)
  L <- 1
  
  # Calculam capetele acului
  y1 <- y_center - (L/2) * sin(angle)
  y2 <- y_center + (L/2) * sin(angle)
  x_center <- runif(N_plot, 0, 6) 
  x1 <- x_center - (L/2) * cos(angle)
  x2 <- x_center + (L/2) * cos(angle)
  
  # Verificam intersectia: daca y1 si y2 sunt de o parte si de alta a unei linii intregi
  hit <- floor(y1) != floor(y2)
  
  df_seg <- data.frame(x1, y1, x2, y2, hit)
  
  p <- ggplot() +
    # Liniile podelei
    geom_hline(yintercept = 0:4, linetype="dashed", color="gray40") +
    # Acele
    geom_segment(data=df_seg, aes(x=x1, y=y1, xend=x2, yend=y2, color=hit), size=1) +
    scale_color_manual(values=c("FALSE"="red", "TRUE"="blue"), 
                       labels=c("Nu intersecteaza", "Intersecteaza")) +
    labs(title="Simulare Vizuala: Acul lui Buffon", x="", y="") +
    theme_minimal() +
    theme(axis.text = element_blank(), panel.grid = element_blank())
  
  print(p)
}

plot_buffon_simple()

# 2. Vizualizare pentru Subpunctul B (Crucea lui Buffon)
plot_buffon_cross <- function() {
  N_plot <- 100
  y_center <- runif(N_plot, 0, 4)
  x_center <- runif(N_plot, 0, 6)
  theta <- runif(N_plot, 0, pi)
  L <- 1
  
  # Acul 1
  y1a <- y_center - (L/2) * sin(theta)
  y1b <- y_center + (L/2) * sin(theta)
  x1a <- x_center - (L/2) * cos(theta)
  x1b <- x_center + (L/2) * cos(theta)
  hit1 <- floor(y1a) != floor(y1b)
  
  # Acul 2 (perpendicular)
  y2a <- y_center - (L/2) * sin(theta + pi/2)
  y2b <- y_center + (L/2) * sin(theta + pi/2)
  x2a <- x_center - (L/2) * cos(theta + pi/2)
  x2b <- x_center + (L/2) * cos(theta + pi/2)
  hit2 <- floor(y2a) != floor(y2b)
  
  hit_total <- hit1 | hit2 # Daca macar unul atinge
  
  df1 <- data.frame(x1=x1a, y1=y1a, x2=x1b, y2=y1b, hit=hit_total)
  df2 <- data.frame(x1=x2a, y1=y2a, x2=x2b, y2=y2b, hit=hit_total)
  
  p <- ggplot() +
    geom_hline(yintercept = 0:4, linetype="dashed", color="gray40") +
    geom_segment(data=df1, aes(x=x1, y=y1, xend=x2, yend=y2, color=hit), size=0.8) +
    geom_segment(data=df2, aes(x=x1, y=y1, xend=x2, yend=y2, color=hit), size=0.8) +
    scale_color_manual(values=c("FALSE"="orange", "TRUE"="purple")) +
    labs(title="Simulare Vizuala: Crucea lui Buffon", subtitle="Mov = Cel putin o intersectie") +
    theme_minimal() +
    theme(axis.text = element_blank(), panel.grid = element_blank())
  
  print(p)
}

plot_buffon_cross()

# 3. Vizualizare pentru Subpunctul E (Grila Laplace)
plot_buffon_grid <- function() {
  N_plot <- 100
  d1 <- 2; d2 <- 2; L <- 1
  x_c <- runif(N_plot, 0, 6)
  y_c <- runif(N_plot, 0, 6)
  theta <- runif(N_plot, 0, 2*pi)
  
  x1 <- x_c - (L/2)*cos(theta); x2 <- x_c + (L/2)*cos(theta)
  y1 <- y_c - (L/2)*sin(theta); y2 <- y_c + (L/2)*sin(theta)
  
  # Verificare intersectie cu grila 
  hit_v <- floor(x1/d1) != floor(x2/d1)
  hit_h <- floor(y1/d2) != floor(y2/d2)
  hit <- hit_v | hit_h
  
  df <- data.frame(x1, y1, x2, y2, hit)
  
  p <- ggplot() +
    geom_vline(xintercept = seq(0, 8, d1), color="gray") +
    geom_hline(yintercept = seq(0, 8, d2), color="gray") +
    geom_segment(data=df, aes(x=x1, y=y1, xend=x2, yend=y2, color=hit), size=1) +
    scale_color_manual(values=c("FALSE"="red", "TRUE"="darkgreen")) +
    labs(title="Problema lui Laplace (Grila)", subtitle="Verde = Intersectie") +
    theme_void() +
    theme(panel.border = element_rect(colour = "black", fill=NA, size=1))
  
  print(p)
}

plot_buffon_grid()

# 4. Vizualizare comparativa
plot_compare_convergence <- function() {
  set.seed(42)
  N <- 3000 # Numarul de pasi
  target <- 2/pi # Valoarea corecta
  
  # Simulare acum simplu
  x1 <- runif(N, 0, 0.5)
  th1 <- runif(N, 0, pi)
  hits1 <- (x1 <= 0.5 * sin(th1))
  # Calculam media cumulativa 
  est_simplu <- cumsum(hits1) / (1:N)
  
  # Simulare cruce
  x2 <- runif(N, 0, 0.5)
  th2 <- runif(N, 0, pi); th2_perp <- th2 + pi/2
  # Numarul de intersectii pentru cruce (0, 1 sau 2)
  h_a <- (x2 <= 0.5 * sin(th2))
  h_b <- (x2 <= 0.5 * abs(sin(th2_perp)))
  Z <- h_a + h_b
  # Variabila de interes este Z/2
  est_cruce <- cumsum(Z/2) / (1:N)
  
  # Pregatire date pentru plot
  df_plot <- rbind(
    data.frame(Pas=1:N, Valoare=est_simplu, Metoda="Acul Simplu (Var Mare)"),
    data.frame(Pas=1:N, Valoare=est_cruce,  Metoda="Crucea (Var Mica)")
  )
  
  # Grafic
  p <- ggplot(df_plot, aes(x=Pas, y=Valoare, color=Metoda)) +
    # Linia teoretica (Tinta)
    geom_hline(yintercept=target, linetype="dashed", color="black", size=1) +
    # Liniile de estimare
    geom_line(alpha=0.8, size=0.8) +
    # Limite axe 
    coord_cartesian(ylim=c(0.55, 0.75)) +
    scale_color_manual(values=c("red", "blue")) +
    labs(title="Comparatie Eficiență: Acul Simplu vs. Crucea",
         y="Valoarea Estimată", x="Număr de Simulări") +
    theme_minimal() +
    theme(legend.position="bottom")
  
  print(p)
}

plot_compare_convergence()
