# exercitiul 3

# subpunctul a

simulare_buffon_clasic <- function(N = 10000) {
  
  # 1. generare variabile (pozitie si unghi)
  # x este distanta pana la linie (la 0.5)
  x <- runif(N, min = 0, max = 0.5)
  
  # theta este unghiul (0 la pi)
  theta <- runif(N, min = 0, max = pi)
  
  # 2. verificare conditie intersectie
  # proiectia verticala a jumatatii de ac (L/2 = 0.5)
  limita <- 0.5 * sin(theta)
  intersectie <- x <= limita
  
  # 3. calcul probabilitate
  prob_estimata <- mean(intersectie)
  return (prob_estimata)
}

# rulare pentru N mare
set.seed(42)
rezultat <- simulare_buffon_clasic(N = 10000)
# rezultat asteptat: aprox 0.6366

# subpunctul b

simulare_cruce <- function(N = 10000) {
  # 1. genenerare (centru comun x, unghi theta1)
  x <- runif(N, min = 0, max = 0.5)
  theta1 <- runif(N, min = 0, max = pi)
  
  # acul 2 este perpendicular (adaugam pi/2)
  theta2 <- theta1 + pi/2
  
  # 2. verificare intersectii
  # acul 1
  i1 <- x <= 0.5 * sin(theta1)
  # acul 2 (folosim abs() deoarece sin poate fi negativ > pi)
  i2 <- x <= 0.5 * abs(sin(theta2))
  
  # 3. variabila Z (total intersectii: 0, 1 sau 2)
  Z <- as.numeric(i1) + as.numeric(i2)
  
  # variabila de interes este Z/2
  valori <- Z / 2
  
  # 4. returnam statisticile
  return (list(
    media = mean(valori),
    varianta = var(valori)
  ))
}

# rulare si afisare
set.seed(123)
rezultate <- simulare_cruce(10000)
# media empirica ar trebui sa fie aprox 0.6366
# varianta empirica ar trebui sa fie aprox 0.099

# subpunctul c

simulare_general <- function(N, L, d) {
  if (L >= d) stop("L trebuie sa fie mai mic decat d")
  
  # 1. generare (x pana la d/2)
  x <- runif(N, min = 0, max = d/2)
  theta <- runif(N, min = 0, max = pi)
  
  # 2. conditia (L/2)
  intersectie <- x <= (L/2) * sin(theta)
  
  # 3. rezultat
  prob_est <- mean(intersectie)
  prob_teo <- (2 * L) / (pi * d)
  
  return (list(
    estimat = prob_est,
    teoretic = prob_teo
  ))
}

# exemplu: L=0.8, d=2
# rezultat asteptat: 0.8 / pi = aprox 0.2546
simulare_general(10000, 0.8, 2)

# subpunctul d

simulare_linie_aleatoare <- function(N, L, d) {
  # 1. generam parametrii LINIEI
  # r = distanta de la centrul acului la linie
  r <- runif(N, min = 0, max = d/2)
  
  # alpha = unghiul normalei la linie
  alpha <- runif(N, min = 0, max = 2*pi)
  
  # 2. conditia de intersectie
  # acul e fix pe axa Ox. proiectia sa pe normala este (L/2)*|cos(alpha)|
  # linia intersecteaza daca distanta r este mai mica decat aceasta proiectie
  limita <- (L/2) * abs(cos(alpha))
  intersectie <- r <= limita
  
  # 3. calcul
  prob_est <- mean(intersectie)
  prob_teo <- (2 * L) / (pi * d)
  
  return (list(
    estimat = prob_est,
    teoretic = prob_teo
  ))
}

# testam pentru L=0.8, d=2
# asteptat: 0.2546
simulare_linie_aleatoare(10000, 0.8, 2)

# subpunctul e

simulare_grid <- function(N, L, d1, d2) {
  # 1. generare variabile
  # x raportat la d1, y raportat la d2
  x <- runif(N, min = 0, max = d1/2)
  y <- runif(N, min = 0, max = d2/2)
  
  theta <- runif(N, min = 0, max = pi/2)
  
  # 2. verificare intersectii individuale
  # intersectie verticala (depinde de cosinus in aces sistem de axe)
  int_vert <- x <= (L/2) * cos(theta)
  # intersectie orizontala (depinde de sinus)
  int_oriz <- y <= (L/2) * sin(theta)
  
  # 3. reuniune (SAU logic)
  # acul intersecteaza planul daca atinge ORICARE linie
  intersectie_totala <- int_vert | int_oriz
  
  # 4. calcul
  prob_est <- mean(intersectie_totala)
  prob_teo <- (L * (2*d1 + 2*d2 - L)) / (pi * d1 * d2)
  
  return (list(
    estimat = prob_est,
    teoretic = prob_teo
  ))
}

# exemplu: L=1, d1=2, d2=3
# asteptat: aprox 0.477
simulare_grid(10000, 1, 2, 3)

# subpunctul f

algoritm_las_vegas <- function(tinta, interval_max = 100) {
  incercari <- 0
  gasit <- FALSE
  
  # ruleaza pana gaseste solutia corecta
  while (!gasit) {
    incercari <- incercari + 1
    # generam o propunere aleatoare
    propunere <- sample(1:interval_max, 1)
    
    # verificam daca este solutia corecta
    if (propunere == tinta) {
      gasit <- TRUE
    }
  }
  
  # returneaza numarul de pasi (care este variabil)
  # totusi, rezultatul "Am gasit tinta" este garantat corect
  return (incercari)
}

# rulare: cautam numarul 42 in intervalul [1, 100]
set.seed(123)
pasi_necesari <- algoritm_las_vegas(42, 100)
cat("Algoritmul a gasit solutia corecta dupa ", pasi_necesari, " incercari.\n")
