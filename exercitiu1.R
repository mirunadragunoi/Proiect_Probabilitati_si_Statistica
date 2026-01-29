# PROIECT - EXERCITIUL 1
# SIMULAREA UNUI VECTOR ALEATOR PE DISCUL UNITAR

# incarc pachetele necesare
library(ggplot2)
library(gridExtra)
library(grid)

# setez seed pentru reproductibilitate
set.seed(42)

# constanta
N <- 1000

# CERINTA 2 - metoda acceptarii si respingerii
rejection_sampling <- function(n) {
  points <- matrix(nrow = 0, ncol = 2)
  attempts <- 0
  
  while (nrow(points) < n) {
    x <- runif(1, -1, 1)
    y <- runif(1, -1, 1)
    attempts <- attempts + 1
    
    if (x^2 + y^2 <= 1) {
      points <- rbind(points, c(x, y))
    }
  }
  
  colnames(points) <- c("X", "Y")
  acceptance_rate <- n / attempts
  
  cat(sprintf("Metoda 1 - Acceptare-Respingere:\n"))
  cat(sprintf("  Puncte generate: %d\n", n))
  cat(sprintf("  Incercari totale: %d\n", attempts))
  cat(sprintf("  Rata acceptare: %.4f (teoretic: %.4f)\n\n", 
              acceptance_rate, pi/4))
  
  return(list(points = points, acceptance_rate = acceptance_rate, 
              attempts = attempts))
}

# CERINTA 5 - metoda coordonatelor polare
polar_method <- function(n) {
  # Theta ~ Uniform(0, 2π)
  theta <- runif(n, 0, 2*pi)
  
  # R ~ f_R(r) = 2r => R = sqrt(U), U ~ Uniform(0,1)
  r <- sqrt(runif(n, 0, 1))
  
  # transformare carteziene
  x <- r * cos(theta)
  y <- r * sin(theta)
  
  points <- cbind(x, y)
  colnames(points) <- c("X", "Y")
  
  cat(sprintf("Metoda 2 - Coordonate Polare:\n"))
  cat(sprintf("  Puncte generate: %d\n\n", n))
  
  return(points)
}

# cerinta 3 - calculul mediei distantei
compute_distances <- function(points) {
  sqrt(points[, 1]^2 + points[, 2]^2)
}

analyze_distances <- function(points, method_name) {
  distances <- compute_distances(points)
  empirical_mean <- mean(distances)
  theoretical_mean <- 2/3
  
  cat(sprintf("Analiza distantelor - %s:\n", method_name))
  cat(sprintf("  Media empirica: %.6f\n", empirical_mean))
  cat(sprintf("  Media teoretica: %.6f (2/3)\n", theoretical_mean))
  cat(sprintf("  Eroare relativa: %.2f%%\n\n", 
              abs(empirical_mean - theoretical_mean) / theoretical_mean * 100))
  
  return(distances)
}

# vizualizari
plot_results <- function(points1, points2, dist1, dist2, acceptance_rate) {
  # preg date
  df1 <- data.frame(X = points1[, 1], Y = points1[, 2])
  df2 <- data.frame(X = points2[, 1], Y = points2[, 2])
  
  # 1. PUNCTE METODA 1
  p1 <- ggplot(df1, aes(x = X, y = Y)) +
    geom_point(alpha = 0.5, size = 1.5, color = "blue") +
    annotate("path", x = cos(seq(0, 2*pi, length.out = 100)),
             y = sin(seq(0, 2*pi, length.out = 100)),
             color = "red", size = 1) +
    coord_fixed(ratio = 1, xlim = c(-1.2, 1.2), ylim = c(-1.2, 1.2)) +
    labs(title = sprintf("Metoda 1: Acceptare-Respingere (N=%d, Rata=%.3f)", 
                         N, acceptance_rate),
         x = expression(X[1]), y = expression(X[2])) +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5, size = 10))
  
  # 2. PUNCTE METODA 2
  p2 <- ggplot(df2, aes(x = X, y = Y)) +
    geom_point(alpha = 0.5, size = 1.5, color = "green") +
    annotate("path", x = cos(seq(0, 2*pi, length.out = 100)),
             y = sin(seq(0, 2*pi, length.out = 100)),
             color = "red", size = 1) +
    coord_fixed(ratio = 1, xlim = c(-1.2, 1.2), ylim = c(-1.2, 1.2)) +
    labs(title = sprintf("Metoda 2: Coordonate Polare (N=%d)", N),
         x = expression(X[1]), y = expression(X[2])) +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5, size = 10))
  
  # 3. HISTOGRAMA DISTANTE
  df_dist <- data.frame(
    Distance = c(dist1, dist2),
    Method = rep(c("Metoda 1", "Metoda 2"), c(length(dist1), length(dist2)))
  )
  
  r_theory <- seq(0, 1, length.out = 100)
  df_theory <- data.frame(r = r_theory, density = 2*r_theory)
  
  p3 <- ggplot() +
    geom_histogram(data = df_dist, aes(x = Distance, y = after_stat(density), 
                                       fill = Method),
                   alpha = 0.5, position = "identity", bins = 30) +
    geom_line(data = df_theory, aes(x = r, y = density), 
              color = "red", size = 1) +
    geom_vline(xintercept = 2/3, color = "red", linetype = "dashed", size = 1) +
    scale_fill_manual(values = c("Metoda 1" = "blue", "Metoda 2" = "green")) +
    labs(title = "Distributia Distantelor (f_R(r) = 2r)",
         x = "Distanta R", y = "Densitate") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5, size = 10),
          legend.position = "bottom")
  
  # 4. CDF EMPIRIC VS TEORETIC
  r_sorted <- sort(dist1)
  df_cdf <- data.frame(
    r = rep(r_sorted, 2),
    CDF = c(seq_along(r_sorted) / length(r_sorted), r_sorted^2),
    Type = rep(c("Empiric", "Teoretic F_R(r)=r^2"), each = length(r_sorted))
  )
  
  p4 <- ggplot(df_cdf, aes(x = r, y = CDF, color = Type, linetype = Type)) +
    geom_line(size = 1) +
    scale_color_manual(values = c("Empiric" = "black", 
                                  "Teoretic F_R(r)=r^2" = "red")) +
    scale_linetype_manual(values = c("Empiric" = "solid", 
                                     "Teoretic F_R(r)=r^2" = "dashed")) +
    labs(title = "Functia de Repartitie pentru R", x = "r", y = "F(r)") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5, size = 10),
          legend.position = "bottom")
  
  # 5. DISTRIBUTIA UNGHIULUI Θ
  theta2 <- atan2(points2[, 2], points2[, 1])
  theta2 <- ifelse(theta2 < 0, theta2 + 2*pi, theta2)
  
  p5 <- ggplot(data.frame(Theta = theta2), aes(x = Theta)) +
    geom_histogram(aes(y = after_stat(density)), bins = 30, 
                   fill = "green", alpha = 0.7, color = "black") +
    geom_hline(yintercept = 1/(2*pi), color = "red", 
               linetype = "dashed", size = 1) +
    labs(title = "Distributia Unghiului Θ (f_Θ = 1/(2π))",
         x = "Θ (radiani)", y = "Densitate") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5, size = 10))
  
  # 6. Q-Q PLOT
  r_squared <- sort(dist1^2)
  df_qq <- data.frame(
    theoretical = qunif(ppoints(length(r_squared))),
    sample = r_squared
  )
  
  p6 <- ggplot(df_qq, aes(x = theoretical, y = sample)) +
    geom_point(alpha = 0.5, color = "blue") +
    geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
    labs(title = "Q-Q Plot: R^2 vs Uniform(0,1)",
         x = "Cuantile teoretice", y = "Cuantile empirice") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5, size = 10))
  
  # combin si salvez
  combined <- grid.arrange(p1, p2, p3, p4, p5, p6, ncol = 3,
                           top = textGrob("Rezultate Exercitiul 1",
                                          gp = gpar(fontsize = 14, font = 2)))
}

# teste pt statistici
statistical_tests <- function(points1, points2) {
  dist1 <- compute_distances(points1)
  dist2 <- compute_distances(points2)
  
  cat("Teste Statistice:\n")
  
  # test chi-patrat
  n_bins <- 10
  bin_edges <- seq(0, 1, length.out = n_bins + 1)
  observed <- hist(dist1, breaks = bin_edges, plot = FALSE)$counts
  expected <- sapply(1:n_bins, function(i) {
    (bin_edges[i+1]^2 - bin_edges[i]^2) * N
  })
  
  chi2_stat <- sum((observed - expected)^2 / expected)
  p_chi <- 1 - pchisq(chi2_stat, n_bins - 1)
  
  cat(sprintf("1. Chi-patrat (distributie radiala):\n"))
  cat(sprintf("   X^2= %.4f, p-value = %.4f\n", chi2_stat, p_chi))
  cat(sprintf("   %s (α=0.05)\n\n", 
              ifelse(p_chi > 0.05, "Acceptam uniformitatea", 
                     "Respingem uniformitatea")))
  
  # test KS pentru R^2
  ks_test <- ks.test(dist1^2, "punif", 0, 1)
  cat(sprintf("2. Kolmogorov-Smirnov (R^2~ Uniform(0,1)):\n"))
  cat(sprintf("   D = %.4f, p-value = %.4f\n", 
              ks_test$statistic, ks_test$p.value))
  cat(sprintf("   %s (α=0.05)\n\n",
              ifelse(ks_test$p.value > 0.05, "Acceptam uniformitatea",
                     "Respingem uniformitatea")))
  
  # compar metodele
  ks_comp <- ks.test(dist1, dist2)
  cat(sprintf("3. Comparatie metode (KS):\n"))
  cat(sprintf("   D = %.4f, p-value = %.4f\n",
              ks_comp$statistic, ks_comp$p.value))
  cat(sprintf("   Metodele %s\n\n",
              ifelse(ks_comp$p.value > 0.05, "produc aceeasi distributie",
                     "produc distributii diferite")))
}

# functia principala
main <- function() {
  cat("\n")
  cat("EXERCITIUL 1 - Simulare Disc Unitar\n")
  
  # Cerinta 2: Metoda acceptarii si respingerii
  result1 <- rejection_sampling(N)
  points1 <- result1$points
  
  # Cerinta 5: Metoda coordonatelor polare  
  points2 <- polar_method(N)
  
  # Cerinta 3: Analiza distantelor
  dist1 <- analyze_distances(points1, "Metoda 1")
  dist2 <- analyze_distances(points2, "Metoda 2")
  
  # vizualizari
  plot_results(points1, points2, dist1, dist2, result1$acceptance_rate)
  
  # teste statistice
  statistical_tests(points1, points2)
  
  cat("Executie finalizata cu succes!\n")
  
  return(list(
    points_method1 = points1,
    points_method2 = points2,
    distances_method1 = dist1,
    distances_method2 = dist2
  ))
}

# execut
results <- main()
