##### 1.) Verificarea dacă o funcție introdusă de utilizator este
# funcție de masă/densitate de probabilitate:

#     Pentru a verifica dacă o funcție este o funcție de masă,
# putem verifica dacă suma valorilor funcției este 1.

#     Pentru a verifica dacă o funcție este o densitate de
# probabilitate, putem verifica dacă integrala funcției pe
# întregul domeniu este 1.

mass_function <- function(x) {
  ifelse(x %in% c(1, 2, 3), 1 / 3, 0)
}

density_function <- function(x) {
  dnorm(x, mean = 0, sd = 1)
}

# Verificare dacă o funcție este funcție de masă

is_prob_mass <- function(f) {
  sum_f <- sum(f(0:100))
  return(abs(sum_f - 1) < 1e-6)
}


# Verificare dacă o funcție este densitate de probabilitate
is_prob_density <- function(f) {
  integral <- integrate(f, lower=-Inf, upper=Inf)
  return(abs(integral$value - 1) < 1e-6)
}

is_prob_density(density_function)
is_prob_mass(density_function)

##### 2.) Determinarea constantei de normalizare:

#     Pentru a determina constanta de normalizare, putem integra
# funcția peste întregul domeniu și calcula inversul valorii integrale.

# Determinarea constantei de normalizare
find_normalization_constant <- function(f) {
  integral <- integrate(f, lower = -Inf, upper = 1)
  if (integral$message != "OK") {
    cat("Nu s-a putut calcula integrala.")
    return(NULL)
  }
  return(1 / integral$value)
}

find_normalization_constant(mass_function)

##### 3.) Reprezentarea grafică a densității de probabilitate/funcției
# de masă și a funcției de repartiție:

#     Folosind funcțiile grafice din R, putem crea grafice pentru densitatea
# de probabilitate/funcția de masă și funcția de repartiție.

#     Pentru funcțiile de repartiție care nu pot fi definite explicit,
# putem crea aproximări prin generarea unui set mare de valori și
# calcularea proporției valorilor mai mici decât fiecare valoare în set.

# Reprezentarea grafică a densității de probabilitate/funcției de masă

plot_graphics <- function(f) {
  x <- seq(-10, 10, length.out = 1000)
  density <- f(x)

  jpeg("./ProiectR/plot_density.jpg")
  plot(x, density, type = "l", main = "Densitate de probabilitate/Funcție de masă")
  dev.off()

  cumulative <- sapply(x, function(t) integrate(f, -10, t)$value)

  jpeg("./ProiectR/plot_cumulative.jpg")
  plot(x, cumulative, type = "l", main = "Funcție de repartiție")
  dev.off()
}

plot_graphics(mass_function)

##### 4.) Calculul mediei, dispersiei și a momentelor inițiale și centrate:

#     Medie: Calculăm suma produselor fiecărei valori cu probabilitatea
# corespunzătoare.
#     Dispersie: Calculăm suma pătratelor diferenței dintre fiecare
# valoare și media, ponderată cu probabilitatea corespunzătoare.

#     Moment inițial de ordin k: Calculăm suma produselor fiecărei
# valori ridicate la puterea k, ponderată cu probabilitatea corespunzătoare.

#     Moment central de ordin k: Calculăm suma produselor diferenței
# dintre fiecare valoare și media ridicate la puterea k, ponderată cu
# probabilitatea corespunzătoare.

calculeaza_momente <- function(functie) {
  rezultate <- c()

  # Calcularea mediei
  medie <- integrate(function(x) x * functie(x), lower = -Inf, upper = Inf)$value
  rezultate <- c(rezultate, medie)

  # Calcularea dispersiei
  dispersie <- integrate(function(x) (x - medie)^2 * functie(x), lower = -Inf, upper = Inf)$value
  rezultate <- c(rezultate, dispersie)

  # Calcularea momentelor inițiale și centrate până la ordinul 4
  for (i in 1:4) {
    moment_initial <- integrate(function(x) x^i * functie(x), lower = -Inf, upper = Inf)$value
    moment_centrat <- integrate(function(x) (x - medie)^i * functie(x), lower = -Inf, upper = Inf)$value

    # Verificăm dacă momentul inițial există
    if (!is.nan(moment_initial)) {
      rezultate <- c(rezultate, moment_initial)
    } else {
      # Dacă momentul inițial nu există, afișăm un mesaj corespunzător
      rezultate <- c(rezultate, paste("Momentul inițial de ordin", i, "nu există"))
    }

    # Verificăm dacă momentul centrat există
    if (!is.nan(moment_centrat)) {
      rezultate <- c(rezultate, moment_centrat)
    } else {
      # Dacă momentul centrat nu există, afișăm un mesaj corespunzător
      rezultate <- c(rezultate, paste("Momentul centrat de ordin", i, "nu există"))
    }
  }

  return(rezultate)
}

calculeaza_momente(density_function)

##### 5.) Calculul mediei și dispersiei unei variabile aleatoare g(X):

#     Pentru o funcție g(X), putem calcula media și dispersia rezultatei
# aplicării funcției asupra variabilei aleatoare X.

#     Putem utiliza funcțiile definite anterior pentru calculul mediei
# și dispersiei.

# Calculul mediei și dispersiei unei variabile aleatoare g(X)
f <- density_function
g <- function(x) {
  return(x^2)
}


calculeaza_medie_dispersie <- function(f, g) {
  medie <- integrate(function(x) f(x) * g(x), lower = -Inf, upper = Inf)$value
  
  dispersie <- integrate(function(x) (g(x) - medie)^2 * f(x), lower = -Inf, upper = Inf)$value
  
  rezultate <- list(medie = medie, dispersie = dispersie)
  
  return(rezultate)
}

print(paste(calculeaza_medie_dispersie(f, g)))

##### 6.) Calculul diferitelor tipuri de probabilități asociate unei variabile aleatoare:

#     Implementăm funcții pentru calculul probabilităților condiționate,
# probabilităților marginale, etc., în funcție de cerințe.

#     Vom folosi formulele și proprietățile probabilităților pentru a
# realiza calculele corespunzătoare.

# Calculul probabilității condiționate
calculate_conditional_probability <- function(f_joint, f_marginal, event) {
  # event este un vector care specifică valorile
  # evenimentului de forma c(X1=x1, X2=x2, ...)
  return(f_joint(event) / f_marginal(event[-1]))
}

# Calculul probabilității marginale
calculate_marginal_probability <- function(f_joint, variables_to_sum_out, values_to_sum_out) {
  # variables_to_sum_out este un vector cu variabilele care trebuie eliminate
  # values_to_sum_out este un vector cu valorile corespunzătoare variabilelor
  # de eliminat

  indices_to_keep <- !variables_to_sum_out %in% names(values_to_sum_out)
  event <- c(values_to_sum_out, rep(0, length(variables_to_sum_out) - length(values_to_sum_out)))
  sum_over <- variables_to_sum_out[indices_to_keep]
  probabilities <- numeric()

  for (i in seq_along(sum_over)) {
    event[sum_over[i]] <- 0
    probabilities <- c(probabilities, f_joint(event))
  }


  return(sum(probabilities))
}

##### 7.) Calculul covarianței și coeficientului de corelație
# pentru două variabile aleatoare:

#     Pentru a calcula covarianța, vom utiliza definiția acesteia
# ca suma produselor diferențelor dintre valorile fiecărei variabile
# și media lor, ponderate cu probabilitățile corespunzătoare.

#     Pentru a calcula coeficientul de corelație,
# vom utiliza formula care implică covarianța și deviațiile
# standard ale celor două variabile.

# Calculul covarianței
calculate_covariance <- function(x1, x2, probs) {
  mean_x1 <- calculate_mean(x1, probs)
  mean_x2 <- calculate_mean(x2, probs)
  return(sum((x1 - mean_x1) * (x2 - mean_x2) * probs))
}

# Calculul coeficientului de corelație
calculate_correlation <- function(x1, x2, probs) {
  covariance <- calculate_covariance(x1, x2, probs)
  sd_x1 <- sqrt(calculate_variance(x1, probs))
  sd_x2 <- sqrt(calculate_variance(x2, probs))
  return(covariance / (sd_x1 * sd_x2))
}

##### 8.) Construirea funcțiilor de masă/densităților marginale și condiționate:

#     Vom utiliza funcțiile definite anterior pentru a construi
# funcțiile de masă/densităților marginale și condiționate.

#     Funcția de masă/densitatea marginală se obține prin
# eliminarea variabilelor care nu sunt de interes din funcția
#  de masă/densitatea comună.

#     Funcția de masă/densitatea condiționată se obține prin
# împărțirea funcției de masă/densității marginale la funcția
# de masă/densitatea marginală a variabilei condiționate.

# Construirea funcției de masă marginală
build_marginal_pmf <- function(f_joint, variables_to_keep) {
  return(function(event) {
    event_marginal <- event[variables_to_keep]
    return(f_joint(event_marginal))
  })
}

# Construirea funcției de masă condiționată
build_conditional_pmf <- function(f_joint, f_marginal, variable_to_condition, value_to_condition) {
  return(function(event) {
    event_conditional <- c(event, value_to_condition)
    return(f_joint(event_conditional) / f_marginal(event_conditional[-1]))
  })
}

##### 9.) Verificarea dacă un set de valori este extrase dintr-o anumită repartiție:

solve_sets <- function() {
  # Seturile de valori
  set_a <- c(7, 4, 2, 11, 2, 1, 2, 1, 6, 6, 0, 1, 3, 9, 7, 0, 1, 14, 0, 5, 1, 5, 2, 4, 3, 1, 0, 0, 26, 1)
  set_b <- c(-1.91, -0.97, 4.59, 2.19, -0.86, -0.74, -0.60, -1.29, 0.93, 1.42, 2.14, -2.01, 2.60, 1.45, 2.60, -3.32, -3.62, 3.09, 2.91, 3.60, -0.83, -0.27, 1.82, -1.38, -1.76, 1.43, -0.59, -1.34, 2.07, 1.02)
  set_c <- c(0.90, 8.91, 0.06, 1.85, 1.61, 6.50, 0.26, 0.04, 0.62, 1.01, 3.42, 1.45, 3.44, 0.46, 0.55, 0.09, 2.22, 0.65, 0.61, 6.45, 0.27, 4.81, 2.27, 0.34, 4.51, 0.42, 3.71, 2.59, 0.42, 11.18)
  set_d <- c(4.83, 4.37, 5.57, 4.22, 5.96, 5.11, 5.52, 4.81, 5.19, 4.19, 4.73, 5.92, 5.63, 4.53, 4.67, 4.84, 5.25, 5.06, 5.98, 5.25, 4.60, 4.11, 4.32, 5.09, 5.25, 5.10, 4.36, 5.40, 5.33, 4.65)
  set_e <- c(11, 11, 10, 10, 10, 6, 5, 9, 11, 10, 14, 8, 11, 6, 13, 9, 14, 16, 14, 10, 7, 7, 11, 12, 9, 5, 12, 15, 9, 12)

  # 9.1 Histograma, mediana, media și deviația standard
  hist(set_a, main = "Histograma setului A")
  median_a <- median(set_a)
  mean_a <- mean(set_a)
  sd_a <- sd(set_a)
  print(paste("Mediana setului A:", median_a))
  print(paste("Media setului A:", mean_a))
  print(paste("Deviația standard a setului A:", sd_a))

  hist(set_b, main = "Histograma setului B")
  median_b <- median(set_b)
  mean_b <- mean(set_b)
  sd_b <- sd(set_b)
  print(paste("Mediana setului B:", median_b))
  print(paste("Media setului B:", mean_b))
  print(paste("Deviația standard a setului B:", sd_b))

  hist(set_c, main = "Histograma setului C")
  median_c <- median(set_c)
  mean_c <- mean(set_c)
  sd_c <- sd(set_c)
  print(paste("Mediana setului C:", median_c))
  print(paste("Media setului C:", mean_c))
  print(paste("Deviația standard a setului C:", sd_c))

  hist(set_d, main = "Histograma setului D")
  median_d <- median(set_d)
  mean_d <- mean(set_d)
  sd_d <- sd(set_d)
  print(paste("Mediana setului D:", median_d))
  print(paste("Media setului D:", mean_d))
  print(paste("Deviația standard a setului D:", sd_d))

  hist(set_e, main = "Histograma setului E")
  median_e <- median(set_e)
  mean_e <- mean(set_e)
  sd_e <- sd(set_e)
  print(paste("Mediana setului E:", median_e))
  print(paste("Media setului E:", mean_e))
  print(paste("Deviația standard a setului E:", sd_e))

  # 9.2 Identificarea repartiției

  # Testul Shapiro-Wilk pentru fiecare set de valori
  shapiro_test_a <- shapiro.test(set_a)
  shapiro_test_b <- shapiro.test(set_b)
  shapiro_test_c <- shapiro.test(set_c)
  shapiro_test_d <- shapiro.test(set_d)
  shapiro_test_e <- shapiro.test(set_e)

  # Afișarea rezultatelor testului Shapiro-Wilk
  print("Rezultatele testului Shapiro-Wilk pentru setul A:")
  print(shapiro_test_a)

  print("Rezultatele testului Shapiro-Wilk pentru setul B:")
  print(shapiro_test_b)

  print("Rezultatele testului Shapiro-Wilk pentru setul C:")
  print(shapiro_test_c)

  print("Rezultatele testului Shapiro-Wilk pentru setul D:")
  print(shapiro_test_d)

  print("Rezultatele testului Shapiro-Wilk pentru setul E:")
  print(shapiro_test_e)

  # Graficul Q-Q pentru fiecare set de valori
  qqnorm(set_a)
  qqline(set_a)
  qqnorm(set_b)
  qqline(set_b)
  qqnorm(set_c)
  qqline(set_c)
  qqnorm(set_d)
  qqline(set_d)
  qqnorm(set_e)
  qqline(set_e)


  # 9.3 Estimarea parametrilor în baza celor 5 eșantioane
  estimate_parameters <- function(data) {
    mean_estimate <- mean(data)
    sd_estimate <- sd(data)
    return(list(mean = mean_estimate, sd = sd_estimate))
  }

  # Estimarea parametrilor pentru fiecare set de valori
  params_a <- estimate_parameters(set_a)
  params_b <- estimate_parameters(set_b)
  params_c <- estimate_parameters(set_c)
  params_d <- estimate_parameters(set_d)
  params_e <- estimate_parameters(set_e)

  # Afișarea rezultatelor
  print("Estimările parametrilor pentru setul A:")
  print(params_a)

  print("Estimările parametrilor pentru setul B:")
  print(params_b)

  print("Estimările parametrilor pentru setul C:")
  print(params_c)

  print("Estimările parametrilor pentru setul D:")
  print(params_d)

  print("Estimările parametrilor pentru setul E:")
  print(params_e)

  # 9.4 Verificarea verosimilității extragerii dintr-o repartiție normală
  # (nu este implementată în codul de mai sus)
}

# Apelarea funcției pentru rezolvarea exercițiului
solve_sets()

##### 10.)

solve_questions <- function() {
  # Citirea setului de valori de la tastatura
  values <- readline(prompt = "Introduceti setul de valori (separate prin spatiu): ")
  values <- strsplit(values, " ")[[1]]

  # Convertirea valorilor in tipul numeric
  values <- as.numeric(values)

  # 9.1 Histograma, mediana, media si deviatia standard
  hist(values, main = "Histograma setului de valori")
  median_val <- median(values)
  mean_val <- mean(values)
  sd_val <- sd(values)
  print(paste("Mediana setului de valori:", median_val))
  print(paste("Media setului de valori:", mean_val))
  print(paste("Deviatia standard a setului de valori:", sd_val))

  # 9.2 Identificarea repartitiei


  # NU MERGE!!!   library(MASS)

  # fit_distribution <- function(data) {
  #   dist_names <- c("normal", "exponential", "gamma", "lognormal", "weibull")
  #   fit_results <- list()

  #   for (dist_name in dist_names) {
  #     fit <- fitdistr(data, dist_name)
  #     fit_results[[dist_name]] <- fit
  #   }

  #   return(fit_results)
  # }

  # # Apelarea functiei pentru setul de valori introdus de utilizator
  # fit_results <- fit_distribution(values)

  # # Afisarea rezultatelor
  # for (dist_name in names(fit_results)) {
  #   print(paste("Repartitia", dist_name))
  #   print(fit_results[[dist_name]])
  # }

  # 9.3 Estimarea parametrilor
  estimate_parameters <- function(data) {
    mean_estimate <- mean(data)
    sd_estimate <- sd(data)
    return(list(mean = mean_estimate, sd = sd_estimate))
  }

  params <- estimate_parameters(values)
  print("Estimarile parametrilor:")
  print(params)

  # 9.4 Verificarea verosimilitatii extragerii dintr-o repartitie normala - se poate adauga cod aici pentru a verifica verosimilitatea
}

# Apelarea functiei
solve_questions()