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

calculeaza_momente_masa  <- function(functie) {
  if (!is.numeric(functie)) {
    stop("Funcția de masă trebuie să fie de tip numeric")
  }
  
  rezultate <- c()

  # Calcularea mediei
  medie <- sum(functie * seq_along(functie))
  rezultate <- c(rezultate, medie)

  # Calcularea dispersiei
  dispersie <- sum(functie * (seq_along(functie) - medie)^2)
  rezultate <- c(rezultate, dispersie)

  # Calcularea momentelor inițiale și centrate până la ordinul 4
  for (i in 1:4) {
    moment_initial <- sum(functie * seq_along(functie)^i)
    moment_centrat <- sum(functie * (seq_along(functie) - medie)^i)

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

calculeaza_momente_densitate <- function(functie) {
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

calculeaza_momente_masa(mass_function)

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
calculeaza_probabilitati <- function(P_X, P_Y_given_X = NULL, P_X_given_Y = NULL) {
  rezultate <- list()

  # Calcularea probabilității P(X)
  P_X_valoare <- function(x) {
    return(P_X[x])
  }
  rezultate$P_X <- P_X_valoare

  # Calcularea probabilității P(Y|X)
  if (!is.null(P_Y_given_X)) {
    P_Y_given_X_valoare <- function(y, x) {
      return(P_Y_given_X[y, x] / P_X[x])
    }
    rezultate$P_Y_given_X <- P_Y_given_X_valoare
  }

  # Calcularea probabilității P(X|Y)
  if (!is.null(P_X_given_Y)) {
    P_X_given_Y_valoare <- function(x, y) {
      return(P_X_given_Y[x, y] / sum(P_X_given_Y[, y]))
    }
    rezultate$P_X_given_Y <- P_X_given_Y_valoare
  }

  return(rezultate)
}

# Exemplu 1: Probabilități marginale
P_X <- c(0.3, 0.7)  # P(X = 1) = 0.3, P(X = 2) = 0.7

rezultate <- calculeaza_probabilitati(P_X)

# Calcularea și afișarea probabilității P(X = 1)
print(rezultate$P_X(1))

# Calcularea și afișarea probabilității P(X = 2)
print(rezultate$P_X(2))


# Exemplu 2: Probabilități condiționate
P_Y_given_X <- matrix(c(0.2, 0.8, 0.6, 0.4), nrow = 2)  # P(Y = 1 | X = 1) = 0.2, P(Y = 2 | X = 1) = 0.8, P(Y = 1 | X = 2) = 0.6, P(Y = 2 | X = 2) = 0.4

rezultate <- calculeaza_probabilitati(P_X, P_Y_given_X = P_Y_given_X)

# Calcularea și afișarea probabilității P(Y = 1 | X = 1)
print(rezultate$P_Y_given_X(1, 1))

# Calcularea și afișarea probabilității P(Y = 2 | X = 2)
print(rezultate$P_Y_given_X(2, 2))


# Exemplu 3: Probabilități condiționate invers
P_X_given_Y <- matrix(c(0.4, 0.6, 0.7, 0.3), nrow = 2)  # P(X = 1 | Y = 1) = 0.4, P(X = 2 | Y = 1) = 0.6, P(X = 1 | Y = 2) = 0.7, P(X = 2 | Y = 2) = 0.3

rezultate <- calculeaza_probabilitati(P_X, P_X_given_Y = P_X_given_Y)

# Calcularea și afișarea probabilității P(X = 1 | Y = 1)
print(rezultate$P_X_given_Y(1, 1))

# Calcularea și afișarea probabilității P(X = 2 | Y = 2)
print(rezultate$P_X_given_Y(2, 2))


# Exemplu 4: Utilizarea tuturor tipurilor de probabilități
P_X <- c(0.3, 0.7)
P_Y_given_X <- matrix(c(0.2, 0.8, 0.6, 0.4), nrow = 2)
P_X_given_Y <- matrix(c(0.4, 0.6, 0.7, 0.3), nrow = 2)

rezultate <- calculeaza_probabilitati(P_X, P_Y_given_X = P_Y_given_X, P_X_given_Y = P_X_given_Y)

# Calcularea și afișarea probabilității P(X = 2)
print(rezultate$P_X(2))

# Calcularea și afișarea probabilității P(Y = 1 | X = 1)
print(rezultate$P_Y_given_X(1, 1))

# Calcularea și afișarea probabilității P(X = 2 | Y = 2)
print(rezultate$P_X_given_Y(2, 2))


##### 7.) Calculul covarianței și coeficientului de corelație
# pentru două variabile aleatoare:

calculeaza_cov_corelatie <- function(P_XY, P_X, P_Y) {
  # Calcularea mediei variabilei X
  media_X <- sum(P_X * seq_along(P_X))

  # Calcularea mediei variabilei Y
  media_Y <- sum(P_Y * seq_along(P_Y))

  # Calcularea covarianței
  covarianța <- sum((seq_along(P_XY) - media_X) * (seq_along(P_XY) - media_Y) * P_XY)

  # Calcularea deviației standard a lui X și Y
  deviatie_X <- sqrt(sum(((seq_along(P_X) - media_X) ^ 2) * P_X))
  deviatie_Y <- sqrt(sum(((seq_along(P_Y) - media_Y) ^ 2) * P_Y))

  # Calcularea coeficientului de corelație
  corelatie <- covarianța / (deviatie_X * deviatie_Y)

  rezultate <- list(
    covarianța = covarianța,
    corelatie = corelatie
  )

  return(rezultate)
}

# Funcția de masă/densitatea comună a celor două variabile aleatoare X și Y
P_XY <- matrix(c(0.1, 0.2, 0.3, 0.4), nrow = 2)  # Exemplu fictiv

# Funcția de masă/densitatea marginală a variabilei X
P_X <- c(0.3, 0.7)

# Funcția de masă/densitatea marginală a variabilei Y
P_Y <- c(0.4, 0.6)

# Calcularea covarianței și coeficientului de corelație
rezultate <- calculeaza_cov_corelatie(P_XY, P_X, P_Y)

# Afișarea rezultatelor
print(rezultate$covarianța)
print(rezultate$corelatie)


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
calculeaza_distributii <- function(f_comuna, valoare_X = NULL, valoare_Y = NULL) {
  rezultate <- list()

  # Calcularea funcțiilor de masă/densității marginale a variabilei X
  f_marginal_X <- apply(f_comuna, 1, sum)
  rezultate$marginal_X <- f_marginal_X

  # Calcularea funcțiilor de masă/densității marginale a variabilei Y
  f_marginal_Y <- apply(f_comuna, 2, sum)
  rezultate$marginal_Y <- f_marginal_Y

  # Verificarea și calcularea funcțiilor de masă/densității condiționate
  if (!is.null(valoare_X)) {
    if (valoare_X < 1 || valoare_X > nrow(f_comuna)) {
      stop("Valoarea specificată pentru X este invalidă")
    }
    f_conditional <- f_comuna[valoare_X, ] / sum(f_comuna[valoare_X, ])
    rezultate$conditional_Y_given_X <- f_conditional
  }

  if (!is.null(valoare_Y)) {
    if (valoare_Y < 1 || valoare_Y > ncol(f_comuna)) {
      stop("Valoarea specificată pentru Y este invalidă")
    }
    f_conditional <- f_comuna[, valoare_Y] / sum(f_comuna[, valoare_Y])
    rezultate$conditional_X_given_Y <- f_conditional
  }

  return(rezultate)
}

f_comuna <- matrix(c(0.1, 0.2, 0.3, 0.4), nrow = 2)
calculeaza_distributii(f_comuna, valoare_X = 1, valoare_Y = 2)

##### 9.) Verificarea dacă un set de valori este extrase dintr-o anumită repartiție:

  # Seturile de valori
  set_a <- c(7, 4, 2, 11, 2, 1, 2, 1, 6, 6, 0, 1, 3, 9, 7, 0, 1, 14, 0, 5, 1, 5, 2, 4, 3, 1, 0, 0, 26, 1)
  set_b <- c(-1.91, -0.97, 4.59, 2.19, -0.86, -0.74, -0.60, -1.29, 0.93, 1.42, 2.14, -2.01, 2.60, 1.45, 2.60, -3.32, -3.62, 3.09, 2.91, 3.60, -0.83, -0.27, 1.82, -1.38, -1.76, 1.43, -0.59, -1.34, 2.07, 1.02)
  set_c <- c(0.90, 8.91, 0.06, 1.85, 1.61, 6.50, 0.26, 0.04, 0.62, 1.01, 3.42, 1.45, 3.44, 0.46, 0.55, 0.09, 2.22, 0.65, 0.61, 6.45, 0.27, 4.81, 2.27, 0.34, 4.51, 0.42, 3.71, 2.59, 0.42, 11.18)
  set_d <- c(4.83, 4.37, 5.57, 4.22, 5.96, 5.11, 5.52, 4.81, 5.19, 4.19, 4.73, 5.92, 5.63, 4.53, 4.67, 4.84, 5.25, 5.06, 5.98, 5.25, 4.60, 4.11, 4.32, 5.09, 5.25, 5.10, 4.36, 5.40, 5.33, 4.65)
  set_e <- c(11, 11, 10, 10, 10, 6, 5, 9, 11, 10, 14, 8, 11, 6, 13, 9, 14, 16, 14, 10, 7, 7, 11, 12, 9, 5, 12, 15, 9, 12)

  # 9.1 Histograma, mediana, media și deviația standard
analiza_set_valori <- function(set_valori) {
  # Histograma valorilor
  hist(set_valori, breaks = "FD", main = "Histograma - Setul de valori", xlab = "Valoare", ylab = "Frecvență", col = "gray", border = "black")
  
  # Calculul medianei, mediei și deviației standard
  median_val <- median(set_valori)
  mean_val <- mean(set_valori)
  dev_stdev <- sd(set_valori)
  
  # Afisarea medianei, mediei si deviatiei standard pe desenul histogramelor
  abline(v = median_val, col = "red", lwd = 3, lty = 2)
  abline(v = mean_val, col = "blue", lwd = 3, lty = 2)
  text(median_val, par("usr")[4], round(median_val, 2), pos = 4, col = "red", offset = 0.2)
  text(mean_val, par("usr")[4], round(mean_val, 2), pos = 2, col = "blue", offset = 0.2)
  text(par("usr")[2], par("usr")[4], paste("Deviația standardă:", round(dev_stdev, 2)), pos = 1, col = "black", offset = 0.2)
}

analiza_set_valori(set_c)

  # 9.2 Identificarea repartiției



library(MASS)
library(fitdistrplus)

identifica_distributie <- function(data) {
  # Fit Normal distribution
  norm_fit <- fitdistr(data[data > 0], "normal")
  
  # Fit Exponential distribution
  exp_fit <- fitdistr(data[data > 0], "exponential")
  
  # Fit Poisson distribution
  pois_fit <- fitdistr(data[data > 0], "poisson")

  #Fit Gamma distribution
  gamma_fit <- fitdistr(data[data > 0], "gamma")
  
  # Calculate AIC values for each distribution
  norm_aic <- AIC(norm_fit)
  exp_aic <- AIC(exp_fit)
  pois_aic <- AIC(pois_fit)
  gamma_aic <- AIC(gamma_fit)
  
  # Identify the distribution with the lowest AIC
  if (norm_aic < exp_aic && norm_aic < pois_aic && norm_aic < gamma_aic) {
    return("Normal")
  } else if (exp_aic < norm_aic && exp_aic < pois_aic && exp_aic < gamma_aic) {
    return("Exponential")
  } else if( gamma_aic < norm_aic && gamma_aic < pois_aic && gamma_aic < exp_aic) {
    return("Gamma")
  } else {
    return("Poisson")
  }
}

identifica_distributie(set_e)

  # 9.3 Estimarea parametrilor în baza celor 5 eșantioane

  estimate_parameters <- function(data) {
  # Estimarea prin metoda verosimilității maxime (MLE)
  mle_fit <- fitdist(data, "norm", method = "mle")
  mle_mean <- mle_fit$estimate[1]
  mle_sd <- mle_fit$estimate[2]
  
  # Estimarea prin metoda momentelor
  moment_mean <- mean(data)
  moment_sd <- sd(data)
  
  # Returnarea rezultatelor
  result <- list(
    MLE_mean = mle_mean,
    MLE_sd = mle_sd,
    Moment_mean = moment_mean,
    Moment_sd = moment_sd
  )
  
  return(result)
}

estimate_parameters(set_a)

  # 9.4 Verificarea verosimilității extragerii dintr-o repartiție normală

  perform_normality_tests <- function(data) {
  # Testul Shapiro-Wilk
  shapiro_test <- shapiro.test(data)
  pValue <- shapiro_test$p.value
  
  if (pValue > 0.05) {
    print("Distribuția este normală")
  } else {
    print("Distribuția nu este normală")
  }
  return(shapiro_test)
}

perform_normality_tests(set_b)


# Apelarea funcției pentru rezolvarea exercițiului

##### 10.)
analiza_set_personalizat <- function() {
  # Citirea setului de valori de la tastatură
  set_valori <- readline(prompt = "Introduceți setul de valori (separate prin virgulă): ")
  set_valori <- as.numeric(strsplit(set_valori, ",")[[1]])
  
  # Meniu interactiv
  while (TRUE) {
    cat("\nAlegeți opțiunea dorită:")
    cat("\n1. Histograma, mediana, media și deviația standard")
    cat("\n2. Identificarea repartiției")
    cat("\n3. Estimarea parametrilor în baza celor 5 eșantioane")
    cat("\n4. Verificarea verosimilității extragerii dintr-o repartiție normală")
    cat("\n0. Ieșire\n")
    
    optiune <- readline(prompt = "Opțiune: ")
    
    if (optiune == "0") {
      break
    }
    
    if (optiune == "1") {
      # 9.1 Histograma, mediana, media și deviația standard
      analiza_set_valori(set_valori)
    } else if (optiune == "2") {
      # 9.2 Identificarea repartiției
      distributie_identificata <- identifica_distributie(set_valori)
      print(paste("Repartiția identificată:", distributie_identificata))
    } else if (optiune == "3") {
      # 9.3 Estimarea parametrilor în baza celor 5 eșantioane
      estimare_parametri <- estimate_parameters(set_valori)
      print("Estimarea parametrilor:")
      print(paste("Metoda verosimilității maxime (MLE) - Mean:", estimare_parametri$MLE_mean, "SD:", estimare_parametri$MLE_sd))
      print(paste("Metoda momentelor - Mean:", estimare_parametri$Moment_mean, "SD:", estimare_parametri$Moment_sd))
    } else if (optiune == "4") {
      # 9.4 Verificarea verosimilității extragerii dintr-o repartiție normală
      perform_normality_tests(set_valori)
    } else {
      cat("Opțiune invalidă! Vă rugăm să alegeți o opțiune validă.\n")
    }
  }
}

analiza_set_personalizat()
