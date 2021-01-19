### Nettoyage de l'environnement de travail (si nécessaire) ###

#rm(list = ls())

### Installation du package ###

#devtools::install_github("Jawad-Boulahfa/RadixSort")

### Chargement du package ###

library(RadixSort)

### Choix de la taille du vecteur à trier et de la précision des nombres décimaux
n <- 10
precision <- 2

### Nombres entiers naturels ###

V <- sample.int(n, replace = TRUE)*sign
cat("Vecteur à trier:", V, "\n")
cat("Vecteur trié (radix sort):", radix_sort_Rcpp(V), "\n")
cat("Vecteur trié (quick sort):", quick_sort_Rcpp_opti(V), "\n")

### Nombres entiers relatifs ###

sign <- runif(n,min=0,max=1)
sign[sign >= 0.5] <- 1
sign[sign < 0.5] <- -1
V <- sample.int(n, replace = TRUE)*sign
cat("Vecteur à trier:", V, "\n")
cat("Vecteur trié (radix sort):", radix_sort_Rcpp(V), "\n")
cat("Vecteur trié (quick sort):", quick_sort_Rcpp_opti(V), "\n")

### Nombres décimaux positifs ###

V <- round(runif(n, min=0, max = n), precision) 
cat("Vecteur à trier:", V, "\n")
cat("Vecteur trié (radix sort):", radix_sort_Rcpp_decimal(V), "\n")
cat("Vecteur trié (quick sort):", quick_sort_Rcpp_opti(V), "\n")

### Nombres décimaux signés ###

sign <- runif(n,min=0,max=1)
sign[sign>=0.5] <- 1
sign[sign<0.5] <- -1
V <- round(runif(n, min=0, max = n)*sign, precision) 
cat("Vecteur à trier:", V, "\n")
cat("Vecteur trié (radix sort):", radix_sort_Rcpp_decimal(V), "\n")
cat("Vecteur trié (quick sort):", quick_sort_Rcpp_opti(V), "\n")

### Vecteur trié dans l'ordre décroissant ###

V <- n:1
cat("Vecteur à trier:", V, "\n")
cat("Vecteur trié (radix sort):", radix_sort_Rcpp(V), "\n")
cat("Vecteur trié (quick sort):", quick_sort_Rcpp_opti(V), "\n")