# Wczytanie biblioteki GA
library(GA)

# Funkcja celu - przestrzeń poszukiwań
fp <- function(x1, x2) {
  return ((6*x1^3 - x2) * exp(-x1^3 + x2^3))
}

# Tworzenie siatki punktów w przestrzeni poszukiwań
x1 <- seq(-4, 4, by=0.05)
x2 <- seq(-4.4, 4.4, by=0.05)
z <- outer(x1, x2, fp)

# Wizualizacja funkcji celu w przestrzeni 3D
persp(x1, x2, z, theta=30, phi=30, expand=0.5, col=7) 

# Wizualizacja funkcji celu jako obraz
image(x1, x2, z, col=terrain.colors(50))

# Dodanie kontur do wizualizacji
contour(x1, x2, z, add=T, col="grey50", nlevels=50)

# Funkcja monitorująca postęp algorytmu genetycznego
fm <- function(obj) {
  # Dodanie konturu do wizualizacji
  contour(x1, x2, z, col="darkgrey", nlevels=50,
          main=paste("iteration =", obj@iter))
  # Dodanie punktów reprezentujących populację
  points(obj@population, pch=20)
}

# Uruchomienie algorytmu genetycznego
wyniki <- ga(type="real-valued", lower=c(-4, -4.4), upper=c(4, 4.4),
              fitness=function(x) -fp(x[1], x[2]), monitor=fm,
              popSize=20, pcrossover=0.85, pmutation=0.05,
              elitism=5, maxiter=60, seed=10)

# Podsumowanie wyników algorytmu genetycznego
summary(wyniki)

# Wygenerowanie wykresów przedstawiających proces ewolucji
plot(wyniki)
