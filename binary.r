# Instalujemy/włączamy wymagane pakiety
#install.packages("GA")
library(GA)
# Definiujemy zbiór danych i limit wartości napastników
napastnicyDb = data.frame(
  nazwisko = c("Rodrygo", "Haaland","Vinicius", "Kane", "Martinez", "Grealish", 
                "Benzema", "Dzeko", "Leao", "Ramos"),
  ocena = c(7.26, 7.41, 7.11, 6.99, 7.0, 7.25, 7.16, 7.05, 7.03, 7.01),
  wartosc = c(100, 180, 150, 110, 100, 75, 15, 4, 90, 50)
)
napastnicyLimit = 300
# Dodajemy ograniczenie dla maksymalnej liczby napastników
maxNapastnicy = 3
# Definiujemy funkcję przystosowania
fitnessFunc = function(chr) {
  calkowitaOcenaChr = chr %*% napastnicyDb$ocena
  calkowitaWartoscChr = chr %*% napastnicyDb$wartosc
  # Nowe warunki sprawdzające ograniczenia
  if (calkowitaWartoscChr > napastnicyLimit || sum(chr) > maxNapastnicy) 
    return(-calkowitaOcenaChr) 
  else return(calkowitaOcenaChr)
}
# Uruchamiamy algorytm genetyczny dla zadanych parametrów
wyniki = ga(type="binary", nBits=10, fitness=fitnessFunc, popSize=100,
            pcrossover=0.85, pmutation=0.05, elitism=5, maxiter=30, seed=10)
# Podsumowanie działania algorytmu genetycznego		   
summary(wyniki)
plot(wyniki)
# Dekodowanie (prezentacja) pojedynczego rozwiązania
decode = function(chr) {
  print("Rozwiązanie: ")
  print(napastnicyDb[chr == 1, ])
  print(paste("Wartość zawodników transfermarkt =", chr %*% napastnicyDb$wartosc))
  print(paste("Suma ocen zawodników sofascore =", chr %*% napastnicyDb$ocena))
  print(paste("Liczba zawodników =", sum(chr)))
}
decode(wyniki@solution[1,])

