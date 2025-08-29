# Tsuga Canadensis 

#Monteith 1979 (New York), Range: 2.5 - 55 cm, Whole tree (above stump)

#Calling packages

library(fitdistrplus)
library(distr)
library(dplyr)

# First Step  CREATE 10,000 RANDOM DBH // Large Numer DBH
# Specie: Tsuga canadensis

# Equation 1 Monteith 1979 (New York)
# Units DBH mm and biomass kg 

rsqp9_1<-0.99 ##Published R^2 value 
minDBH9_1<-2.5*10 #From Jenkin´s *10 conversion to mm
maxDBH9_1<-55*10 #From Jenkin´s *10 conversion to mm
a_9_1<- 6.1371 
b_9_1<- -0.2785  
c_9_1<- 0.004286 
d_9_1<- 2

##CREATE 10,000 matrix

test9_1 <- matrix(rnorm(10000 * 1000), nrow = 10000, ncol = 1000) #Old form 
#test9_1 <- matrix(rnorm(10000 * 1000, mean = 0, sd = 0.1), nrow = 10000, ncol = 1000) # one form 
#test9_1 <- matrix(rnorm(10000 * 1000), nrow = 10000, ncol = 1000) / 10 # Other form 
test9_1 <- pmin(pmax(test9_1, -3), 3)  # truncar a ±3σ ->This works

##Create Random DBHs over the range sampled, dbh= mm unit

dbhTsugaCa_1 <- minDBH9_1 + (maxDBH9_1 - minDBH9_1) * runif(10000, min = 0, max = 1)

## CALCULATE BIOMASS

## calculate the biomass using the published equation form 
## biomass(kg) = a+b*dia+c*(dia^d)

meany9_1 <-a_9_1 + b_9_1*(dbhTsugaCa_1) + c_9_1*((dbhTsugaCa_1)^d_9_1)

#View(meany9_1)
## Introduce Random Error into calculated biomass ##
## this next part fuzzies the biomasses to create 1000 different populations ## 

ys9_1 <- matrix(rep(meany9_1, times = 1000), nrow = length(meany9_1), ncol = 1000)

# creating the steps used to generate wider ranges of fuzzed biomases. This progression
# will create 1000 different biomass sets. It can be adjusted to larger or smaller steps
# as needed to match as close as possible the published R-square.

#stdevs <- seq(1, 200.8, by = 0.2)            
#stdevs9_1 <- seq(0.1, 100, length.out=1000)  #works better
stdevs9_1 <- seq(0.05, 1, length.out=1000)
stdevs2_9_1 <- matrix(rep(stdevs9_1, each = 10000), nrow = 10000, ncol = length(stdevs9_1))  
dbh2_9_1 <- matrix(rep(dbhTsugaCa_1, times = 1000), nrow = length(dbhTsugaCa_1), ncol = 1000)

#psuedys9_1 <- ys9_1 + stdevs2_9_1 * test9_1  # this makes the new biomases if no heteroscedasticity #

psuedys9_1 <- ys9_1 + stdevs2_9_1 * test9_1 * dbh2_9_1 # This makes the new biomasses if heteroscedasticity

#this makes the new biomasses with heteroscedasticity #

rsq2_9_1 <- numeric(1000)  # memory allocation is all, speeds up

for (i in 1:1000) {  # get stats on the datasets using the fit vs. psuedo-population
  
  sst9_1 <- sum((psuedys9_1[, i] - mean(psuedys9_1[, i]))^2)  # Total sum of squares
  sse9_1 <- sum((psuedys9_1[, i] - meany9_1)^2)  # Sum of squares of the error 
  rsq2_9_1[i] <- 1 - (sse9_1 / sst9_1)  # Coefficient of determination R²
}

## To search for closest dataset to the published R^2, subtract the R^2 from
## the R^2 of the equation and select the diff closest to zero.  
## This is vectorized saving a for loop

diffs9_1 <- abs(rsq2_9_1 - rsqp9_1)
I9_1 <- which.min(diffs9_1)  # Find the index of the minimum value
BMTsugaCa_1<- psuedys9_1[, I9_1]  # Select corresponding column

I9_1
diffs9_1[64]

## Create figure for checking if result is reasonable ##

plot((dbhTsugaCa_1), BMTsugaCa_1, pch = 16, xlab = "DBH (cm)", ylab = "Biomass (kg)", main = "Tsuga Canadiensis")

# Write the data in an Excel file

PseudoDataTsugaCa1 <- data.frame(dbhTsugaCa_1, BMTsugaCa_1)
PseudoDataTsugaCa1 <- subset(PseudoDataTsugaCa1, BMTsugaCa_1>1)
PseudoDataTsugaCa1$eq <-"eq1_tsuga"

head(PseudoDataTsugaCa1)
PseudoDataTsugaCa1$dbhTsugaCa_1 <- PseudoDataTsugaCa1$dbhTsugaCa_1 / 10  #convertion to cm

head(PseudoDataTsugaCa1)

#

#Aún no sé como encontrar los coeficientes aquí :(((

## Logaritmic differences

noiter <- 10000
coefficients9_1 <- data.frame(intercept = rep(NA, noiter), slope = rep(NA, noiter))

for (i in 1:noiter) {
  
  datatofit_1 <- sample_n(PseudoDataTsugaCa1, 33, replace = FALSE)
  modelfit_1 <- lm(BMTsugaCa_1 ~ log(dbhTsugaCa_1), data = na.omit(datatofit_1))
  coefficients9_1[i, ] <- unname(coef(modelfit_1))
}

# Muestra las primeras filas
head(coefficients9_1)
head(datatofit_1)

#Mean
InterTsuga<-mean(coefficients9$intercept)
SlopeTsuga<-mean(coefficients9$slope)

any(is.na(datatofit)) #NA revision in the data

#SD
SDInterTsuga<-sd(coefficients9$intercept) #standar deviation intercept
SDSlopeTsuga<-sd(coefficients9$slope)

#Percentile
#50th percentile
QinterTsuga<-quantile(coefficients9$intercept, probs = 0.5)
QSlopeTsuga<-quantile(coefficients9$slope, probs = 0.5)



View(coefficients9)

# Name columns for clarity            Nombrar columnas para claridad
colnames(coefficients9) <- c("intercept_Tsuga", "slope_Tsuga")
# Adding the correlative 
coefficients9$correlative <- seq_len(nrow(coefficients9))





#View(PseudoDataTsugaCa1)
#plot(PseudoDataTsugaCa1$dbhTsugaCa, PseudoDataTsugaCa1$BMTsugaCa, pch = 16, xlab = "DBH (mm)", ylab = "Biomass (kg)", main = "Tsuga Canadiensis")
#plot(PseudoDataTsugaCa1$dbhTsugaCa, PseudoDataTsugaCa1$BMTsugaCa,pch = 16, col = rgb(0.2, 0.4, 0.6, 0.5), azul semi-transparente, xlab = "DBH (mm)", ylab = "Biomass (kg)", main = "Tsuga canadensis",cex = 1.2)  # tamaño de los puntos
#grid()  # agrega rejilla

# Specifies the full path to save the file
#write.csv(PseudoDataTsugaCa1, file = "TsugaCa.csv1", row.names = FALSE)

# Real Data #
Tsuga_Monteith <- data.frame(
  location = "New York",
  source   = "Monteith 1979",
  dbh      = seq(from = 2.5, to = 55, by = 2.5),
  kg       = c(2, 3, 9, 21, 38, 61, 89, 122, 160, 204,
               254, 308, 368, 434, 504, 581, 662, 749,
               841, 938, 1041, 1150)
)

Tsuga_Monteith

# pseudo data
plot(PseudoDataTsugaCa1$dbhTsugaCa, PseudoDataTsugaCa1$BMTsugaCa,
     pch = 16,
     col = rgb(0.2, 0.4, 0.6, 0.5),   # azul semi-transparente
     xlab = "DBH (cm)", 
     ylab = "Biomass (kg)", 
     main = "Tsuga canadensis",
     cex = 1.2)
grid()

# Monteith 1979
points(Tsuga_Monteith$dbh, Tsuga_Monteith$kg,
       pch = 17,   # triángulos
       col = rgb(0.8, 0.2, 0.2, 0.7), # rojo semi-transparente
       cex = 1.3)

legend("topleft",
       legend = c("Pseudo data", "Monteith 1979"),
       col = c(rgb(0.2,0.4,0.6,0.5), rgb(0.8,0.2,0.2,0.7)),
       pch = c(16, 17),
       bty = "n")

