#Tsuga canadensis

#Calling packages

library(fitdistrplus)
library(distr)
library(dplyr)

## First Step  CREATE 10,000 RANDOM DBH // Large Numer DBH


##Specie: Tsuga canadensis  TsugaCa

rsqp9<-0.995 ##Published R^2 value 
minDBH9<-10 #From Jenkin´s
maxDBH9<-55 #From Jenkin´s
B0_9<- -2.2925 #From Ry´s paper
B1_9<- 2.35  #From Ry´s paper
CF9<- 1.003 #Should we include it?

##CREATE 10,000 RANDOM DBH

test9 <- matrix(rnorm(10000 * 1000), nrow = 10000, ncol = 1000)

##Create Random DBHs over the range sampled 


#Cm unit

dbhTsugaCa <- minDBH9 + (maxDBH9 - minDBH9) * runif(10000, min = 0, max = 1)

## CALCULATE BIOMASS##

## calculate the biomass using the published equation form

meany9 <-(exp(B0_9 + B1_9 * log(dbhTsugaCa))*CF9) #Should I multiply for the CF?

##Introduce Random Error into calculated biomass

##this next part fuzzies the biomasses to create 1000 different populations ## 

ys9 <- matrix(rep(meany9, times = 1000), nrow = length(meany9), ncol = 1000)

## creating the steps used to generate wider ranges of fuzzed biomasses. This progression
## will create 1000 different biomass sets. It can be adjusted to larger or smaller steps
## as needed to match as close as possible the published R-square.

#stdevs <- seq(1, 200.8, by = 0.2)            
stdevs9 <- seq(0.1, 100, length.out=1000)  #works better

stdevs2_9 <- matrix(rep(stdevs9, each = 10000), nrow = 10000, ncol = length(stdevs9))  
dbh2_9 <- matrix(rep(dbhTsugaCa, times = 1000), nrow = length(dbhTsugaCa), ncol = 1000)

#psuedys=ys+stdevs2.*(test);%this makes the new biomasses if no heteroscedasticity #

psuedys9 <- ys9 + stdevs2_9 * test9 * dbh2_9

#this makes the new biomasses with heteroscedasticity #

rsq2_9 <- numeric(1000)  # memory allocation is all, speeds up

for (i in 1:1000) {  # get stats on the datasets using the fit vs. psuedo-population
  
  sst9 <- sum((psuedys9[, i] - mean(psuedys9[, i]))^2)  # Suma de cuadrados total // Total sum of squares
  sse9 <- sum((psuedys9[, i] - meany9)^2)  # Suma de cuadrados del error // Sum of squares of the error
  rsq2_9[i] <- 1 - (sse9 / sst9)  # Coeficiente de determinación R² // Coefficient of determination R²
}


## To search for closest dataset to the published R^2, subtract the R^2 from
## the R^2 of the equation and select the diff closest to zero.  
## This is vectorized saving a for loop


diffs9 <- abs(rsq2_9 - rsqp9)
I9 <- which.min(diffs9)  # Find the index of the minimum value
BMTsugaCa<- psuedys9[, I9]  # Select corresponding column

## Create figure for checking if result is reasonable ##


plot(dbhTsugaCa, BMTsugaCa, pch = 16, xlab = "DBH (cm)", ylab = "Biomass (kg)", main = "Tsuga Canadiensis")

# Write the data in an Excel file

PseudoDataTsugaCa <- data.frame(dbhTsugaCa, BMTsugaCa)
PseudoDataTsugaCa <- subset(PseudoDataTsugaCa, BMTsugaCa>1)

plot(PseudoDataTsugaCa$dbhTsugaCa, PseudoDataTsugaCa$BMTsugaCa, pch = 16, xlab = "DBH (cm)", ylab = "Biomass (kg)", main = "Tsuga Canadiensis")

# Specifies the full path to save the file

write.csv(PseudoDataTsugaCa, file = "TsugaCa.csv", row.names = FALSE)

## print(sse)
## mean(rsq2)


## Logaritmic differences

noiter<-10000
coefficients9 <- data.frame(intercept=rep(NA,noiter),slope=rep(NA,noiter))
for(i in 1:noiter){
  datatofit<- sample_n(PseudoDataTsugaCa,30,replace=FALSE)
  modelfit <- lm(log(BMTsugaCa) ~ log(dbhTsugaCa), data = na.omit(datatofit)) #Just add the other part
  
  
  coefficients9[i,] <- unname(coef(modelfit))
  
  
}



InterTsuga<-mean(coefficients9$intercept)
SlopeTsuga<-mean(coefficients9$slope)


any(is.na(datatofit)) #NA revision in the data


View(coefficients9)

# Name columns for clarity            Nombrar columnas para claridad
colnames(coefficients9) <- c("intercept_Tsuga", "slope_Tsuga")
# Adding the correlative 
coefficients9$correlative <- seq_len(nrow(coefficients9))

SDInterTsuga<-sd(coefficients9$intercept) #standar deviation intercept
SDSlopeTsuga<-sd(coefficients9$slope)


### NEW COVARIANCE ##

library(MASS)

cov_matrix_Tsuga <- cov(coefficients9)
mean_vector_Tsuga <- colMeans(coefficients9)


View(cov_matrix_Tsuga)

# Simulate new pairs of a and b      Simular nuevos pares de a y b
sim_ab_Tsuga <- as.data.frame(mvrnorm(n = 10000, mu = mean_vector_Tsuga, Sigma = cov_matrix_Tsuga))



# Name columns for clarity            Nombrar columnas para claridad
colnames(sim_ab_Tsuga) <- c("intercept_Tsuga", "slope_Tsuga")
sim_ab_Tsuga$correlative <- seq_len(nrow(sim_ab_Tsuga))
View(sim_ab_Tsuga)


## Is it true "? 



# Originial Data Datos originales
plot(coefficients9$intercept, coefficients9$slope, 
     main = "Original vs Simulated Tsuga", col = "blue", pch = 16, cex = 0.5,
     xlab = "Intercept", ylab = "Slope")

# Add SImulations Agregar simulaciones
points(sim_ab_Tsuga$intercept, sim_ab_Tsuga$slope, col = rgb(1, 0, 0, 0.3), pch = 16)
legend("topright", legend = c("Original", "Simulated"),
       col = c("blue", "red"), pch = 16)




