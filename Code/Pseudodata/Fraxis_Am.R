#Fraxis americana FraxisAm

#Calling packages

library(fitdistrplus)
library(distr)
library(dplyr)

## First Step  CREATE 10,000 RANDOM DBH // Large Numer DBH


##Specie: Fraxis americana FraxisAm

rsqp4<-0.994 ##Published R^2 value 
minDBH4<-10 #From Jenkin´s
maxDBH4<-55 #From Jenkin´s
B0_4<- -1.7624 #From Ry´s paper
B1_4<- 2.307  #From Ry´s paper
CF4<- 1.004 #Should we include it?

##CREATE 10,000 RANDOM DBH

test4 <- matrix(rnorm(10000 * 1000), nrow = 10000, ncol = 1000)

##Create Random DBHs over the range sampled 


#Cm unit

dbhFraxisAm <- minDBH4 + (maxDBH4 - minDBH4) * runif(10000, min = 0, max = 1)

## CALCULATE BIOMASS##

## calculate the biomass using the published equation form

meany4 <- ((B0_4 + B1_4 * log(dbhFraxisAm)))*CF4 #Should I multiply for the CF?

##Introduce Random Error into calculated biomass

##this next part fuzzies the biomasses to create 1000 different populations ## 

ys4 <- matrix(rep(meany4, times = 1000), nrow = length(meany4), ncol = 1000)

## creating the steps used to generate wider ranges of fuzzed biomasses. This progression
## will create 1000 different biomass sets. It can be adjusted to larger or smaller steps
## as needed to match as close as possible the published R-square.

#stdevs <- seq(1, 200.8, by = 0.2)            
stdevs4 <- seq(0.1, 100, length.out=1000)  #works better

stdevs2_4 <- matrix(rep(stdevs4, each = 10000), nrow = 10000, ncol = length(stdevs4))  
dbh2_4 <- matrix(rep(dbhFraxisAm, times = 1000), nrow = length(dbhFraxisAm), ncol = 1000)

psuedys4 <- ys4 + stdevs2_4 * test4 #this makes the new biomasses if no heteroscedasticity #

#psuedys4 <- ys4 + stdevs2_4 * test4 * dbh2_4

#this makes the new biomasses with heteroscedasticity #

rsq2_4 <- numeric(1000)  # memory allocation is all, speeds up

for (i in 1:1000) {  # get stats on the datasets using the fit vs. psuedo-population
  
  sst4 <- sum((psuedys4[, i] - mean(psuedys4[, i]))^2)  # Suma de cuadrados total // Total sum of squares
  sse4 <- sum((psuedys4[, i] - meany4)^2)  # Suma de cuadrados del error // Sum of squares of the error
  rsq2_4[i] <- 1 - (sse4 / sst4)  # Coeficiente de determinación R² // Coefficient of determination R²
}


## To search for closest dataset to the published R^2, subtract the R^2 from
## the R^2 of the equation and select the diff closest to zero.  
## This is vectorized saving a for loop


diffs4 <- abs(rsq2_4 - rsqp4)
I4 <- which.min(diffs4)  # Find the index of the minimum value
BMFraxisAm<- psuedys4[, I4]  # Select corresponding column

## Create figure for checking if result is reasonable ##


plot(log(dbhFraxisAm), BMFraxisAm, pch = 16, xlab = "DBH (cm)", ylab = "Biomass (kg)", main = "Fraxis Americana")

# Write the data in an Excel file

PseudoDataFraxisAm <- data.frame(dbhFraxisAm, BMFraxisAm)
PseudoDataFraxisAm <- subset(PseudoDataFraxisAm, BMFraxisAm>1)

plot(PseudoDataFraxisAm$dbhFraxisAm, PseudoDataFraxisAm$BMFraxisAm, pch = 16, xlab = "DBH (cm)", ylab = "Biomass (kg)", main = "Fraxis Americana")

# Specifies the full path to save the file

write.csv(PseudoDataFraxisAm, file = "FraxisAm.csv", row.names = FALSE)

## print(sse)
## mean(rsq2)


## Logaritmic differences

noiter<-10000
coefficients4 <- data.frame(intercept=rep(NA,noiter),slope=rep(NA,noiter))
for(i in 1:noiter){
  datatofit<- sample_n(PseudoDataFraxisAm,20,replace=FALSE)
  modelfit <- lm((BMFraxisAm) ~ log(dbhFraxisAm), data = na.omit(datatofit)) #Just add the other part
  
  
  coefficients4[i,] <- unname(coef(modelfit))
  
  
}


#Mean
Interfraxis<-mean(coefficients4$intercept)
Slopefraxis<-mean(coefficients4$slope)


any(is.na(datatofit)) #NA revision in the data

#50th percentile
QinterFraxis<-quantile(coefficients4$intercept, probs = 0.5)
QSlopeFraxis<-quantile(coefficients4$slope, probs = 0.5)

#SD 
SDInterceptfraxis<-sd(coefficients4$intercept) #standar deviation intercept
SDSlopefraxis<-sd(coefficients4$slope)



#View(coefficients4)
# Name columns for clarity            Nombrar columnas para claridad
colnames(coefficients4) <- c("intercept_Fraxis", "slope_Fraxis")
# Adding the correlative 
coefficients4$correlative <- seq_len(nrow(coefficients4))

### FINISH METHOD 1 HERE ####




### NEW COVARIANCE ##

library(MASS)

cov_matrix_Fraxis <- cov(coefficients4)
mean_vector_Fraxis <- colMeans(coefficients4)


View(cov_matrix_Fraxis)

# Simulate new pairs of a and b      Simular nuevos pares de a y b
sim_ab_Fraxis <- as.data.frame(mvrnorm(n = 10000, mu = mean_vector_Fraxis, Sigma = cov_matrix_Fraxis))



# Name columns for clarity            Nombrar columnas para claridad
colnames(sim_ab_Fraxis) <- c("intercept_Fraxis", "slope_Fraxis")
sim_ab_Fraxis$correlative <- seq_len(nrow(sim_ab_Fraxis))
View(sim_ab_Fraxis)


## Is it true "? 



# Originial Data Datos originales
plot(coefficients4$intercept, coefficients4$slope, 
     main = "Original vs Simulated", col = "blue", pch = 16, cex = 0.5,
     xlab = "Intercepto", ylab = "Slope")

# Add SImulations Agregar simulaciones
points(sim_ab_Fraxis$intercept, sim_ab_Fraxis$slope, col = rgb(1, 0, 0, 0.3), pch = 16)
legend("topright", legend = c("Original", "Simulated"),
       col = c("blue", "red"), pch = 16)




