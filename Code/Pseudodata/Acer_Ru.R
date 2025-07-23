#acer rubrum HacerRu

#Calling packages

library(fitdistrplus)
library(distr)
library(dplyr)

## First Step  CREATE 10,000 RANDOM DBH // Large Numer DBH


##Specie: acer rubrum HacerRu

rsqp5<-0.99 ##Published R^2 value 
minDBH5<-10 #From Jenkin´s
maxDBH5<-64 #From Jenkin´s
B0_5<- -2.1298 #From Ry´s paper
B1_5<- 2.3914  #From Ry´s paper
CF5<- 1.006 #Should we include it?

##CREATE 10,000 RANDOM DBH

test5 <- matrix(rnorm(10000 * 1000), nrow = 10000, ncol = 1000)

##Create Random DBHs over the range sampled 


#Cm unit

dbhAcerRu <- minDBH5 + (maxDBH5 - minDBH5) * runif(10000, min = 0, max = 1)

## CALCULATE BIOMASS##

## calculate the biomass using the published equation form

meany5 <- ((B0_5 + B1_5 * log(dbhAcerRu)))*CF5 #Should I multiply for the CF?

##Introduce Random Error into calculated biomass

##this next part fuzzies the biomasses to create 1000 different populations ## 

ys5 <- matrix(rep(meany5, times = 1000), nrow = length(meany5), ncol = 1000)

## creating the steps used to generate wider ranges of fuzzed biomasses. This progression
## will create 1000 different biomass sets. It can be adjusted to larger or smaller steps
## as needed to match as close as possible the published R-square.

#stdevs <- seq(1, 200.8, by = 0.2)             
stdevs5 <- seq(0.1, 100, length.out=1000)  #works better

stdevs2_5 <- matrix(rep(stdevs5, each = 10000), nrow = 10000, ncol = length(stdevs5))  
dbh2_5 <- matrix(rep(dbhAcerRu, times = 1000), nrow = length(dbhAcerRu), ncol = 1000)

psuedys5 <- ys5 + stdevs2_5 * test5 #this makes the new biomasses if no heteroscedasticity #

#psuedys5 <- ys5 + stdevs2_5 * test5 * dbh2_5

#this makes the new biomasses with heteroscedasticity #

rsq2_5 <- numeric(1000)  # memory allocation is all, speeds up

for (i in 1:1000) {  # get stats on the datasets using the fit vs. psuedo-population
  
  sst5 <- sum((psuedys5[, i] - mean(psuedys5[, i]))^2)  # Suma de cuadrados total // Total sum of squares
  sse5 <- sum((psuedys5[, i] - meany5)^2)  # Suma de cuadrados del error // Sum of squares of the error
  rsq2_5[i] <- 1 - (sse5 / sst5)  # Coeficiente de determinación R² // Coefficient of determination R²
}


## To search for closest dataset to the published R^2, subtract the R^2 from
## the R^2 of the equation and select the diff closest to zero.  
## This is vectorized saving a for loop


diffs5 <- abs(rsq2_5 - rsqp5)
I5 <- which.min(diffs5)  # Find the index of the minimum value
BMAcerRu<- psuedys5[, I5]  # Select corresponding column

## Create figure for checking if result is reasonable ##


plot(log(dbhAcerRu), BMAcerRu, pch = 16, xlab = "DBH (cm)", ylab = "Biomass (kg)", main = "Acer Rubrum")

# Write the data in an Excel file

PseudoDataAcerRu <- data.frame(dbhAcerRu, BMAcerRu)
PseudoDataAcerRu <- subset(PseudoDataAcerRu, BMAcerRu>1)

plot(PseudoDataAcerRu$dbhAcerRu, PseudoDataAcerRu$BMAcerRu, pch = 16, xlab = "DBH (cm)", ylab = "Biomass (kg)", main = "Acer Rubrum")




# Specifies the full path to save the file

write.csv(PseudoDataAcerRu, file = "AcerRu.csv", row.names = FALSE)

## print(sse)
## mean(rsq2)


## Logaritmic differences

noiter<-10000
coefficients5 <- data.frame(intercept=rep(NA,noiter),slope=rep(NA,noiter))
for(i in 1:noiter){
  datatofit<- sample_n(PseudoDataAcerRu,60,replace=FALSE) #Este es el que hay que cambiar 
  modelfit <- lm((BMAcerRu) ~ log(dbhAcerRu), data = na.omit(datatofit)) #Just add the other part
  
  coefficients5[i,] <- unname(coef(modelfit))
  
}

#Mean
InterAcer<-mean(coefficients5$intercept)
SlopeAcer<-mean(coefficients5$slope)


any(is.na(datatofit)) #NA revision in the data

#SD
SDInterAcer<-sd(coefficients5$intercept) #standar deviation intercept
SDSlopeAcer<-sd(coefficients5$slope)


#50th percentile
QinterAcerRu<-quantile(coefficients2$intercept, probs = 0.5)
QSlopeAcerRu<-quantile(coefficients2$slope, probs = 0.5)


View(coefficients5)
# Name columns for clarity            Nombrar columnas para claridad
colnames(coefficients5) <- c("intercept_AcerRu", "slope_AcerRu")
# Adding the correlative 
coefficients5$correlative <- seq_len(nrow(coefficients5))






#


library(ggplot2)

# Gráfico de dispersión entre intercepto y pendiente
ggplot(coefficients5, aes(x = slope, y = intercept)) +
  geom_point(alpha = 0.3, color = "darkblue") +
  geom_smooth(method = "lm", se = TRUE, color = "red") +
  labs(
    title = "Relación entre pendiente e intercepto en modelos alométricos",
    x = "Pendiente (slope)",
    y = "Intercepto (intercept)"
  ) +
  theme_minimal()

#


### NEW COVARIANCE ##

library(MASS)

cov_matrix_AcerRu <- cov(coefficients5)
mean_vector_AcerRu <- colMeans(coefficients5)


View(cov_matrix_AcerRu)

# Simulate new pairs of a and b      Simular nuevos pares de a y b
sim_ab_AcerRu <- as.data.frame(mvrnorm(n = 10000, mu = mean_vector_AcerRu, Sigma = cov_matrix_AcerRu))



# Name columns for clarity            Nombrar columnas para claridad
colnames(sim_ab_AcerRu) <- c("intercept_AcerRu", "slope_AcerRu")
sim_ab_AcerRu$correlative <- seq_len(nrow(sim_ab_AcerRu))
View(sim_ab_AcerRu)


## Is it true "? 



# Originial Data Datos originales
plot(coefficients5$intercept, coefficients5$slope, 
     main = "Original vs Simulated Acer Rubrum", col = "blue", pch = 16, cex = 0.5,
     xlab = "Intercepto", ylab = "Slope")

# Add SImulations Agregar simulaciones
points(sim_ab_AcerRu$intercept, sim_ab_AcerRu$slope, col = rgb(1, 0, 0, 0.3), pch = 16)
legend("topright", legend = c("Original", "Simulated"),
       col = c("blue", "red"), pch = 16)




