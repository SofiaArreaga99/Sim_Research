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
minDBH9_1<-10*10 #From Jenkin´s *10 conversion to mm
maxDBH9_1<-55*10 #From Jenkin´s *10 conversion to mm
a_9_1<- 6.1371 
b_9_1<- -0.2785  
c_9_1<- 0.004286 
d_9_1<- 2

##CREATE 10,000 matrix

test9_1 <- matrix(rnorm(10000 * 1000), nrow = 10000, ncol = 1000)

##Create Random DBHs over the range sampled 
#mm unit

dbhTsugaCa_1 <- minDBH9_1 + (maxDBH9_1 - minDBH9_1) * runif(10000, min = 0, max = 1)

## CALCULATE BIOMASS

## calculate the biomass using the published equation form 
## biomass(kg) = a+b*dia+c*(dia^d)

meany9_1 <-a_9_1 + b_9_1*(dbhTsugaCa_1) + c_9_1*((dbhTsugaCa_1)^d_9_1)
#View(meany9_1)
## Introduce Random Error into calculated biomass ##
## this next part fuzzies the biomasses to create 1000 different populations ## 

ys9_1 <- matrix(rep(meany9_1, times = 1000), nrow = length(meany9_1), ncol = 1000)

# creating the steps used to generate wider ranges of fuzzed biomasses. This progression
# will create 1000 different biomass sets. It can be adjusted to larger or smaller steps
# as needed to match as close as possible the published R-square.

#stdevs <- seq(1, 200.8, by = 0.2)            
stdevs9_1 <- seq(0.1, 100, length.out=1000)  #works better

stdevs2_9_1 <- matrix(rep(stdevs9_1, each = 10000), nrow = 10000, ncol = length(stdevs9_1))  
dbh2_9_1 <- matrix(rep(dbhTsugaCa_1, times = 1000), nrow = length(dbhTsugaCa_1), ncol = 1000)
psuedys9_1 <- ys9_1 + stdevs2_9_1 * test9_1  # this makes the new biomases if no heteroscedasticity #

#psuedys9 <- ys9 + stdevs2_9 * test9 * dbh2_9 # This makes the new biomasses if heteroscedasticity

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

## Create figure for checking if result is reasonable ##

plot(log(dbhTsugaCa_1), BMTsugaCa_1, pch = 16, xlab = "Log DBH (mm)", ylab = "Biomass (kg)", main = "Tsuga Canadiensis")

# Write the data in an Excel file

PseudoDataTsugaCa1 <- data.frame(dbhTsugaCa_1, BMTsugaCa_1)
PseudoDataTsugaCa1 <- subset(PseudoDataTsugaCa1, BMTsugaCa_1>1)
PseudoDataTsugaCa1$eq <-"eq1_tsuga"

#View(PseudoDataTsugaCa1)
#plot(PseudoDataTsugaCa1$dbhTsugaCa, PseudoDataTsugaCa1$BMTsugaCa, pch = 16, xlab = "DBH (mm)", ylab = "Biomass (kg)", main = "Tsuga Canadiensis")
plot(PseudoDataTsugaCa1$dbhTsugaCa, PseudoDataTsugaCa1$BMTsugaCa,
     pch = 16,
     col = rgb(0.2, 0.4, 0.6, 0.5),  # azul semi-transparente
     xlab = "DBH (mm)", 
     ylab = "Biomass (kg)", 
     main = "Tsuga canadensis",
     cex = 1.2)  # tamaño de los puntos
grid()  # agrega rejilla

# Specifies the full path to save the file

write.csv(PseudoDataTsugaCa1, file = "TsugaCa.csv1", row.names = FALSE)

## print(sse)
## mean(rsq2)

########################### Second_Equation ####################################

# Range: 2 - 34 cm, Whole tree (above stump)
# First Step  CREATE 10,000 RANDOM DBH // Large Number DBH
# Equation 2 Ker 1980 a (New Brunswick)
# Units DBH cm and biomass kg 

rsqp9_2<-0.987 ##Published R^2 value 
minDBH9_2<-2 #From Jenkin´s *10 conversion to mm
maxDBH9_2<-34 #From Jenkin´s *10 conversion to mm
a_9_2<- 0.1617 
b_9_2<- 2.1536 


##CREATE 10,000 matrix

test9_2 <- matrix(rnorm(10000 * 1000), nrow = 10000, ncol = 1000)
#View(test9_2)

##Create Random DBHs over the range sampled 
#mm unit

dbhTsugaCa_2 <- minDBH9_2 + (maxDBH9_2 - minDBH9_2) * runif(10000, min = 0, max = 1)

## CALCULATE BIOMASS

## calculate the biomass using the published equation form 
## biomass(kg) = exp(a+b*log(dbh))

meany9_2 <-(a_9_2 + b_9_2*log(dbhTsugaCa_2))

#View(meany9_2)
## Introduce Random Error into calculated biomass ##
## this next part fuzzies the biomasses to create 1000 different populations ## 

ys9_2 <- matrix(rep(meany9_2, times = 1000), nrow = length(meany9_2), ncol = 1000)

# creating the steps used to generate wider ranges of fuzzed biomasses. This progression
# will create 1000 different biomass sets. It can be adjusted to larger or smaller steps
# as needed to match as close as possible the published R-square.

#stdevs <- seq(1, 200.8, by = 0.2)            

test9_2 <- pmin(pmax(test9_2, -1), 1)  # truncar a ±1σ ->This works 

stdevs9_2 <- seq(0.05, 0.15, length.out=1000)  #
stdevs2_9_2 <- matrix(rep(stdevs9_2, each = 10000), nrow = 10000, ncol = length(stdevs9_2))  
dbh2_9_2 <- matrix(rep(dbhTsugaCa_2, times = 1000), nrow = length(dbhTsugaCa_2), ncol = 1000)
psuedys9_2 <- ys9_2 + stdevs2_9_2 * test9_2  # this makes the new biomases if no heteroscedasticity #

#psuedys9 <- ys9 + stdevs2_9 * test9 * dbh2_9_2 # This makes the new biomasses if heteroscedasticity

#this makes the new biomasses with heteroscedasticity #

rsq2_9_2 <- numeric(1000)  # memory allocation is all, speeds up

for (i in 1:1000) {  # get stats on the datasets using the fit vs. psuedo-population
  
  sst9_2 <- sum((psuedys9_2[, i] - mean(psuedys9_2[, i]))^2)  # Total sum of squares
  sse9_2 <- sum((psuedys9_2[, i] - meany9_2)^2)  # Sum of squares of the error 
  rsq2_9_2[i] <- 1 - (sse9_2 / sst9_2)  # Coefficient of determination R²
}

## To search for closest dataset to the published R^2, subtract the R^2 from
## the R^2 of the equation and select the diff closest to zero.  
## This is vectorized saving a for loop

diffs9_2 <- abs(rsq2_9_2 - rsqp9_2)
I9_2 <- which.min(diffs9_2)  # Find the index of the minimum value
BMTsugaCa_2<- psuedys9_2[, I9_2]  # Select corresponding column

## Create figure for checking if result is reasonable ##

plot(log(dbhTsugaCa_2), BMTsugaCa_2, pch = 16, xlab = "Log DBH (cm)", ylab = "Biomass (kg)", main = "Tsuga Canadiensis")

# Write the data in an Excel file


PseudoDataTsugaCa2 <- data.frame(dbhTsugaCa_2, BMTsugaCa_2)
PseudoDataTsugaCa2 <- subset(PseudoDataTsugaCa2, BMTsugaCa_2>1)
PseudoDataTsugaCa2$eq <-"eq2_tsuga"

#View(PseudoDataTsugaCa2)
#plot(PseudoDataTsugaCa1$dbhTsugaCa, PseudoDataTsugaCa1$BMTsugaCa, pch = 16, xlab = "DBH (mm)", ylab = "Biomass (kg)", main = "Tsuga Canadiensis")
plot(PseudoDataTsugaCa2$dbhTsugaCa, PseudoDataTsugaCa2$BMTsugaCa,
     pch = 16,
     col = rgb(0.2, 0.4, 0.6, 0.5),  # azul semi-transparente
     xlab = "DBH (cm)", 
     ylab = "Biomass (kg)", 
     main = "Tsuga canadensis",
     cex = 1.2)  # tamaño de los puntos
grid()  # agrega rejilla

# Specifies the full path to save the file

write.csv(PseudoDataTsugaCa2, file = "TsugaCa2.csv", row.names = FALSE) 

## print(sse)
## mean(rsq2)


# Combine in just one data frame #
#SAME UNITS#

View(PseudoDataTsugaCa1)
PseudoDataTsugaCa1$dbhTsugaCa_1 <- PseudoDataTsugaCa1$dbhTsugaCa_1 / 10  #convertion to cm
PseudoDataTsugaCa2$BMTsugaCa_2 <- exp(PseudoDataTsugaCa2$BMTsugaCa_2) #Exp to biomass in kg

# SAME columns names # 
names(PseudoDataTsugaCa2)[1] <- "dbhTsugaCa_1"
names(PseudoDataTsugaCa2)[2] <- "BMTsugaCa_1"

PseudoDataTsugaCaTogether <- rbind(PseudoDataTsugaCa1, PseudoDataTsugaCa2)

