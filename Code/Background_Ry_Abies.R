#Abies balsamea
#Simulate biomass values for a set of regional biomass equations.
library(RColorBrewer)
color.pal <- brewer.pal(n=9, name="Set1")


#Simulate biomass allometric equations along the range of diameters used in the original
#publication. Atleast 10 points each.

#Baskerville 1965 (New Brunswick)
#Range:2.54 - 25.4 cm
eq1 <- data.frame(location = "New Brunswich",
                  source = "Baskerville 1965",
                  dbh=seq(from=6, to=25, by=2))
eq1$est_kg = (10^(0.086 + 2.53*(log10(eq1$dbh/2.54))))/2.205

View(eq1)


#Freedman et al 1982 (Nova Scotia)
#Range: 2.5 - 28.3 cm
eq2 <- data.frame(location = "Nova Scotia",
                  source = "Freedman et al 1982",
                  dbh=seq(from=6, to=25, by=2))
eq2$est_kg = 1.02 * exp(-2.2304 + 2.3263*(log(eq2$dbh)))

#Ker 1984
#Range: 0.1 - 40 cm
eq3 <- data.frame(location = "NC & NB",
                  source = "Ker 1984",
                  dbh=seq(from=13, to=40, by=3))
eq3$est_kg <- 0.1746*(eq3$dbh^(2.1555))

#Honer 1971 (Ontario)
#Range:10.16 - 33.02 cm.
#This equation gives the biomass of trees above a 6 in stump. 
eq4 <- data.frame(location = "Ontario",
                  source = "Honer 1971",
                  dbh=seq(from=12, to=30, by=2))
eq4$est_kg <- exp(0.4441 + 2.4975*log(eq4$dbh/2.54))/2.205


#Young et al 1980 (Maine)
#Range: 2.54 - 50.80cm
#This equation gives the biomass of trees above a 6 in stump.
eq5 <- data.frame(location = "Maine",
                  source = "Young et al 1980",
                  dbh=seq(from=14, to=50, by=4))
eq5$est_kg <- exp(0.5958 + 2.4017*(log(eq5$dbh/2.54)))/2.205


#Combine simulated pseudo-data and view in a scatter plot
psd_data <- rbind(eq1, eq2, eq3, eq4, eq5)
plot(est_kg ~ dbh, data=psd_data, pty=3, 
     main = "Abies balsamae", xlab = "DBH (cm)", ylab="Biomass (kg)")
plot(log(est_kg) ~ log(dbh), data=psd_data, pty=3,
     main = "Abies balsamae Transformed", xlab = "ln DBH", ylab="ln Biomass")


#Perform OLS regression on the ln transformed pseudodata
abies_new <- lm(log(est_kg) ~ log(dbh), data=psd_data)
summary(abies_new)
aov <- anova(abies_new)
b0 <- abies_new$coefficients[1]
b1 <- abies_new$coefficients[2]

#Assign MSE and RMSE values
MSE <- aov$`Mean Sq`[2]
RMSE <- sqrt(MSE)
(r2 <- summary(abies_new)$r.squared)

#Calculate and view the estimated relative error (E.R.E)
(ere <- exp(RMSE)) 

#Calculate and view the correction factor
(CF <- exp(MSE/2))

psd_data$new_est <- round(exp(b0 + b1*log(psd_data$dbh)), 2) * CF
psd_data$jenkins <- exp(-2.5385 + 2.4814*log(psd_data$dbh))
psd_data$abs_diff <- abs((psd_data$est_kg-psd_data$new_est)/psd_data$est_kg)
psd_data$jenk_diff <- abs((psd_data$jenkins-psd_data$new_est)/psd_data$jenkins)

#Calculate mean percent difference between NE equation all regional equations
(mean_diff <- mean(psd_data$abs_diff))

#Calcualte mean percent difference between NE equation and jenkins equation
(jenk_diff <- mean(psd_data$jenk_diff))

#Simulate new equation, original equations, and Jenkins equation along the range of
#tree diameters in the CFI dataset.

dbh <- seq(from=5, to=55, by=5)
#Allometric Equations
baskerville <- (10^(0.086 + 2.53*(log10(dbh/2.54))))/2.205
freedman <- (exp(-2.2304 + 2.3263*log(dbh)))*1.02
honer <- (exp(0.4441 + 2.4975*log(dbh/2.54)))/2.205
ker <- 0.1746*(dbh^2.1555) 
young <- exp(0.5958 + 2.4017*log(dbh/2.54))/2.205 
jenkins <- exp(-2.5385 + 2.4814*log(dbh))
new <- (exp(b0 + b1*log(dbh)))*CF

#Plot new equation in comparison with original equations, along with fit statistcs
jpeg("C:/R Projects/Biomass_Modeling/Figures/Abies_balsameae.jpg", 
     width=18, height=10, units="in", res=72)
par(mar=c(5,5,3,1), lwd=1.5, cex=2, cex.lab=1.5, cex.axis=1.5)
plot(new ~ dbh, type="l", ylim=c(0,max(jenkins)), col=color.pal[1], lwd=2.5,
     main="Abies balsamea", xlab="DBH (cm)", ylab="Biomass (kg)")
lines(dbh, baskerville, lty=2, col=color.pal[2])
lines(dbh, freedman, lty=2, col=color.pal[3])
lines(dbh, honer, lty=2, col=color.pal[4])
lines(dbh, ker, lty=2, col=color.pal[5])
lines(dbh, young, lty=2, col=color.pal[7])
lines(dbh, jenkins, col=color.pal[2], lty=1, lwd=2.5)

legend("bottomright", 
       legend = c("New Equation", "Baskerville", "Freedman", "Honer", "Ker", "Young", "Jenkins"), 
       bty="n", lty=c(1,2,2,2,2,2,1), cex=1,
       lwd = c(2.5, 1, 1, 1, 1, 1, 2.5),
       col=c(color.pal[1:5], color.pal[7], color.pal[2]))
text(x=5, y=1200, pos=4, labels=paste("CF:", round(CF, 2), sep=" "), cex=1)
text(x=5, y=1100, pos=4, labels=paste("R-sq:", round(r2, 2), sep=" "), cex=1)
text(x=5, y=1000, pos=4, labels=paste("E.R.E:", round(ere, 2), sep=" "), cex=1)
text(x=5, y=900, pos=4, labels=paste("%DIFF_ORIG:", round(mean_diff * 100, 1), "%", sep=" "), cex=1)
text(x=5, y=800, pos=4, labels=paste("%DIFF_Jenk:", round(jenk_diff * 100, 1), "%", sep=" "), cex=1)
dev.off()
