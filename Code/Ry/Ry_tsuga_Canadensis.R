#Tsuga canadensis
#Simulate biomass values for a series of regional biomass equations
library(RColorBrewer)
color.pal <- brewer.pal(n=9, name="Set1")

#Simulate biomass allometric equations along the range of diameters used in the original
#publication. Atleast 10 points each.

#Monteith 1979 (New York)
#Range: 2.5 - 55 cm
eq1 <- data.frame(location="New York",
                  source="Monteith 1979",
                  dbh = seq(from=10, to=55, by=5))
eq1$est_kg <- 6.1371 - 0.2785*(eq1$dbh*10) + 0.004286*((eq1$dbh*10)^2)

#Young et al 1980 (Maine)
#Range: 2.54 - 50.8 cm
eq2 <- data.frame(location="Maine",
                  source="Young 1980",
                  dbh = seq(from=10, to=50, by=4))
eq2$est_kg <- exp(0.6803 + 2.3617*log(eq2$dbh/2.54))/2.205
#####
eq3 <- data.frame(location="New Brunswick",
                  source = "Ker 1980",
                  dbh = seq(from=10, to=34, by=3))
eq3$est_kg <- exp(-1.8223 + 2.1536*log(eq3$dbh))

#Combine the simulated pseudo-data and view in a scatter plot
psd_data <- rbind(eq1, eq2, eq3)
plot(est_kg ~ dbh, data=psd_data, pty=3, 
     main = "Tsuga canadensis", xlab = "DBH (cm)", ylab="Biomass (kg)")
plot(log(est_kg) ~ log(dbh), data=psd_data, pty=3,
     main = "Tsuga canadensis", xlab = "ln DBH", ylab="ln Biomass")

#Perform OLS regression on the ln transformed data
tsuga_new <- lm(log(est_kg) ~ log(dbh), data=psd_data)
summary(tsuga_new)
aov <- anova(tsuga_new)
b0 <- tsuga_new$coefficients[1]
b1 <- tsuga_new$coefficients[2]

#Assign MSE and RMSE values
MSE <- aov$`Mean Sq`[2]
RMSE <- sqrt(MSE)
(r2 <- summary(tsuga_new)$r.squared)

#Calculate and view the estimated relative error (E.R.E)
(ere <- exp(RMSE)) 

#Calculate and view the correction factor
(CF <- exp(MSE/2))

psd_data$new_est <- round(exp(b0 + b1*log(psd_data$dbh)), 2) * CF
psd_data$jenkins <- exp(-2.5384 + 2.4814*log(psd_data$dbh))
psd_data$abs_diff <- abs((psd_data$est_kg-psd_data$new_est)/psd_data$est_kg)
psd_data$jenk_diff <- abs((psd_data$jenkins-psd_data$new_est)/psd_data$jenkins)

#Calculate mean percent difference between NE equation all regional equations
(mean_diff <- mean(psd_data$abs_diff))

#Calcualte mean percent difference between NE equation and jenkins equation
(jenk_diff <- mean(psd_data$jenk_diff))

#Create sequence of dbh values (cm) along the range of balsam fir sizes in cfi data
dbh <- seq(from=5, to=95, by=5)

#Allometic Equations
monteith <- 6.1371 - 0.2785*(dbh*10) + 0.004286*((dbh*10)^2)
young <- exp(0.6803 + 2.3617*log(dbh/2.54))/2.205
ker <- exp(-1.8223 + 2.1536*log(dbh))
jenkins <- exp(-2.5384 + 2.4814*log(dbh))
new <- exp(b0 + b1*log(dbh)) * CF

#Plot new equation in comparison with original equations, along with fit statistcs
jpeg("C:/R Projects/Biomass_Modeling/Figures/Tsuga_canadensis.jpg", 
     width=18, height=10, units="in", res=72)
par(lwd=1.5, cex=1.5, cex.lab=1.5, cex.axis=1.25)
plot(new ~ dbh, type="l", ylim=c(0, max(jenkins)), col=color.pal[1], lwd=2.5,
     main="Tsuga canadensis", xlab="DBH (cm)", ylab="Biomass (kg)")
lines(dbh, monteith, lty=2, col=color.pal[2])
lines(dbh, young, lty=2, col=color.pal[3])
lines(dbh, ker, lty=2, col=color.pal[4])
lines(dbh, jenkins, col=color.pal[2], lty=1, lwd=2.5)

legend("topleft", 
       legend = c("New Equation", "Monteith", "Young", "Ker", "Jenkins"), 
       bty="n", lty=c(1,2,2,2,1),
       lwd = c(2.5, 1, 1,1, 2.5),
       col=c(color.pal[1:4], color.pal[2]))
text(x=5, y=4000, pos=4, labels=paste("CF:", round(CF, 2), sep=" "))
text(x=5, y=3700, pos=4, labels=paste("R-sq:", round(r2, 3), sep=" "))
text(x=5, y=3400, pos=4, labels=paste("E.R.E:", round(ere, 2), sep=" "))
text(x=5, y=3100, pos=4, labels=paste("%DIFF_ORIG:", round(mean_diff * 100, 1), "%", sep=" "))
text(x=5, y=2800, pos=4, labels=paste("%DIFF_Jenk:", round(jenk_diff * 100, 1), "%", sep=" "))
dev.off()
