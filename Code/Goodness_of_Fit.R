# Goodness of fit :) 


# Example: 

#We have differents trees in a forest and we are expecting to have the same proportion 
#Ho= Equal distribution of "correct choices"
#Ha= Not equal distribution 

# Observed vs. Theoretical Data

# We are taking a sample of n= 100 of the whole population 

observed = c(20,20,25,35)
theo = c(1/4,1/4,1/4,1/4) #This value has to be in proportion

chisq.test (x=observed,
            p=theo)

# Two by two table  

# Crear tabla 2x2 con un tema de naturaleza
data <- matrix(c(25, 15,   # Con humedal: 25 con aves, 15 sin aves
                 8,  32),  # Sin humedal: 8 con aves, 32 sin aves
               nrow = 2,
               byrow = TRUE)

colnames(data) <- c("Aves_presentes", "Aves_ausentes")
rownames(data) <- c("Con_humedal", "Sin_humedal")

tabla <- as.table(data)
tabla

chisq.test(tabla, correct = TRUE)








