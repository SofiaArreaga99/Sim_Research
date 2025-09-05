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

#Ho= The wetlands and not wetlands have the same amount of birds
#Ha= The weltands and not wetlands doesn´t have the same amount of birds

# two by two table
data <- matrix(c(25, 15,   # Inside a wetland: 25 with birds, 15 without birds
                 8,  32),  # Outside a wetland: 8 with birds, 32 without birds
               nrow = 2,
               byrow = TRUE)

colnames(data) <- c("Birds_presence", "Birds_Not_Presence")
rownames(data) <- c("Inside_W", "Not_W")

table <- as.table(data)
table

chisq.test(table, correct = TRUE)

#Do we want more information?

prop.test(table)


# fisher test as a complement 

#Ho= The proportion of birds in wetlands is the same as the proportion in not wetlands
#Ha= The proportion is different 

fisher.test(table)





